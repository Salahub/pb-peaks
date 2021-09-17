library("readxl")
library("maps")

dadsSheet <- "BluelogOct2020.xlsx"

regextr <- function(pattern, x, ...) {
    mtch <- regmatches(x, regexec(pattern, x, ...))
    sapply(mtch, function(el) if (length(el) != 0) el else NA)
}

formDates <- function(x) {
    if (is(x, "POSIXct") | is(x, "POSIXlt") | is(x, "Date")) {
        as.numeric(format(x, "%H")) +
            as.numeric(format(x, "%M"))/60
    } else x
}

preProcess <- function(data) {
    varnms <- c("ispeak", "date", "peak", "elev", "gain", "timeout",
                "timeback") # some column names
    naRows <- apply(data, 1, function(row) all(is.na(row)))
    new <- data[!naRows, 1:7]
    colnames(new) <- varnms
    new$timeout <- formDates(new$timeout)
    new$timeback <- formDates(new$timeback)
    new$ispeak <- !is.na(new$ispeak)
    new
}

regNames <- function(names) {
    nms <- gsub("Mt\\.|Mt", "Mount", names)
    nms <- gsub("Mtn|Mountn|Mountn\\.", "Mountain", nms)
    nms <- gsub(" \\(.+\\)?", "", nms)
    nms <- gsub("St\\. |St ", "Saint ", nms)
    nms
}

getPeaks <- function(ids = (-1e5):1e5,
                     failConn = "failed.txt") {
    pagePat <- "<h1>" # identifier of data section
    failPat <- scan(file(failConn), character()) # failure pattern
    n <- length(ids)
    pburl <- "https://peakbagger.com/peak.aspx?pid=" # query url
    pause <- 0.01 # system sleep time
    data <- character(n)
    for (ii in seq_along(ids)) {
        id <- ids[ii]
        cat(paste("Trying", id, "| ")) # status update
        conn <- url(paste0(pburl, id))
        tryPage <- tryCatch(scan(conn, character(), sep = "\n"),
                            error = function(err) {
                                scan(file(failConn),
                                     character())}) # wifi drop
        page <- tryPage[grepl(pagePat, tryPage)] # get relevant part
        data[ii] <- page # store
    }
    failures <- data == failPat # identify failures
    list(data = data[!failures], failed = ids[failures])
}

extractPeaks <- function(pages) {
    ## small helper function
    metricGet <- function(el) if (length(el) == 2) el[2] else el[1]
    ## define useful regex patterns
    latlonPat <- "(?<=<br\\/>)[0-9\\.\\,\\s\\-]+?(?= \\(Dec Deg\\))"
    namePat <- "(?<=<h1>).+?(?=<\\/h1>)"
    elevPat <- "(?<=Elevation\\: ).+?(?= meters)"
    isolPat <- "(?<=True Isolation\\: ).+?(?= km)"
    cntryPat <- "(?<=Country<\\/td><td>).+?(?=<\\/td>)"
    statePat <- "(?<=State\\/Province<\\/td><td>).+?(?=</td>)"
    ascentPat <- "(?<=Ascent Info<\\/b><br\\/><br\\/>).+?(?=<br\\/>)"
    visitPat <- "(?<=This page has been served )[0-9]+"
    patterns <- c(latlon = latlonPat, name = namePat,
                  elevation = elevPat, isolation = isolPat,
                  country = cntryPat, region = statePat,
                  ascents = ascentPat, visits = visitPat)
    ## extract patterns
    extracts <- lapply(patterns, regextr, x = pages, perl = TRUE)
    ## additional processing
    splitName <- strsplit(extracts$name, ", ")
    name <- iconv(sapply(splitName, function(el) el[1]),
                  from = "UTF-8", to = "latin1")
    splitLatLon <- strsplit(extracts$latlon, ", ")
    lat <- as.numeric(sapply(splitLatLon, function(el) el[1]))
    lon <- as.numeric(sapply(splitLatLon, function(el) el[2]))
    splitElev <- strsplit(extracts$elevation, ", ")
    elevChar <- sapply(splitElev, metricGet)
    elev <- as.numeric(gsub("\\+", "", elevChar))
    splitIsol <- strsplit(extracts$isolation, ", ")
    isol <- as.numeric(sapply(splitIsol, metricGet))
    asc <- as.numeric(regextr("[0-9]+", extracts$ascents))
    asc[grepl("No ascents", extracts$ascents)] <- 0
    ## clean up countries/regions
    cleaner <- function(el, pat) iconv(gsub(el, pattern = pat,
                                            replacement = ""),
                                       from = "UTF-8",
                                       to = "latin1")
    countries <- strsplit(extracts$country, "<br\\/>")
    highest <- lapply(countries, grepl, pattern = "\\(Highest Point\\)")
    countries <- lapply(countries, cleaner,
                        pat = "\\(Highest Point\\)")
    regions <- strsplit(extracts$region, "<br\\/>")
    regions <- lapply(regions, cleaner,
                      pat = "\\(Highest Point\\)")
    ## output data
    list(name = name, countries = countries, region = regions,
         highest = highest, lat = lat, lon = lon, elevation = elev,
         isolation = isol, ascents = asc,
         urlvisits = as.numeric(extracts$visits))
}

## get database peaks
if (!("pbPeakPages.RDS" %in% list.files())) {
    pbPeakPages <- getPeaks()
} else pbPeakPages <- readRDS("pbPeakPages.RDS")

## extract database peaks
pbPeaks <- extractPeaks(pbPeakPages)
pbPeaks$elevation[100002] <- 380 # manual hack for Moon Hill, China
nonNull <- which(!is.na(pbPeaks$name) & pbPeaks$name != "Invalid Peak ID" &
                 pbPeaks$name != "Failed")
pbPeaks <- lapply(pbPeaks, function(feature) feature[nonNull])

## dad's peaks
sheetNames <- excel_sheets(dadsSheet) # get names
messy <- lapply(setNames(sheetNames, sheetNames),
                read_excel, path = dadsSheet) # load list of sheets
years <- grepl("[0-9]{4}", names(messy)) # sheets with peaks
prePeaks <- lapply(messy[years], preProcess)
peaks <- do.call(rbind, prePeaks)
peaks$elevNum <- as.numeric(peaks$elev)
peaks$peak <- regNames(peaks$peak) # regularize names
peakInds <- which(peaks$ispeak & !is.na(peaks$peak))
onlyPeaks <- peaks[peakInds,]

## can we match them to the peakbagger data?
factorPeaks <- factor(onlyPeaks$peak)
allMatches <- sapply(levels(factorPeaks),
                     function(nm) which(nm == pbPeaks$name))
numMatches <- sapply(allMatches, length)
noMatch <- levels(factorPeaks)[numMatches == 0]
anyMatch <- allMatches[numMatches >= 1]
## try to get the "best" candidates among the many match peaks
bestCand <- function(elev, cands, tol = 0.1) {
    diffs <- abs(elev - pbPeaks$elevation[cands])
    ord <- order(diffs)
    valid <- diffs[ord]/elev <= tol
    cands[ord[valid]][1]
}
anyBest <- sapply(names(anyMatch),
                  function(nm) bestCand(mean(onlyPeaks$elevNum[onlyPeaks$peak == nm],
                                             na.rm = TRUE),
                                        anyMatch[[nm]]))
onlyPeaks$inPB <- onlyPeaks$peak %in% c(names(anyBest)[!is.na(anyBest)])

## get the lat/lon data
matchLatLonEl <- sapply(anyBest[!is.na(anyBest)],
                        function(ind) {
                            c(pbPeaks$lat[ind],
                              pbPeaks$lon[ind],
                              pbPeaks$elevation[ind])
                        })
## add a few manual ones
matchLatLonEl <- cbind(matchLatLonEl,
                       "Nagartsang Peak" = c(27.80907, 86.71369, 5083),
                       "Illizinas Norte" = c(-0.64947, -78.72079, 5126),
                       "Shymbulak" = c(43.124799, 77.116385, 3450),
                       "Hohtälli" = c(45.988992, 7.802662, 3273),
                       "Altai Peak (2585)" = c(49.80746, 86.59011, 2585))

## list of peaks done together
withMe <- c("Prairie Mountain", "Lagazuoi Piccolo", "Mount Rae",
            "Ampersand Mountain", "Ha Ling Peak", "Heart Mountain",
            "Angels Landing", "Peitlerkofel", "Mauna Kea")

## relate these back to dad's data
match2peaks <- match(colnames(matchLatLonEl), onlyPeaks$peak)

## the plots (tiff format?)
png("DavesPeaks.png", width = 60, height = 45, units = "cm", res = 144)
tiff("DavesPeaks.tif", width = 60, height = 45, units = "cm", res = 216)
par(fig = c(0,1,0.5,1), new = TRUE)
## elevation by date
pal <- c("firebrick", "black")
with(onlyPeaks, plot(date, elevNum, type = "n", xlab = "Date",
     ylab = "Elevation (m)", main = "Elevation over the Years"))
for (ii in 1:nrow(onlyPeaks)) {
    tempElev <- onlyPeaks$elevNum[ii]
    if (is.na(tempElev)) {
        tempElev <- 0
        tempPch <- 1
    } else {
        tempPch <- 20
    }
    tempDate <- onlyPeaks$date[ii]
    tempCol <- pal[2 - onlyPeaks$inPB[ii]]
    if (tempElev < 0) tempCol <- "steelblue"
    lines(rep(tempDate, 2), c(0, tempElev),
          col = adjustcolor(tempCol, alpha.f = 0.2))
    points(tempDate, tempElev, pch = tempPch, col = tempCol)
    if (!is.na(tempElev) & tempElev > 5000) {
        text(tempDate, tempElev, onlyPeaks$peak[ii], pos = 4,
             cex = 0.75)
    }
}
## world map with matches
theMap <- map("world", plot = FALSE)
backyard <- list(xleft = -130, ybottom = 30, xright = -100,
                 ytop = 55)
scaling <- 2.5
par(fig = c(0,0.67,0,0.5), new = TRUE)
pal <- sapply(c("firebrick", "steelblue"), adjustcolor, alpha.f = 0.2)
plot(NA, xlim = c(-180,180), ylim = c(-90,90), xaxs = "i", yaxs = "i",
     xaxt = "n", yaxt = "n", xlab = "", ylab = "", frame.plot = FALSE,
     main = "My Dad's Peaks")
lines(theMap, col = "gray50")
do.call(rect, backyard)
pointCols <- pal[1 + (matchLatLonEl[3,] < 0)]
#pointCols[colnames(matchLatLonEl) %in% withMe] <- adjustcolor("seagreen", alpha.f = 0.2)
points(matchLatLonEl[2,], matchLatLonEl[1,], pch = 24,
       cex = scaling*sqrt(abs(matchLatLonEl[3,]/max(matchLatLonEl[3,]))),
       bg = pointCols,
       col = "gray20")
## local map with matches
par(fig = c(0.67,1,0,0.5), new = TRUE)
plot(NA, xlim = c(backyard$xleft, backyard$xright),
     ylim = c(backyard$ybottom, backyard$ytop), xaxs = "i", yaxs = "i",
     xaxt = "n", yaxt = "n", xlab = "", ylab = "", frame.plot = TRUE,
     main = "The Backyard")
lines(theMap, col = "gray50")
points(matchLatLonEl[2,], matchLatLonEl[1,], pch = 24,
       cex = scaling*sqrt(abs(matchLatLonEl[3,]/max(matchLatLonEl[3,]))),
       bg = pointCols)
dev.off()

