getWaterSpeedRecordFromWiki <- function() {
# Water Speed Record (Prop-driven and Jet Hydroplane)

	library(XML)
	library(RCurl)

	nHeaderLines <- 1
	url <- "https://en.wikipedia.org/wiki/Water_speed_record"
	tables <- readHTMLTable(getURL(url), header=nHeaderLines) 
	table <- tables[[2]]
	date <- as.vector(table$V5)
	speedRecordMph <- as.vector(table$V1)

	year <- vector()
	speedMph <- vector()
	for (it in (nHeaderLines+1):length(date)) {
	
		# Get time in fraction of year
		epoch1900 <- as.POSIXlt(date[it], format = "%d %B %Y")
		fractionYear <- 1900 + epoch1900$year + epoch1900$yday/365;
		year <- append(year, fractionYear)

		# Remove units and kilometer comment
		fastestMph <- gsub("mph \\(.*\\)", "", speedRecordMph[it])
		speedMph <- append(speedMph, as.numeric(fastestMph))	
	}

	data <- data.frame(year, speedMph)
	return(data)
}