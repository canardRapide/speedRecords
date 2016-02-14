getMotorcyleSpeedRecordFromWiki <- function() {
	# Land Speed Record (Motorcycle)
	
	library(XML)
	library(RCurl)

	nHeaderLines <- 2
	url <- "https://en.wikipedia.org/wiki/Motorcycle_land-speed_record"
	tables <- readHTMLTable(getURL(url), header = nHeaderLines) 
	table <- tables[[1]]
	date <- as.vector(table$V1)
	speedRecordMph <- as.vector(table$V6)

	year <- vector()
	speedMph <- vector()
	for (it in (nHeaderLines+1):length(date)) {
		
		# Remove citations
		date[it] <- gsub("\\[.*\\]", "", date[it])

		# Get time in fraction of year
		if (nchar(date[it]) == 4) {
			epoch1900 <- as.POSIXlt(date[it], format = "%Y")
		} else {
			epoch1900 <- as.POSIXlt(date[it], format = "%B %d, %Y")
			# One entry had different date format
			if (is.na(epoch1900)) {
				epoch1900 <- as.POSIXlt(date[it], format = "%d %B %Y")
			}
		}
		fractionYear <- 1900 + epoch1900$year + epoch1900$yday/365;
		year <- append(year, fractionYear)
		
		# Remove citations
		fastestMph <- gsub("\\[.*\\]", "", speedRecordMph[it])
		speedMph <- append(speedMph, as.numeric(fastestMph))	
	}

	data <- data.frame(year, speedMph)
	return(data)
}