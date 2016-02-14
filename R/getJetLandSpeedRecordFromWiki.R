getJetLandSpeedRecordFromWiki <- function() {
# Land Speed Record (Jet and Rocket Propulsion)

	library(XML)
	library(RCurl)

	nHeaderLines <- 2
	url <- "https://en.wikipedia.org/wiki/Land_speed_record"
	tables <- readHTMLTable(getURL(url), header = nHeaderLines) 
	table <- tables[[3]]
	date <- as.vector(table$V1)
	speedOverKilometerMph <- as.vector(table$V6)
	speedOverMileMph <- as.vector(table$V8)
	
	speedMph <- vector()
	year <- vector()
	for (it in (nHeaderLines+1):length(date)) {
	
		# Get time in fraction of year
		epoch1900 <- as.POSIXlt(date[it], format = "%B %d, %Y")
		fractionYear = 1900 + epoch1900$year + epoch1900$yday/365;
		year <- append(year, fractionYear)
	
		# Get speed over mile
		if (speedOverKilometerMph[it] == "") {
			fastestMph <- speedOverMileMph[it]
		} else {
			fastestMph <- max( c(speedOverKilometerMph[it],
				speedOverMileMph[it]) )
		}
		# Remove citations
		fastestMph <- gsub("\\[.*\\]", "", fastestMph)
		speedMph <- append(speedMph, as.numeric(fastestMph))		
	}
	
	data <- data.frame(year, speedMph)
	return(data)
}