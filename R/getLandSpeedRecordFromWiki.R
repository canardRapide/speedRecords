getLandSpeedRecordFromWiki <- function() {
	# Land Speed Record (Wheel-driven)
	
	library(XML)
	library(RCurl)

	nHeaderLines <- 2
	url <- "https://en.wikipedia.org/wiki/Land_speed_record"
	tables <- readHTMLTable(getURL(url), header = nHeaderLines) 
	table <- tables[[2]]
	date <- as.vector(table$V1)
	speedOverKilometerMph <- as.vector(table$V6)
	speedOverMileMph <- as.vector(table$V8)

	year <- vector()
	speedMph <- vector()
	for (it in (nHeaderLines+1):length(date)) {
	
		# Get time in fraction of year
		epoch1900 <- as.POSIXlt(date[it], format = "%B %d, %Y")
		# One entry had different date format
		if (is.na(epoch1900)) {
			epoch1900 <- as.POSIXlt(date[it], format = "%d %B %Y")
		}
		fractionYear <- 1900 + epoch1900$year + epoch1900$yday/365;
		year <- append(year, fractionYear)
	
		# Get speed record
		if (speedOverKilometerMph[it] == "—") {
			# If speed over kilometer is a hyphen, columns shifted one entry left
			# Speed over mile is column V7 instead of V8
			fastestMph <- as.character(table[it,"V7"])
		} else if (speedOverKilometerMph[it] == "") {
			fastestMph <- speedOverMileMph[it]
		} else {
			fastestMph <- speedOverKilometerMph[it]
		}	
		# Remove footnote
		fastestMph <- gsub("\\[.*\\]", "", fastestMph)
		speedMph <- append(speedMph, as.numeric(fastestMph))
	}

	data <- data.frame(year, speedMph)
	return(data)
}