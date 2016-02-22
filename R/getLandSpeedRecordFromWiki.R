getLandSpeedRecordFromWiki <- function() {
	# Land Speed Record (Wheel-driven)
	
	library(rvest)
	
	nHeaderLines <- 1
	url <- "https://en.wikipedia.org/wiki/Land_speed_record"
	
	tables <- html(url) %>% html_nodes(".wikitable") %>% html_table(fill = TRUE)
	table <- tables[[1]]
	date <- as.vector(table[[1]])
	speedOverKilometerMph <- as.vector(table[[6]])
	speedOverMileMph <- as.vector(table[[8]])
	
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
		if ((speedOverKilometerMph[it] == "—") || (speedOverKilometerMph[it] == "")) {
			# If speed over kilometer is a hyphen or empty, use speed over mile
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