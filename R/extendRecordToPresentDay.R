extendRecordToPresentDay <- function(data) {
	
	year <- as.numeric(format(Sys.Date(), "%Y"))
	speedMph <- tail(data[["speedMph"]], n=1)
	newEntry <- data.frame(year, speedMph)
	data <- rbind(data, newEntry)
	
	return(data)
}