rm(list=ls())

source("R/getLandSpeedRecordFromWiki.R")
source("R/getJetLandSpeedRecordFromWiki.R")
source("R/getWaterSpeedRecordFromWiki.R")
source("R/getMotorcycleSpeedRecordFromWiki.R")

# Scrape speed records from Wikipedia tables
# Land speed record (wheel-driven)
landSpeedRecord <- getLandSpeedRecordFromWiki()

# Land speed record (jet and rocket propulsion)
jetLandSpeedRecord <- getJetLandSpeedRecordFromWiki()

# Water speed record (prop-driven and jet hydroplane)
waterSpeedRecord <- getWaterSpeedRecordFromWiki()

# Motorcycle speed record
motorcycleSpeedRecord <- getMotorcyleSpeedRecordFromWiki()

# Speed of sound at sea level
speedOfSoundSeaLevelMph = 761.2

# Plot and axes size
# Box size (inches)
heightIn <- 6;
widthIn <- 7;
ticIn <- 1/8;

# x-axis 
xMin <- 1890
xMax <- 2016
xStep <- 10

# y-axis 
yMin <- 0
yMax <- 900
yStep <- 100

# Plot 
quartz(width = widthIn, height = heightIn)
# Wheel-driven
plot(landSpeedRecord[["year"]], landSpeedRecord[["speedMph"]],
	axes = FALSE, ann = FALSE,  
	type = "l", col = "darkseagreen", lwd = 3,
	xlim = c(xMin, xMax), ylim = c(yMin, yMax), xaxs = "i", yaxs = "i")
# Jet and Rocket Propulsion	
lines(jetLandSpeedRecord[["year"]], jetLandSpeedRecord[["speedMph"]],
	type = "l", col = "sandybrown", lwd = 3)
# Motorcycle
lines(motorcycleSpeedRecord[["year"]], motorcycleSpeedRecord[["speedMph"]], 
	type = "l", col = "gray", lwd = 3)	
# Prop-driven	
year <- waterSpeedRecord[["year"]]
speed <- waterSpeedRecord[["speedMph"]]
lines(year[speed < 200], speed[speed < 200],
	type = "l", col = "turquoise", lwd = 3)
# Jet Hydroplane
lines(year[speed > 200], speed[speed > 200],
	type = "l", col = "mediumpurple1", lwd = 3)
# Speed of Sound at Sea Level	
lines(c(xMin, xMax), c(speedOfSoundSeaLevelMph, speedOfSoundSeaLevelMph),
	lty = 2, col = "black", lwd = 2)

# Set axes
box()
#axis(1, at = c(xMin + xStep*0:((xMax-xMin)/xStep), xMax), tck = ticIn/widthIn,
#	cex.axis = 0.5)
axis(1, at = c(xMin + xStep*0:((xMax-xMin)/xStep), xMax), tck = ticIn/widthIn,
	labels = FALSE)
lablist <- as.vector(c(xMin + xStep*0:((xMax-xMin)/xStep), xMax))
text(c(xMin + xStep*0:((xMax-xMin)/xStep), xMax) - 3, par("usr")[3] - 20,
	labels = lablist, srt = 45, pos = 1, xpd = TRUE)
axis(2, at = yMin + yStep*0:((yMax-yMin)/yStep), las = 2, tck = ticIn/heightIn)

# Title and axes labels
title("Land and Water Speed Records")
title(xlab = "Year")
title(ylab = "Speed (mph)")

# Curve labels
text(1935, 320, "Wheel-driven Car",
	pos = 2, font = 2, col = "darkseagreen")
text(1985, 600, "Jet or Rocket\nPropulsion Car",
	pos = 1, font = 2, col = "sandybrown")
text(1908, 200, "Motorcycle", 
	pos = 1, font = 2, col = "gray")	
text(1945, 120, "Prop-driven Boat",
	pos = 4, font = 2, col = "turquoise")
text(1993, 300, "Jet Hydroplane\nBoat",
	pos = 1, font = 2, col = "mediumpurple1")
text(1900, 730, "Speed of Sound at Sea Level",
	pos = 4, font = 2, col = "black", cex = 0.7)

# Markers
markerSize <- 0.6
textSize <- 0.7

# Henry Ford
points(landSpeedRecord[["year"]][6], landSpeedRecord[["speedMph"]][6],
	pch = 19, lwd = 1, cex = markerSize, col = "black")
text(1903, 50, "Henry Ford\nFord 999 Racer",
	pos = 4, font = 3, cex = textSize, col = "black")
# Craig Breedlove
points(jetLandSpeedRecord[["year"]][1], jetLandSpeedRecord[["speedMph"]][1],
	pch = 19, lwd = 1, cex = markerSize, col = "black")
text(1965, 435, 'Craig Breedlove\nSpirit of America',
	pos = 2, font = 3, cex = textSize, col = "black")
# Andy Green
points(tail(jetLandSpeedRecord[["year"]], 1), tail(jetLandSpeedRecord[["speedMph"]], 1),
	pch = 19, lwd = 1, cex = markerSize, col = "black")
text(2000, 790, 'Andy Green\nThrustSSC',
	pos = 2, font = 3, cex = textSize, col = "black")
	
# Donald Campbell
points(waterSpeedRecord[["year"]][21:27], waterSpeedRecord[["speedMph"]][21:27],
	pch = 19, lwd = 1, cex = markerSize, col = "royalblue")
text(1958, 250, "Donald Campbell\nBluebird K7",
	pos = 2, font = 3, cex = textSize, col = "royalblue")
# Gar Wood
points(waterSpeedRecord[["year"]][14], waterSpeedRecord[["speedMph"]][14],
	pch = 19, lwd = 1, cex = markerSize, col = "black")
text(1931, 60, 'Gar Wood\nMiss America X\n(Single-Keel)',
	pos = 4, font = 3, cex = textSize, col = "black")
# Ken Warby
points(tail(waterSpeedRecord[["year"]], 1), tail(waterSpeedRecord[["speedMph"]], 1),
	pch = 19, lwd = 1, cex = markerSize, col = "black")
text(1982, 350, 'Ken Warby\nSpirit of Australia',
	pos = 2, font = 3, cex = textSize, col = "black")

# Write pdf
dev.copy2pdf(device = quartz, file = "recordsVsTime.pdf",
	width = widthIn, height = heightIn, paper = "USr")

#dev.off()


