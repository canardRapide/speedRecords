rm(list=ls())

source("R/getLandSpeedRecordFromWiki.R")
source("R/getJetLandSpeedRecordFromWiki.R")
source("R/getMotorcycleSpeedRecordFromWiki.R")
source("R/getWaterSpeedRecordFromWiki.R")
source("R/extendRecordToPresentDay.R")

# Speed of sound at sea level
speedOfSoundSeaLevelMph = 761.2

# Scrape speed records from Wikipedia tables
# Land speed record (wheel-driven)
landSpeedRecord <- getLandSpeedRecordFromWiki()

# Land speed record (jet and rocket propulsion)
jetLandSpeedRecord <- getJetLandSpeedRecordFromWiki()

# Motorcycle speed record
motorcycleSpeedRecord <- getMotorcyleSpeedRecordFromWiki()

# Water speed record (prop-driven and jet hydroplane)
waterSpeedRecord <- getWaterSpeedRecordFromWiki()
# Split into prop-driven and jet hydroplane boats
speedSplitMph <- 200
year <- waterSpeedRecord[["year"]]
speedMph <- waterSpeedRecord[["speedMph"]]
waterPropSpeedRecord <- data.frame(year[speedMph < speedSplitMph],
	speedMph[speedMph < speedSplitMph])
waterJetSpeedRecord <- data.frame(year[speedMph >= speedSplitMph],
	speedMph[speedMph >= speedSplitMph])
colnames(waterPropSpeedRecord) <- c("year", "speedMph")	
colnames(waterJetSpeedRecord) <- c("year", "speedMph")	

# Extend unbroken records to present day
landSpeedRecord <- extendRecordToPresentDay(landSpeedRecord)
jetLandSpeedRecord <- extendRecordToPresentDay(jetLandSpeedRecord)
waterPropSpeedRecord <- extendRecordToPresentDay(waterPropSpeedRecord)
waterJetSpeedRecord <- extendRecordToPresentDay(waterJetSpeedRecord)
motorcycleSpeedRecord <- extendRecordToPresentDay(motorcycleSpeedRecord)

# Plot and axes size
# Box size (inches)
heightIn <- 7;
widthIn <- 9;
# Axes tick size
tickIn <- 1/8;

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
# See http://research.stowers-institute.org/efg/R/Graphics/Basics/mar-oma/ for margin description
# Outer margin
par(oma = c(4,3,0.5,1))
# Margin
par(mar = c(4.5,11,4,1))

# Curves
# Wheel-driven
plot(landSpeedRecord[["year"]], landSpeedRecord[["speedMph"]],
	axes = FALSE, ann = FALSE,  
	type = "s", col = "darkseagreen", lwd = 3,
	xlim = c(xMin, xMax), ylim = c(yMin, yMax),
	xaxs = "i", yaxs = "i")
# Jet and Rocket Propulsion	
lines(jetLandSpeedRecord[["year"]], jetLandSpeedRecord[["speedMph"]],
	type = "s", col = "sandybrown", lwd = 3)
# Motorcycle
lines(motorcycleSpeedRecord[["year"]], motorcycleSpeedRecord[["speedMph"]], 
	type = "s", col = "gray", lwd = 3)	
# Prop-driven	
lines(waterPropSpeedRecord[["year"]], waterPropSpeedRecord[["speedMph"]],
	type = "s", col = "turquoise", lwd = 3)
# Jet Hydroplane
lines(waterJetSpeedRecord[["year"]], waterJetSpeedRecord[["speedMph"]],
	type = "s", col = "mediumpurple1", lwd = 3)
# Speed of Sound at Sea Level	
lines(c(xMin, xMax), c(speedOfSoundSeaLevelMph, speedOfSoundSeaLevelMph),
	lty = 2, col = "black", lwd = 2)

# Set axes
box()
# Bottom x-axis (year labels rotated 45 degrees)
axis(1, at = c(xMin + xStep*0:((xMax-xMin)/xStep), xMax), tck = tickIn/widthIn,
	labels = FALSE)
lablist <- as.vector(c(xMin + xStep*0:((xMax-xMin)/xStep), xMax))
text(c(xMin + xStep*0:((xMax-xMin)/xStep), xMax) - 3, par("usr")[3] - 20,
	labels = lablist, srt = 45, pos = 1, xpd = TRUE)
# Left y-axis	
axis(2, at = yMin + yStep*0:((yMax-yMin)/yStep), las = 2, tck = tickIn/heightIn,
	labels = TRUE, cex.axis = 1)
par(xpd = TRUE)
text(1875, 450, "Speed (mph)", srt = 90, cex = 1)
# Top x-axis ticks only
axis(3, at = c(xMin + xStep*0:((xMax-xMin)/xStep), xMax), tck = tickIn/widthIn,
	labels = FALSE)
# Right y-axis ticks only	
axis(4, at = yMin + yStep*0:((yMax-yMin)/yStep), las = 2, tck = tickIn/heightIn,
	labels = FALSE)

# Add additional y-axes for km/h and m/s
# km/h
kph2mph <- 0.621371
# y-axis 
yMin <- 0
yMax <- 1400
yStep <- 200
lablist <- as.vector(c(yMin + yStep*0:((yMax-yMin)/yStep)))	
axis(2, at = (yMin + yStep*0:((yMax-yMin)/yStep))*kph2mph, las = 2, tck = tickIn/heightIn,
	labels = lablist, col.axis = "red", line = 5, cex.axis = 0.8, col = "red")
text(1861, -50, "km/h", srt = 0, cex = 0.8, col = "red")
# m/s
mps2mph <- 2.23694
# y-axis 
yMin <- 0
yMax <- 400
yStep <- 50
lablist <- as.vector(c(yMin + yStep*0:((yMax-yMin)/yStep)))	
axis(2, at = (yMin + yStep*0:((yMax-yMin)/yStep))*mps2mph, las = 2, tck = tickIn/heightIn,
	labels = lablist, col.axis = "blue", line = 8.5, cex.axis = 0.8, col = "blue")
text(1846, -50, "m/s", srt = 0, cex = 0.8, col = "blue")

# Title and axes labels
text(1930, 1000, "Land and Water Speed Records", srt = 0, cex = 2)
title(xlab = "Year")

# Curve labels
text(1935, 320, "Wheel-driven Car",
	pos = 2, font = 2, col = "darkseagreen")
text(1985, 600, "Jet or Rocket\nPropulsion Car",
	pos = 1, font = 2, col = "sandybrown")
text(1995, 400, "Motorcycle", 
	pos = 1, font = 2, col = "gray")	
text(1977, 120, "Prop-driven Boat",
	pos = 4, font = 2, col = "turquoise")	
text(1993, 300, "Jet Hydroplane\nBoat",
	pos = 1, font = 2, col = "mediumpurple1")
text(1900, 730, "Speed of Sound at Sea Level",
	pos = 4, font = 2, col = "black", cex = 0.7)

# Markers
markerSize <- 0.6
textSize <- 0.7

# Interesting points
# Glenn Curtiss
points(motorcycleSpeedRecord[["year"]][2], motorcycleSpeedRecord[["speedMph"]][2],
	pch = 19, lwd = 1, cex = markerSize, col = "black")
text(1905, 180, "Glenn Curtiss\nCurtiss V-8\n(Unofficial)",
	pos = 4, font = 3, cex = textSize, col = "black")

# Henry Ford
points(landSpeedRecord[["year"]][6], landSpeedRecord[["speedMph"]][6],
	pch = 19, lwd = 1, cex = markerSize, col = "black")
text(1905.25, 110, "Henry Ford\nFord 999\nRacer",
	pos = 2, font = 3, cex = textSize, col = "black")
	
# First Electronic Timing	
points(landSpeedRecord[["year"]][8], landSpeedRecord[["speedMph"]][8],
	pch = 19, lwd = 1, cex = markerSize, col = "black")
text(1912, 30, "Electronic\nTiming",
	pos = 2, font = 3, cex = textSize*0.9, col = "black")	
lines(c(1908, landSpeedRecord[["year"]][8]), c(60, landSpeedRecord[["speedMph"]][8]-10),
	lty = 1, col = "black", lwd = 0.5)

# First 2-way Record
points(landSpeedRecord[["year"]][9], landSpeedRecord[["speedMph"]][9],
	pch = 19, lwd = 1, cex = markerSize, col = "black")
text(1912, 30, "2-way\nRecord",
	pos = 4, font = 3, cex = textSize*0.9, col = "black")
lines(c(1916, landSpeedRecord[["year"]][9]), c(60, landSpeedRecord[["speedMph"]][9]-10),
	lty = 1, col = "black", lwd = 0.5)	
		
# Craig Breedlove
points(jetLandSpeedRecord[["year"]][1], jetLandSpeedRecord[["speedMph"]][1],
	pch = 19, lwd = 1, cex = markerSize, col = "black")
text(1965, 435, 'Craig Breedlove\nSpirit of America',
	pos = 2, font = 3, cex = textSize, col = "black")
	
# Andy Green
points(jetLandSpeedRecord[["year"]][length(jetLandSpeedRecord[["year"]])-1],
	jetLandSpeedRecord[["speedMph"]][length(jetLandSpeedRecord[["year"]])-1],
	pch = 19, lwd = 1, cex = markerSize, col = "black")
text(2000, 790, 'Andy Green\nThrustSSC',
	pos = 2, font = 3, cex = textSize, col = "black")
	
# Donald Campbell
points(waterJetSpeedRecord[["year"]][1:7], waterJetSpeedRecord[["speedMph"]][1:7],
	pch = 19, lwd = 1, cex = markerSize, col = "royalblue")
text(1958, 250, "Donald Campbell\nBluebird K7",
	pos = 2, font = 3, cex = textSize, col = "royalblue")
	
# Gar Wood
points(waterPropSpeedRecord[["year"]][14], waterPropSpeedRecord[["speedMph"]][14],
	pch = 19, lwd = 1, cex = markerSize, col = "black")
text(1931, 60, 'Gar Wood\nMiss America X\n(Single-Keel)',
	pos = 4, font = 3, cex = textSize, col = "black")
	
# Ken Warby
points(waterJetSpeedRecord[["year"]][length(waterJetSpeedRecord[["year"]])-1],
	waterJetSpeedRecord[["speedMph"]][length(waterJetSpeedRecord[["year"]])-1],
	pch = 19, lwd = 1, cex = markerSize, col = "black")
text(1982, 350, 'Ken Warby\nSpirit of Australia',
	pos = 2, font = 3, cex = textSize, col = "black")

# Write pdf
dev.copy2pdf(device = quartz, file = "recordsVsTime.pdf",
	width = widthIn, height = heightIn, paper = "USr")


