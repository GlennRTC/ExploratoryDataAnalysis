download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip", destfile = "/home/glenn/Documents/ExploratoryDataAnalysis/datafile.zip")
unzip("datafile.zip")
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

##QUESTIONS:

# 1-. Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
# Using the base plotting system, make a plot showing the total PM2.5 emission from
# all sources for each of the years 1999, 2002, 2005, and 2008.

# STEP 1 - CREATE A AGGREGATION OF EMISSIONS PER YEAR
EmissionsPY <- aggregate(Emissions ~ year, NEI, sum)
# STEP 2 - CREATE THE PLOT
plot(EmissionsPY$year, (EmissionsPY$Emissions)/10^6, type = "o", main = "United States PM2.5 Emissions per Year", xlab = "Year", ylab = "Total PM2.5 Emissions")

# 2-. Have total emissions from PM2.5 decreased in the Baltimore City, Maryland
# (fips=="24510") from 1999 to 2008? 
# Use the base plotting system to make a plot answering this question.

# STEP 1 - SUBSETTING AND AGGREGATION DATA FROM BALTIMORE = "24510"
BASub<- subset(NEI, NEI$fips == "24510")
BAEmiPY <- aggregate(Emissions ~ year, BASub, sum)
# STEP 2 - CREATE PLOT
plot(BAEmiPY$year, (BAEmiPY$Emissions)/10^6, type = "o", main = "Baltimore PM2.5 Emissions per Year", xlab = "Year", ylab = "Total PM2.5 Emissions (10^6)", col = c("blue", "green", "red", "green"))

# 3-. Of the four types of sources indicated by the (point, nonpoint, onroad, nonroad) 
# variable, which of these four sources have seen decreases in emissions from 1999–2008 
# for Baltimore City? Which have seen increases in emissions from 1999–2008? 
# Use the ggplot2 plotting system to make a plot answer this question.

# STEP 1 - AGGREGATING DATA FROM BALTIMORE USING PREVIOUS SUBSETTING
BASub<- subset(NEI, NEI$fips == "24510")
BAtypeEPY <- aggregate(Emissions ~ year + type, BASub, sum)
# STEP 2 - CREATING THE PLOT
library(ggplot2)
ggplot(BAtypeEPY,aes(year,Emissions, col = type, fill = type)) +
    geom_line() + 
    geom_point() +
    theme_bw() + guides(fill=FALSE) +
    facet_grid(.~type,scales = "free",space="free") +
    labs(x="year", y=expression("Total PM2.5 Emissions (10^6)")) +
    labs(title=expression("PM2.5 Emissions, Baltimore City 1999-2008 sorte by Source Type"))

# 4-. Across the United States, how have emissions from coal combustion-related 
# sources changed from 1999–2008?

# STEP 1 - SUBSETTING AND AGGREGATING
CoalComb <- SCC[grepl("coal", SCC$Short.Name, ignore.case = TRUE),]
NEI_CC <- NEI[NEI$SCC %in% CoalComb$SCC,]
CC_EPY <- aggregate(Emissions ~ year, NEI_CC, sum)
# STEP 2 - CREATING PLOT
library(ggplot2)
ggplot(CC_EPY, aes(year, Emissions/10^5)) +
    geom_line(col = "green") + 
    geom_point(col = "red", pch = 3, lwd = 4) +
    theme_bw() +  guides(fill=FALSE) +
    labs(x="year", y=expression("Total PM2.5 Emissions (10^5)")) + 
    labs(title=expression("United States PM2.5 Coal Combustion Source Emissions [1999-2008]"))

# 5-. How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?

# STEP 1 - SUBSETTING AND AGGREGATING
BASub_Motor <- subset(NEI, NEI$type == "ON-ROAD" & NEI$fips == "24510")
BA_MotorEPY <- aggregate(Emissions ~ year, BASub_Motor, sum)
# STEP 2 - CREATING PLOT
library(ggplot2)
ggplot(BA_MotorEPY, aes(year, Emissions)) +
    geom_line(col = "green") + 
    geom_point(col = "red", pch = 3, lwd = 4) +
    theme_bw() +  guides(fill=FALSE) +
    labs(x="year", y=expression("Total PM2.5 Emissions")) + 
    labs(title=expression("PM2.5 Emissions, Baltimore City by Motor Source [1999-2008]"))    

# 6-. Compare emissions from motor vehicle sources in Baltimore City with emissions from 
# motor vehicle sources in Los Angeles County, California (fips == "06037"). 
# Which city has seen greater changes over time in motor vehicle emissions?

# STEP 1 - SUBSETTING AND AGGREGATING
BASub_Motor <- subset(NEI, NEI$type == "ON-ROAD" & NEI$fips == "24510")
BA_MotorEPY <- aggregate(Emissions ~ year, BASub_Motor, sum)
BA_MotorEPY$city <- "BA"
CASub_Motor <- subset(NEI, NEI$type == "ON-ROAD" & NEI$fips == "06037")
CA_MotorEPY <- aggregate(Emissions ~ year, CASub_Motor, sum)
CA_MotorEPY$city <- "LA"
ComparCity <- rbind(BA_MotorEPY, CA_MotorEPY)

ggplot(ComparCity, aes(x=factor(year), y=Emissions, fill=city)) +
    geom_bar(aes(fill=year),stat="identity") +
    facet_grid(space="free", .~city) +
    guides(fill=FALSE) + theme_bw() +
    labs(x="year", y=expression("Total PM2.5 Emissions")) + 
    labs(title=expression("PM2.5 Emissions, BA vs LA by Motor Source [1999-2008]"))

