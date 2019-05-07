#plot2.R - baltimore city fips=="24510"
#read data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#make the years a factor
NEI$year <- as.factor(NEI$year)

#take baltimore only
sm0 <- subset(NEI, fips == "24510")

#total of emissions by year
sm1 <- with(sm0, tapply(Emissions, year, sum, na.rm = T))

#the plot of total pm2.5 emissions in Baltimore
plot(x = names(sm1), y = sm1, ylab = "PM2.5 Emissions", xlab = "Years")
title(main = "Total PM2.5 Emissions in Baltimore City by Year")
dev.copy(png, file = "plot2.png")
dev.off()