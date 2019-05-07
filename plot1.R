#initiation


#plot1.R
#read data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#make the years a factor
NEI$year <- as.factor(NEI$year)

#total of emissions by year
sm0 <- with(NEI, tapply(Emissions, year, sum, na.rm = T))

plot(x = names(sm0), y = sm0, ylab = "PM2.5 Emissions", xlab = "Years")
title(main = "Total PM2.5 Emissions in the US by Year")
dev.copy(png, file = "plot1.png")
dev.off()
