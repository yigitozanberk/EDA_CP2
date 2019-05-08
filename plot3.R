#plot3.R
#initiation
library(ggplot2)
library(dplyr)
library(reshape2)
#read data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#subset for baltimore city
mn0 <- subset(NEI, fips == "24510")

#means of different types on yearly basis
mn1 <- with(mn0, tapply(Emissions, list(year, type), mean))
#melt the data into tidy data
mt <- melt(mn1)
colnames(mt) <- c("year", "type", "value")

#plot the data of average yearly emissions of pm2.5 in Baltimore City by
# different types of emittors
g <- ggplot(mt, aes(year, value))
g <- g + geom_point() + facet_grid(.~ type) 
g <- g + geom_smooth(method = "lm") 
g <- g + labs(title = "Average Yearly Emissions of PM2.5 in Baltimore")
print(g)
dev.copy(png, file = "plot3.png")
dev.off()