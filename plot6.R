#plot6.R
#initiation
library(ggplot2)
library(dplyr)
library(reshape2)
#read data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#subset the identifiers of coal combustion related data
cdata <- unique(as.character(SCC$SCC[grepl("Vehicle", SCC$EI.Sector)]))

#subset the actual data
vehicle <- subset(NEI, SCC %in% cdata)

#subset for baltimore city
bal_vehic <- subset(vehicle, fips == "24510")

#subset for LA
la_vehic <- subset(vehicle, fips=="06037")

#baltimore emission sums
balsum <- aggregate(Emissions ~ year, data = bal_vehic, FUN = sum)
#means
balmean <- aggregate(Emissions ~ year, data = bal_vehic, FUN = mean)

#LA emission sums
lasum <- aggregate(Emissions ~ year, data = la_vehic, FUN = sum)
#means
lamean <- aggregate(Emissions ~ year, data = la_vehic, FUN = mean)

#total data
mydat <- cbind(balsum, balmean[,2], lasum[,2], lamean[,2])
colnames(mydat) <- c("year", "balsum", "balmean", "lasum", "lamean")

#Emission levels < 1 by SCC codes boxplot
g1 <- ggplot(balsum, aes(year, Emissions)) + geom_point() 
g1 <- g1 + geom_smooth(method = "lm") + coord_cartesian(ylim = c(0, 5000))
g1 <- g1 + ggtitle("Baltimore Total Emissions")
g2 <- ggplot(lasum, aes(year, Emissions)) + geom_point() 
g2 <- g2 + geom_smooth(method = "lm") + coord_cartesian(ylim = c(0, 5000))
g2 <- g2 + ggtitle("LA Total Emissions")
g3 <- ggplot(balmean, aes(year, Emissions)) + geom_point()
g3 <- g3 + geom_smooth(method = "lm") + coord_cartesian(ylim = c(0, 100))
g3 <- g3 + ggtitle("Baltimore Average Emissions")
g4 <- ggplot(lamean, aes(year, Emissions)) + geom_point()
g4 <- g4 + geom_smooth(method = "lm") + coord_cartesian(ylim = c(0 , 100))
g4 <- g4 + ggtitle("LA Average Emissions")
grid.arrange(g1, g2, g3, g4)

dev.copy(png, file = "plot6.png")
dev.off()
