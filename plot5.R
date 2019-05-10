
#plot5.R
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
vehic <- subset(vehicle, fips == "24510")
#vehicles with emission more than 1 ton per year
heavy_v <- subset(vehic, Emissions >= 1)
#vehicles with emission less than 1 ton per year
normal_v <- subset(vehic, Emissions < 1)

#total emission each year
sm0 <- with(vehic, tapply(Emissions, year, sum, na.rm = T))
sm1 <- paste(as.integer(sm0), "Tons|")
cap <- paste("1999 Total:", sm1[1], "2002 Total:", sm1[2], "2005 Total:", 
             sm1[3], "2008 Total:", 
             sm1[4], sep = " ")


#Emission levels < 1 by SCC codes boxplot
g1 <- qplot(x = Emissions, data = normal_v, 
            facets = .~year) + labs(subtitle = "Boxplot: Yearly Emissions Below 1 Ton")
g1 <- g1 + ggtitle(cap) + theme(plot.title = element_text(size = 10))

#Emission levels >= 1 by SCC codes boxplot
g2 <- qplot(x = Emissions, data = heavy_v, 
            facets = .~year) + labs(subtitle = "Boxplot: Yearly Emissions Above 1 Ton")
grid.arrange(g1, g2, nrow = 2)

dev.copy(png, file = "plot5.png")
dev.off()
