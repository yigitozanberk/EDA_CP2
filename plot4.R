#plot4.R
#initiation
library(ggplot2)
library(dplyr)
library(reshape2)
#read data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#subset the identifiers of coal combustion related data
cdata <- unique(as.character(SCC$SCC[grepl("Coal", SCC$EI.Sector)]))

#subset the actual data
coal <- subset(NEI, SCC %in% cdata)

#total emission each year
sm0 <- with(coal, tapply(Emissions, year, sum, na.rm = T))
sm1 <- paste(as.integer(sm0), "Tons|")
cap <- paste("1999 Total:", sm1[1], "2002 Total:", sm1[2], "2005 Total:", 
             sm1[3], "2008 Total:", 
             sm1[4], sep = " ")

#look at unique sources
yr1 <- unique(subset(coal, year == 1999, c(fips, SCC)))
yr2 <- unique(subset(coal, year == 2002, c(fips, SCC)))
yr3 <- unique(subset(coal, year == 2005, c(fips, SCC)))
yr4 <- unique(subset(coal, year == 2008, c(fips, SCC)))

yr1 <- paste(yr1[, 1], yr1[, 2], sep = ".")
yr2 <- paste(yr2[, 1], yr2[, 2], sep = ".")
yr3 <- paste(yr3[, 1], yr3[, 2], sep = ".")
yr4 <- paste(yr4[, 1], yr4[, 2], sep = ".")

#fips.SCC combinations observed in all years
all1 <- intersect(yr1, yr2)
all2 <- intersect(yr3, yr4)
all4 <- intersect(all1, all2)

coal$fips.SCC <- with(coal, paste(fips, SCC, sep = "."))
c1 <- subset(coal, year == 1999 & fips.SCC %in% all4)
c2 <- subset(coal, year == 2002 & fips.SCC %in% all4)
c3 <- subset(coal, year == 2005 & fips.SCC %in% all4)
c4 <- subset(coal, year == 2008 & fips.SCC %in% all4) #there are duplicate rows
c5 <- aggregate(Emissions ~ fips.SCC + fips + SCC + Pollutant + type + year,data= c4,FUN=sum)

c1fips <- aggregate(Emissions ~ fips, data = c1, FUN = sum)
c2fips <- aggregate(Emissions ~ fips, data = c2, FUN = sum)
c3fips <- aggregate(Emissions ~ fips, data = c3, FUN = sum)
c4fips <- aggregate(Emissions ~ fips, data = c5, FUN = sum)

ctop <- merge(c1fips, c2fips, by = "fips")
ctop <- merge(ctop, c3fips, by = "fips")
colnames(ctop) <- c("fips", "Emissions1999", "Emissions2002", "Emissions2005")
ctop <- merge(ctop, c4fips, by = "fips")
colnames(ctop) <- c("fips", "Emissions1999", "Emissions2002", "Emissions2005",
                    "Emissions2008")

#total observations
tot <- aggregate(Emissions ~ year, data = coal, FUN = sum)

#plotting of prevalent county&source wise plots and overall changes
g1<- subset(ctop, Emissions1999 < 200)

par(mfrow = c(1,2), mar = c(6, 5, 2, 1), oma = c(0, 0, 2, 0))
with(g1, plot(rep(1999, 860), g1[, 2], xlim = c(1998, 2010), 
              ylim = c(0, 250), 
              main = "Emission Below 200 Tons",
              sub = "Coal Combustion-Related Sources",
              xlab = "Years 1999 - 2008",
              ylab = "PM2.5 Emission in Tons"))
with(g1, points(rep(2002, 860), g1[, 3]))
with(g1, points(rep(2005, 860), g1[, 4]))
with(g1, points(rep(2008, 860), g1[, 5]))
segments(rep(1999, 860), g1[,2], rep(2002, 860), g1[,3],
         col = rgb(red = 0, green = 0, blue = 1, alpha = 0.2))
segments(rep(2002, 860), g1[,3], rep(2005, 860), g1[,4],
         col = rgb(red = 0, green = 0, blue = 1, alpha = 0.2))
segments(rep(2005, 860), g1[,4], rep(2008, 860), g1[,5],
         col = rgb(red = 0, green = 0, blue = 1, alpha = 0.2))
mtext(cap, outer = TRUE, cex = 0.8)

#plot2
with(ctop, plot(rep(1999, 1103), ctop[, 2], xlim = c(1998, 2010), 
                ylim = c(0, 28000), 
                main = "All Sources - Emissions",
                sub = "Coal Combustion-Related Sources",
                xlab = "Years 1999 - 2008",
                ylab = "PM2.5 Emission in Tons"))
with(ctop, points(rep(2002, 1103), ctop[, 3]))
with(ctop, points(rep(2005, 1103), ctop[, 4]))
with(ctop, points(rep(2008, 1103), ctop[, 5]))
segments(rep(1999, 1103), ctop[,2], rep(2002, 1103), ctop[,3],
         col = rgb(red = 0, green = 0, blue = 1, alpha = 0.2))
segments(rep(2002, 1103), ctop[,3], rep(2005, 1103), ctop[,4],
         col = rgb(red = 0, green = 0, blue = 1, alpha = 0.2))
segments(rep(2005, 1103), ctop[,4], rep(2008, 1103), ctop[,5],
         col = rgb(red = 0, green = 0, blue = 1, alpha = 0.2))
mtext(cap, outer = TRUE, cex = 0.8)

dev.copy(png, file = "plot4.png")
dev.off()