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

#mark sources with emissions greater than 20 tons per year
mn1 <- mutate(mn0, treshold = Emissions > 20)

#take mean values accordingly
mn2 <- with(mn1, tapply(Emissions, list(year, type, treshold), mean))
#melt the data into tidy data
mt <- melt(mn2)
colnames(mt) <- c("year", "type", "treshold_ID" ,"value")

#Total emissions
sm0 <- with(mn0, tapply(Emissions, year, sum, na.rm = T))
sm1 <- paste(as.integer(sm0), "Tons|")
cap <- paste("1999 Total:", sm1[1], "2002 Total:", sm1[2], "2005 Total:", 
             sm1[3], "2008 Total:", 
             sm1[4], sep = " ")




#plot the data of average yearly emissions of pm2.5 in Baltimore City by
# different types of emittors
g <- ggplot(mt, aes(year, value))
g <- g + geom_point() + facet_grid(treshold_ID ~ type, scales = "free_y") 
g <- g + geom_smooth(method = "lm") 
g <- g + labs(title = "Average Yearly Emissions of PM2.5 in Baltimore", 
              subtitle = "Sources of Above(Bottom Row) and Below(Top Row) 20 Tons per Year",
              caption = cap) + ylab("PM2.5 Emission in Tons")
print(g)
dev.copy(png, file = "plot3.png")
dev.off()