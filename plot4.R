#plot4.R
#initiation
library(ggplot2)
library(reshape2)
#read data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#subset the identifiers of coal combustion related data
cdata <- unique(as.character(SCC$SCC[grepl("Coal", SCC$EI.Sector)]))

#subset the actual data
coal <- subset(NEI, SCC %in% cdata)

#means of emissions by year
mn0 <- melt(with(coal, tapply(Emissions, year, mean)))
colnames(mn0) <- c("year", "average_emission")

# the plot of average emission of coal cumbustion related sources by year
g <- ggplot(mn0, aes(year, average_emission))
g <- g + geom_point() + geom_smooth(method = "lm")
g <- g + labs(title = "Average Emission of Coal Cumbustion Related Sources")
print(g)
dev.copy(png, file = "plot4.png")
dev.off()