
#plot6.R
#initiation
library(ggplot2)
library(dplyr)
library(reshape2)
library(gridExtra)
#read data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#subset the identifiers of vehicle related data
onRoadClassifiers <- filter(SCC, 
                            grepl("^(on-road|onroad)$", 
                                  Data.Category, ignore.case = TRUE)) 
nonRoadClassifiers <- filter(SCC, 
                             grepl("^(non-road|nonroad)$", 
                                   Data.Category, ignore.case = TRUE))
#baltimore
onRoad <- NEI %>% filter(fips == 24510, SCC %in% onRoadClassifiers$SCC) %>% group_by(year)

nonRoad <- NEI %>% filter(fips == 24510, 
                          SCC %in% nonRoadClassifiers$SCC) %>% group_by(year)

onRoadSummary <- summarize_at(onRoad, vars(Emissions), sum)

nonRoadSummary <- summarize_at(nonRoad, vars(Emissions), sum)

g1 <- ggplot(onRoadSummary, aes(year, Emissions)) + geom_area(fill = "blue")
g1 <- g1 + coord_cartesian(ylim = c(0, 4800))
g1 <- g1 + ggtitle("On Road - Baltimore")
g2 <- ggplot(nonRoadSummary, aes(year, Emissions)) + geom_area(fill = "pink")
g2 <- g2 + coord_cartesian(ylim = c(0, 4800))
g2 <- g2 + ggtitle("Non Road - Baltimore")

#la
onRoadla <- NEI %>% filter(fips == "06037", SCC %in% onRoadClassifiers$SCC) %>% group_by(year)

nonRoadla <- NEI %>% filter(fips == "06037", 
                            SCC %in% nonRoadClassifiers$SCC) %>% group_by(year)

onRoadSummaryla <- summarize_at(onRoadla, vars(Emissions), sum)

nonRoadSummaryla <- summarize_at(nonRoadla, vars(Emissions), sum)

g3 <- ggplot(onRoadSummaryla, aes(year, Emissions)) + geom_area(fill = "blue")
g3 <- g3 + coord_cartesian(ylim = c(0, 4800))
g3 <- g3 + ggtitle("On Road - LA")
g4 <- ggplot(nonRoadSummaryla, aes(year, Emissions)) + geom_area(fill = "pink")
g4 <- g4 + coord_cartesian(ylim = c(0, 4800))
g4 <- g4 + ggtitle("Non Road - LA")

grid.arrange(g1, g2, g3, g4, nrow = 2)

dev.copy(png, file = "plot6.png")
dev.off()
