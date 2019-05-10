#Copy and paste the R code file for the plot uploaded in the previous question.
NEI <- readRDS("summarySCC_PM25.rds") SCC <- readRDS("Source_Classification_Code.rds") library(dplyr) Data1999 <- NEI %>% filter(year==1999) Data2002 <- NEI %>% filter(year==2002) Data2005 <- NEI %>% filter(year==2005) Data2008 <- NEI %>% filter(year==2008)  TotalEmmissions <- c(sum(Data1999$Emissions), sum(Data2002$Emissions), sum(Data2005$Emissions), sum(Data2008$Emissions)) years <- unique(NEI$year) Total_Emission_DF <- data.frame(cbind(years, TotalEmmissions))  png("plot1.png") barplot(TotalEmmissions, col = c("lightblue", "mistyrose","lightcyan", "lavender")) axis(1, at = 1:4,labels = c(1999, 2002, 2005, 2008) ) title("Total PM2.5 Emissions") dev.off()

NEI <- readRDS("summarySCC_PM25.rds") SCC <- readRDS("Source_Classification_Code.rds") library(dplyr) library(ggplot2)  coal <- grep("coal", SCC$SCC.Level.Three, ignore.case = TRUE ) coal_SCC <-as.vector(SCC$SCC[coal]) coal_related <-NEI %>% filter(SCC %in% coal_SCC) coal_sum <- summarise(coal_related %>% group_by(year), sum = sum(Emissions))  png("plot4.png") ggplot(coal_sum, aes(year, sum)) + geom_area(fill= "lightcyan") dev.off()



onRoadClassifiers <- filter(classification, grepl("^(on-road|onroad)$", Data.Category, ignore.case = TRUE)) 
nonRoadClassifiers <- filter(classification, grepl("^(non-road|nonroad)$", Data.Category, ignore.case = TRUE))

onRoad <- summary %>% filter(fips == 24510, SCC %in% onRoadClassifiers$SCC) %>% group_by(year)

nonRoad <- summary %>% filter(fips == 24510, SCC %in% nonRoadClassifiers$SCC) %>% group_by(year)

onRoadSummary <- summarize_at(onRoad, vars(Emissions), sum)

nonRoadSummary <- summarize_at(nonRoad, vars(Emissions), sum)

