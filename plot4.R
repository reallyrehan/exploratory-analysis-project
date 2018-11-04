sourceData <- readRDS('Source_Classification_Code.rds')
data <- readRDS('summarySCC_PM25.rds')
data$year <- as.factor(data$year)

coalM <- grep('Coal|coal',sourceData$Short.Name)
coalData <- sourceData[coalM,]

data.intersect <- merge(data,coalData)
data2 <- data.intersect %>% 
        group_by(year) %>% 
        summarise(sumEm = sum(Emissions))
ggplot(data2,aes(year,sumEm))+
        geom_bar(stat="identity", fill="steelblue")+
        ylab('Coal Emissions')

