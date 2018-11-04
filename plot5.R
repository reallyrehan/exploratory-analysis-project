sourceData <- readRDS('Source_Classification_Code.rds')
data <- readRDS('summarySCC_PM25.rds')
data$year <- as.factor(data$year)


balt <- subset(data,fips=='24510')


motorM <- grep('motor|Motor',sourceData$Short.Name)
motorData <- sourceData[motorM,]

data.intersect <- merge(balt,motorData)
data2 <- data.intersect %>% 
        group_by(year) %>% 
        summarise(sumEm = sum(Emissions))
ggplot(data2,aes(year,sumEm))+
        geom_bar(stat="identity", fill="steelblue")+
        ylab('Motor Emissions')
