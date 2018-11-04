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

la <- subset(data,fips=='06037')
data.intersect <- merge(la,motorData)
data3 <- data.intersect %>% 
        group_by(year) %>% 
        summarise(sumEm = sum(Emissions))
data2$sumla <- data3$sumEm
names(data2) <- c('Years','Baltimore','Los.Angeles')
ggplot(data2,aes(Years,group=1))+
        geom_line(aes(y=Baltimore,color='Baltimore'))+
        geom_line(aes(y=Los.Angeles,color='Los Angeles'))+
        theme_linedraw()+
        ylab('Types of Sources')+
        xlab('Years')
