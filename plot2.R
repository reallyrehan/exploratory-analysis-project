data <- readRDS('summarySCC_PM25.rds')
data$year <- as.factor(data$year)


balt <- subset(data,fips=='24510')
balt2 <- balt %>% 
        group_by(year) %>% 
        summarise(sumEm = sum(Emissions))
barplot(height=balt2$sumEm, col='red',names.arg=balt2$year, xlab="years", ylab=expression('total PM'[2.5]*' emission'),main=expression('Total PM'[2.5]*' emissions at various years in Baltimore'))

