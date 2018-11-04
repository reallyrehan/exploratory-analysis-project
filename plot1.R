data <- readRDS('summarySCC_PM25.rds')
data$year <- as.factor(data$year)

library(dply)
data2 <- data %>% 
        group_by(year) %>% 
        summarise(sumEm = sum(Emissions))

barplot(height=data2$sumEm, col='red',names.arg=data2$year, xlab="years", ylab=expression('total PM'[2.5]*' emission'),main=expression('Total PM'[2.5]*' emissions at various years'))
