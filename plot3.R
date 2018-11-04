data <- readRDS('summarySCC_PM25.rds')

data$year <- as.factor(data$year)


balt <- subset(data,fips=='24510')

library(ggplot2)
data$type <- as.factor(data$type)

data.by.group <- data %>% 
        group_by(year,type) %>% 
        summarise(sumEm = sum(Emissions))

library(tidyr)
wb <- spread(data = data.by.group, 
             key = type,
             value = sumEm)
names(wb) <- make.names(names(wb))

ggplot(wb,aes(year,group=1))+
        geom_line(aes(y=NON.ROAD,color='NON.ROAD'))+
        geom_line(aes(y=NONPOINT,color='NONPOINT'))+
        geom_line(aes(y=ON.ROAD,color='ON.ROAD'))+
        geom_line(aes(y=POINT,color='POINT'))+
        theme_linedraw()+
        ylab('Types of Sources')+
        xlab('Years')
        
balt.by.group <- balt %>% 
        group_by(year,type) %>% 
        summarise(sumEm = sum(Emissions))

library(tidyr)
wb <- spread(data = balt.by.group, 
             key = type,
             value = sumEm)
names(wb) <- make.names(names(wb))

ggplot(wb,aes(year,group=1))+
        geom_line(aes(y=NON.ROAD,color='NON.ROAD'))+
        geom_line(aes(y=NONPOINT,color='NONPOINT'))+
        geom_line(aes(y=ON.ROAD,color='ON.ROAD'))+
        geom_line(aes(y=POINT,color='POINT'))+
        theme_linedraw()+
        ylab('Types of Sources')+
        xlab('Years')
