getwd()


setwd('./Documents/coursera/Course 4 - Exploratory Data Analysis/Course Project')
a <- list.files()

sourceData <- readRDS(a[2])
data <- readRDS(a[3])
names(data)


library(dplyr)
data$year <- as.factor(data$year)

data2 <- data %>% 
        group_by(year) %>% 
        summarise(sumEm = sum(Emissions))

barplot(height=data2$sumEm, col='red',names.arg=data2$year, xlab="years", ylab=expression('total PM'[2.5]*' emission'),main=expression('Total PM'[2.5]*' emissions at various years'))



#PLOT 2
balt <- subset(data,fips=='24510')
balt2 <- balt %>% 
        group_by(year) %>% 
        summarise(sumEm = sum(Emissions))
barplot(height=balt2$sumEm, col='red',names.arg=data2$year, xlab="years", ylab=expression('total PM'[2.5]*' emission'),main=expression('Total PM'[2.5]*' emissions at various years'))



#PLOT 3b 
library(ggplot2)
data$type <- as.factor(data$type)
summary(data$type)

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
        xlab('Years')+
        
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

#PLOT4
coalM <- grep('Coal|coal',sourceData$Short.Name)
coalData <- sourceData[coalM,]

data.intersect <- merge(data,coalData)
data2 <- data.intersect %>% 
        group_by(year) %>% 
        summarise(sumEm = sum(Emissions))
ggplot(data2,aes(year,sumEm))+
        geom_bar(stat="identity", fill="steelblue")
        

#PLOT5
motorM <- grep('motor|Motor',sourceData$Short.Name)
motorData <- sourceData[coalM,]

data.intersect <- merge(balt,motorData)
data2 <- data.intersect %>% 
        group_by(year) %>% 
        summarise(sumEm = sum(Emissions))
ggplot(data2,aes(year,sumEm))+
        geom_bar(stat="identity", fill="steelblue")

#PLOT6
motorM <- grep('motor|Motor',sourceData$Short.Name)
motorData <- sourceData[coalM,]

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
