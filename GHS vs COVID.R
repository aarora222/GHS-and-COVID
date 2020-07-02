library(tidyverse)
library(ggpubr)

#import data from GHS index (2019) and WHO (July 2020)
GHS0 <- read.csv("GHS_data.csv")
GHS <- subset(GHS0, select = -c(url,Country_link,Inner.Glyph,X_url_input))
COV0 <- read.csv("COVID_data.csv")
COV1 <- subset(COV0,select = -c(Date_reported,New_cases,New_deaths))

#function for data preparation (extract most recent cumulative cases)
country <- function(x){
  COV1[COV1$Country==x,]
}

#create covid data frame with most recent cumaltive cases
COV <- data.frame(matrix(ncol=5,nrow=0))
colnames(COV) <-c('Country_code','Country','WHO_region','Cumulative_cases','Cumulative_deaths')

for (i in 1:length(unique(COV1$Country))){
  COV <- rbind(COV,tail(country(unique(COV1$Country)[i]),1))
}

#merge GHS and COVID data, create column for death rate, arrange income and population in intuitive order
total <- merge(GHS,COV,by="Country")
total$Death_rate <- total$Cumulative_deaths/total$Cumulative_cases
final <- total[,c(1,5,6,3,9,10,11)]

final$Income <- factor(final$Income,levels = c("Low income","Lower middle income","Upper middle income","High income"))
final$Population <- factor(final$Population,levels = c("<1m","1-10m","10-50m","50-100m","100m+"))

#Index score vs death rate vs income (sorted by population category)
ggplot(final,mapping=aes(x=Index.Score,y=Death_rate,colour=Income)) +
  geom_point() +
  #geom_smooth(se=FALSE) +
  facet_wrap(~ Population)

#Index score vs death rate (sorted by population and income categories)
ggplot(final,mapping=aes(x=Index.Score,y=Death_rate)) +
  geom_point() +
  geom_smooth(se=FALSE) +
  stat_cor() +
  facet_grid(Population ~ Income)

#Index score vs death rate for population at least 1m (sorted by population and income categories)
ggplot(final[final['Population']!="<1m",],mapping=aes(x=Index.Score,y=Death_rate)) +
  geom_point() +
  stat_cor() +
  facet_grid(Population ~ Income)

#Index score vs death rate for high or upper middle income countries with population at least 1 million
ggplot(final[(final['Income']=='High income'|final['Income']=='Upper middle income')& final['Population']!="<1m",],mapping=aes(x=Index.Score,y=Death_rate)) +
  geom_point() +
  geom_smooth(se=FALSE) +
  stat_cor() +
  ggtitle('IS vs DR for High/UM income countries with Pop at least 1 m') +
  xlab('Index Score') +
  ylab('Death Rate')

#Index score vs death rate for low or lower middle income countries with population at least 1 million
ggplot(final[(final['Income']=='Low income'|final['Income']=='Lower middle income')&final['Population']!="<1m",],mapping=aes(x=Index.Score,y=Death_rate)) +
  geom_point() +
  geom_smooth(se=FALSE) +
  stat_cor()

#Income vs death rate for high income countries with population at least 1 million
ggplot(final[final['Population']!="<1m" & final['Income']=='High income',],mapping=aes(x=Income,y=Death_rate,colour=Population)) +
  geom_jitter(width=0.1)
  #geom_smooth(se=FALSE)
