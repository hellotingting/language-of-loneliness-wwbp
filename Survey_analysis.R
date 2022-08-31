# install.packages("tidyr")
# install.packages("dplyr")
# install.packages("reshape2")
# install.packages("data.table")
# install.packages("psych")
# install.packages("ppcor")
# install.packages("GPArotation")
# install.packages("corrplot")
library(tidyr)
library(dplyr)
library(data.table)
library(psych)
library(ppcor)
library(ggplot2)
library(GPArotation)
library(corrplot)

Data_old<-read.csv(file= "user_level_aggregated_outcomes_TL.csv")
ID_Data<-read.csv(file= "user_id.csv")
names(ID_Data)[1] <- "group_id"
Data<-subset(Data_old, user_id %in% ID_Data$group_id)

Data$education<-as.factor(Data$education)
Data$sex<-as.factor(Data$sex)
Data$income<-as.factor(Data$income)

#get demographic info
#psych::describe(Data)
summary(Data$sex)
summary(Data$education)
summary(Data$income)

#3-item ucla loneliness: UCLA-1,4,6; reliability test
psych::alpha(Data[,c(13,16,18)])
##calculate total score for 3 items
Data<-Data%>%
  mutate(UCLA_3item_sum=ucla_1+ucla_4+ucla_6)

#phq: reliability test
psych::alpha(Data[,c(65:73)])

summary(Data$UCLA_3item_sum)

#correlations: age, PHQ total, loneliness total
#install.packages("correlation")
library(correlation)
data_clean<-Data[,c(3,74,186)]
#get the CI value
COR1<-data_clean %>% 
  correlation(method="pearson", p_adjust = "BH")
display(COR1, digit=4)


#partial correlations
# controlled for the 3rd variable
pcor.test(data_clean$age,data_clean$PHQ_score,data_clean$UCLA_3item_sum, method="pearson")

pcor.test(data_clean$age,data_clean$UCLA_3item_sum,data_clean$PHQ_score, method="pearson")

pcor.test(data_clean$UCLA_3item_sum,data_clean$PHQ_score,data_clean$age, method="pearson")

#get the CI value

par1<-data_clean %>% 
  correlation(method="pearson", partial = T, p_adjust = "BH")

display(par1, digit=4)

# compare between genders
t.test(UCLA_3item_sum~isFemale, data=Data)
t.test(PHQ_score~isFemale, data=Data)
