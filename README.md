# Stack-BusLab-12.07.2020a
#library(partykit)
#library(Formula)
#library(ggplot2)
#install tidyverse
#install.packages("tidyverse")
#library(dplyr)
#______________________________________
#capstone project 
name <- "JodiReneAnderson"
print(name, quote = FALSE)

# Script name:  Capstone
# Created on: January 9,2021
# Author:  JodiRene Anderson
# Purpose: StackEducation Capstone
# Version: 1.0


HealthPlansData<- read.csv("C:/Users/jr19a/OneDrive/Desktop/Stack Education/capstone/HEALTH_PLANS.csv")
colnames(HealthPlansData)
summary(HealthPlansData)
DeductionData<- read.csv("C:/Users/jr19a/OneDrive/Desktop/Stack Education/capstone/5yearsGICdeductionsPlandeductions.csv")
colnames(DeductionData)
summary(DeductionData)
EmployeeMasterData<- read.csv("C:/Users/jr19a/OneDrive/Desktop/Stack Education/capstone/EmployeeMasterData10.2020.csv")
colnames(EmployeeMasterData)
summary(EmployeeMasterData)
EmployeeRetentionData<- read.csv("C:/Users/jr19a/OneDrive/Desktop/Stack Education/capstone/EmployeeJobSalaryRetention.csv")
colnames(EmployeeRetentionData)
summary(EmployeeRetentionData)
SalaryGrid<-read.csv("C:/Users/jr19a/OneDrive/Desktop/Stack Education/capstone/TeacherSalaryGridsMetroWest.csv",header = T)
colnames(SalaryGrid)
summary(SalaryGrid)
#_____________________________________________________________________

head(HealthPlansData)
df <- HealthPlansData
scatter <- ggplot(data=df, aes(x = Total.Premium, y = Monthly.Rate))
scatter + geom_point(aes(color= Individal.or.Family)) +
  xlab("Total.Premium") +  ylab("Monthly.Rate") +
  ggtitle("Premium v Monthly Rate") 

hist(HealthPlansData$Total.Premium)
hist(HealthPlansData$Monthly.Rate, xlab = 'Monthly Rate', main = 'Monthly Rate Health Plans')

str(dfHealthPlans)
table(dfHealthPlans$Individal.or.Family,useNA = "ifany")
table(dfHealthPlans$X20.Week.Pay.Deduction,useNA = 'ifany')
table(dfHealthPlans$X26.Week.Pay.Deduction,useNA =  'ifany')
View(dfHealthPlans)
dfHealthPlans <- HealthPlansData
dfHealthPlans<-dfHealthPlans[,c(2,7,8)]

dfHealthPlans$Individal.or.Family<-as.factor(dfHealthPlans$Individal.or.Family)
#creating a model 'HealthPlan_ctree' / what you are trying to determine goes before the sqiggly line
HealthPlan_ctree <- ctree(Individal.or.Family ~ X20.Week.Pay.Deduction + X26.Week.Pay.Deduction, data = dfHealthPlans)

plot(HealthPlan_ctree)
#___________________________________________________________
#removing columns that are not useful or duplicate of others Data_new<-data_old[,-1]
EmployeeMaster <- EmployeeMasterData [,-c(3,6,9,15:17,21:29,32,36)]
DeductionData <- DeductionData[,-c(5,6)]
#(wanting to sum employee number by school year, the employee.amount / so ideally employee number
#  will have 5 results - less columns to merge data sets)

Sum.DeductionData <- DeductionData %>% 
  group_by(School.Year,Employee) %>% 
  summarise(Employee.Amount = sum(Employee.Amount))
SUM.DeductionData <- as.data.frame(SUM.DeductionData)
