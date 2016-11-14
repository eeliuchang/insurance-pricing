library(ggplot2)
setwd("~/Downloads/Health Insurance Project/health-insurance-marketplace")
load('20160803.Rdata')
numOfPlan_vs_year <- aggregate(planid14 ~ BusinessYear, planrates, length)
barplot(numOfPlan_vs_year[,2],xlab='Business Year', ylab='Number of Plans',col='red',names.arg = as.character(numOfPlan_vs_year[,1]))
avg_rate_vs_year <- aggregate(IndividualRate ~ BusinessYear, planrates, mean)
barplot(avg_rate_vs_year[,2],xlab='Business Year', ylab='Mean of Individual Rate',col='red',names.arg = as.character(avg_rate_vs_year[,1]))

numOfPlan_vs_IssuerId <- aggregate(planid14 ~ IssuerId, planrates, length)
barplot(numOfPlan_vs_IssuerId[,2],xlab='IssuerId', ylab='Number of Plans',col=blues9,names.arg = as.character(numOfPlan_vs_IssuerId[,1]))
avg_rate_vs_IssuerId <- aggregate(IndividualRate ~ IssuerId, planrates, mean)
barplot(avg_rate_vs_IssuerId[,2],xlab='IssuerId', ylab='Mean of Individual Rate',col='red',names.arg = as.character(avg_rate_vs_IssuerId[,1]))
hist(numOfPlan_vs_IssuerId[,2],breaks=20,col='blue',xlab='Number of Plans')
hist(avg_rate_vs_IssuerId[,2],breaks=20,col='yellow',xlab='Individual Rate')
issuerId_expensive <- subset(avg_rate_vs_IssuerId,IndividualRate>400)


numOfPlan_vs_state <- aggregate(planid14 ~ StateCode, planrates, length)
barplot(numOfPlan_vs_state[,2],xlab='State', ylab='Number of Plans', col='purple', names.arg = as.character(numOfPlan_vs_state[,1]),las=2)
avg_rate_vs_state <- aggregate(IndividualRate ~ StateCode, planrates, mean)
barplot(avg_rate_vs_state[,2],xlab='State', ylab='Mean of Individual Rate',col='red',names.arg = as.character(avg_rate_vs_state[,1]),las=2)
numOfPlan_vs_state_and_IssuerId <- aggregate(planid14 ~ StateCode+IssuerId, planrates, length)
length(numOfPlan_vs_state_and_IssuerId[,1])
length(unique(planrates$IssuerId))
length(unique(planrates$StateCode))
#xtabs(planid14 ~ StateCode + IssuerId, numOfPlan_vs_state_and_IssuerId)
sum(aggregate(IssuerId ~ StateCode, numOfPlan_vs_state_and_IssuerId, length)[,2]) ## each state can have several issuerIDs but one issuerId can only be in one state
avg_rate_vs_state_IssuerId_PlanType <- aggregate(IndividualRate ~ StateCode + IssuerId + PlanType, planrates, mean)
arrange(avg_rate_vs_state_IssuerId_PlanType,desc(StateCode))
#xtabs(IndividualRate ~ StateCode + IssuerId, avg_rate_vs_state_and_IssuerId)

numOfPlan_vs_type <- aggregate(planid14 ~ PlanType, planrates, length)
barplot(numOfPlan_vs_type[,2],xlab='Plan Type', ylab='Number of Plans', col='green', names.arg = as.character(numOfPlan_vs_type[,1]))
avg_rate_vs_type <- aggregate(IndividualRate ~ PlanType, planrates, mean)
barplot(avg_rate_vs_type[,2],xlab='Plan Type', ylab='Average Plan Rate', col='orange', names.arg = as.character(avg_rate_vs_type[,1]))

numOfPlan_vs_metal <- aggregate(planid14 ~ MetalLevel, planrates, length)
barplot(numOfPlan_vs_metal[,2],xlab='Metal Level', ylab='Number of Plans', col='green', names.arg = as.character(numOfPlan_vs_metal[,1]))
avg_rate_vs_metal <- aggregate(IndividualRate ~ MetalLevel, planrates, mean)
barplot(avg_rate_vs_metal[,2],xlab='Metal Level', ylab='Average Plan Rate', col='orange', names.arg = as.character(avg_rate_vs_metal[,1]))

planrates_2016 <- subset(planrates, BusinessYear==2016)
avg_rate_vs_state_type_metal <- aggregate(IndividualRate ~ StateCode + PlanType + MetalLevel, planrates_2016, mean)
avg_copay_vs_state_type_metal <- aggregate(coinsurance ~ StateCode + PlanType + MetalLevel, planrates_2016, mean)
avg_moop_vs_state_type_metal <- aggregate(moop_in ~ StateCode + PlanType + MetalLevel, planrates_2016, mean)
avg_deduct_vs_state_type_metal <- aggregate(deduct_in ~ StateCode + PlanType + MetalLevel, planrates_2016, mean)
arrange(avg_rate_vs_state_type_metal,StateCode)
length(unique(planrates_2016$StateCode))
length(unique(planrates_2016$PlanType))

par(mfrow=(c(4,4)))
for (state in c('IL'))
{
  data_in_state <- subset(avg_rate_vs_state_type_metal,StateCode == state)
  for( metal in unique(data_in_state$MetalLevel))
  {
    data_in_state_metal <- subset(data_in_state,StateCode==state & MetalLevel==metal)
    barplot(data_in_state_metal$IndividualRate,xlab=metal, ylab='Average Individual Rate',las=2,names.arg=data_in_state_metal$PlanType,main=state)
  }
}

for (state in c('IL'))
{
  data_in_state <- subset(avg_copay_vs_state_type_metal,StateCode == state)
  for( metal in unique(data_in_state$MetalLevel))
  {
    data_in_state_metal <- subset(data_in_state,StateCode==state & MetalLevel==metal)
    barplot(data_in_state_metal$coinsurance,xlab=metal, ylab='Average Copay',las=2,names.arg=data_in_state_metal$PlanType,main=state)
  }
}

for (state in c('IL'))
{
  data_in_state <- subset(avg_moop_vs_state_type_metal,StateCode == state)
  for( metal in unique(data_in_state$MetalLevel))
  {
    data_in_state_metal <- subset(data_in_state,StateCode==state & MetalLevel==metal)
    barplot(data_in_state_metal$moop_in,xlab=metal, ylab='Average MOOP',las=2,names.arg=data_in_state_metal$PlanType,main=state)
  }
}

for (state in c('IL'))
{
  data_in_state <- subset(avg_deduct_vs_state_type_metal,StateCode == state)
  for( metal in unique(data_in_state$MetalLevel))
  {
    data_in_state_metal <- subset(data_in_state,StateCode==state & MetalLevel==metal)
    barplot(data_in_state_metal$deduct,xlab=metal, ylab='Average Deduct',las=2,names.arg=data_in_state_metal$PlanType,main=state)
  }
}

