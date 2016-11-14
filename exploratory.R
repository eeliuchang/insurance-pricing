load('20160803.Rdata')

planrates$BusinessYear <- factor(planrates$BusinessYear)
planrates$StateCode <- factor(planrates$StateCode)
planrates$RatingAreaId <- factor(planrates$RatingAreaId)
planrates$PlanType <- factor(planrates$PlanType)
planrates$MetalLevel <-  factor(planrates$MetalLevel)

mod_1 <- lm(IndividualRate ~ moop_in + coinsurance + deduct_in + combined, data= planrates)
summary(mod_1)

mod_2 <- lm(IndividualRate ~ BusinessYear + moop_in + coinsurance + deduct_in + combined, data= planrates)
summary(mod_2)

mod_3 <- lm(IndividualRate ~ BusinessYear + StateCode + moop_in + coinsurance + deduct_in + combined, data= planrates)
summary(mod_3)

mod_4 <- lm(IndividualRate ~ BusinessYear + StateCode + RatingAreaId + moop_in + coinsurance + deduct_in + combined, data= planrates)
summary(mod_4)

mod_5 <- lm(IndividualRate ~ BusinessYear + StateCode + RatingAreaId + PlanType + moop_in + coinsurance + deduct_in + combined, data= planrates)
summary(mod_5)

mod_6 <- lm(IndividualRate ~ BusinessYear + StateCode + RatingAreaId + PlanType + moop_in + coinsurance + deduct_in + combined + MetalLevel, data= planrates)
summary(mod_6)
anova(lm(IndividualRate ~ BusinessYear + StateCode + RatingAreaId + PlanType + moop_in + coinsurance + deduct_in + combined + MetalLevel + MetalLevel*StateCode, data= planrates))
summary(lm(IndividualRate ~ BusinessYear + StateCode + RatingAreaId + PlanType + moop_in + coinsurance + deduct_in + combined + MetalLevel + MetalLevel*StateCode, data= planrates))


summary(lm(log(IndividualRate) ~ BusinessYear + StateCode + RatingAreaId + PlanType + moop_in + coinsurance + deduct_in + combined + MetalLevel, data= planrates))

## Aggregation on the state level
for (state in unique(planrates$StateCode))
{
  print(state)
  planrates_state <- planrates[planrates$StateCode==state,]
  print(summary(lm(log(IndividualRate) ~ moop_in + coinsurance + deduct_in + combined + MetalLevel, data= planrates_state))$r.squared)
  print(summary(lm(log(IndividualRate) ~ moop_in + coinsurance + deduct_in + combined , data= planrates_state))$r.squared)
  print(summary(lm(IndividualRate ~  moop_in + coinsurance + deduct_in + combined + MetalLevel , data= planrates_state))$r.squared)
}
### generally works good (mean=0.5-0.6)
###ID only has 2014 data
### AK only has PPO plan
### Works very bad for VA data


## Aggregation on the metal level
for (metal in unique(planrates$MetalLevel))
{
  print(metal)
  planrates_metal <- planrates[planrates$MetalLevel==metal,]
  print(summary(lm(log(IndividualRate) ~ moop_in + StateCode + coinsurance + deduct_in + combined , data= planrates_metal))$r.squared)
  print(summary(lm(IndividualRate ~  moop_in + StateCode + coinsurance + deduct_in + combined , data= planrates_metal))$r.squared)
}
### not working well... mean=0.3-0.4


## Aggregation on the type level
for (type in unique(planrates$PlanType))
{
  if (type != "Indemnity"){
    print(type)
    planrates_type <- planrates[planrates$PlanType==type,]
    print(summary(lm(log(IndividualRate) ~ moop_in +  StateCode + coinsurance + deduct_in + combined + MetalLevel, data= planrates_type))$r.squared)
    print(summary(lm(log(IndividualRate) ~ moop_in + StateCode +coinsurance + deduct_in + combined , data= planrates_type))$r.squared)
    print(summary(lm(IndividualRate ~  moop_in + StateCode + coinsurance + deduct_in + combined + MetalLevel , data= planrates_type))$r.squared)
  }
}
### generally works good (need to have statecode in) mean = 0.6-0.7
### Indemnity plan just offered in one state and has only several obs.

## Aggregation on the state and type level
for (state in unique(planrates$StateCode))
{
  planrates_state <- planrates[ planrates$StateCode==state,]
  for (type in unique(planrates_state$PlanType) )
  {
    print(state)
    print(type)
    planrates_state_type <- planrates[planrates_state$PlanType==type,]
    print(length(planrates_state_type[,1]))
    print(summary(lm(log(IndividualRate) ~ moop_in + coinsurance + deduct_in + combined + MetalLevel, data= planrates_state_type))$r.squared)
    #print(summary(lm(log(IndividualRate) ~ moop_in  +coinsurance + deduct_in + combined , data= planrates_state_type))$r.squared)
    #print(summary(lm(IndividualRate ~  moop_in + coinsurance + deduct_in + combined + MetalLevel , data= planrates_state_type))$r.squared)
  }
}

###mean=0.4-0.5


## best to aggregate over state or plantype

##adding some benefits in 


planrates_benefits <- cbind(planrates[,10:12],planrates[,14:16],planrates[,18:421])
summary(colSums(planrates_benefits ))
hist(colSums(planrates_benefits),breaks=10)
length(planrates_benefits[,colSums(planrates_benefits) > 17043])
## there are 28 benefits which all of the plans have, let's remove them
planrates_benefits <- planrates_benefits[,colSums(planrates_benefits)!=17044]
length(planrates_benefits[,colSums(planrates_benefits)  > 17000])
planrates_benefits_small <- planrates_benefits[,colSums(planrates_benefits) > 16000]
length(planrates_benefits_small[1,])
planrates_1 <- data.frame(IndividualRate = planrates$IndividualRate,BusinessYear=planrates$BusinessYear,StateCode=planrates$StateCode,PlanType=planrates$PlanType,MetalLevel=planrates$MetalLevel,moop_in=planrates$moop_in, coinsurance=planrates$coinsurance,deduct_in=planrates$deduct_in,combined = planrates$combined,planrates_benefits_small)


names(planrates_1)
summary(lm(IndividualRate ~ . , data = planrates_1)) $r.squared
summary(lm(IndividualRate ~ BusinessYear + PlanType + MetalLevel + moop_in + coinsurance + deduct_in + combined, data=planrates ))$r.squared
##Adding the top benefits can significantly improve the fit
 

for (state in unique(planrates_1$StateCode))
{
  planrates_state <- planrates_1[planrates_1$StateCode==state,]
  variety_condition <- (length(unique(planrates_state$BusinessYear))>1) &&(length(unique(planrates_state$PlanType))>1) && (length(unique(planrates_state$MetalLevel))>1)
  if(variety_condition){
    print(state)
    print(summary(lm(log(IndividualRate) ~ BusinessYear + PlanType + MetalLevel + moop_in + coinsurance + deduct_in + combined, data=planrates_state ))$r.squared)
    print(summary(lm(log(IndividualRate) ~ BusinessYear + PlanType + MetalLevel + moop_in + coinsurance + deduct_in + combined +accidentaldental+allergytesting+chemotherapy+chiropracticcare+diabeteseducation+dialysis+eyeglassesforchildren+habilitationservices+infusiontherapy+reconstructivesurgery+rehabilitativeoccupationalandrehabilitativephysicaltherapy+rehabilitativespeechtherapy+skillednursingfacility+transplant+wellbabyvisitsandcare, data= planrates_state))$r.squared)
  }
}
