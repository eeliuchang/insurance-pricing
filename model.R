setwd("~/Downloads/Health Insurance Project/health-insurance-marketplace")
load('20160803.Rdata')

##find all the sets in benefits
planrates_rating <-merge(planrates, premiums, by=c('planid14','BusinessYear'))
benefit_names <- names(planrates_rating)[19:421]
planrates_rating_benefit <- planrates_rating[benefit_names] + 0
planrates_final <- cbind(BusinessYear=planrates_rating$BusinessYear,
                         StateCode=planrates_rating$StateCode,
                         RatingAreaId = planrates_rating$RatingAreaId.y, planrates_rating_benefit,IssuerId =planrates_rating$IssuerId)
attach(planrates_final)
tx <- planrates_final[which(BusinessYear==2016 & StateCode=='TX'),]
tx8 <- planrates_final[which(BusinessYear==2016 & StateCode=='TX'& RatingAreaId=='Rating Area 8'),]
unique(planrates_final[which(BusinessYear==2016 & StateCode=='TX'),]$RatingAreaId)
sum(duplicated(as.list(planrates_final))) ##137
num_dup_tx8 <- sum(duplicated(as.list(tx8))) ##382
num_uniq_tx8 <- length(unique(as.list(tx8))) ##24
tx8_dup <- tx8[,duplicated(as.list(tx8))]
tx8_uniq <- tx8[,!duplicated(as.list(tx8))]
colSums(tx8_uniq[4:24])
find_benefit_set(tx8)

tx_tb<- matrix(nrow = 27,ncol = 5)
colnames(tx_tb) <- c('Rating_Area','Num_Plans', 'Num_Unique_Benefit_Set','Num_Unique_Benefit_Set_offered_By_Multi_Issuers','Num_benefits_Not_Offered')
tx_tb[,1] <- c(as.character(unique(tx$RatingAreaId)),'TX')
for (ra in unique(tx$RatingAreaId)){
  tx_area_data <- tx[which(tx$RatingAreaId == ra),] 
  tx_area_uniq <- tx_area_data[,!duplicated(as.list(tx_area_data))]
  write(paste('////////',ra,'\n','Number of Unique Benefits:', length(names(tx_area_uniq))-4), file='benefit_cluster.txt',append = TRUE)
  tx_tb[which( tx_tb[,1] == ra),3]<- length(names(tx_area_uniq))-4
  tx_tb[which( tx_tb[,1] == ra),4] <- find_num_uniq_benefit_offered_multi_issuer(tx_area_uniq)
  tx_tb[which( tx_tb[,1] == ra),5] <- sum(colSums(tx_area_data[4:length(names(tx_area_data))]) == 0)
  
  find_benefit_set(tx_area_data)
  write(paste('////','Number of plans available: ',length(tx_area_data[,1])),file='benefit_cluster.txt',append = TRUE)
  tx_tb[which( tx_tb[,1] == ra),2] <- length(tx_area_data[,1])
  write(paste('////','Number of appearances: '),file='benefit_cluster.txt',append = TRUE)
  write.table(colSums(tx_area_uniq[4:length(names(tx_area_uniq))]),file='benefit_cluster.txt',append = TRUE,col.names=TRUE)
  
}
tx_tb[ 27,2] <- length(tx[,1])
tx_uniq <- tx[,!duplicated(as.list(tx))]
tx_tb[27,3]<- length(names(tx_uniq))-4
uniq_tx <- tx[,!duplicated(as.list(tx))]
tx_tb[27,4] <- find_num_uniq_benefit_offered_multi_issuer(uniq_tx)
tx_tb[27,5] <- sum(colSums(tx[4:length(names(tx))]) == 0)

write.table(tx_tb,file='20160822.csv',append = TRUE,col.names=TRUE,sep = ',')


find_num_uniq_benefit_offered_multi_issuer <- function(uniq_data){
  
  count <- 0
  index_IssureId <- length(uniq_data)
  for (index_uniq_benefit in 4:(length(uniq_data)-1))
  {
    sum_unique_IssuerId <- sum(unique(uniq_data[,index_IssureId] *uniq_data[,index_uniq_benefit]) != 0)
    if (sum_unique_IssuerId >= 2){
      count = count + 1
    }
  }
  
  return(count)
  
}

### the data should have first three columns as BusinessYear, StateCode, RatingAreaId
find_benefit_set <- function(dta){
  flag <- rep(0,dim(dta)[2] -1)
  k <- 1
  benefit_set <- list()
  for (i in 4:dim(dta)[2]-1){
    if (flag[i] == 0){
      benefit_set[k] =names(dta)[i]
      for(j in i:dim(dta)[2]-1){
        if(j>i && flag[j]==0){
          if (identical(dta[,i],dta[,j])){
            benefit_set[k] <- paste(benefit_set[k], names(dta)[j],sep = ', ')
            flag[j] <- 1
          }
        }
      }
      k = k+1
    }
  }
  #   benefit_cluster <- as.data.frame(do.call(rbind, lapply(seq_along(benefit_set), function(i){
  #   data.frame(CLUSTER=i, benefit_set[[i]])
  # })))
    #lapply(benefit_set, write, "benefit_cluster.txt", append=TRUE, ncolumns=1000, sep='\n')
    
  
}

###############
if (sum(uniq_data[,index_IssureId] *uniq_data[,index_uniq_benefit] != tx_area_data$IssuerId[1]) >0)
 count = count +1
###############

which(duplicated(as.list(tx8)))
names(tx8)[which(duplicated(as.list(tx8)))]
unique(as.list(tx8_dup))
##remove all the duplicated columns
tx8_clean <- cbind(tx8[,!duplicated(as.list(tx8))])
