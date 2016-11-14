benefits <- read.csv('BenefitsCostSharing.csv')
benefits[,1] <- as.character(benefits[,1])
save(benefits, file='benefits.Rdata')

load(file='benefits.Rdata')

#library(tm)
# bname <- benefits[,1]
# myCorpus < Corpus(VectorSource(bname))
# myCorpus <- tm_map(myCorpus, tolower)
# myCorpus <- tm_map(myCorpus, removePunctuation, preserve_intra_word_dahses=False)
# myCorpus <- tm_map(myCorpus, removeWords, stopwords=('English'))

bname <- benefits[,1]
length(unique(bname)) ##861
bname <- clean_text_vector(bname)
length(unique(bname))##732


a <- vector()
b <- vector()

library(RecordLinkage)
bname_unique <- unique(bname)
count =0
for (i in 1:length(bname_unique)){
  for (j in i:length(bname_unique)){
    sim <- levenshteinSim(bname_unique[i],bname_unique[j])
    #print(dist)
    if (sim >0.90 && sim != 1){
      count <- count+1
      a[count] <- bname_unique[i]
      b[count] <- bname_unique[j]

    }

  }
}
#print(count) ##156
x <- data.frame(a,b)
write.table(x,file= 'similar_benefit_names.csv',row.names=FALSE,col.names=FALSE)

c <- rep(1,count)

c[c(1,2,4,19:28,45,47,48,51,56,60,70,71,113,118,126,128:132,135,149)] <- 0

d <- vector()
e <- vector()

count1 = 0
for (i in 1:count){
    #print(i)
    #print(c[i])
    if (c[i] == 1 ){
      count1 <- count1+1
      d[count1] <- a[i]
      e[count1] <- b[i]
    }
}
print(count1)
x <- data.frame(d,e)
write.table(x,file= 'similar_benefit_names1.csv',row.names=FALSE,col.names=FALSE,,sep= '\t')

#matching names
bname <- data.frame(bname)
index <- match(bname[,1],x$e,nomatch=0)
bname[index != 0,1] <- x$d[index]

save(bname,file='matched name column.csv')

load(file='matched name column.csv')

benefits[,1] <- bname[,1]

load('rate.Rdata')

plan <- read.csv('PlanAttributes.csv')


##function to clean text vector data
clean_text_vector <- function(temp){
  temp <- tolower(temp)
  temp <- stringr::str_replace_all(temp,"[[:punct:]]",' ')
  temp <- stringr::str_replace_all(temp,'[\\s]+','')
  gsub("^\\s+|\\s+$", "", temp)
}

# ## function to calculate similarities of benefit names and return the text with 
# similar_text <- function(text1, text2){
#   dist <- levenshteinSim(text1,text2)
#   print(dist)
#   if (dist >0.9)
#     print(c(text1, text2))
# }