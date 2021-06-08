#open datasets
onetnoc <- read.csv("./Data/Output/eis_onetnoc_remote.csv")
manual <- read.csv("./Data/Output/eis_manual_remote.csv")

noc40 <- read.csv("./Data/Output/noc_noc40_crosswalk.csv")

onetnoc <- onetnoc[,-1]
manual <- manual[,-1]
noc40 <- noc40[,-1]

onetnoc <- subset(onetnoc, geography=="Canada [1]")
manual <- subset(manual, geography=="Canada [1]")

onetnoc$noc_code <- gsub("[^0-9]", "",  onetnoc$occupation)
manual$noc_code <- gsub("[^0-9]", "",  manual$occupation)

onetnoc <- onetnoc[,c(1,9,4)]
manual <- manual[,c(1,13,4)]

#need to add missing zero's to noc40 first entries
noc40$nchar <- nchar(noc40$noc_code) 

noc40$noc_code <- as.character(noc40$noc_code)

for (i in 1:nrow(noc40)){
  ifelse(noc40$nchar[i]==2, 
         noc40$noc_code[i]<-paste("00", noc40$noc_code[i], sep=""),
         noc40$noc_code[i])
}

for (i in 1:nrow(noc40)){
  ifelse(noc40$nchar[i]==3, 
       noc40$noc_code[i]<-paste("0", noc40$noc_code[i], sep=""),
       noc40$noc_code[i])
}

noc40 <- noc40[,-3]

onetnoc <- merge(onetnoc, noc40, by=c("noc_code"))
manual <- merge(manual, noc40, by=c("noc_code"))

#compute weighted remote work index

i <- "    00 Senior management occupations"
i <- "    06 Middle management occupations in retail and wholesale trade and customer services"
i <- "    31 Professional occupations in health (except nursing)"

#parenthesis in 31 is giving a problem. remove

onetnoc$noc40 <- gsub("\\s*\\([^\\)]+\\)", 
                      "", onetnoc$noc40)

manual$noc40 <- gsub("\\s*\\([^\\)]+\\)", 
                      "", manual$noc40)

onetnoc$remote_work_weighted <- 0
manual$remote_work_weighted <- 0

for (i in unique(onetnoc$noc40)){
  #onet noc
  onetnoc_temp <- subset(onetnoc, noc40==get("i"))
  
  employment_temp <- sum(onetnoc_temp$employment)
  
  onetnoc_temp$weights <- onetnoc_temp$employment/employment_temp
  
  onetnoc_temp$remote_work_weighted <- weighted.mean(onetnoc_temp$remote_work, 
                                            w=onetnoc_temp$weights)
  
  onetnoc[grep(get("i"), onetnoc$noc40),
          grep("remote_work_weighted", colnames(onetnoc))] <- onetnoc_temp$remote_work_weighted

  #manual
  
  manual_temp <- subset(manual, noc40==get("i"))
  
  employment_temp <- sum(manual_temp$employment)
  
  manual_temp$weights <- manual_temp$employment/employment_temp
  
  manual_temp$remote_work_weighted <- weighted.mean(manual_temp$remote_work, 
                                           manual_temp$weights)
  
  manual[grep(get("i"), manual$noc40),
         grep("remote_work_weighted", colnames(onetnoc))] <- manual_temp$remote_work_weighted
  
}

onetnoc_ag <- aggregate(onetnoc, by=list(onetnoc$noc40), FUN=mean)

manual_ag <- aggregate(manual, by=list(manual$noc40), FUN=mean)

onetnoc_ag <- onetnoc_ag[,c(1,6)]
manual_ag <- manual_ag[,c(1,6)]

colnames(onetnoc_ag) <- c("noc40", "remote_work_onet")
colnames(manual_ag) <- c("noc40", "remote_work_manual")

noc40_remote <- merge(onetnoc_ag, manual_ag, by=c("noc40"))

#problems again with two occupations that need to manually be sorted to match LFS id
unique(noc40_remote$noc40)

noc40_remote2 <- noc40_remote

noc40_remote2[2,] <- noc40_remote[grep("01-05 Specialized middle management occupations", noc40_remote[,1]),]
noc40_remote2[3,] <- noc40_remote[grep("06 Middle management occupations in retail and wholesale trade and customer services", noc40_remote[,1]),]
noc40_remote2[4,] <- noc40_remote[grep("07-09 Middle management occupations in trades, transportation, production and utilities", noc40_remote[,1]),]

noc40_remote2[5:40,] <- noc40_remote[3:38,]

noc40_remote2$occupation <- noc40_remote2[,1]
noc40_remote2$noc40 <- rownames(noc40_remote2)

#save file
write.csv(noc40_remote2, file = "./Data/Output/NOC_40_remote_work.csv")

##
#NOC 10
#open datasets
onetnoc <- read.csv("./Data/Output/eis_onetnoc_remote.csv")
manual <- read.csv("./Data/Output/eis_manual_remote.csv")

noc10 <- read.csv("./Data/Output/noc_noc10_crosswalk.csv")

onetnoc <- onetnoc[,-1]
manual <- manual[,-1]
noc10 <- noc10[,-1]

onetnoc <- subset(onetnoc, geography=="Canada [1]")
manual <- subset(manual, geography=="Canada [1]")

onetnoc$noc_code <- gsub("[^0-9]", "",  onetnoc$occupation)
manual$noc_code <- gsub("[^0-9]", "",  manual$occupation)

onetnoc <- onetnoc[,c(1,9,4)]
manual <- manual[,c(1,13,4)]

#need to add missing zero's to noc10 first entries
noc10$nchar <- nchar(noc10$noc_code) 

noc10$noc_code <- as.character(noc10$noc_code)

for (i in 1:nrow(noc10)){
  ifelse(noc10$nchar[i]==2, 
         noc10$noc_code[i]<-paste("00", noc10$noc_code[i], sep=""),
         noc10$noc_code[i])
}

for (i in 1:nrow(noc10)){
  ifelse(noc10$nchar[i]==3, 
         noc10$noc_code[i]<-paste("0", noc10$noc_code[i], sep=""),
         noc10$noc_code[i])
}

noc10 <- noc10[,-3]

onetnoc <- merge(onetnoc, noc10, by=c("noc_code"))
manual <- merge(manual, noc10, by=c("noc_code"))

#compute weighted remote work index

i <- unique(onetnoc$noc10)[2]

onetnoc$remote_work_weighted <- 0
manual$remote_work_weighted <- 0

for (i in unique(onetnoc$noc10)){
  #onet noc
  onetnoc_temp <- subset(onetnoc, noc10==get("i"))
  
  employment_temp <- sum(onetnoc_temp$employment)
  
  onetnoc_temp$weights <- onetnoc_temp$employment/employment_temp
  
  onetnoc_temp$remote_work_weighted <- weighted.mean(onetnoc_temp$remote_work, 
                                                     w=onetnoc_temp$weights)
  
  onetnoc[grep(get("i"), onetnoc$noc10),
          grep("remote_work_weighted", colnames(onetnoc))] <- onetnoc_temp$remote_work_weighted
  
  #manual
  
  manual_temp <- subset(manual, noc10==get("i"))
  
  employment_temp <- sum(manual_temp$employment)
  
  manual_temp$weights <- manual_temp$employment/employment_temp
  
  manual_temp$remote_work_weighted <- weighted.mean(manual_temp$remote_work, 
                                                    manual_temp$weights)
  
  manual[grep(get("i"), manual$noc10),
         grep("remote_work_weighted", colnames(onetnoc))] <- manual_temp$remote_work_weighted
  
}

onetnoc_ag <- aggregate(onetnoc, by=list(onetnoc$noc10), FUN=mean)

manual_ag <- aggregate(manual, by=list(manual$noc10), FUN=mean)

onetnoc_ag <- onetnoc_ag[,c(1,6)]
manual_ag <- manual_ag[,c(1,6)]

colnames(onetnoc_ag) <- c("noc10", "remote_work_onet")
colnames(manual_ag) <- c("noc10", "remote_work_manual")

noc10_remote <- merge(onetnoc_ag, manual_ag, by=c("noc10"))

noc10_remote$occupation <- noc10_remote[,1]
noc10_remote$noc10 <- rownames(noc10_remote)


#save file
write.csv(noc10_remote, file = "./Data/Output/NOC_10_remote_work.csv")
