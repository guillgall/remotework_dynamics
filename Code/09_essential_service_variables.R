


#ESSENTIAL SERVICE
##add essential service variable
essential <- read.csv("./Data/Input/LMIC/lmic_essential_workers.csv")

#need to add missing zero's to essential first entries
essential$nchar <- nchar(essential$NOC.code) 

essential$NOC.code <- as.character(essential$NOC.code)

for (i in 1:nrow(essential)){
  ifelse(essential$nchar[i]==2, 
         essential$NOC.code[i]<-paste("00", essential$NOC.code[i], sep=""),
         essential$NOC.code[i])
}

for (i in 1:nrow(essential)){
  ifelse(essential$nchar[i]==3, 
         essential$NOC.code[i]<-paste("0", essential$NOC.code[i], sep=""),
         essential$NOC.code[i])
}

essential <- essential[,-4]

colnames(essential)[2:3] <- c("noc_code", "essential")

essential$dummy <- 1

for (i in 1:nrow(essential)){
  if (essential$essential[i]=="No"){
    essential$dummy[i] <- 0
  }
}

#NOC NOC 40 crosswalk
noc40 <- read.csv("./Data/Output/noc_noc40_crosswalk.csv")
noc40 <- noc40[,-1]

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


essential_noc <- merge(essential, noc40, by=c("noc_code"))

###
####employment data
manual <- read.csv("./Data/Output/eis_manual_remote.csv")

manual <- manual[,-1]

manual <- subset(manual, geography=="Canada [1]")

manual$noc_code <- gsub("[^0-9]", "",  manual$occupation)

manual <- manual[,c(1,4)]

essential_manual <- merge(essential_noc, manual, by=c("noc_code"))

essential_manual$essential_share_weighted <- 0

i <- unique(essential_manual$noc40)[2]

#parenthesis in 31 is giving a problem. remove

essential_manual$noc40 <- gsub("\\s*\\([^\\)]+\\)", 
                      "", essential_manual$noc40)


for (i in unique(essential_manual$noc40)){
   
  #manual
  manual_temp <- subset(essential_manual, noc40==get("i"))
  
  employment_temp <- sum(manual_temp$employment)
  
  manual_temp$weights <- manual_temp$employment/employment_temp
  
  manual_temp$essential_share_weighted <- weighted.mean(manual_temp$dummy, 
                                                    manual_temp$weights)
  
  essential_manual[grep(get("i"), essential_manual$noc40),
         grep("essential_share_weighted", colnames(essential_manual))] <- manual_temp$essential_share_weighted
  
}

essential_manual_ag <- aggregate( essential_manual, by=list( essential_manual$noc40), FUN=mean)

essential_manual_ag <- essential_manual_ag[,c(1,8)]

#problems again with two occupations that need to manually be sorted to match LFS id
unique(essential_manual_ag$noc40)

essential_manual_ag2 <- essential_manual_ag

essential_manual_ag2[2,] <- essential_manual_ag[grep("01-05 Specialized middle management occupations", essential_manual_ag[,1]),]
essential_manual_ag2[3,] <- essential_manual_ag[grep("06 Middle management occupations in retail and wholesale trade and customer services", essential_manual_ag[,1]),]
essential_manual_ag2[4,] <- essential_manual_ag[grep("07-09 Middle management occupations in trades, transportation, production and utilities", essential_manual_ag[,1]),]

essential_manual_ag2[5:40,] <- essential_manual_ag[3:38,]

essential_manual_ag2$occupation <- essential_manual_ag2[,1]
essential_manual_ag2$noc40 <- rownames(essential_manual_ag2)

essential_manual_ag2 <- essential_manual_ag2[,c(2:4)]

colnames(essential_manual_ag2)[3] <- "NOC_40"

#merge with essential service dummy variable
dummy_es <- read.csv("./Data/Input/Essential_Service/essential_service_dummy.csv")
dummy_es$NOC_40 <- 1:40

dummy_es <- merge(dummy_es, 
                  essential_manual_ag2, by=c("NOC_40"))

#merge with LFS employment variation data
variation_oc <- read.csv("./Data/Output/NOC_40_employment_variation.csv")

variation_oc <- variation_oc[,-1]

variation_oc <- merge(variation_oc, 
                      dummy_es, by=c("NOC_40"))

variation_oc <- variation_oc[,-c(14,17)]

#save files
write.csv(variation_oc, file = "./Data/Output/NOC_40_essential_employment_variation.csv")






