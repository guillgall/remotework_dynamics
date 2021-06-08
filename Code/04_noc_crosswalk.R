##EIS
eis <- read.csv("./Data/Input/EIS/canada.csv")
eis <- eis[-c(1:9,700:nrow(eis)),-5]
#separate code from name
eis$noc_code <- gsub("[^0-9]", "",  eis[,1])
#only keep 2 digit occupations
eis$nchar <- nchar(eis$noc_code) 

#NOC 40
eis$noc40 <- ""
eis$noc40 <- ifelse(eis$nchar==2,eis$noc40<-eis[,1],"")

#after inspection and seeing LFS NOC 40 groups, 
#need to add to manually add two groups of EIS
#01-05 Specialized middle management occupations
#07-09 Middle management occupations in trades, transportation, production and utilities


eis$noc40[grep("01-05 Specialized middle management occupations", eis[,1])] <- 
          "01-05 Specialized middle management occupations"

eis$noc40[grep("07-09 Middle management occupations in trades, transportation, production and utilities", eis[,1])] <- 
       "07-09 Middle management occupations in trades, transportation, production and utilities"

##
for (i in 2:nrow(eis)){
  eis$noc40[i] <- ifelse(eis$noc40[i]=="",
                         eis$noc40[i]<- eis$noc40[i-1],
                         eis$noc40[i])
}

noc40 <- as.data.frame(unique(eis$noc40[-c(1:2)]))
noc40$lfs_id <- 1:40

colnames(noc40)[1] <- "Occuppation"

##
eisb <- subset(eis, nchar==4)

#similar than before, but opposite: need to manually delete two 2 digit occupation groups

eisb <- eisb[-c(grep("01-05 Specialized middle management occupations", eisb[,1])),]

eisb <- eisb[-c(grep("07-09 Middle management occupations in trades, transportation, production and utilities", eisb[,1])),]

eisb <- eisb[,c(5,7)]

#save file
write.csv(eisb, file = "./Data/Output/noc_noc40_crosswalk.csv")

##################
##NOC 10
##EIS
eis <- read.csv("./Data/Input/EIS/canada.csv")
eis <- eis[-c(1:9,700:nrow(eis)),-5]
#separate code from name
eis$noc_code <- gsub("[^0-9]", "",  eis[,1])
#only keep 2 digit occupations
eis$nchar <- nchar(eis$noc_code) 

eis$noc10 <- ""
eis$noc10 <- ifelse(eis$nchar==1,eis$noc10<-eis[,1],"")

check <- unique(eis$noc10)

for (i in 2:nrow(eis)){
  eis$noc10[i] <- ifelse(eis$noc10[i]=="",
                         eis$noc10[i]<- eis$noc10[i-1],
                         eis$noc10[i])
}

noc10 <- as.data.frame(unique(eis$noc10[-c(1:2)]))
noc10$lfs_id <- 1:10

colnames(noc10)[1] <- "Occuppation"

##
eisb <- subset(eis, nchar==4)

eisb <- eisb[,c(5,7)]

#save file
write.csv(eisb, file = "./Data/Output/noc_noc10_crosswalk.csv")

