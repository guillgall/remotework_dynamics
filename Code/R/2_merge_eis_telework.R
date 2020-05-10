library("readxl")

#open ies data
labor <- read.csv("./Data/Output/merge.csv")

#separate code from name
labor$noc_code <- gsub("[^0-9]", "",  labor[,2])

#only keep 4 digit occupations
labor$nchar <- nchar(labor$noc_code) 

##
#open manual remote work index
manual <- read.csv("./Data/Input/Manual/manualnoc.csv")

#compute average of both introspection
for (i in 1:nrow(manual)){
  manual$teleworkable[i] <- mean(c(manual[i,3], manual[i,4]))
  manual$diff[i] <- manual[i,3]-manual[i,4]
}
  
#separate code from name
manual$noc_code <- gsub("[^0-9]", "",  manual[,2]) 

labor_manual <- merge(labor, manual, by="noc_code")

#save file
write.csv(labor_manual, file = "./Data/Output/eis_manual_remote.csv")

###
##now use crosswalk ONET NOC by BIIE
onetnoc <- read.csv("./Data/Input/BIIE/onetnoc.csv")
onetnoc <- as.data.frame(onetnoc[,-1])

#open ONET remote work index from DN and merge
onet_remote <- read.csv("./Data/Input/DN/occupations_workathome.csv")
colnames(onet_remote)[1] <- "onet"

#merge by onet code
onetnoc_remote <- merge(onet_remote, onetnoc, by="onet")

noc <- unique(onetnoc_remote$noc_title)

#separate noc code
onetnoc_remote$noc_code <- gsub("[^0-9]", "",  onetnoc_remote$noc_title)

#clean
onetnoc_remote <- onetnoc_remote[,c(4,3)]


duplicated(onetnoc_remote)

#remove duplicated rows
onetnoc_remote2 <- onetnoc_remote[-which(duplicated(onetnoc_remote)), ]

#separate code from name
onetnoc_remote2$noc_code <- gsub("[^0-9]", "",  onetnoc_remote2[,1])

#now merge eis with remote work index using noc code
merge2 <- merge(labor, onetnoc_remote2, by="noc_code")

#save file
write.csv(merge2, file = "./Data/Output/eis_onetnoc_remote.csv")

c1 <- unique(manual$noc_code)
c2 <- unique(merge2$noc_code)
compare <- setdiff(unique(manual$noc_code), unique(merge2$noc_code))


#some occupation code differences between onetnoc(BIIE) and onet(NM)

inter <- intersect(onet_remote$onet, onetnoc$onet)
sdiff <- setdiff(onet_remote$onet, onetnoc$onet)

#683 shared elements, 285 differences

#however, after merging with ies data
#canada check
labor_can <- subset(labor, geography=="Canada [1]")
merge2_can <- subset(merge2, geography=="Canada [1]")

inter2 <- intersect(unique(labor$noc_code), unique(merge2$noc_code))
sdiff2 <- setdiff(unique(labor$noc_code), unique(merge2$noc_code))

#483 intersection
#19 differences. which one are they?

#TBC
labor_can[grep("0011", labor_can$noc_code),2]
