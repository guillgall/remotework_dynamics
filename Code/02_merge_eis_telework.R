#EIS DATA
eis <- read.csv("./Data/Output/eis_merge.csv")

#eis <- subset(eis, geography=="Canada [1]")

#separate code from name
eis$noc_code <- gsub("[^0-9]", "",  eis[,2])

#only keep 4 digit occupations
eis$nchar <- nchar(eis$noc_code) 
eis <- subset(eis, nchar=="4")

#0105" "0709: 
#two 2 digit occupations still in EIS? manually delete
eis <- eis[-c(grep("0105", eis$noc_code),
              grep("0709", eis$noc_code)),]

geo <- unique(eis$geography)
25500/51

##MANUAL DATA (FROM IES?)
manual <- read.csv("./Data/Input/Manual/manualnoc.csv")

#separate code from name
manual$noc_code <- gsub("[^0-9]", "",  manual[,2])

setdiff(eis$noc_code, manual$noc_code)

#before I changes file on May 27, 2021, 4pm BA
#5125: Translators, terminologists and interpreters
#Why not in manual file? Shelley's email is correct

#Now: OK

#compute average of both introspection
for (i in 1:nrow(manual)){
  manual$remote_work[i] <- mean(c(manual[i,3], manual[i,4]))
  manual$diff[i] <- manual[i,3]-manual[i,4]
}

eis_manual <- merge(eis, manual, by="noc_code")

#save file
write.csv(eis_manual, file = "./Data/Output/eis_manual_remote.csv")

#####
##crosswalk ONET NOC by BIIE
biie <- read.csv("./Data/Input/BIIE/onetnoc.csv")
biie <- as.data.frame(biie[,-1])

#ONET remote work index from DN 
dn <- read.csv("./Data/Input/DN/occupations_workathome.csv")
colnames(dn)[1] <- "onet"

#merge by onet code
biie_dn <- merge(biie, dn, by="onet")

#biie has 1038-922=116 occupations more than DN/ONET...

#separate noc code
biie_dn$noc_code <- gsub("[^0-9]", "",  biie_dn$noc_title)

#in previous script I deleted duplicates, but I should of really aggregated...
#since at NOC level they might have different remote work index
#example: NOC code 0422, 0731, etc.

biie_dn_ag_noc <- biie_dn[,c(6,5)]

biie_dn_ag_noc2 <- aggregate(biie_dn_ag_noc, 
                             by=list(biie_dn_ag_noc$noc_code), 
                             FUN="mean")

biie_dn_ag_noc2 <- biie_dn_ag_noc2[,c(1,3)]
colnames(biie_dn_ag_noc2) <- c("noc_code", "remote_work")

###
#eis with remote work index using noc code
eis_onet <- merge(eis, biie_dn_ag_noc2, 
                  by="noc_code")

#save file
write.csv(eis_onet, file = "./Data/Output/eis_onetnoc_remote.csv")

#check differences
#EIS has 17 occupations more than DN/ONET

eis <- unique(eis_manual$noc_code)
onetnoc <- unique(eis_onet$noc_code)

inter <- intersect(unique(eis_manual$noc_code), unique(eis_onet$noc_code))
sdiff <- setdiff(unique(eis_manual$noc_code), unique(eis_onet$noc_code))
sdiff