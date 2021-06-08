#open data
lfs <- read.csv("~/Desktop/Data/LFS/lfs_17_21.csv")

lfs <- subset(lfs, SURVYEAR=="2019")

#employment (at work)
lfs$emp <- as.numeric(lfs$LFSSTAT <=2)

#weighted employment
lfs$w_emp <- lfs$emp*lfs$FINALWT 

lfs2 <- lfs[,c(grep("SURVMNTH", colnames(lfs)),
               grep("w_emp", colnames(lfs)),
               grep("NAICS_21", colnames(lfs)),
               grep("NOC_40", colnames(lfs)),
               grep("PROV", colnames(lfs)),
               grep("CMA", colnames(lfs)))]

dis <- c("NAICS_21", "PROV", "CMA")

i <- "NAICS_21"

for (i in dis){
  lfs_ag <- aggregate(lfs2, by=list(lfs2[,grep(get("i"), colnames(lfs2))], 
                                   lfs2$NOC_40), FUN=sum)
  
  lfs_ag <- lfs_ag[,c(1,2,4)]
  
  colnames(lfs_ag) <- c(get("i"),
                        "noc40",
                        "w_emp")
  
  #merge with remote work data
  noc40 <- read.csv("./Data/Output/NOC_40_remote_work.csv")
  
  colnames(noc40)[1:2] <- c("noc40", "occupation")
  
  lfs_remote <- merge(lfs_ag, noc40, by=c("noc40"))
  
  lfs_category_remote <- as.data.frame(matrix(ncol=3, nrow=0))

  k <- 1
  
  for (k in sort(unique(lfs_remote[,2]))){
    
    lfs_remote_temp <- subset(lfs_remote, lfs_remote[,2]==k)
    
    onet <- weighted.mean(lfs_remote_temp$remote_work_onet, lfs_remote_temp$w_emp)
    
    manual <- weighted.mean(lfs_remote_temp$remote_work_manual, lfs_remote_temp$w_emp)
    
    lfs_category_remote <- rbind(lfs_category_remote,
                                 c(get("k"), onet, manual))

    
  }
  
  colnames(lfs_category_remote) <- c(get("i"),
                                 "remote_work_onet", 
                                 "remote_work_manual")
  
  if (i=="NAICS_21"){
    lfs_category_remote$name <- c("Agriculture",
                               "Forestry and logging and support activities for forestry",
                               "Fishing, hunting and trapping",
                               "Mining, quarrying, and oil and gas extraction",
                               "Utilities",
                               "Construction",
                               "Manufacturing - durable goods",
                               "Manufacturing - non-durable goods",
                               "Wholesale trade",
                               "Retail trade",
                               "Transportation and warehousing",
                               "Finance and insurance",
                               "Real estate and rental and leasing",
                               "Professional, scientific and technical services",
                               "Business, building and other support services",
                               "Educational services",
                               "Health care and social assistance",
                               "Information, culture and recreation",
                               "Accommodation and food services",
                               "Other services (except public administration)",
                               "Public administration")
  }
  
  if (i=="PROV"){
    lfs_category_remote$name <- c("Newfoundland",
                                  "Prince Edward Island",
                                  "Nova Scotia",
                                  "New Brunswick",
                                  "Québec",
                                  "Ontario",
                                  "Manitoba",
                                  "Saskatchewan",
                                  "Alberta",
                                  "British Columbia")
  }
  
  
  if (i=="CMA"){
    lfs_category_remote$name <- c("Other CMA or non-CMA",
                                  "Québec",
                                  "Montréal",
                                  "Ottawa",
                                  "Toronto",
                                  "Hamilton",
                                  "Winnipeg",
                                  "Calgary",
                                  "Edmonton",
                                  "Vancouver")
  }
  

  #save file
  file_name <- paste(get("i"), "remote_work.csv", sep="_")
  
  file_name <- paste("./Data/Output/", file_name, sep="")
  
  write.csv(lfs_category_remote, file = get("file_name"))
  
}

