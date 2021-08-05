#open data
lfs <- read.csv("~/Desktop/Data/LFS/lfs_17_21.csv")

lfs <- subset(lfs, SURVYEAR=="2020" & SURVMNTH<5)

#employment (at work)
lfs$emp <- as.numeric(lfs$LFSSTAT <=2)

#weighted employment
lfs$w_emp <- lfs$emp*lfs$FINALWT 

lfs2 <- lfs[,c(grep("SURVMNTH", colnames(lfs)),
               grep("w_emp", colnames(lfs)),
               grep("NAICS_21", colnames(lfs)),
               grep("NOC_40", colnames(lfs)),
               grep("NOC_10", colnames(lfs)),
               grep("PROV", colnames(lfs)),
               grep("CMA", colnames(lfs)))]

dis <- c("NAICS_21", "NOC_40", "NOC_10", "PROV", "CMA")

i <- "NOC_10"

for (i in dis){

  lfs_temp <- aggregate(lfs2$w_emp, 
                        by=list(lfs2[,grep(get("i"), colnames(lfs2))],
                               lfs2$SURVMNTH), FUN=sum)
  
  colnames(lfs_temp) <- c(get("i"), "month", "employment")
  
  #wide to long
  lfs_temp_w <- dcast(lfs_temp, lfs_temp[,1] ~ month, value.var="employment")
  
  colnames(lfs_temp_w) <- c(get("i"), "jan20", "feb20", "mar20", "apr20")
  
  lfs_temp_w$janfeb <- (lfs_temp_w$feb20/lfs_temp_w$jan20-1)*100
  lfs_temp_w$febmar <- (lfs_temp_w$mar20/lfs_temp_w$feb20-1)*100
  lfs_temp_w$marapr <- (lfs_temp_w$apr20/lfs_temp_w$mar20-1)*100
  lfs_temp_w$febapr <- (lfs_temp_w$apr20/lfs_temp_w$feb20-1)*100
  
  ##OPEN REMOTE WORK INDEX ESTIMATES AND MERGE
   
  file_name <- paste(get("i"), "remote_work.csv", sep="_")
  
  file_name <- paste("./Data/Output/", file_name, sep="")
  
  remote_work_temp <- read.csv(get("file_name"))
  colnames(remote_work_temp)[2] <- c(get("i"))
  
  lfs_temp_w <- merge(lfs_temp_w, remote_work_temp, by=(get("i")))
  
  #save file
  file_name <- paste(get("i"), "employment_variation.csv", sep="_")
  
  file_name <- paste("./Data/Output/", file_name, sep="")
  
  write.csv(lfs_temp_w, file = get("file_name"))
  
}



