#open data
lfs <- read.csv("~/Desktop/Data/LFS/lfs_17_21.csv")

lfs <- subset(lfs, SURVYEAR=="2019" & lfs$LFSSTAT <=2)

#merge with NOC 40 remote work index

noc40 <- read.csv("./Data/Output/NOC_40_remote_work.csv")
colnames(noc40)[2] <- c("NOC_40")

lfs <- merge(lfs, noc40, by="NOC_40")

lfs$emp <- as.numeric(lfs$LFSSTAT <=2)
lfs$male <- as.numeric(lfs$SEX==1)
lfs$parttime <- as.numeric(lfs$FTPTMAIN==2)
lfs$nonimmigrant <- as.numeric(lfs$IMMIG==3)
lfs$less50 <- as.numeric(lfs$AGE_12<8)
lfs$private <- as.numeric(lfs$COWMAIN==2)
lfs$small_firm <- as.numeric(lfs$FIRMSIZE<=2)
lfs$single <- as.numeric(lfs$MARSTAT==6)
lfs$no_college <- as.numeric(lfs$EDUC<3)
lfs$job_nature <- as.numeric(lfs$PERMTEMP>1)

lfs$wage <- lfs$HRLYEARN*lfs$UHRSMAIN
median_wage <- median(lfs$wage, na.rm=TRUE)
lfs$below_median_wage <- as.numeric(lfs$wage < median_wage)

#REGRESSIONS

ols1 <- lm(male ~ remote_work_onet, data=lfs)
ols2 <- lm(parttime ~ remote_work_onet, data=lfs)
ols3 <- lm(below_median_wage ~ remote_work_onet, data=lfs)
ols4 <- lm(no_college ~ remote_work_onet, data=lfs)
ols5 <- lm(nonimmigrant ~ remote_work_onet, data=lfs)
ols6 <- lm(less50 ~ remote_work_onet, data=lfs)
ols7 <- lm(single ~ remote_work_onet, data=lfs)
ols8 <- lm(small_firm ~ remote_work_onet, data=lfs)
ols9 <- lm(private ~ remote_work_onet, data=lfs)
ols10 <- lm(job_nature ~ remote_work_onet, data=lfs)

ols11 <- lm(male ~ remote_work_manual, data=lfs)
ols12 <- lm(parttime ~ remote_work_manual, data=lfs)
ols13 <- lm(below_median_wage ~ remote_work_manual, data=lfs)
ols14 <- lm(no_college ~ remote_work_manual, data=lfs)
ols15 <- lm(nonimmigrant ~ remote_work_manual, data=lfs)
ols16 <- lm(less50 ~ remote_work_manual, data=lfs)
ols17 <- lm(single ~ remote_work_manual, data=lfs)
ols18 <- lm(small_firm ~ remote_work_manual, data=lfs)
ols19 <- lm(private ~ remote_work_manual, data=lfs)
ols20 <- lm(job_nature ~ remote_work_manual, data=lfs)

workers_cat <- as.data.frame(c("Male", 
                               "Part time employed", 
                               "Below median wage",
                               "No college education", 
                               "Non-immigrant", 
                               "Age less than 50 years",
                               "Single", 
                               "Employed in small firm (<100 emp", 
                               "Private sector employee", 
                               "Seasonal/contractual job"))

workers_cat$estimates_onetnoc <- c(summary(ols1)$coefficients[2],
                           summary(ols2)$coefficients[2],
                           summary(ols3)$coefficients[2],
                           summary(ols4)$coefficients[2],
                           summary(ols5)$coefficients[2],
                           summary(ols6)$coefficients[2],
                           summary(ols7)$coefficients[2],
                           summary(ols8)$coefficients[2],
                           summary(ols9)$coefficients[2],
                           summary(ols10)$coefficients[2])       

workers_cat$estimates_manual <- c(summary(ols11)$coefficients[2],
                                     summary(ols12)$coefficients[2],
                                     summary(ols13)$coefficients[2],
                                     summary(ols14)$coefficients[2],
                                     summary(ols15)$coefficients[2],
                                     summary(ols16)$coefficients[2],
                                     summary(ols17)$coefficients[2],
                                     summary(ols18)$coefficients[2],
                                     summary(ols19)$coefficients[2],
                                     summary(ols20)$coefficients[2])        


colnames(workers_cat)[1] <- c("category")

#save files
write.csv(workers_cat, file = "./Data/Output/worker_heterogeneity_regression.csv")

##INCOME PERCENTILE

lfs2 <- lfs

lfs2 <- lfs2[order(lfs2$wage),]
lfs2 <- lfs2[!is.na(lfs2$wage),]

lfs2$decile <- as.numeric(cut_number(lfs2$wage, 10))

lfs2 <- lfs2[,c(63,64,78)]

lfs_ag <- aggregate(lfs2[,c(1:3)], by=list(lfs2$decile), FUN=mean)

#save files
write.csv(lfs_ag, file = "./Data/Output/income_decile_remote_work.csv")

