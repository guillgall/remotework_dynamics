#open datasets
onetnoc <- read.csv("./Data/Output/eis_onetnoc_remote.csv")
manual <- read.csv("./Data/Output/eis_manual_remote.csv")

#convert mean income to numeric (some NA's otherwise)
onetnoc$mean.income <- as.numeric(onetnoc$mean.income)
manual$mean.income <- as.numeric(manual$mean.income)
#manual$remote_workable <- as.numeric(manual$remote_workable)

##onet estimation
#remote_work jobs
onetnoc$remote_work_q <- onetnoc$employment*onetnoc$remote_work
#remote_work income:
onetnoc$remote_work_p <- (onetnoc$employment*onetnoc$mean.income)*onetnoc$remote_work

##manual estimation
#remote_work jobs: 
manual$remote_work_q <- manual$employment*manual$remote_work
#remote_work income:
manual$remote_work_p <- (manual$employment*manual$mean.income)*manual$remote_work

#total income (needed for later)
onetnoc$labor_income <- onetnoc$employment*onetnoc$mean.income
manual$labor_income <- manual$employment*manual$mean.income

geographies <- unique(onetnoc$geography)

i <- geographies[1]

geo_estimates <- as.data.frame(matrix(ncol=7, nrow=1))
colnames(geo_estimates) <- c("geography", 
                             "estimate_jobs_onet", "estimate_wages_onet",
                             "estimate_jobs_manual", "estimate_wages_manual",
                             "total_employment", "total_wages")

for (i in geographies){
  
  temp1 <- subset(onetnoc, geography==get("i"))
  temp2 <- subset(manual, geography==get("i"))
  
  temp3 <- as.data.frame(matrix(ncol=7, nrow=1))
  colnames(temp3) <- c("geography", 
                       "estimate_jobs_onet", "estimate_wages_onet",
                       "estimate_jobs_manual", "estimate_wages_manual",
                       "total_employment", "total_wages")
  
  temp3[1,] <- c(get("i"), 
                 sum(temp1$remote_work_q)/sum(temp1$emp),
                 sum(temp1$remote_work_p)/sum(temp1$labor_income),
                 sum(temp2$remote_work_q)/sum(temp2$emp),
                 sum(temp2$remote_work_p)/sum(temp2$labor_income),
                 sum(temp1$emp),
                 sum(temp1$mean.income))
  
  geo_estimates <- rbind(geo_estimates, temp3)
  
}

geo_estimates <- geo_estimates[-1,]

#clean geographies name
#second Quebec is Quebec city
geo_estimates$geography <- gsub("Québec", "Quebec City", geo_estimates$geography)
#remove "[1]" in some names
geo_estimates$geography <- gsub("\\[|1\\]", "", geo_estimates$geography)
#remove french translation
geo_estimates$geography <- gsub("/.*","",geo_estimates$geography)
#remove end empty space from names
geo_estimates$geography <- trimws(geo_estimates$geography, which=c("both"))
#remove accents
geo_estimates$geography <- gsub("é", "e", geo_estimates$geography)
geo_estimates$geography <- gsub("è", "e", geo_estimates$geography)
#remove dash
geo_estimates$geography <- gsub("-", " ", geo_estimates$geography)
#fix Ottawa's division name
geo_estimates$geography <- gsub("partie du Quebec", "Quebec part)", geo_estimates$geography)
geo_estimates$geography <- gsub("Ontario part", "Ontario part)", geo_estimates$geography)


#generate dummy variable for provinces

geo_estimates$province <- ifelse(
                          (geo_estimates$geography == "Alberta" | 
                             geo_estimates$geography == "British Columbia" |
                             geo_estimates$geography == "Manitoba" |
                             geo_estimates$geography == "New Brunswick" | 
                             geo_estimates$geography == "Newfoundland and Labrador" |
                             geo_estimates$geography == "Nova Scotia" |
                             geo_estimates$geography == "Ontario" | 
                             geo_estimates$geography == "Prince Edward Island" | 
                             geo_estimates$geography == "Quebec" | 
                             geo_estimates$geography == "Saskatchewan"),
                          1, 0)


#territory dummy variable
geo_estimates$territory <- ifelse(
  (geo_estimates$geography == "Yukon" | 
     geo_estimates$geography == "Northwest Territories" | 
     geo_estimates$geography == "Nunavut"),
  1, 0)

#canada  dummy variable
geo_estimates$canada <- ifelse(
  (geo_estimates$geography=="Canada"),
  1, 0)

#city  dummy variable
geo_estimates$city <- ifelse(
  (geo_estimates$province == 0 & 
     geo_estimates$territory == 0 & 
     geo_estimates$canada == 0),
  1, 0)

#save file
write.csv(geo_estimates, file = "./Data/Output/remote_work_geographies_estimates.csv")


