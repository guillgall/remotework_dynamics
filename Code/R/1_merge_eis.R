library("readxl")

files <- list.files(path="./Data/Input/EIS", pattern='[.]csv')

i <- files[1]

labor_data <- as.data.frame(matrix(ncol=5, nrow=1))
colnames(labor_data) <- c("occupation", "employment", "median.income", "mean.income", "geography")

for (i in files){
  
  path <- paste("./Data/Input/EIS/",get("i"), sep="")
  
  temp1 <- read.csv(path)
  
  geo_name <- gsub('Geography = ','\\1',temp1[1,1])
  
  colnames(temp1) <- c("occupation", "employment", "median.income", "mean.income")

  temp1 <- temp1[-c(1:8),-5]
  temp1$geography <- print(geo_name)
  
  labor_data <- rbind(labor_data, temp1)
  
}

labor_data <- labor_data[-1,]

unique(labor_data$geography)

#save file
write.csv(labor_data, file = "./Data/Output/merge.csv")
