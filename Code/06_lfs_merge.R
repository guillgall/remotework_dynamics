files <- list.files(path="~/Desktop/Data/LFS/2017-2021", pattern='[.]csv')

files

lfs <- as.data.frame(matrix(ncol=60, nrow=1))

for (i in files){
  
  path <- paste("~/Desktop/Data/LFS/2017-2021/",get("i"), sep="")
  
  temp1 <- read.csv(path)
  
  colnames(lfs) <- colnames(temp1)
  
  lfs <- rbind(lfs, temp1)
  
  print(i)
  
}  

lfs <- lfs[-1,]

write.csv(lfs, file = "~/Desktop/Data/LFS/lfs_17_21.csv")
