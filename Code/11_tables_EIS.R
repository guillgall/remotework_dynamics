#open data
estimates <- read.csv("./Data/Output/remote_work_geographies_estimates.csv")

estimates <- estimates[,-1]

colnames(estimates)[c(2,3,6,7)] <- c("Unweighted", "Weighted by Wages", 
                                   "Employment", "Wages")

estimates <- estimates[order(-estimates$Unweighted),]

#need for later aggregation for small city and rural estimation
estimates$remote_work_job <- estimates$Unweighted*estimates$Employment
estimates$remote_work_wages <- estimates$`Weighted by Wages`*estimates$Wages

#canada as a whole
canada <- subset(estimates, canada==1)

print(xtable(canada[,2:3],
             caption = "Share of jobs that can be done at home",
             label = "tab:canada",
             align = "ccc"),
      sanitize.text.function=function(x){x},
      include.rownames = FALSE,
      type="latex", 
      caption.placement = "top",
      file="./Data/Output/table_canada.tex")


#cities

cities <- subset(estimates, city==1)
colnames(cities)[1] <- c("City")

#all cities

print(xtable(cities[,1:3],
             caption = "Share of jobs that can be done at home, by city",
             label = "tab:cities",
             align = "clcc"),
      sanitize.text.function=function(x){x},
      include.rownames = FALSE,
      type="latex", 
      caption.placement = "top",
      file="./Data/Output/table_cities.tex")


#10 largest cities, plus smaller cities, plus rural
#Ottawa (keep main observation)
cities_table <- cities
cities_table <- cities_table[-c(grep("Ontario part", cities_table$City),
                               grep("Quebec part", cities_table$City)),]

cities_table <- cities_table[order(-cities_table$Employment),]

cities_large <- cities_table[1:10,]

#to aggregate across small cities
cities_small <- cities_table[11:nrow(cities_table),]

cities_small["Small Cities",] <- 0
cities_small["Small Cities",2:ncol(cities_small)] <- colSums(cities_small[,2:ncol(cities_small)])
cities_small["Small Cities","Unweighted"] <- cities_small$remote_work_job[nrow(cities_small)]/cities_small$Employment[nrow(cities_small)]
cities_small["Small Cities","Weighted by Wages"] <- cities_small$remote_work_wages[nrow(cities_small)]/cities_small$Wages[nrow(cities_small)]
cities_small["Small Cities",1] <- "Rest of smaller cities"


#merge small cities and rural into large city table
cities_large <- cities_large[order(-cities_large$Unweighted),]

colnames(cities_small)[1] <- "City"

cities_large <- rbind(cities_large, cities_small["Small Cities",])

#city employment share (fraction of total)
cities_large$'National Employment Share' <- cities_large$Employment/canada[1,"Employment"]

  
   
print(xtable(cities_large[,c(1,2,3,14)],
             caption = "Share of jobs that can be done at home, 10 largest cities and (aggregated) smaller cities",
             label = "tab:table_cities_largest",
             align = "clccc"),
      sanitize.text.function=function(x){x},
      include.rownames = FALSE,
      type="latex", 
      caption.placement = "top",
      file="./Data/Output/table_cities_largest.tex")


###

#rural estimation: see footnote 5
#first need to aggregate across all urban
cities_ag <- cities_table

cities_ag["Urban",] <- 0
cities_ag["Urban",2:ncol(cities_ag)] <- colSums(cities_ag[,2:ncol(cities_ag)])
cities_ag["Urban","Unweighted"] <- cities_ag$remote_work_job[nrow(cities_ag)]/cities_ag$Employment[nrow(cities_ag)]
cities_ag["Urban","Weighted by Wages"] <- cities_ag$remote_work_wages[nrow(cities_ag)]/cities_ag$Wages[nrow(cities_ag)]
cities_ag["Urban",1] <- "Urban"

cities_ag["Rural",] <- 0

cities_ag["Rural","Employment"] <- canada[1,"Employment"] - cities_ag["Urban","Employment"]

#note: wages for all canada are in hundreds (relative to city unit)

cities_ag["Rural","Wages"] <- canada[1,"Wages"]*100-cities_ag["Urban","Wages"]


cities_ag["Rural","Unweighted"] <- (canada[1,"remote_work_job"]-cities_ag["Urban","remote_work_job"])/cities_ag["Rural","Employment"]

cities_ag["Rural","Weighted by Wages"] <- (canada[1,"remote_work_wages"]-cities_ag["Urban","remote_work_wages"])/cities_ag["Rural","Wages"]

#rural estimate:
round(cities_ag["Rural","Unweighted"],3) 
#rural employment share (fraction of total)
cities_ag$'National Employment Share' <- cities_ag$Employment/canada[1,"Employment"]
round(cities_ag["Rural","National Employment Share"],2)

#note: something strange with wages of urban vs canada total? need for rural computation

###

#provinces

provincial <- subset(estimates, province==1)

colnames(provincial)[1] <- c("Province")

print(xtable(provincial[,1:3],
             caption = "Share of jobs that can be done at home, by province",
             label = "tab:provinces",
             align = "clcc"),
      sanitize.text.function=function(x){x},
      include.rownames = FALSE,
      type="latex", 
      caption.placement = "top",
      file="./Data/Output/table_provinces.tex")



#territories

territory <- subset(estimates, territory==1)

colnames(territory)[1] <- c("Territory")

print(xtable(territory[,1:3],
             caption = "Share of jobs that can be done at home, by territory",
             label = "tab:territory",
             align = "clcc"),
      sanitize.text.function=function(x){x},
      include.rownames = FALSE,
      type="latex", 
      caption.placement = "top",
      file="./Data/Output/table_territory.tex")

#cities: share and size
cities_large <- cities
cities_large <- cities_large[-c(grep("Ontario part", cities_large$City),
                                grep("Quebec part", cities_large$City)),]

g <- ggplot(data=cities_large, aes(y=Unweighted, x=Employment/1000000)) 
g <- g +  geom_point() + geom_smooth(method=lm, colour="black", se=FALSE , formula=y~log(x))
#g <- g + geom_text(aes(label=City),hjust=0, vjust=0)
g <- g + theme_bw()
g <- g + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12))
g <- g + labs(y="Remote Work Index", x = "Employment (millions)")
g

#save file
pdf("./Data/Output/cities_share_size.pdf")
g
dev.off()

##TABLE A1: Comparison of the Benchmark Remote Work
#Index and the Alternative Remote Work Index

#open datasets
onetnoc <- read.csv("./Data/Output/eis_onetnoc_remote.csv")
manual <- read.csv("./Data/Output/eis_manual_remote.csv")

onetnoc <- subset(onetnoc, geography=="Canada [1]")
manual <- subset(manual, geography=="Canada [1]")

onetnoc <- onetnoc[,c(4,10)]
manual <- manual[,c(4,14)]

merge <- merge(onetnoc, manual, by=c("occupation"))

colnames(merge) <- c("Occupation", "Benchmark", "Alternative")

merge$diff <- abs(merge$Benchmark - merge$Alternative)

merge <- merge[order(-merge$diff),]

large_diff <- subset(merge, diff==1)

large_diff <- large_diff[order(-large_diff$Benchmark),]

large_diff[,2:3] <- round(large_diff[,2:3],0)

print(xtable(large_diff[,1:3],
             caption = "Comparison of the Benchmark Remote Work
Index and the Alternative Remote Work Index",
             label = "tab:tablea1",
             align = "llcc"),
      sanitize.text.function=function(x){x},
      include.rownames = FALSE,
      type="latex", 
      caption.placement = "top",
      file="./Data/Output/table_comparison_a1.tex")

##ROBUSTNESS: compare indices
g <- ggplot(data=provincial, aes(y=provincial[,2], x=provincial[,4])) 
g <- g +  geom_point() + geom_smooth(method=lm, colour="black", se=FALSE)
g <- g + geom_text_repel(aes(label=Province),hjust=0, vjust=0)
g <- g + theme_bw()
g <- g + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12))
g <- g + labs(y="Benchmark", x = "Alternative")
g

#save file
pdf("./Data/Output/jobs_provinces_comparison_robustness.pdf")
g
dev.off()
