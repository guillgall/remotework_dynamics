library(xtable)
library(ggplot2)
library(ggrepel)

#open data
estimates <- read.csv("./Data/Output/telework_estimates.csv")

estimates <- estimates[,-1]

colnames(estimates)[c(2,3,6,7)] <- c("Unweighted", "Weighted by Wages", 
                                   "Employment", "Wages")

estimates <- estimates[order(-estimates$Unweighted),]

#need for later aggregation for small city and rural estimation
estimates$teleworkable_job <- estimates$Unweighted*estimates$Employment
estimates$teleworkable_wages <- estimates$`Weighted by Wages`*estimates$Wages


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
cities_small["Small Cities","Unweighted"] <- cities_small$teleworkable_job[nrow(cities_small)]/cities_small$Employment[nrow(cities_small)]
cities_small["Small Cities","Weighted by Wages"] <- cities_small$teleworkable_wages[nrow(cities_small)]/cities_small$Wages[nrow(cities_small)]
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
cities_ag["Urban","Unweighted"] <- cities_ag$teleworkable_job[nrow(cities_ag)]/cities_ag$Employment[nrow(cities_ag)]
cities_ag["Urban","Weighted by Wages"] <- cities_ag$teleworkable_wages[nrow(cities_ag)]/cities_ag$Wages[nrow(cities_ag)]
cities_ag["Urban",1] <- "Urban"

cities_ag["Rural",] <- 0

cities_ag["Rural","Employment"] <- canada[1,"Employment"] - cities_ag["Urban","Employment"]

#note: wages for all canada are in hundreds (relative to city unit)

cities_ag["Rural","Wages"] <- canada[1,"Wages"]*100-cities_ag["Urban","Wages"]


cities_ag["Rural","Unweighted"] <- (canada[1,"teleworkable_job"]-cities_ag["Urban","teleworkable_job"])/cities_ag["Rural","Employment"]

cities_ag["Rural","Weighted by Wages"] <- (canada[1,"teleworkable_wages"]-cities_ag["Urban","teleworkable_wages"])/cities_ag["Rural","Wages"]

#rural estimate:
cities_ag["Rural","Unweighted"] 
#rural employment share (fraction of total)
cities_ag$'National Employment Share' <- cities_ag$Employment/canada[1,"Employment"]
cities_ag["Rural","National Employment Share"]

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


##
#plots
g <- ggplot(data=provincial, aes(y=provincial[,2], x=provincial[,3])) 
g <- g +  geom_point() + geom_smooth(method=lm, colour="black", se=FALSE)
g <- g + geom_text_repel(aes(label=Province),hjust=0, vjust=0)
g <- g + theme_bw()
g <- g + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12))
g <- g + labs(y="Unweighted", x = "Weighted by Wages")
g

#save file
pdf("./Data/Output/jobs_wages_provinces.pdf")
g
dev.off()

g <- ggplot(data=cities, aes(y=cities[,2], x=cities[,3])) 
g <- g +  geom_point() + geom_smooth(method=lm, colour="black", se=FALSE)
#g <- g + geom_text(aes(label=City),hjust=0, vjust=0)
g <- g + theme_bw()
g <- g + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12))
g <- g + labs(y="Unweighted", x = "Weighted by Wages")
g

#save file
pdf("./Data/Output/jobs_wages_cities.pdf")
g
dev.off()

g <- ggplot(data=territory, aes(y=territory[,2], x=territory[,3])) 
g <- g +  geom_point() + geom_smooth(method=lm, se=FALSE)
g <- g + geom_text(aes(label=Territory),hjust=0, vjust=0)
g <- g + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12))
g <- g + labs(y="Unweighted", x = "Weighted by Wages")
g

#save file
#pdf("./Data/Output/jobs_wages_territory.pdf")
#g
#dev.off()

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

g <- ggplot(data=cities, aes(y=cities[,2], x=cities[,4])) 
g <- g +  geom_point() + geom_smooth(method=lm, colour="black", se=FALSE)
#g <- g + geom_text(aes(label=City),hjust=0, vjust=0)
g <- g + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12))
g <- g + labs(y="Benchmark", x = "Alternative")
g

#save file
pdf("./Data/Output/jobs_cities_comparison_robustness.pdf")
g
dev.off()

g <- ggplot(data=territory, aes(y=territory[,2], x=territory[,4])) 
g <- g +  geom_point() + geom_smooth(method=lm, se=FALSE)
g <- g + geom_text(aes(label=Territory),hjust=0, vjust=0)
g <- g + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12))
g <- g + labs(y="Benchmark", x = "Alternative")
g

#save file
#pdf("./Data/Output/jobs_territory_comparison_robustness.pdf")
#g
#dev.off()

#compare benchmark vs alternative remote work index
compare <- read.csv("./Data/Output/remote_differs_benchmark_alternative.csv")

colnames(compare)[1] <- "Occupation"

print(xtable(compare,
             caption = "Remote Work Index: Benchmark vs. Alternative Example Comparison",
             label = "tab:remote_work_index_comparison",
             align = "llcc"),
      sanitize.text.function=function(x){x},
      include.rownames = FALSE,
      type="latex", 
      caption.placement = "top",
      file="./Data/Output/remote_work_index_comparison.tex")


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

