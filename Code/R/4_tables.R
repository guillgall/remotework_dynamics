library(xtable)
library(ggplot2)

#open data
estimates <- read.csv("./Data/Output/telework_estimates.csv")

estimates <- estimates[,-1]

colnames(estimates)[2:3] <- c("Unweighted", "Weighted by Wages")

estimates <- estimates[order(-estimates$Unweighted),]

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

#cities

cities <- subset(estimates, city==1)

colnames(cities)[1] <- c("City")

print(xtable(cities[,1:3],
             caption = "Share of jobs that can be done at home, by city",
             label = "tab:cities",
             align = "clcc"),
      sanitize.text.function=function(x){x},
      include.rownames = FALSE,
      type="latex", 
      caption.placement = "top",
      file="./Data/Output/table_cities.tex")

#for slides:
print(xtable(cities[1:18,1:3],
             caption = "Share of jobs that can be done at home, by city",
             label = "tab:provinces",
             align = "clcc"),
      sanitize.text.function=function(x){x},
      include.rownames = FALSE,
      type="latex", 
      size="scriptsize",
      caption.placement = "top",
      file="./Data/Output/table_cities_slides1.tex")

print(xtable(cities[19:nrow(cities),1:3],
             caption = "Share of jobs that can be done at home, by city",
             label = "tab:provinces",
             align = "clcc"),
      sanitize.text.function=function(x){x},
      include.rownames = FALSE,
      type="latex", 
      size="scriptsize",
      caption.placement = "top",
      file="./Data/Output/table_cities_slides2.tex")

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
g <- g +  geom_point() + geom_smooth(method=lm, se=FALSE)
g <- g + geom_text(aes(label=Province),hjust=0, vjust=0)
g <- g + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12))
g <- g + labs(y="Unweighted", x = "Weighted by Wages")
g

#save file
pdf("./Data/Output/jobs_wages_provinces.pdf")
g
dev.off()

g <- ggplot(data=cities, aes(y=cities[,2], x=cities[,3])) 
g <- g +  geom_point() + geom_smooth(method=lm, se=FALSE)
g <- g + geom_text(aes(label=City),hjust=0, vjust=0)
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
g <- g +  geom_point() + geom_smooth(method=lm, se=FALSE)
g <- g + geom_text(aes(label=Province),hjust=0, vjust=0)
g <- g + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12))
g <- g + labs(y="Benchmark", x = "Alternative")
g

#save file
pdf("./Data/Output/jobs_provinces_comparison_robustness.pdf")
g
dev.off()

g <- ggplot(data=cities, aes(y=cities[,2], x=cities[,4])) 
g <- g +  geom_point() + geom_smooth(method=lm, se=FALSE)
g <- g + geom_text(aes(label=City),hjust=0, vjust=0)
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
