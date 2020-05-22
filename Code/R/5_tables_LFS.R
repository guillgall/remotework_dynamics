library(xtable)
library(ggplot2)
library(stargazer)

#open data
provinces <- read.csv("./Data/Output/remote_work_province.csv")
cities <- read.csv("./Data/Output/remote_work_big_cities.csv")
industry <- read.csv("./Data/Output/remote_work_industry.csv")

colnames(provinces)[2:3] <- c("Benchmark", "Alternative")
colnames(cities) <- c("City", "Benchmark", "Alternative", "National Employment Share")
colnames(industry)[2:3] <- c("Benchmark", "Alternative")

provinces <- provinces[order(-provinces$Benchmark),]
cities <- cities[order(-cities$Benchmark),]
industry <- industry[order(-industry$Benchmark),]

print(xtable(provinces,
             caption = "Share of jobs that can be done at home, by province (LFS estimates)",
             label = "tab:provinces_lfs",
             align = "clcc"),
      sanitize.text.function=function(x){x},
      include.rownames = FALSE,
      type="latex", 
      caption.placement = "top",
      file="./Data/Output/table_provinces_lfs.tex")

print(xtable(cities,
             caption = "Share of jobs that can be done at home, by city (LFS estimates)",
             label = "tab:cities_lfs",
             align = "clccc"),
      sanitize.text.function=function(x){x},
      include.rownames = FALSE,
      type="latex", 
      caption.placement = "top",
      file="./Data/Output/table_cities_lfs.tex")

print(xtable(industry,
             caption = "Share of jobs that can be done at home, by industry (LFS estimates)",
             label = "tab:industry_lfs",
             align = "clcc"),
      sanitize.text.function=function(x){x},
      include.rownames = FALSE,
      type="latex", 
      caption.placement = "top",
      file="./Data/Output/table_industry_lfs.tex")

##
#worker characteristic plots
workers <- read.csv("./Data/Output/remote_worker_demographic_economic.csv")

colnames(workers) <- c("char", "benchmark", "alternative")

workers$char <- factor(workers$char, 
                       levels = workers$char[order(workers$benchmark)])


g <- ggplot(data=workers, aes(x=char, y=benchmark)) 
g <- g +  geom_point(stat="identity") + coord_flip()
g <- g + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12))
g <- g + theme(axis.title.x=element_blank(), axis.title.y=element_blank())
g

#save file
pdf("./Data/Output/worker_characteristics_regression_plot.pdf")
g
dev.off()

#dynamics: worker characteristic plots 
#employment dynamics plots

workers_dynamics <- read.csv("./Data/Output/remote_worker_characteristics.csv")
dynamics_2digit <- read.csv("./Data/Output/remote_employment_dynamics_2_digit_occp_with_code.csv")
dynamics_10oc <- read.csv("./Data/Output/remote_employment_dynamics_10_occp.csv")
dynamics_industry <- read.csv("./Data/Output/remote_employment_dynamics_industry_level.csv")
dynamics_province <- read.csv("./Data/Output/remote_employment_dynamics_province.csv")
dynamics_city <- read.csv("./Data/Output/remote_employment_dynamics_CMAs.csv")

#workers characteristics

colnames(workers_dynamics) <- c("char", "janfeb20", "febmar20", "marapr20", "febapr20")

workers_dynamics$char <- factor(workers_dynamics$char, 
                                levels = workers_dynamics$char[order(workers_dynamics$febmar20)])

g <- ggplot(data=workers_dynamics, aes(x=char, y=febmar20)) 
g <- g +  geom_bar(stat="identity") + coord_flip()
g <- g + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12))
g <- g + theme(axis.title.x=element_blank(), axis.title.y=element_blank())
g

workers_merge <- merge(workers, workers_dynamics, by="char")

g <- ggplot(data=workers_merge, aes(y=febmar20, x=benchmark)) 
g <- g +  geom_point() + geom_smooth(method=lm, se=FALSE)
g <- g + geom_text(aes(label=char),hjust=0, vjust=0, size = 5)
g <- g + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12))
g <- g + labs(y="Change (%) in Employment", x = "Beta")
g <- g + theme(legend.position="none")
g



#2 digit occupations

colnames(dynamics_2digit) <- c("code", "dummy", 
                                     "essential_share",
                                     "occupation",
                                     "benchmark", "alternative",
                                     "janfeb20", "febmar20", 
                                     "marapr20", "febapr20")

#dynamics_2digit <- dynamics_2digit[-nrow(dynamics_2digit),]

dynamics_2digit$essential <- "Essential"

for (i in 1:nrow(dynamics_2digit)){
  if (dynamics_2digit$dummy[i]=="0"){
    dynamics_2digit$essential[i] <- "Non-Essential"
  }
}


#2 digit

g <- ggplot(data=dynamics_2digit, aes(y=febmar20, x=benchmark, color=essential)) 
g <- g +  geom_point() + geom_smooth(method=lm, se=FALSE)
g <- g + geom_text(aes(label=code),hjust=0, vjust=0, size = 5)
g <- g + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12))
g <- g + labs(y="Change (%) in Employment", x = "Remote-Work Index")
g <- g + theme(legend.position="none")
g

#save file
pdf("./Data/Output/dynamics_2digit_febmar20.pdf")
g
dev.off()

g <- ggplot(data=dynamics_2digit, aes(y=febapr20, x=benchmark, color=essential)) 
g <- g +  geom_point() + geom_smooth(method=lm, se=FALSE)
g <- g + geom_text(aes(label=code),hjust=0, vjust=0, size = 5)
g <- g + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12))
g <- g + labs(y="Change (%) in Employment", x = "Remote-Work Index")
g <- g + theme(legend.position="none")
g

#save file
pdf("./Data/Output/dynamics_2digit_febapr20.pdf")
g
dev.off()

g <- ggplot(data=dynamics_2digit, aes(y=marapr20, x=benchmark, color=essential)) 
g <- g +  geom_point() + geom_smooth(method=lm, se=FALSE)
g <- g + geom_text(aes(label=code),hjust=0, vjust=0, size = 5)
g <- g + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12))
g <- g + labs(y="Change (%) in Employment", x = "Remote-Work Index")
g <- g + theme(legend.position="none")
g

#save file
pdf("./Data/Output/dynamics_2digit_marapr20.pdf")
g
dev.off()


#10 broad occupation

colnames(dynamics_10oc) <- c("occupation",
                             "benchmark", "alternative",
                             "janfeb20", "febmar20", 
                             "marapr20", "febapr20")


g <- ggplot(data=dynamics_10oc, aes(y=febmar20, x=benchmark)) 
g <- g +  geom_point() + geom_smooth(method=lm, se=FALSE)
g <- g + geom_text(aes(label=occupation),hjust=0, vjust=0)
g <- g + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12))
g <- g + labs(y="Change (%) in Employment", x = "Remote-Work Index")
g

#save file
pdf("./Data/Output/dynamics_10oc_febmar20.pdf")
g
dev.off()

g <- ggplot(data=dynamics_10oc, aes(y=febapr20, x=benchmark)) 
g <- g +  geom_point() + geom_smooth(method=lm, se=FALSE)
g <- g + geom_text(aes(label=occupation),hjust=0, vjust=0)
g <- g + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12))
g <- g + labs(y="Change (%) in Employment", x = "Remote-Work Index")
g


#save file
pdf("./Data/Output/dynamics_10oc_febapr20.pdf")
g
dev.off()

g <- ggplot(data=dynamics_10oc, aes(y=marapr20, x=benchmark)) 
g <- g +  geom_point() + geom_smooth(method=lm, se=FALSE)
g <- g + geom_text(aes(label=occupation),hjust=0, vjust=0)
g <- g + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12))
g <- g + labs(y="Change (%) in Employment", x = "Remote-Work Index")
g


#save file
pdf("./Data/Output/dynamics_10oc_marapr20.pdf")
g
dev.off()


#province
colnames(dynamics_province) <- c("province",
                                 "benchmark", "alternative",
                                 "janfeb20", "febmar20", 
                                 "marapr20", "febapr20")


g <- ggplot(data=dynamics_province, aes(y=febapr20, x=benchmark)) 
g <- g +  geom_point() + geom_smooth(method=lm, se=FALSE)
g <- g + geom_text(aes(label=province),hjust=0, vjust=0)
g <- g + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12))
g <- g + labs(y="Change (%) in Employment", x = "Remote-Work Index")
g

#save file
pdf("./Data/Output/dynamics_province_febapr20.pdf")
g
dev.off()

g <- ggplot(data=dynamics_province, aes(y=febmar20, x=benchmark)) 
g <- g +  geom_point() + geom_smooth(method=lm, se=FALSE)
g <- g + geom_text(aes(label=province),hjust=0, vjust=0)
g <- g + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12))
g <- g + labs(y="Change (%) in Employment", x = "Remote-Work Index")
g

#save file
pdf("./Data/Output/dynamics_province_febmar20.pdf")
g
dev.off()

g <- ggplot(data=dynamics_province, aes(y=marapr20, x=benchmark)) 
g <- g +  geom_point() + geom_smooth(method=lm, se=FALSE)
g <- g + geom_text(aes(label=province),hjust=0, vjust=0)
g <- g + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12))
g <- g + labs(y="Change (%) in Employment", x = "Remote-Work Index")
g

#save file
pdf("./Data/Output/dynamics_province_marapr20.pdf")
g
dev.off()

#city
colnames(dynamics_city) <- c("city",
                             "benchmark", "alternative",
                             "janfeb20", "febmar20", 
                             "marapr20", "febapr20")

g <- ggplot(data=dynamics_city, aes(y=febmar20, x=benchmark)) 
g <- g +  geom_point() + geom_smooth(method=lm, se=FALSE)
g <- g + geom_text(aes(label=city),hjust=0, vjust=0)
g <- g + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12))
g <- g + labs(y="Change (%) in Employment", x = "Remote-Work Index")
g

#save file
pdf("./Data/Output/dynamics_city_febmar20.pdf")
g
dev.off()


g <- ggplot(data=dynamics_city, aes(y=febapr20, x=benchmark)) 
g <- g +  geom_point() + geom_smooth(method=lm, se=FALSE)
g <- g + geom_text(aes(label=city),hjust=0, vjust=0)
g <- g + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12))
g <- g + labs(y="Change (%) in Employment", x = "Remote-Work Index")
g

#save file
pdf("./Data/Output/dynamics_city_febapr20.pdf")
g
dev.off()

g <- ggplot(data=dynamics_city, aes(y=marapr20, x=benchmark)) 
g <- g +  geom_point() + geom_smooth(method=lm, se=FALSE)
g <- g + geom_text(aes(label=city),hjust=0, vjust=0)
g <- g + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12))
g <- g + labs(y="Change (%) in Employment", x = "Remote-Work Index")
g

#save file
pdf("./Data/Output/dynamics_city_marapr20.pdf")
g
dev.off()




#industry
colnames(dynamics_industry) <- c("industry",
                                 "benchmark", "alternative",
                                 "janfeb20", "febmar20", 
                                 "marapr20", "febapr20")


g <- ggplot(data=dynamics_industry, aes(y=febmar20, x=benchmark)) 
g <- g +  geom_point() + geom_smooth(method=lm, se=FALSE)
g <- g + geom_text(aes(label=industry),hjust=0, vjust=0)
g <- g + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12))
g <- g + labs(y="Change (%) in Employment", x = "Remote-Work Index")
g

#save file
pdf("./Data/Output/dynamics_industry_febmar20.pdf")
g
dev.off()

g <- ggplot(data=dynamics_industry, aes(y=febapr20, x=benchmark)) 
g <- g +  geom_point() + geom_smooth(method=lm, se=FALSE)
g <- g + geom_text(aes(label=industry),hjust=0, vjust=0)
g <- g + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12))
g <- g + labs(y="Change (%) in Employment", x = "Remote-Work Index")
g

#save file
pdf("./Data/Output/dynamics_industry_febapr20.pdf")
g
dev.off()

g <- ggplot(data=dynamics_industry, aes(y=marapr20, x=benchmark)) 
g <- g +  geom_point() + geom_smooth(method=lm, se=FALSE)
g <- g + geom_text(aes(label=industry),hjust=0, vjust=0)
g <- g + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12))
g <- g + labs(y="Change (%) in Employment", x = "Remote-Work Index")
g

#save file
pdf("./Data/Output/dynamics_industry_marapr20.pdf")
g
dev.off()


#############
#regressions

#2-digit occupations
ols1 <- lm(janfeb20 ~ benchmark, data=dynamics_2digit)
ols2 <- lm(janfeb20 ~ benchmark + essential, data=dynamics_2digit)
ols3 <- lm(febmar20 ~ benchmark, data=dynamics_2digit)
ols4 <- lm(febmar20 ~ benchmark + essential, data=dynamics_2digit)
ols5 <- lm(marapr20 ~ benchmark, data=dynamics_2digit)
ols6 <- lm(marapr20 ~ benchmark + essential, data=dynamics_2digit)
ols7 <- lm(febapr20 ~ benchmark, data=dynamics_2digit)
ols8 <- lm(febapr20 ~ benchmark + essential, data=dynamics_2digit)

stargazer(ols1, ols2, ols3, ols4, ols5, ols6, ols7, ols8,
          dep.var.labels=c("$\\triangle q_{j,Jan,Feb}$", 
                           "$\\triangle q_{j,Feb,Mar}$",
                           "$\\triangle q_{j,Mar,Apr}$",
                           "$\\triangle q_{j,Feb,Apr}$"),
          covariate.labels=c("$S_{j} (Benchmark)$",
                             "$D_{j}$"),
          dep.var.caption="Monthly Percentage Change in Employment",
          label="tab:regression_dynamics_2digit",
          title="Employment Change and Remote Work Index: 2-digit level",
          omit.stat=c("ser", "f"),
          font.size="footnotesize",
          notes = c("$\\triangle q_{j,t,\\tau}$ is the percentage change in employment between month $t$ and $\\tau$ in occupation $j$,",
          "$S_{j} (Benchmark)$ is the Remote Work Index and $D_{j}$ is the Essential Service Dummy variable.",
          "Source: author's calculations."),
          notes.align = "l",
          out="./Data/Output/regression_dynamics_2digit.tex")



#alternative index
ols1 <- lm(janfeb20 ~ alternative, data=dynamics_2digit)
ols2 <- lm(janfeb20 ~ alternative + essential, data=dynamics_2digit)
ols3 <- lm(febmar20 ~ alternative, data=dynamics_2digit)
ols4 <- lm(febmar20 ~ alternative + essential, data=dynamics_2digit)
ols5 <- lm(marapr20 ~ alternative, data=dynamics_2digit)
ols6 <- lm(marapr20 ~ alternative + essential, data=dynamics_2digit)
ols7 <- lm(febapr20 ~ alternative, data=dynamics_2digit)
ols8 <- lm(febapr20 ~ alternative + essential, data=dynamics_2digit)


stargazer(ols1, ols2, ols3, ols4, ols5, ols6, ols7, ols8,
          dep.var.labels=c("$\\triangle q_{j,Jan,Feb}$", 
                           "$\\triangle q_{j,Feb,Mar}$",
                           "$\\triangle q_{j,Mar,Apr}$",
                           "$\\triangle q_{j,Feb,Apr}$",
                           "$\\triangle q_{j,Jan,Feb}$", 
                           "$\\triangle q_{j,Feb,Mar}$",
                           "$\\triangle q_{j,Mar,Apr}$",
                           "$\\triangle q_{j,Feb,Apr}$"),
          covariate.labels=c("$S_{j} (Alternative)$",
                             "$D_{j}$"),
          label="tab:regression_dynamics_2digit_alternative",
          title="Employment Change and Remote Work Index: 2-digit level (Robustness Check: Alternative Remote Work Index)",
          omit.stat=c("ser", "f"),
          font.size="footnotesize",
          out="./Data/Output/regression_dynamics_2digit_alternative.tex")

#alternative essential service index (with benchmark remote work index)
ols1 <- lm(janfeb20 ~ benchmark + essential, data=dynamics_2digit)
ols2 <- lm(janfeb20 ~ benchmark + essential_share, data=dynamics_2digit)
ols3 <- lm(febmar20 ~ benchmark + essential, data=dynamics_2digit)
ols4 <- lm(febmar20 ~ benchmark + essential_share, data=dynamics_2digit)
ols5 <- lm(marapr20 ~ benchmark + essential, data=dynamics_2digit)
ols6 <- lm(marapr20 ~ benchmark + essential_share, data=dynamics_2digit)
ols7 <- lm(febapr20 ~ benchmark + essential, data=dynamics_2digit)
ols8 <- lm(febapr20 ~ benchmark + essential_share, data=dynamics_2digit)

stargazer(ols1, ols2, ols3, ols4, ols5, ols6, ols7, ols8,
          dep.var.labels=c("$\\triangle q_{j,Jan,Feb}$", 
                           "$\\triangle q_{j,Feb,Mar}$",
                           "$\\triangle q_{j,Mar,Apr}$",
                           "$\\triangle q_{j,Feb,Apr}$"),
          covariate.labels=c("$S_{j} (Benchmark)$",
                             "$D_{j}$",
                             "$ES_{j}$"),
          label="tab:regression_dynamics_2digit_robustness_essential_service",
          title="Employment Change and Remote Work Index: 2-digit level (Robustness Check: Essential Service Index)",
          omit.stat=c("ser", "f"),
          font.size="footnotesize",
          out="./Data/Output/regression_dynamics_2digit_robustness_essential_service.tex")

#alternative essential service index (with alternative remote work index)
ols1 <- lm(janfeb20 ~ alternative + essential, data=dynamics_2digit)
ols2 <- lm(janfeb20 ~ alternative + essential_share, data=dynamics_2digit)
ols3 <- lm(febmar20 ~ alternative + essential, data=dynamics_2digit)
ols4 <- lm(febmar20 ~ alternative + essential_share, data=dynamics_2digit)
ols5 <- lm(marapr20 ~ alternative + essential, data=dynamics_2digit)
ols6 <- lm(marapr20 ~ alternative + essential_share, data=dynamics_2digit)
ols7 <- lm(febapr20 ~ alternative + essential, data=dynamics_2digit)
ols8 <- lm(febapr20 ~ alternative + essential_share, data=dynamics_2digit)

stargazer(ols1, ols2, ols3, ols4, ols5, ols6, ols7, ols8,
          dep.var.labels=c("$\\triangle q_{j,Jan,Feb}$", 
                           "$\\triangle q_{j,Feb,Mar}$",
                           "$\\triangle q_{j,Mar,Apr}$",
                           "$\\triangle q_{j,Feb,Apr}$"),
          covariate.labels=c("$S_{j} (Alternative)$",
                             "$D_{j}$",
                             "$ES_{j}$"),
          label="tab:regression_dynamics_2digit_alternative_robustness_essential_service",
          title="Employment Change and Remote Work Index: 2-digit level (Robustness Check: Alternative Remote Work Index and Essential Service Index)",
          omit.stat=c("ser", "f"),
          font.size="footnotesize",
          out="./Data/Output/regression_dynamics_2digit_alternative_robustness_essential_service.tex")

#10 occupation group
ols1 <- lm(janfeb20 ~ benchmark, data=dynamics_10oc)
ols2 <- lm(febmar20 ~ benchmark, data=dynamics_10oc)
ols3 <- lm(marapr20 ~ benchmark, data=dynamics_10oc)
ols4 <- lm(febapr20 ~ benchmark, data=dynamics_10oc)
ols5 <- lm(janfeb20 ~ alternative, data=dynamics_10oc)
ols6 <- lm(febmar20 ~ alternative, data=dynamics_10oc)
ols7 <- lm(marapr20 ~ alternative, data=dynamics_10oc)
ols8 <- lm(febapr20 ~ alternative, data=dynamics_10oc)

stargazer(ols1, ols2, ols3, ols4, ols5, ols6, ols7, ols8,   
          dep.var.labels=c("$\\triangle q_{j,Jan,Feb}$", 
                           "$\\triangle q_{j,Feb,Mar}$",
                           "$\\triangle q_{j,Mar,Apr}$",
                           "$\\triangle q_{j,Feb,Apr}$",
                           "$\\triangle q_{j,Jan,Feb}$", 
                           "$\\triangle q_{j,Feb,Mar}$",
                           "$\\triangle q_{j,Mar,Apr}$",
                           "$\\triangle q_{j,Feb,Apr}$"),
          covariate.labels=c("$S_{j}(Benchmark)$",
                             "$S_{j}(Alternative)$"),
          label="tab:regression_dynamics_10oc",
          title="Employment Change and Remote Work Index: 10 Occupation Group",
          omit.stat=c("ser", "f"),
          font.size="footnotesize",
          out="./Data/Output/regression_dynamics_10oc.tex")

#industry
ols1 <- lm(janfeb20 ~ benchmark, data=dynamics_industry)
ols2 <- lm(febmar20 ~ benchmark, data=dynamics_industry)
ols3 <- lm(marapr20 ~ benchmark, data=dynamics_industry)
ols4 <- lm(febapr20 ~ benchmark, data=dynamics_industry)
ols5 <- lm(janfeb20 ~ alternative, data=dynamics_industry)
ols6 <- lm(febmar20 ~ alternative, data=dynamics_industry)
ols7 <- lm(marapr20 ~ alternative, data=dynamics_industry)
ols8 <- lm(febapr20 ~ alternative, data=dynamics_industry)

stargazer(ols1, ols2, ols3, ols4, ols5, ols6, ols7, ols8,   
          dep.var.labels=c("$\\triangle q_{j,Jan,Feb}$", 
                           "$\\triangle q_{j,Feb,Mar}$",
                           "$\\triangle q_{j,Mar,Apr}$",
                           "$\\triangle q_{j,Feb,Apr}$",
                           "$\\triangle q_{j,Jan,Feb}$", 
                           "$\\triangle q_{j,Feb,Mar}$",
                           "$\\triangle q_{j,Mar,Apr}$",
                           "$\\triangle q_{j,Feb,Apr}$"),
          covariate.labels=c("$S_{j}(Benchmark)$",
                             "$S_{j}(Alternative)$"),
          label="tab:regression_dynamics_industry",
          title="Employment Change and Remote Work Index: Industry",
          omit.stat=c("ser", "f"),
          font.size="footnotesize",
          out="./Data/Output/regression_dynamics_industry.tex")

#province
ols1 <- lm(janfeb20 ~ benchmark, data=dynamics_province)
ols2 <- lm(febmar20 ~ benchmark, data=dynamics_province)
ols3 <- lm(marapr20 ~ benchmark, data=dynamics_province)
ols4 <- lm(febapr20 ~ benchmark, data=dynamics_province)
ols5 <- lm(janfeb20 ~ alternative, data=dynamics_province)
ols6 <- lm(febmar20 ~ alternative, data=dynamics_province)
ols7 <- lm(marapr20 ~ alternative, data=dynamics_province)
ols8 <- lm(febapr20 ~ alternative, data=dynamics_province)

stargazer(ols1, ols2, ols3, ols4, ols5, ols6, ols7, ols8,   
          dep.var.labels=c("$\\triangle q_{j,Jan,Feb}$", 
                           "$\\triangle q_{j,Feb,Mar}$",
                           "$\\triangle q_{j,Mar,Apr}$",
                           "$\\triangle q_{j,Feb,Apr}$",
                           "$\\triangle q_{j,Jan,Feb}$", 
                           "$\\triangle q_{j,Feb,Mar}$",
                           "$\\triangle q_{j,Mar,Apr}$",
                           "$\\triangle q_{j,Feb,Apr}$"),
          covariate.labels=c("$S_{j}(Benchmark)$",
                             "$S_{j}(Alternative)$"),
          label="tab:regression_dynamics_province",
          title="Employment Change and Remote Work Index: Province",
          omit.stat=c("ser", "f"),
          font.size="footnotesize",
          out="./Data/Output/regression_dynamics_province.tex")


#city
ols1 <- lm(janfeb20 ~ benchmark, data=dynamics_city)
ols2 <- lm(febmar20 ~ benchmark, data=dynamics_city)
ols3 <- lm(marapr20 ~ benchmark, data=dynamics_city)
ols4 <- lm(febapr20 ~ benchmark, data=dynamics_city)
ols5 <- lm(janfeb20 ~ alternative, data=dynamics_city)
ols6 <- lm(febmar20 ~ alternative, data=dynamics_city)
ols7 <- lm(marapr20 ~ alternative, data=dynamics_city)
ols8 <- lm(febapr20 ~ alternative, data=dynamics_city)

stargazer(ols1, ols2, ols3, ols4, ols5, ols6, ols7, ols8,   
          dep.var.labels=c("$\\triangle q_{j,Jan,Feb}$", 
                           "$\\triangle q_{j,Feb,Mar}$",
                           "$\\triangle q_{j,Mar,Apr}$",
                           "$\\triangle q_{j,Feb,Apr}$",
                           "$\\triangle q_{j,Jan,Feb}$", 
                           "$\\triangle q_{j,Feb,Mar}$",
                           "$\\triangle q_{j,Mar,Apr}$",
                           "$\\triangle q_{j,Feb,Apr}$"),
          covariate.labels=c("$S_{j}(Benchmark)$",
                             "$S_{j}(Alternative)$"),
          label="tab:regression_dynamics_city",
          title="Employment Change and Remote Work Index: City",
          omit.stat=c("ser", "f"),
          font.size="footnotesize",
          out="./Data/Output/regression_dynamics_city.tex")


#by percentile of income distribution

remote_perc <- read.csv("./Data/Output/remote_income_percentile.csv")

g <- ggplot(data=remote_perc, aes(y=Benchmark, x=income.percentile)) 
g <- g +  geom_point() + geom_smooth()
g <- g + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12))
g <- g + labs(y="Remote-Work Index", x = "Income Percentile")
g <- g + theme(legend.position="none")
g

#save file
pdf("./Data/Output/remote_income_percentile.pdf")
g
dev.off()

g <- ggplot(data=remote_perc, aes(y=Alternative, x=income.percentile)) 
g <- g +  geom_point() + geom_smooth()
g <- g + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12))
g <- g + labs(y="Remote-Work Index", x = "Income Percentile")
g <- g + theme(legend.position="none")
g

#female share by industry
female_industry <- read.csv("./Data/Output/remote_employment_female_industry.csv")

colnames(female_industry) <- c("sector", "benchmark", "alternative", "female.share")

g <- ggplot(data=female_industry, aes(y=benchmark, x=female.share)) 
g <- g +  geom_point() + geom_smooth(method=lm, se=FALSE)
g <- g + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12))
g <- g + labs(y="Remote-Work Index", x = "Female Share")
g <- g + theme(legend.position="none")
g

female_2digit <- read.csv("./Data/Output/remote_employment_female_2_digit occp_with_code.csv")

colnames(female_2digit)[3:6] <- c("occupation", "benchmark", "alternative", "female.share")

g <- ggplot(data=female_2digit, aes(y=benchmark, x=female.share)) 
g <- g +  geom_point() + geom_smooth(method=lm, se=FALSE)
g <- g + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12))
g <- g + labs(y="Remote-Work Index", x = "Female Share")
g <- g + theme(legend.position="none")
g

###
#essential service dummy robustness

dynamics_2digit_check <- read.csv("./Data/Output/remote_employment_dynamics_2_digit_occp_with_code_revised.csv")

colnames(dynamics_2digit_check) <- c("code", "dummy", 
                                     "essential_share",
                                     "occupation",
                                     "benchmark", "alternative",
                                     "janfeb20", "febmar20", 
                                     "marapr20", "febapr20")

dynamics_2digit_check <- dynamics_2digit_check[order(dynamics_2digit_check$essential_share),]

dynamics_2digit_check$essential_share_round <- round(dynamics_2digit_check$essential_share,0)


ols1 <- lm(janfeb20 ~ benchmark + essential_share, data=dynamics_2digit_check)
ols2 <- lm(febmar20 ~ benchmark + essential_share, data=dynamics_2digit_check)
ols3 <- lm(marapr20 ~ benchmark + essential_share, data=dynamics_2digit_check)
ols4 <- lm(febapr20 ~ benchmark + essential_share, data=dynamics_2digit_check)

summary(ols1)
summary(ols2)
summary(ols3)
summary(ols4)

g <- ggplot(data=dynamics_2digit_check, aes(y=dummy, x=essential_share)) 
g <- g +  geom_point() + geom_smooth(method=lm, se=FALSE)
g <- g + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12))
g <- g + labs(y="Essential Dummy", x = "Essential Share")
g <- g + theme(legend.position="none")
g


g <- ggplot(data=dynamics_2digit_check, aes(y=dummy, x=)) 
g <- g +  geom_point() + geom_smooth(method=lm, se=FALSE)
g <- g + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12))
g <- g + labs(y="Essential Dummy", x = "Essential Share")
g <- g + theme(legend.position="none")
g

