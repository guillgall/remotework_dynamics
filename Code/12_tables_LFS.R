##NOC 40 

noc_40 <- read.csv("./Data/Output/NOC_40_remote_work.csv")

noc_40$occupation <- gsub('[0-9]+', '', noc_40$occupation)
noc_40$occupation <- gsub('-', '', noc_40$occupation)
noc_40$occupation <- trimws(noc_40$occupation)

noc_40 <- noc_40[,-1]

noc_40 <- noc_40[,c(4,2,3)]

colnames(noc_40) <- c("Occupation", "Benchmark", "Alternative")

print(xtable(noc_40,
             caption = "Share of jobs that can be done at home, by 2-digit occupation",
             label = "tab:industry_lfs",
             align = "clcc"),
      sanitize.text.function=function(x){x},
      include.rownames = FALSE,
      type="latex", 
      caption.placement = "top",
      size="small",
      file="./Data/Output/table_noc_40.tex")


###DYNAMICS
#OCCUPATION (NOC 40)

dynamics_2digit <- read.csv("./Data/Output/NOC_40_essential_employment_variation.csv")


#2 digit occupations

dynamics_2digit <- dynamics_2digit[,-1]

#2 digit

g <- ggplot(data=dynamics_2digit, aes(y=febmar, x=remote_work_onet)) 
g <- g +  geom_point() + geom_smooth(method=lm, colour="black", se=FALSE)
#g <- g + geom_text(aes(label=code),hjust=0, vjust=0, size = 5)
g <- g + theme_bw()
g <- g + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12))
g <- g + labs(y="Change (%) in Employment", x = "Remote-Work Index")
g <- g + theme(legend.position="none")
g

#save file
pdf("./Data/Output/dynamics_2digit_febmar20.pdf")
g
dev.off()

g <- ggplot(data=dynamics_2digit, aes(y=febapr, x=remote_work_onet)) 
g <- g +  geom_point() + geom_smooth(method=lm, colour="black", se=FALSE)
#g <- g + geom_text(aes(label=code),hjust=0, vjust=0, size = 5)
g <- g + theme_bw()
g <- g + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12))
g <- g + labs(y="Change (%) in Employment", x = "Remote-Work Index")
g <- g + theme(legend.position="none")
g

#save file
pdf("./Data/Output/dynamics_2digit_febapr20.pdf")
g
dev.off()

g <- ggplot(data=dynamics_2digit, aes(y=marapr, x=remote_work_onet)) 
g <- g +  geom_point() + geom_smooth(method=lm, colour="black", se=FALSE)
#g <- g + geom_text(aes(label=code),hjust=0, vjust=0, size = 5)
g <- g + theme_bw()
g <- g + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12))
g <- g + labs(y="Change (%) in Employment", x = "Remote-Work Index")
g <- g + theme(legend.position="none")
g

#save file
pdf("./Data/Output/dynamics_2digit_marapr20.pdf")
g
dev.off()

####regressions

#2-digit occupations
ols1 <- lm(janfeb ~ remote_work_onet, data=dynamics_2digit)
ols2 <- lm(janfeb ~ remote_work_onet + dummy, data=dynamics_2digit)
ols3 <- lm(febmar ~ remote_work_onet, data=dynamics_2digit)
ols4 <- lm(febmar ~ remote_work_onet + dummy, data=dynamics_2digit)
ols5 <- lm(marapr ~ remote_work_onet, data=dynamics_2digit)
ols6 <- lm(marapr ~ remote_work_onet + dummy, data=dynamics_2digit)
ols7 <- lm(febapr ~ remote_work_onet, data=dynamics_2digit)
ols8 <- lm(febapr ~ remote_work_onet + dummy, data=dynamics_2digit)

summary(ols8)

stargazer(ols1, ols2, ols3, ols4, ols5, ols6, ols7, ols8,
          dep.var.labels=c("$\\triangle q_{j,Jan,Feb}$", 
                           "$\\triangle q_{j,Feb,Mar}$",
                           "$\\triangle q_{j,Mar,Apr}$",
                           "$\\triangle q_{j,Feb,Apr}$"),
          covariate.labels=c("$S_{j} (Benchmark)$",
                             "$D_{j}$"),
          dep.var.caption="Monthly Percentage Change in Employment",
          label="tab:regression_dynamics_2digit",
          title="Employment Change and Remote Work Index: Two-Digit level (Benchmark Remote Work Index and Essential Service Dummy Variable)",
          omit.stat=c("ser", "f"),
          font.size="footnotesize",
          notes = c("$\\triangle q_{j,t,\\tau}$ is the percentage change in employment between month $t$ and $\\tau$ in occupation $j$,",
                    "$S_{j} (Benchmark)$ is the Remote Work Index and $D_{j}$ is the Essential Service Dummy variable.",
                    "Source: Author's calculations."),
          notes.align = "l",
          out="./Data/Output/regression_dynamics_2digit.tex")

#alternative index
ols1 <- lm(janfeb ~ remote_work_manual, data=dynamics_2digit)
ols2 <- lm(janfeb ~ remote_work_manual + dummy, data=dynamics_2digit)
ols3 <- lm(febmar ~ remote_work_manual, data=dynamics_2digit)
ols4 <- lm(febmar ~ remote_work_manual + dummy, data=dynamics_2digit)
ols5 <- lm(marapr ~ remote_work_manual, data=dynamics_2digit)
ols6 <- lm(marapr ~ remote_work_manual + dummy, data=dynamics_2digit)
ols7 <- lm(febapr ~ remote_work_manual, data=dynamics_2digit)
ols8 <- lm(febapr ~ remote_work_manual + dummy, data=dynamics_2digit)


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
          title="Employment Change and Remote Work Index: Two-Digit level (Alternative Remote Work Index and Essential Service Dummy Variable)",
          omit.stat=c("ser", "f"),
          font.size="footnotesize",
          notes = c("$\\triangle q_{j,t,\\tau}$ is the percentage change in employment between month $t$ and $\\tau$ in occupation $j$,",
                    "$S_{j} (Alternative)$ is the Alternative Remote Work Index and $D_{j}$ is the Essential Service Dummy variable.",
                    "Source: Author's calculations."),
          notes.align = "l",
          out="./Data/Output/regression_dynamics_2digit_alternative.tex")

#alternative essential service index (with benchmark remote work index)
ols1 <- lm(janfeb ~ remote_work_onet, data=dynamics_2digit)
ols2 <- lm(janfeb ~ remote_work_onet + essential_share_weighted, data=dynamics_2digit)
ols3 <- lm(febmar ~ remote_work_onet, data=dynamics_2digit)
ols4 <- lm(febmar ~ remote_work_onet + essential_share_weighted, data=dynamics_2digit)
ols5 <- lm(marapr ~ remote_work_onet, data=dynamics_2digit)
ols6 <- lm(marapr ~ remote_work_onet + essential_share_weighted, data=dynamics_2digit)
ols7 <- lm(febapr ~ remote_work_onet, data=dynamics_2digit)
ols8 <- lm(febapr ~ remote_work_onet + essential_share_weighted, data=dynamics_2digit)

stargazer(ols1, ols2, ols3, ols4, ols5, ols6, ols7, ols8,
          dep.var.labels=c("$\\triangle q_{j,Jan,Feb}$", 
                           "$\\triangle q_{j,Feb,Mar}$",
                           "$\\triangle q_{j,Mar,Apr}$",
                           "$\\triangle q_{j,Feb,Apr}$"),
          covariate.labels=c("$S_{j} (Benchmark)$",
                             "$ES_{j}$"),
          label="tab:regression_dynamics_2digit_robustness_essential_service",
          title="Employment Change and Remote Work Index: Two-Digit level (Benchmark Remote Work Index and Essential Service Index)",
          omit.stat=c("ser", "f"),
          font.size="footnotesize",
          notes = c("$\\triangle q_{j,t,\\tau}$ is the percentage change in employment between month $t$ and $\\tau$ in occupation $j$,",
                    "$S_{j} (Benchmark)$ is the Remote Work Index and $ES_{j}$ is the Essential Service variable.",
                    "Source: Author's calculations."),
          notes.align = "l",
          out="./Data/Output/regression_dynamics_2digit_robustness_essential_service.tex")

#alternative essential service index (with alternative remote work index)
ols1 <- lm(janfeb ~ remote_work_manual, data=dynamics_2digit)
ols2 <- lm(janfeb ~ remote_work_manual + essential_share_weighted, data=dynamics_2digit)
ols3 <- lm(febmar ~ remote_work_manual, data=dynamics_2digit)
ols4 <- lm(febmar ~ remote_work_manual + essential_share_weighted, data=dynamics_2digit)
ols5 <- lm(marapr ~ remote_work_manual, data=dynamics_2digit)
ols6 <- lm(marapr ~ remote_work_manual + essential_share_weighted, data=dynamics_2digit)
ols7 <- lm(febapr ~ remote_work_manual, data=dynamics_2digit)
ols8 <- lm(febapr ~ remote_work_manual + essential_share_weighted, data=dynamics_2digit)

stargazer(ols1, ols2, ols3, ols4, ols5, ols6, ols7, ols8,
          dep.var.labels=c("$\\triangle q_{j,Jan,Feb}$", 
                           "$\\triangle q_{j,Feb,Mar}$",
                           "$\\triangle q_{j,Mar,Apr}$",
                           "$\\triangle q_{j,Feb,Apr}$"),
          covariate.labels=c("$S_{j} (Alternative)$",
                             "$ES_{j}$"),
          label="tab:regression_dynamics_2digit_alternative_robustness_essential_service",
          title="Employment Change and Remote Work Index: Two-Digit level (Alternative Remote Work Index and Essential Service Index)",
          omit.stat=c("ser", "f"),
          font.size="footnotesize",
          notes = c("$\\triangle q_{j,t,\\tau}$ is the percentage change in employment between month $t$ and $\\tau$ in occupation $j$,",
                    "$S_{j} (Alternative)$ is the Alternative Remote Work Index and $ES_{j}$ is the Essential Service variable.",
                    "Source: Author's calculations."),
          notes.align = "l",
          out="./Data/Output/regression_dynamics_2digit_alternative_robustness_essential_service.tex")

#################
###INDUSTRY (NAICS 21)
industry <- read.csv("./Data/Output/NAICS_21_remote_work.csv")
industry <- industry[,c(5,3,4)]
colnames(industry) <- c("Industry", "Benchmark", "Alternative")

industry <- industry[order(-industry$Benchmark),]

print(xtable(industry,
             caption = "Share of jobs that can be done at home, by industry (LFS estimates)",
             label = "tab:industry_lfs",
             align = "clcc"),
      sanitize.text.function=function(x){x},
      include.rownames = FALSE,
      type="latex", 
      caption.placement = "top",
      file="./Data/Output/table_industry_lfs.tex")


dynamics_industry <- read.csv("./Data/Output/NAICS_21_employment_variation.csv")

dynamics_industry <- dynamics_industry[,-1]

g <- ggplot(data=dynamics_industry, aes(y=febmar, x=remote_work_onet)) 
g <- g +  geom_point() + geom_smooth(method=lm, colour="black", se=FALSE)
#g <- g + geom_text(aes(label=industry),hjust=0, vjust=0)
g <- g + theme_bw()
g <- g + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12))
g <- g + labs(y="Change (%) in Employment", x = "Remote-Work Index")
g

#save file
pdf("./Data/Output/dynamics_industry_febmar20.pdf")
g
dev.off()

g <- ggplot(data=dynamics_industry, aes(y=febapr, x=remote_work_onet)) 
g <- g +  geom_point() + geom_smooth(method=lm, colour="black", se=FALSE)
#g <- g + geom_text(aes(label=industry),hjust=0, vjust=0)
g <- g + theme_bw()
g <- g + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12))
g <- g + labs(y="Change (%) in Employment", x = "Remote-Work Index")
g

#save file
pdf("./Data/Output/dynamics_industry_febapr20.pdf")
g
dev.off()

g <- ggplot(data=dynamics_industry, aes(y=marapr, x=remote_work_onet)) 
g <- g +  geom_point() + geom_smooth(method=lm, colour="black", se=FALSE)
#g <- g + geom_text(aes(label=industry),hjust=0, vjust=0)
g <- g + theme_bw()
g <- g + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12))
g <- g + labs(y="Change (%) in Employment", x = "Remote-Work Index")
g

#save file
pdf("./Data/Output/dynamics_industry_marapr20.pdf")
g
dev.off()

#REGRESSION
ols1 <- lm(janfeb ~ remote_work_onet, data=dynamics_industry)
ols2 <- lm(febmar ~ remote_work_onet, data=dynamics_industry)
ols3 <- lm(marapr ~ remote_work_onet, data=dynamics_industry)
ols4 <- lm(febapr ~ remote_work_onet, data=dynamics_industry)
ols5 <- lm(janfeb ~ remote_work_manual, data=dynamics_industry)
ols6 <- lm(febmar ~ remote_work_manual, data=dynamics_industry)
ols7 <- lm(marapr ~ remote_work_manual, data=dynamics_industry)
ols8 <- lm(febapr ~ remote_work_manual, data=dynamics_industry)

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
          notes = c("$\\triangle q_{j,t,\\tau}$ is the percentage change in employment between month $t$ and $\\tau$ in industry $j$,",
                    "$S_{j} (Benchmark)$ is the Remote Work Index and $S_{j} (Alternative)$ is the Alternative Remote Work Index.",
                    "Source: Author's calculations."),
          notes.align = "l",
          out="./Data/Output/regression_dynamics_industry.tex")

#######
##PROVINCE
dynamics_province <- read.csv("./Data/Output/PROV_employment_variation.csv")

#province
dynamics_province <- dynamics_province[,-1]


g <- ggplot(data=dynamics_province, aes(y=febapr, x=remote_work_onet)) 
g <- g +  geom_point() + geom_smooth(method=lm, colour="black", se=FALSE)
#g <- g + geom_text(aes(label=province),hjust=0, vjust=0)
g <- g + theme_bw()
g <- g + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12))
g <- g + labs(y="Change (%) in Employment", x = "Remote-Work Index")
g

#save file
pdf("./Data/Output/dynamics_province_febapr20.pdf")
g
dev.off()

g <- ggplot(data=dynamics_province, aes(y=febmar, x=remote_work_onet)) 
g <- g +  geom_point() + geom_smooth(method=lm, colour="black", se=FALSE)
#g <- g + geom_text(aes(label=province),hjust=0, vjust=0)
g <- g + theme_bw()
g <- g + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12))
g <- g + labs(y="Change (%) in Employment", x = "Remote-Work Index")
g

#save file
pdf("./Data/Output/dynamics_province_febmar20.pdf")
g
dev.off()

g <- ggplot(data=dynamics_province, aes(y=marapr, x=remote_work_onet)) 
g <- g +  geom_point() + geom_smooth(method=lm, colour="black", se=FALSE)
#g <- g + geom_text(aes(label=province),hjust=0, vjust=0)
g <- g + theme_bw()
g <- g + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12))
g <- g + labs(y="Change (%) in Employment", x = "Remote-Work Index")
g

#save file
pdf("./Data/Output/dynamics_province_marapr20.pdf")
g
dev.off()

##REGRESSIONS

ols1 <- lm(janfeb ~ remote_work_onet, data=dynamics_province)
ols2 <- lm(febmar ~ remote_work_onet, data=dynamics_province)
ols3 <- lm(marapr ~ remote_work_onet, data=dynamics_province)
ols4 <- lm(febapr ~ remote_work_onet, data=dynamics_province)
ols5 <- lm(janfeb ~ remote_work_manual, data=dynamics_province)
ols6 <- lm(febmar ~ remote_work_manual, data=dynamics_province)
ols7 <- lm(marapr ~ remote_work_manual, data=dynamics_province)
ols8 <- lm(febapr ~ remote_work_manual, data=dynamics_province)

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
          notes = c("$\\triangle q_{j,t,\\tau}$ is the percentage change in employment between month $t$ and $\\tau$ in province $j$,",
                    "$S_{j} (Benchmark)$ is the Remote Work Index and $S_{j} (Alternative)$ is the Alternative Remote Work Index.",
                    "Source: Author's calculations."),
          notes.align = "l",
          out="./Data/Output/regression_dynamics_province.tex")

###CITY LEVEL
dynamics_city <- read.csv("./Data/Output/CMA_employment_variation.csv")

#city
dynamics_city <- dynamics_city[,-1]

g <- ggplot(data=dynamics_city, aes(y=febmar, x=remote_work_onet)) 
g <- g +  geom_point() + geom_smooth(method=lm, colour="black", se=FALSE)
#g <- g + geom_text(aes(label=city),hjust=0, vjust=0)
g <- g + theme_bw()
g <- g + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12))
g <- g + labs(y="Change (%) in Employment", x = "Remote-Work Index")
g

#save file
pdf("./Data/Output/dynamics_city_febmar20.pdf")
g
dev.off()


g <- ggplot(data=dynamics_city, aes(y=febapr, x=remote_work_onet)) 
g <- g +  geom_point() + geom_smooth(method=lm, colour="black", se=FALSE)
#g <- g + geom_text(aes(label=city),hjust=0, vjust=0)
g <- g + theme_bw()
g <- g + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12))
g <- g + labs(y="Change (%) in Employment", x = "Remote-Work Index")
g

#save file
pdf("./Data/Output/dynamics_city_febapr20.pdf")
g
dev.off()

g <- ggplot(data=dynamics_city, aes(y=marapr, x=remote_work_onet)) 
g <- g +  geom_point() + geom_smooth(method=lm, colour="black", se=FALSE)
#g <- g + geom_text(aes(label=city),hjust=0, vjust=0)
g <- g + theme_bw()
g <- g + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12))
g <- g + labs(y="Change (%) in Employment", x = "Remote-Work Index")
g

#save file
pdf("./Data/Output/dynamics_city_marapr20.pdf")
g
dev.off()


#city
ols1 <- lm(janfeb ~ remote_work_onet, data=dynamics_city)
ols2 <- lm(febmar ~ remote_work_onet, data=dynamics_city)
ols3 <- lm(marapr ~ remote_work_onet, data=dynamics_city)
ols4 <- lm(febapr ~ remote_work_onet, data=dynamics_city)
ols5 <- lm(janfeb ~ remote_work_manual, data=dynamics_city)
ols6 <- lm(febmar ~ remote_work_manual, data=dynamics_city)
ols7 <- lm(marapr ~ remote_work_manual, data=dynamics_city)
ols8 <- lm(febapr ~ remote_work_manual, data=dynamics_city)

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
          notes = c("$\\triangle q_{j,t,\\tau}$ is the percentage change in employment between month $t$ and $\\tau$ in city $j$,",
                    "$S_{j} (Benchmark)$ is the Remote Work Index and $S_{j} (Alternative)$ is the Alternative Remote Work Index.",
                    "Source: Author's calculations."),
          notes.align = "l",
          out="./Data/Output/regression_dynamics_city.tex")


##NOC 10 occupation group
dynamics_10oc <- read.csv("./Data/Output/NOC_10_employment_variation.csv")


dynamics_10oc <- dynamics_10oc[,-1]


g <- ggplot(data=dynamics_10oc, aes(y=febmar, x=remote_work_onet)) 
g <- g +  geom_point() + geom_smooth(method=lm, colour="black", se=FALSE)
#g <- g + geom_text(aes(label=occupation),hjust=0, vjust=0)
g <- g + theme_bw()
g <- g + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12))
g <- g + labs(y="Change (%) in Employment", x = "Remote-Work Index")
g

#save file
pdf("./Data/Output/dynamics_10oc_febmar20.pdf")
g
dev.off()

g <- ggplot(data=dynamics_10oc, aes(y=febapr, x=remote_work_onet)) 
g <- g +  geom_point() + geom_smooth(method=lm, colour="black", se=FALSE)
#g <- g + geom_text(aes(label=occupation),hjust=0, vjust=0)
g <- g + theme_bw()
g <- g + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12))
g <- g + labs(y="Change (%) in Employment", x = "Remote-Work Index")
g


#save file
pdf("./Data/Output/dynamics_10oc_febapr20.pdf")
g
dev.off()

g <- ggplot(data=dynamics_10oc, aes(y=marapr, x=remote_work_onet)) 
g <- g +  geom_point() + geom_smooth(method=lm, colour="black", se=FALSE)
#g <- g + geom_text(aes(label=occupation),hjust=0, vjust=0)
g <- g + theme_bw()
g <- g + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12))
g <- g + labs(y="Change (%) in Employment", x = "Remote-Work Index")
g


#save file
pdf("./Data/Output/dynamics_10oc_marapr20.pdf")
g
dev.off()


#############
#REGRESSIONS

ols1 <- lm(janfeb ~ remote_work_onet, data=dynamics_10oc)
ols2 <- lm(febmar ~ remote_work_onet, data=dynamics_10oc)
ols3 <- lm(marapr ~ remote_work_onet, data=dynamics_10oc)
ols4 <- lm(febapr ~ remote_work_onet, data=dynamics_10oc)
ols5 <- lm(janfeb ~ remote_work_manual, data=dynamics_10oc)
ols6 <- lm(febmar ~ remote_work_manual, data=dynamics_10oc)
ols7 <- lm(marapr ~ remote_work_manual, data=dynamics_10oc)
ols8 <- lm(febapr ~ remote_work_manual, data=dynamics_10oc)

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
          title="Employment Change and Remote Work Index: Ten-Occupation Group",
          omit.stat=c("ser", "f"),
          font.size="footnotesize",
          notes = c("$\\triangle q_{j,t,\\tau}$ is the percentage change in employment between month $t$ and $\\tau$ in occupation $j$,",
                    "$S_{j} (Benchmark)$ is the Remote Work Index and $S_{j} (Alternative)$ is the Alternative Remote Work Index.",
                    "Source: Author's calculations."),
          notes.align = "l",
          out="./Data/Output/regression_dynamics_10oc.tex")

##
#worker characteristic plots
workers <- read.csv("./Data/Output/worker_heterogeneity_regression.csv")

workers <- workers[,-c(1)]

workers$category <- factor(workers$category, 
                       levels = workers$category[order(workers$estimates_onetnoc)])


g <- ggplot(data=workers, aes(x=category, y=estimates_onetnoc)) 
g <- g +  geom_point(stat="identity") + coord_flip()
g <- g + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12))
g <- g + theme_bw()
g <- g + theme(axis.title.x=element_blank(), axis.title.y=element_blank())
g

#save file
pdf("./Data/Output/worker_characteristics_regression_plot.pdf")
g
dev.off()


##BY DECILE OF INCOME DISTRIBUTION

remote_dec <- read.csv("./Data/Output/income_decile_remote_work.csv")

g <- ggplot(data=remote_dec, aes(y=remote_work_onet, x=decile)) 
g <- g +  geom_point() + geom_smooth(colour="black")
g <- g + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12))
g <- g + theme_bw()
g <- g + labs(y="Remote-Work Index", x = "Income Decile")
g <- g + theme(legend.position="none")
g

#save file
pdf("./Data/Output/remote_income_decile.pdf")
g
dev.off()

###
#LFS remote work estimates

#open data
provinces <- read.csv("./Data/Output/PROV_remote_work.csv")
cities <- read.csv("./Data/Output/CMA_remote_work.csv")

provinces <- provinces[,c(5,3,4)]
cities <- cities[,c(5,3,4)]

colnames(provinces) <- c("Provinces", "Benchmark", "Alternative")
colnames(cities) <- c("City", "Benchmark", "Alternative")

#industry <- industry[order(-industry$Benchmark),]
provinces <- provinces[order(-provinces$Benchmark),]
cities <- cities[order(-cities$Benchmark),]

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
             align = "clcc"),
      sanitize.text.function=function(x){x},
      include.rownames = FALSE,
      type="latex", 
      caption.placement = "top",
      file="./Data/Output/table_cities_lfs.tex")

