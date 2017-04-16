
#**********************************************************************************************
                                                  #Processing 
#**********************************************************************************************

#Loading all the necessary packages
packages <- c("bea.R", "acs", "magrittr", "httr", "tidyr", "blsAPI", "rjson", "readxl", "dplyr", "jsonlite",
              "stringr", "rJava", "xlsx", "qdap", "data.table", "plm", "rio", "Zelig", "stargazer")
load <- lapply(packages, require, character.only = T)

#Setting the working directory
setwd("C:/RajuPC/MPP Final Thesis/WorkingDirectory")

#All the keys for different APIs obtained from the respective websites
blskey <- "a9e62413e38741b5aeb814efc5a3d066"
beaKey <- 'C3812F4D-F498-40F8-9F36-9FF5AF65DBD7'
censusKey <- "c7ed765d1b03f4217ccc4b37d31b0dc3580db44e"
datagovkey <- "ubFrNGonfwMQm3lK04C6djaMcqFuIe5mvev4RooI"

#**********************************************************************************************
source("Statistics.R")
#**********************************************************************************************

#Base regression(OLS, FE, RE)
p1_m1_ols <- lm(rep.share ~ unemp_gro + PCI_gro + repshare.lag, merged_df4)
p1_m1_fe <- plm(rep.share ~ unemp_gro + PCI_gro + repshare.lag, merged_df4, model = 'within')
p1_m1_re <- plm(rep.share ~ unemp_gro + PCI_gro + repshare.lag, merged_df4, model = 'random')

phtest(p1_m1_fe, p1_m1_re) #Hausman test to choose between FE and RE shows that FE is better since p-value < 0.05

stargazer::stargazer(p1_m1_ols, p1_m1_fe, p1_m1_re, type = 'text', digits = 2, header = FALSE,   
                     title = 'Base Model (OLS, FE, RE)', font.size = 'normalsize', out = 'p1m1all.htm')

#Interpretation of Base Model:
#As unemployment goes up, republican vote share goes down. 
#As PCI goes up, republican vote share goes down.
#In a county, republican voters are mostly employed people with low PCI (hence "poor workers")


#Adding controls (FE)
p1_m2_fe <- plm(rep.share ~ unemp_gro + PCI_gro + repshare.lag + Pop + white.percent + as.factor(rep_incumb)
                , merged_df4, model = 'within')

stargazer::stargazer(p1_m2_fe, type = 'text', digits = 2, header = FALSE,   
                     title = 'Model with Controls (FE)', font.size = 'normalsize')


#Interpretations of Adding Controls Model:
#The significant effects from base model hold true here as well
#Counties with higher percentage of white vote Republican.
#If the incumbent is Republican, then the voteshare goes slightly down. (See Norpoth to see what effect it is)
# Hence "poor white workers" voting Republican is validated here.

#Introducing interactions (FE)
p1_m3_fe <- plm(rep.share ~ unemp_gro + PCI_gro + repshare.lag + Pop + white.percent + as.factor(rep_incumb)
               + unemp_gro:as.factor(rep_incumb) + PCI_gro:as.factor(rep_incumb) + as.factor(rural)
               + white.percent:as.factor(rural), merged_df4, model = 'within')

stargazer::stargazer(p1_m3_fe, type = 'text', digits = 2, header = FALSE,   
                     title = 'Model with Controls and Interactions (FE)', font.size = 'normalsize', out = 'p1m3fe.htm')

#Interpretations:
#Everything is significant
#Great R-squared
#"rural poor white workers"
#Effect of economic factors is increased in times of incumbency.



#********************************************************************************************
source("Statistics_Part2.R")
#********************************************************************************************

#Base Model (OLS and Logit)
p2_m1_ols <- lm(rep.share.gro ~ unemp_gro + pci_gro, logit.data)

p2_m1_logit <- glm(flip ~ unemp_gro + pci_gro, logit.data, family = binomial(link = "logit"))

stargazer::stargazer(p2_m1_ols, p2_m1_logit, type = 'text', digits = 2, header = FALSE,   
                     title = 'Base Model(OLS and Logit)', font.size = 'normalsize', out = 'p2m1all.htm')

#Find what happened with unemp and pci in between these two years:



#Adding Controls (OLS and Logit)

p2_m3_ols <- lm(rep.share.gro ~ unemp_gro + pci_gro + pop + educ + white.percent + as.factor(rural):white.percent + 
                   + as.factor(rural), logit.data)

p2_m3_logit <- glm(flip ~ unemp_gro + pci_gro + pop + educ + white.percent + as.factor(rural):white.percent + 
                    + as.factor(rural), logit.data, family = binomial(link = "logit"))

stargazer::stargazer(p2_m3_ols, p2_m3_logit, type = 'text', digits = 2, header = FALSE,   
                     title = 'With Controls and Interactions Model (OLS and Logit)', font.size = 'normalsize', out = 'p2m3all.htm')


#Interpretations:



#Adding Interactions (Logit)

stargazer::stargazer(p2_m3_logit, type = 'text', digits = 2, header = FALSE,   
                     title = 'With Interactions Model Logit', font.size = 'normalsize')





p2_m2_relogit <- zelig(flip ~ unemp_gro + pci_gro + pop + white.percent,
                       model = "relogit", data = logit.data, tau = 0.05)
summary(p2_m2_relogit)
t <- setx(p2_m2_relogit) %>% sim(p2_m2_relogit, .)

#************************************************************************
source("heat-map.R")
#************************************************************************
p2_merged_df4 %>% filter(state != "HI") %>% county.heatmap("rep.share") + 
  scale_fill_gradient2(low = "#085BB2", high = "#FF2312", mid = "#4DAFFF", midpoint = 0.40)

logit.data %>% filter(state != "HI") %>% county.heatmap("unemp_gro") + 
  scale_fill_gradient2(low = "#085BB2", high = "#FF2312", mid = "#4DAFFF", midpoint = 0)


logit.data %>% filter(state != "HI") %>% county.heatmap("total") + 
  scale_fill_gradient2(low = "#085BB2", high = "#FF2312", mid = "#4DAFFF", midpoint = 0.49)


#************************************************************************
                              #Scatter plots
#************************************************************************






#********************
  # table(merged_df4$repshare.lag)
