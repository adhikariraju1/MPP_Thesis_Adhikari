
#**********************************************************************************************
                                                  #Processing 
#**********************************************************************************************

#Loading all the necessary packages
packages <- c("bea.R", "acs", "magrittr", "httr", "tidyr", "blsAPI", "rjson", "readxl", "dplyr", "jsonlite",
              "stringr", "rJava", "xlsx", "qdap", "data.table", "plm", "rio", "Zelig", "stargazer", "knitr", "fBasics")
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

#Base regression(OLS, FE, RE) with Unemployment: m1a
p1_m1a_ols <- lm(rep.share ~ unemp_gro + repshare.lag, merged_df4)
p1_m1a_fe <- plm(rep.share ~ unemp_gro + repshare.lag, merged_df4, model = 'within')
p1_m1a_re <- plm(rep.share ~ unemp_gro + repshare.lag, merged_df4, model = 'random')

phtest(p1_m1_fe, p1_m1_re) #Hausman test to choose between FE and RE shows that FE is better since p-value < 0.05

stargazer::stargazer(p1_m1_ols, p1_m1_fe, p1_m1_re, type = 'text', digits = 2, header = FALSE,   
                     title = 'Base Model with Unemployment (OLS, FE, RE)', font.size = 'normalsize', out = 'p1m1a.htm')


#Base regression(OLS, FE, RE) with PCI: m1b
p1_m1b_ols <- lm(rep.share ~ PCI_gro + repshare.lag, merged_df4)
p1_m1b_fe <- plm(rep.share ~ PCI_gro + repshare.lag, merged_df4, model = 'within')
p1_m1b_re <- plm(rep.share ~ PCI_gro + repshare.lag, merged_df4, model = 'random')

phtest(p1_m1b_fe, p1_m1b_re) #Hausman test to choose between FE and RE shows that FE is better since p-value < 0.05

stargazer::stargazer(p1_m1b_ols, p1_m1b_fe, p1_m1b_re, type = 'text', digits = 2, header = FALSE,   
                     title = 'Base Model with PCI (OLS, FE, RE)', font.size = 'normalsize', out = 'p1m1b.htm')


#Full FE Model with Unemployment: m2a
p1_m2a_fe <- plm(rep.share ~ unemp_gro + repshare.lag + Pop + white.percent + as.factor(rep_incumb)
               + unemp_gro:as.factor(rep_incumb) + as.factor(rural)
               + white.percent:as.factor(rural), merged_df4, model = 'within')

stargazer::stargazer(p1_m2a_fe, type = 'text', digits = 2, header = FALSE,   
                     title = 'Full model with Unemployment (FE)', font.size = 'normalsize', out = 'p1m2a.htm')

#Full FE Model with PCI: m2b
p1_m2b_fe <- plm(rep.share ~ PCI_gro + repshare.lag + Pop + white.percent + as.factor(rep_incumb)
                 + PCI_gro:as.factor(rep_incumb) + as.factor(rural)
                 + white.percent:as.factor(rural), merged_df4, model = 'within')

stargazer::stargazer(p1_m2b_fe, type = 'text', digits = 2, header = FALSE,   
                     title = 'Full Model with PCI (FE)', font.size = 'normalsize', out = 'p1m2b.htm')


#********************************************************************************************
source("Statistics_Part2.R")
#********************************************************************************************

#Base Model with Unemployment (OLS and Logit)
p2_m1a_ols <- lm(rep.share.gro ~ unemp_gro, p2_merged_df5)
p2_m1a_logit <- glm(flip ~ unemp_gro, logit.data, family = binomial(link = "logit"))

stargazer::stargazer(p2_m1a_ols, p2_m1a_logit, type = 'text', digits = 2, header = FALSE,   
                     title = 'Base Model with Unemployment (OLS and Logit)', font.size = 'normalsize', out = 'p2m1a.htm')


#Base Model with PCI (OLS and Logit)
p2_m1b_ols <- lm(rep.share.gro ~ pci_gro, p2_merged_df5)
p2_m1b_logit <- glm(flip ~ pci_gro, logit.data, family = binomial(link = "logit"))

stargazer::stargazer(p2_m1b_ols, p2_m1b_logit, type = 'text', digits = 2, header = FALSE,   
                     title = 'Base Model with PCI(OLS and Logit)', font.size = 'normalsize', out = 'p2m1b.htm')


#Full Model with Unemployment (OLS and Logit): 

p2_m2a_ols <- lm(rep.share.gro ~ unemp_gro + pop + educ + white.percent + as.factor(rural):white.percent + 
                   + as.factor(rural), p2_merged_df5)

p2_m2a_logit <- glm(flip ~ unemp_gro + pop + educ + white.percent + as.factor(rural):white.percent + 
                    + as.factor(rural), logit.data, family = binomial(link = "logit"))

stargazer::stargazer(p2_m2a_ols, p2_m2a_logit, type = 'text', digits = 2, header = FALSE,   
                     title = 'Full Model with Unemployment (OLS and Logit)', font.size = 'normalsize', out = 'p2m2a.htm')


#Full Model with Unemployment (OLS and Logit):
p2_m2b_ols <- lm(rep.share.gro ~ pci_gro + pop + educ + white.percent + as.factor(rural):white.percent + 
                   + as.factor(rural), p2_merged_df5)

p2_m2b_logit <- glm(flip ~ pci_gro + pop + educ + white.percent + as.factor(rural):white.percent + 
                      + as.factor(rural), logit.data, family = binomial(link = "logit"))

stargazer::stargazer(p2_m2b_ols, p2_m2b_logit, type = 'text', digits = 2, header = FALSE,   
                     title = 'Full Model with PCI (OLS and Logit)', font.size = 'normalsize', out = 'p2m2b.htm')



#ReLogit:


p2_m2_relogit <- zelig(flip ~ unemp_gro + pci_gro + pop + white.percent,
                       model = "relogit", data = logit.data, tau = 0.05)
summary(p2_m2_relogit)
t <- setx(p2_m2_relogit) %>% sim(p2_m2_relogit, .)


#With relative unemp:

p2_m4_ols <- lm(rep.share.gro ~ rel.unemp, p2_merged_df5)
summary(p2_m4_ols)

p2_m5_ols <- lm(rep.share.gro ~ rel.unemp + pop + educ + white.percent + as.factor(rural):white.percent + 
                  + as.factor(rural), p2_merged_df5)
summary(p2_m5_ols)





#************************************************************************
source("heat-map.R")
#************************************************************************



#Part II:

p2_merged_df4 %>% filter(state != "HI") %>% county.heatmap("rep.share.gro") + 
  scale_fill_gradient2(low = "#085BB2", high = "#FF2312", mid = "#4DAFFF", midpoint = 0) +
  labs(title = "Change in voteshare for Republican party 2012 to 2016")

logit.data %>% filter(state != "HI") %>% county.heatmap("flip") + 
  scale_fill_gradient2(low = "#085BB2", high = "#FF2312", mid = "#4DAFFF", midpoint = 0.5) +
  labs(title = "Counties that flipped from Democrat to Republican 2012 to 2016")

#************************************************************************
                              #Scatter plots
#************************************************************************


#Correlation:
cor(merged_df4[,c("unemp_gro", "PCI_gro")], use="complete.obs", method="pearson")
cor(merged_df4[,c("rep.share", "repshare.lag")], use="complete.obs", method="pearson")



cor(p2_merged_df5[,c("unemp_gro", "pci_gro")], use="complete.obs", method="pearson")

#Descriptive Stats:
descrp_p1 <- merged_df4 %>%
  select(rep.share, unemp_gro, PCI_gro, Pop, repshare.lag, white.percent)

table_p1 <- basicStats(merged_df4)
kable(descrp_p1, align="c", caption="Summary of Descriptive Statistics", digits = 2)




#********************
  # table(merged_df4$repshare.lag)
