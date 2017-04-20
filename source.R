
#**********************************************************************************************
                                                  #Processing 
#**********************************************************************************************

#Loading all the necessary packages
packages <- c("bea.R", "acs", "magrittr", "httr", "tidyr", "blsAPI", "rjson", "readxl", "dplyr", "jsonlite",
              "stringr", "rJava", "xlsx", "qdap", "data.table", "plm", "rio", "Zelig", "stargazer", "knitr", "GGally")
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
#Using rep.share.gro:
#Unemployment (OLS)
m1 <- lm(rep.share.gro ~ unemp_gro, p2_merged_df5)
m2 <- lm(rep.share.gro ~ unemp_gro + pop_thou + uneduc + white.percent + as.factor(rural):white.percent + 
                   + as.factor(rural), p2_merged_df5)
stargazer::stargazer(m1, m2, type = 'text', digits = 2, header = FALSE,   
                     title = 'Model with Unemployment (OLS)', font.size = 'normalsize', out = 'm1m2.htm')

#PCI (OLS)
m3 <- lm(rep.share.gro ~ pci_gro + pci_gro^2, p2_merged_df5)
m4 <- lm(rep.share.gro ~ pci_gro + pci_gro^2 + pop_thou + uneduc + white.percent + as.factor(rural):white.percent + 
                   + as.factor(rural), p2_merged_df5)
stargazer::stargazer(m3, m4, type = 'text', digits = 2, header = FALSE,   
                     title = 'Model with PCI (OLS)', font.size = 'normalsize', out = 'm3m4.htm')

#Using rep.share and a LDV:
#Unemployment (OLS)
m5 <- lm(rep.share ~ unemp_gro + repshare.lag, p2_merged_df5)
m6 <- lm(rep.share ~ unemp_gro + repshare.lag + pop_thou + uneduc + white.percent + as.factor(rural):white.percent + 
                   + as.factor(rural), p2_merged_df5)
stargazer::stargazer(m5, m6, type = 'text', digits = 2, header = FALSE,   
                     title = 'Model with Unemployment (OLS)', font.size = 'normalsize', out = 'm5m6.htm')

#PCI (OLS)
m7 <- lm(rep.share ~ pci_gro + I(pci_gro^2) + repshare.lag, p2_merged_df5)
m8 <- lm(rep.share ~ pci_gro + I(pci_gro^2) + repshare.lag + pop_thou + uneduc + white.percent + as.factor(rural):white.percent + 
                   + as.factor(rural), p2_merged_df5)
stargazer::stargazer(m7, m8, type = 'text', digits = 2, header = FALSE,   
                     title = 'Model with PCI (OLS)', font.size = 'normalsize', out = 'm7m8.htm')

#New Models with Total Jobs and Average Wage:

m20 <- lm(rep.share.gro ~ jobs_gro + av_wage_gro, p2_merged_df5)
m21 <- lm(rep.share.gro ~ jobs_gro + av_wage_gro + pop_thou + uneduc + white.percent + as.factor(rural):white.percent + 
           + as.factor(rural), p2_merged_df5)
m22 <- lm(rep.share.gro ~ jobs_gro + av_wage_gro + pop_thou + uneduc + white.percent + as.factor(rural):uneduc + 
           + as.factor(rural), p2_merged_df5)

####################################Playing around########################33







#ReLogit:

p2_m2_relogit <- zelig(flip ~ unemp_gro + pci_gro + pop + white.percent,
                       model = "relogit", data = logit.data, tau = 0.05)
summary(p2_m2_relogit)
t <- setx(p2_m2_relogit) %>% sim(p2_m2_relogit, .)


#With relative unemp:

p2_m3a_ols <- lm(rep.share.gro ~ rel.unemp, p2_merged_df5)
summary(p2_m3a_ols)

p2_m3b_ols <- lm(rep.share.gro ~ rel.unemp + pop + educ + white.percent + as.factor(rural):white.percent + 
                  + as.factor(rural), p2_merged_df5)
summary(p2_m3b_ols)





#************************************************************************
source("heat-map.R")
#************************************************************************



#Part II:

descrip_df2 <- p2_merged_df5 %>%
  mutate(rep.share.change = rep.share - repshare.lag)
  
descrip_df2 %>% filter(state != "HI") %>% county.heatmap("rep.share.change") + 
  scale_fill_gradient2(low = "#085BB2", high = "#FF2312", mid = "#4DAFFF", midpoint = -0.02) +
  labs(title = "Change in voteshare for Republican party 2012 to 2016")

descrip_df2 %>% filter(state != "HI") %>% county.heatmap("flip") + 
  scale_fill_gradient2(low = "#085BB2", high = "#FF2312", mid = "#4DAFFF", midpoint = 0.5) +
  labs(title = "Counties that flipped from Democrat to Republican 2012 to 2016")

#List the counties that flipped and put it in a table (all the names) for descriptive statistics:
flipped_counties <- descrip_df2 %>%
  filter(flip == 1)

table(flipped_counties$state) #shows the number of counties in each state that flipped.
#Higest are: IA 33, MI 12, MN 19, NY 20, WI 23



#************************************************************************
                              #Scatter plots
#************************************************************************
#Overall unemployment growth
hist(p2_merged_df5$unemp_gro)

#Overall PCI growth
hist(p2_merged_df5$pci_gro)

#Overall unemployment growth for flipped counties
logit.data1 <- logit.data %>%
  dplyr::filter(flip == 1)

hist(logit.data1$unemp_gro)

#Overall pci growth for flipped counties
hist(logit.data1$pci_gro)


#Correlation:
cor(merged_df4[,c("unemp_gro", "PCI_gro")], use="complete.obs", method="pearson")
cor(merged_df4[,c("rep.share", "repshare.lag")], use="complete.obs", method="pearson")

cor(p2_merged_df5[,c("unemp_gro", "pci_gro")], use="complete.obs", method="pearson")







#Descriptive Stats:
descrp_p1 <- merged_df4 %>%
  select(rep.share, unemp_gro, PCI_gro, Pop, repshare.lag, white.percent)

table_p1 <- basicStats(merged_df4)
kable(descrp_p1, align="c", caption="Summary of Descriptive Statistics", digits = 2)


descrp_p2 <- p2_merged_df5 %>%
  select(unemp_gro, pci_gro, pcct_gro, jobs_gro, av_wage_gro)

set.seed(42)
graph1 <- data.frame(unemp_gro = rnorm(100),
               pci_gro = rnorm(100),
               pcct_gro = rnorm(100),
               jobs_gro = rnorm(100),
               av_wage_gro = rnorm(100))
ggpairs(graph1)

#********************
  # table(merged_df4$repshare.lag)
