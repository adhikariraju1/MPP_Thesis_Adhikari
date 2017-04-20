
#**********************************************************************************************
                                                  #Processing 
#**********************************************************************************************

#Loading all the necessary packages
packages <- c("bea.R", "acs", "haven", "plyr", "magrittr", "httr", "tidyr", "blsAPI", "rjson", "readxl", "dplyr", "broom", "jsonlite",
              "stringr", "rJava", "xlsx", "qdap", "data.table", "plm", "rio", "Zelig", "stargazer", "knitr", "lmtest")
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
#********************************************************************************************
source("Statistics_Part2.R")
#********************************************************************************************

#For forecasting:
part1_df <- merged_df4 %>%
  mutate(pci_gro_sq = PCI_gro^2) %>%
  select(county.fips, year, county, state, rep.share, repshare.lag, unemp_gro, 
         PCI_gro, pci_gro_sq, Pop_thou, white.percent, rep_incumb, rural_percent) %>%
  rename(pop_thou = Pop_thou, pci_gro = PCI_gro)


part2_df <- p2_merged_df6 %>%
  mutate(year = "2016") %>%
  mutate(rep_incumb = "0") %>%
  mutate(pci_gro_sq = pci_gro^2) %>%
  select(county.fips, year, county, state, rep.share, repshare.lag, unemp_gro,
         pci_gro, pci_gro_sq, pop_thou, white.percent, rep_incumb, rural_percent)

part2_df$year <- as.numeric(part2_df$year)
part2_df$rep_incumb <- as.numeric(part2_df$rep_incumb)


forecast_df <- bind_rows(part1_df, part2_df)


f1 <- plm(rep.share ~ unemp_gro + repshare.lag, data = filter(forecast_df, year < 2016), model = 'within')
f2 <- plm(rep.share ~ unemp_gro + repshare.lag + pop_thou + white.percent + as.factor(rep_incumb)
          + unemp_gro:as.factor(rep_incumb) + rural_percent:white.percent, data = filter(forecast_df, year < 2016), model = 'within')

stargazer::stargazer(f1, f2, type = 'text', digits = 2, header = FALSE,   
                     title = 'Model with Unemployment (OLS)', font.size = 'normalsize', out = 'fixed_part1.htm')

test1 <- coeftest(f2, vcovHC(f2, method = "arellano"))

stargazer::stargazer(test1, type = 'text', digits = 2, header = FALSE,   
                     title = 'Model with Arellano', font.size = 'normalsize', out = 'fixed_part1_arellano.htm')


augment(f2, newdata = filter(forecast_df, year == 2016))
predict(f2, filter(forecast_df, year == 2016), se.fit = TRUE, interval = "confidence")
predict(f2, filter(forecast_df, year == 2016), se.fit = TRUE, interval = "prediction")




######################################################Alternate####################################################

tidy(f2)
str(fixef(f2))
intercept <- data.frame(county.fips = names(fixef(f2)),
           fixef = as.vector(fixef(f2)))

part2a_df <- merge(part2_df, intercept)
part2a_df <- part2a_df %>%
  mutate(pred_repshare = fixef - unemp_gro*0.0247103253 + repshare.lag*0.7276273733 + pop_thou*0.0001052426 
         + white.percent*0.1558135764 - rep_incumb* 0.0404342633 - unemp_gro*rep_incumb*0.0367096008 + 
           white.percent*rural_percent*0.0057858007)

plot(part2a_df$pred_repshare, part2a_df$rep.share)
abline(0, 1)

##################################################################################################################
part2a_df2 <- part2a_df %>%
  mutate(resid = rep.share - pred_repshare)%>%
  select(county.fips, resid)

p2_merged_df7 <- merge(p2_merged_df6, part2a_df2)


p2_merged_df7_swing <- p2_merged_df7 %>%
  filter(state == "CO" | state == "FL" | state == "IA" 
         | state == "NC" | state == "NH" | state == "OH" | state == "PA" 
         | state == "VA" | state == "NV" | state == "WI")

p2_merged_df7_rust <- p2_merged_df7 %>%
  filter(state == "NY" | state == "PA" | state == "WV" | state == "OH" | state == "IN"
         | state == "MI" | state == "IL" | state == "IA" | state == "WI")





#################################################################################################################
#Base regression(OLS, FE, RE) with Unemployment: m1a
p1_m1a_ols <- lm(rep.share ~ unemp_gro + repshare.lag, merged_df4)
p1_m1a_fe <- plm(rep.share ~ unemp_gro + repshare.lag, merged_df4, model = 'within')
p1_m1a_re <- plm(rep.share ~ unemp_gro + repshare.lag, merged_df4, model = 'random')

phtest(p1_m1a_fe, p1_m1a_re) #Hausman test to choose between FE and RE shows that FE is better since p-value < 0.05

stargazer::stargazer(p1_m1a_ols, p1_m1a_fe, p1_m1a_re, type = 'text', digits = 2, header = FALSE,   
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

#New Model with Share of Manufacturing Jobs and Average Wage and LFPR:

m24 <- lm(resid ~ manu_share_gro + av_wage_gro + lfpr_male_gro + pop_thou + uneduc + white.percent + 
            + rural_percent, p2_merged_df7)
summary(m24)

#For swing states:
m25 <- lm(resid ~ manu_share_gro + av_wage_gro + lfpr_male_gro + pop_thou + uneduc + white.percent + 
            + rural_percent, p2_merged_df7_swing)
summary(m25)

#For rust belt states:
m26 <- lm(resid ~ manu_share_gro + av_wage_gro + lfpr_male_gro + pop_thou + uneduc + white.percent + 
            + rural_percent, p2_merged_df7_rust)
summary(m26)



#************************************************************************
source("heat-map.R")
#************************************************************************



#Part II:

descrip_df2 <- p2_merged_df5 %>%
  mutate(rep.share.change = rep.share - repshare.lag)
  
map1 <- descrip_df2 %>% filter(state != "HI") %>% county.heatmap("rep.share.change") + 
  scale_fill_gradient2(low = "#085BB2", high = "#FF2312", mid = "#4DAFFF", midpoint = -0.02) +
  labs(title = "Change in voteshare for Republican party 2012 to 2016")

map2 <- descrip_df2 %>% filter(state != "HI") %>% county.heatmap("flip") + 
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



#Correlation:
cor(merged_df4[,c("unemp_gro", "PCI_gro")], use="complete.obs", method="pearson")
cor(merged_df4[,c("rep.share", "repshare.lag")], use="complete.obs", method="pearson")

cor(p2_merged_df5[,c("unemp_gro", "pci_gro")], use="complete.obs", method="pearson")



#Descriptive Stats:
descrp_p1 <- merged_df4 %>%
  select(rep.share, unemp_gro, PCI_gro, Pop, repshare.lag, white.percent)


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

################################# FINAL REGRESSIONS ######################################################

f1 <- plm(rep.share ~ unemp_gro + repshare.lag, merged_df4, model = 'within')
f2 <- plm(rep.share ~ unemp_gro + repshare.lag + Pop_thou + white.percent + as.factor(rep_incumb)
          + unemp_gro:as.factor(rep_incumb) + as.factor(rural)
          + white.percent:as.factor(rural), merged_df4, model = 'within')

stargazer::stargazer(f1, f2, type = 'text', digits = 2, header = FALSE,   
                     title = 'Model with Unemployment (OLS)', font.size = 'normalsize', out = 'fixed_part1.htm')

test1 <- coeftest(f2, vcovHC(f2, method = "arellano"))

stargazer::stargazer(test1, type = 'text', digits = 2, header = FALSE,   
                     title = 'Model with Arellano', font.size = 'normalsize', out = 'fixed_part1_arellano.htm')

f3 <- plm(rep.share ~ PCI_gro + repshare.lag, merged_df4, model = 'within')
f4 <- plm(rep.share ~ PCI_gro + repshare.lag + Pop_thou + white.percent + as.factor(rep_incumb)
          + PCI_gro:as.factor(rep_incumb) + as.factor(rural)
          + white.percent:as.factor(rural), merged_df4, model = 'within')

stargazer::stargazer(f1, f2, type = 'text', digits = 2, header = FALSE,   
                     title = 'Model with Unemployment (OLS)', font.size = 'normalsize', out = 'fixed_part1.htm')
