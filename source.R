
#**********************************************************************************************
                                                  #Processing 
#**********************************************************************************************

#Loading all the necessary packages
packages <- c("bea.R", "acs", "haven", "magrittr", "httr", "tidyr", "blsAPI", "rjson", "readxl", "broom", "jsonlite",
              "stringr", "rJava", "xlsx", "qdap", "data.table", "plm", "rio", "Zelig", "stargazer", "knitr", 
              "lmtest", "GGally", "viridis", "ggmap", "ggplot2", "interplot", "plyr", "dplyr")
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

#Part I:
#####
descrp_p1 <- merged_df4 %>%
  select(rep.share, repshare.lag, unemp_gro, Pop_thou, white.percent, rep_incumb, rural_percent)

set.seed(42)

correlation_p1 <- data.frame(rep.share = rnorm(100),
                             repshare.lag = rnorm(100),
                             unemp_gro = rnorm(100))
corplot1 <- ggpairs(correlation_p1)
#####
correlation_p2 <- data.frame(rep.share = rnorm(100),
                             Pop_thou = rnorm(100),
                             white.percent = rnorm(100),
                             rep_incumb = rnorm(100),
                             rural_percent = rnorm(100))
corplot2 <- ggpairs(correlation_p2)
#####
f1 <- plm(rep.share ~ unemp_gro + repshare.lag, merged_df4, model = 'within')
f2 <- plm(rep.share ~ unemp_gro + repshare.lag + Pop_thou + white.percent + as.factor(rep_incumb)
          + unemp_gro:as.factor(rep_incumb) + as.factor(rural)
          + white.percent:as.factor(rural), merged_df4, model = 'within')

test1 <- coeftest(f2, vcovHC(f2, method = "arellano"))
#####
#interplot marginal effects plots

#**************************************************************************************************************************
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

stargazer::stargazer(f2, type = 'text', digits = 2, header = FALSE,   
                     title = 'Model with Unemployment (OLS)', font.size = 'normalsize', out = 'fixed_part1.htm')

test1 <- coeftest(f2, vcovHC(f2, method = "arellano"))

stargazer::stargazer(test1, type = 'text', digits = 2, header = FALSE,   
                     title = 'Model with Arellano', font.size = 'normalsize', out = 'fixed_part1_arellano.htm')


#augment(f2, newdata = filter(forecast_df, year == 2016))
#predict(f2, filter(forecast_df, year == 2016), se.fit = TRUE, interval = "confidence")
#predict(f2, filter(forecast_df, year == 2016), se.fit = TRUE, interval = "prediction")




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

png("Plot.png", width = 1800, height = 1800,
    res = 300)

ggplot(part2a_df) +
  geom_point(aes(x = pred_repshare, y = rep.share), alpha = 0.25) +
  geom_abline(slope = 1, intercept = 0) +
  labs(x = "Predicted Republican Voteshare", y = "Actual Republican Voteshare",
       title = "Residuals from 2016 US Presidential Elections Forecasting",
       caption = "Source: Dave Liep, US Election Atlas") +
  theme_classic()

dev.off()

##################################################################################################################

part2a_df2 <- part2a_df %>%
  mutate(resid = rep.share - pred_repshare)%>%
  select(county.fips, resid)

p2_merged_df8 <- merge(p2_merged_df7, part2a_df2, all.x = TRUE)
######


descrp_p2 <- p2_merged_df8 %>%
  select(resid, manu_share_gro, av_wage_gro, lfpr_male_gro, gini_gro, uneduc)

set.seed(42)

correlation_p3 <- data.frame(resid = rnorm(100),
                             manu_share_gro = rnorm(100),
                             av_wage_gro = rnorm(100),
                             lfpr_male_gro = rnorm(100),
                             gini_gro = rnorm(100),
                             uneduc = rnorm(100))
corplot3 <- ggpairs(correlation_p3)

#####


p2_merged_df8_swing <- p2_merged_df8 %>%
  filter(state == "CO" | state == "FL" | state == "IA" 
         | state == "NC" | state == "NH" | state == "OH" | state == "PA" 
         | state == "VA" | state == "NV" | state == "WI")

p2_merged_df8_rust <- p2_merged_df8 %>%
  filter(state == "NY" | state == "PA" | state == "WV" | state == "OH" | state == "IN"
         | state == "MI" | state == "IL" | state == "IA" | state == "WI")


m24 <- lm(resid ~ -1 + manu_share_gro + av_wage_gro + lfpr_male_gro + gini_gro + uneduc, p2_merged_df8)

#For swing states:
m25 <- lm(resid ~ -1 + manu_share_gro + av_wage_gro + lfpr_male_gro + gini_gro + uneduc, p2_merged_df8_swing)

#For rust belt states:
m26 <- lm(resid ~ -1 + manu_share_gro + av_wage_gro + lfpr_male_gro + gini_gro + uneduc, p2_merged_df8_rust)



m27 <- lm(resid ~ -1 + manu_share_gro + av_wage_gro + lfpr_male_gro + gini_gro + uneduc + lfpr_male_gro:uneduc, p2_merged_df8)

#For swing states:
m28 <- lm(resid ~ -1 + manu_share_gro + av_wage_gro + lfpr_male_gro + gini_gro + uneduc + lfpr_male_gro:uneduc, p2_merged_df8_swing)

#For rust belt states:
m29 <- lm(resid ~ -1 + manu_share_gro + av_wage_gro + lfpr_male_gro + gini_gro + uneduc + lfpr_male_gro:uneduc, p2_merged_df8_rust)

#################################################################################################################



#New Model with Share of Manufacturing Jobs and Average Wage and LFPR:



#************************************************************************
source("heat-map.R")
#************************************************************************
maps_df <- p2_merged_df8 %>%
  mutate(rep.share.change = rep.share - repshare.lag)
  
map1 <- maps_df %>% filter(state != "HI") %>% county.heatmap("rep.share.change") + 
  scale_fill_gradient2(low = "#085BB2", high = "#FF2312", mid = "#4DAFFF", midpoint = -0.02) +
  labs(title = "Change in voteshare for Republican party 2012 to 2016")

map2 <- maps_df %>% filter(state != "HI") %>% county.heatmap("flip") + 
  scale_fill_gradient2(low = "#085BB2", high = "#FF2312", mid = "#4DAFFF", midpoint = 0.5) +
  labs(title = "Counties that flipped from Democrat to Republican 2012 to 2016")

#List the counties that flipped and put it in a table (all the names) for descriptive statistics:
#flipped_counties <- descrip_df2 %>%
 # filter(flip == 1)

#table(flipped_counties$state) #shows the number of counties in each state that flipped.
#Higest are: IA 33, MI 12, MN 19, NY 20, WI 23




################################# FINAL REGRESSIONS ######################################################

