
#**********************************************************************************************
                                                  #Processing 
#**********************************************************************************************

#Loading all the necessary packages
packages <- c("bea.R", "acs", "haven", "httr", "blsAPI", "rjson", "readxl", "broom", "jsonlite",
              "stringr", "rJava", "xlsx", "qdap", "data.table", "plm", "rio", "Zelig", "stargazer", 
              "lmtest", "GGally", "viridis", "ggmap", "sjPlot", "sjmisc", "ggplot2", "knitr", "tidyr", "magrittr", "plyr", "dplyr")
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
#************************************************************************
source("heat-map.R")
#************************************************************************

#Part I:
#Correlation Plots for Part I:
descrp_p1 <- merged_df4 %>%
  select(rep.share, repshare.lag, unemp_gro, Pop_thou, white.percent, rep_incumb, rural_percent)

corplot1 <- descrp_p1 %>%
  select(rep.share, repshare.lag, unemp_gro) %>%
  ggpairs(lower = list("continuous" = "smooth"))

corplot2 <- descrp_p1 %>%
  select(rep.share, Pop_thou, white.percent, rural_percent) %>%
  ggpairs(lower = list("continuous" = "smooth"))


#We see that Los Angeles county is skewing the normal distribution of Population. So we filter it out.
merged_df5 <- merged_df4 %>%
  filter(Pop_thou <= 7500)

#Regression Models:
f1 <- plm(rep.share ~ unemp_gro + repshare.lag, merged_df5, model = 'within')
f2 <- plm(rep.share ~ unemp_gro + repshare.lag + Pop_thou + white.percent + as.factor(rep_incumb)
          + unemp_gro:as.factor(rep_incumb) + rural_percent
          + white.percent:rural_percent, merged_df5, model = 'within')

#Regression after Arellano-Bond:
test1 <- coeftest(f1, vcovHC(f1, method = "arellano"))
test2 <- coeftest(f2, vcovHC(f2, method = "arellano"))

#Marginal effects plots



#**************************************************************************************************************************
#For forecasting:

p2_merged_df7 <- p2_merged_df7 %>%
  mutate(rep_incumb = 0)

tidy(f2)
str(fixef(f2))
intercept <- data.frame(county.fips = names(fixef(f2)),
           fixef = as.vector(fixef(f2)))

p2_merged_df7 <- merge(p2_merged_df7, intercept)

p2_merged_df7 <- p2_merged_df7 %>%
  mutate(pred_repshare = fixef - unemp_gro*0.0247103253 + repshare.lag*0.7276273733 + pop_thou*0.0001052426 
         + white.percent*0.1558135764 - rep_incumb* 0.0404342633 - unemp_gro*rep_incumb*0.0367096008 + 
           white.percent*rural_percent*0.0057858007)

#png("Plot.png", width = 1800, height = 1800,
    #res = 300)
p2_merged_df7 %>%
  filter(!is.na(rep.share)) %>%
ggplot() +
  geom_point(aes(x = pred_repshare, y = rep.share, colour = as.factor(is.rep.2012)), alpha = 0.25) +
  geom_abline(slope = 1, intercept = 0) +
  geom_segment(x = 0, y = 0.5, xend = 0.5, yend = 0.5, linetype = 2) +
  geom_segment(x = 0.5, y = 0, xend = 0.5, yend = 0.5, linetype = 2) +
  labs(x = "Predicted Republican Voteshare", y = "Actual Republican Voteshare",
       title = "Residuals from 2016 US Presidential Elections Forecasting",
       caption = "Source: Dave Liep, US Election Atlas") +
  scale_colour_manual(values = c("steelblue", "red")) +
  theme_classic() +
  theme(
    legend.position = "none"
  )

#dev.off()

p2_merged_df8 <- p2_merged_df7 %>%
  mutate(resid = rep.share - pred_repshare)

##################################################################################################################
#Table in forecasting section:

p2_merged_df8$resid %>% abs() %>% mean(na.rm = T)

bluetored <- p2_merged_df8 %>%
  filter(is.rep.2012 == 0) %>%
  filter(rep.share > 0.50)
  
count(bluetored)
#221 counties changed from blue to red form 2012 to 2016.
table1_b2r <- table(bluetored$state)
table1_b2r

#################################################################33
bluetoredandhigher <- bluetored %>%
  filter(resid > 0)

count(bluetoredandhigher)
#Out of them 152 of them were underpredicted by the forecasting model. 

table2_b2rh <- table(bluetoredandhigher$state)
table2_b2rh


#
redtored <- p2_merged_df8 %>%
  filter(is.rep.2012 == 1) %>%
  filter(rep.share > 0.50) %>%
  filter(resid > 0)

count(redtored)
#1378 out of 2361 Republican counties where our model underpredicted.

table3_r2rh <- table(redtored$state)
table3_r2rh

##################################################################################################################


#For correlation plot for part 2:

descrp_p2 <- p2_merged_df8 %>%
  select(resid, manu_share_gro, av_wage_gro, lfpr_male_gro, gini_gro, uneduc)


corplot3 <- descrp_p2 %>%
  select(resid, manu_share_gro, av_wage_gro, lfpr_male_gro, gini_gro, uneduc) %>%
  ggpairs(lower = list("continuous" = "smooth"))


#Creating separate dataframes for swing and rust-belt states:

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



#New Model with Share of Manufacturing Jobs and Average Wage and LFPR:




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

