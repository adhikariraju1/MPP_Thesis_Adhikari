
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

corplot1 <- descrp_p1 %>%
  select(rep.share, repshare.lag, unemp_gro) %>%
  ggpairs(lower = list("continuous" = "smooth"))
#####
corplot2 <- descrp_p1 %>%
  select(rep.share, Pop_thou, white.percent, rural_percent) %>%
  ggpairs(lower = list("continuous" = "smooth"))

#####

merged_df4 <- merged_df4 %>%
  filter(Pop_thou <= 7500)

f1 <- plm(rep.share ~ unemp_gro + repshare.lag, merged_df4, model = 'within')
f2 <- plm(rep.share ~ unemp_gro + repshare.lag + Pop_thou + white.percent + as.factor(rep_incumb)
          + unemp_gro:as.factor(rep_incumb) + as.factor(rural)
          + white.percent:as.factor(rural), merged_df4, model = 'within')

test1 <- coeftest(f1, vcovHC(f1, method = "arellano"))
test2 <- coeftest(f2, vcovHC(f2, method = "arellano"))
#####
#interplot marginal effects plots

#**************************************************************************************************************************
#For forecasting:

part2_df <- p2_merged_df7 %>%
  mutate(rep_incumb = 0) %>%
  dplyr::select(county.fips, county, state, rep.share, repshare.lag, unemp_gro,
         pci_gro, pop_thou, white.percent, rep_incumb, rural_percent, is.rep.2012)

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
part2a_df %>%
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

dev.off()

p2_merged_df8$resid %>% abs() %>% mean(na.rm = T)
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

