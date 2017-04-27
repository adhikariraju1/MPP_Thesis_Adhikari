
#**********************************************************************************************
                                                  #Processing 
#**********************************************************************************************

#Loading all the necessary packages
packages <- c("bea.R", "acs", "haven", "httr", "blsAPI", "rjson", "readxl", "broom", "jsonlite",
              "stringr", "rJava", "xlsx", "qdap", "data.table", "plm", "rio", "stargazer", 
              "lmtest", "GGally", "viridis", "ggmap", "sjPlot", "sjmisc", "ggplot2", "knitr", "tidyr", 
              "magrittr", "plyr", "dplyr", "gridExtra", "grid")
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
  select(rep.share, repshare.lag, unemp_gro, log.Pop, white.percent, rep_incumb, rural_percent)

corplot1 <- descrp_p1 %>%
  select(rep.share, repshare.lag, unemp_gro) %>%
  ggpairs(lower = list("continuous" = "smooth"))

corplot2 <- descrp_p1 %>%
  select(rep.share, log.Pop, white.percent, rural_percent) %>%
  ggpairs(lower = list("continuous" = "smooth"))



merged_df5 <- merged_df4 

#Regression Models:
f1 <- plm(rep.share ~ unemp_gro + repshare.lag, merged_df5, model = 'within')
f2 <- plm(rep.share ~ unemp_gro + repshare.lag + log(Pop) + white.percent + as.factor(rep_incumb)
          + unemp_gro:as.factor(rep_incumb) + rural_percent
          + white.percent:rural_percent, merged_df5, model = 'within')
f3 <- plm(rep.share ~ unemp_gro + repshare.lag + log(Pop) + white.percent + as.factor(rep_incumb)
          + unemp_gro:as.factor(rep_incumb) + rural_percent
          + white.percent:rural_percent + year, merged_df5, model = 'within')

f2random <- plm(rep.share ~ unemp_gro + repshare.lag + log(Pop) + white.percent + as.factor(rep_incumb)
                  + unemp_gro:as.factor(rep_incumb) + rural_percent
                  + white.percent:rural_percent, merged_df5, model = 'random')

#Regression after Arellano-Bond:
test1 <- coeftest(f1, vcovHC(f1, method = "arellano"))
test2 <- coeftest(f2, vcovHC(f2, method = "arellano"))
test3 <- coeftest(f3, vcovHC(f3, method = "arellano"))


#Marginal effects plots
#Can"t be done.


#**************************************************************************************************************************
#For forecasting:

p2_merged_df7 <- p2_merged_df7 %>%
  mutate(rep_incumb = 0)

tidy(f3)
str(fixef(f3))
intercept <- data.frame(county.fips = names(fixef(f3)),
           fixef = as.vector(fixef(f3)))

p2_merged_df7 <- merge(p2_merged_df7, intercept)

p2_merged_df7 <- p2_merged_df7 %>%
  mutate(pred_repshare = fixef - unemp_gro*0.010424064 + repshare.lag*0.667388497 + log(pop)*0.013094576 
         + white.percent*0.438572256 - rep_incumb* 0.091746860 - unemp_gro*rep_incumb*0.003444465 - 
           white.percent*rural_percent*0.003856760)

#png("Plot.png", width = 1800, height = 1800,
    #res = 300)
p2_merged_df7 %>%
  filter(!is.na(rep.share)) %>%
ggplot() +
  geom_point(aes(x = pred_repshare, y = rep.share, colour = as.factor(is.rep.2012)), alpha = 0.25) +
  geom_abline(slope = 1, intercept = 0) +
  geom_segment(x = 0, y = 0.5, xend = 0.5, yend = 0.5, linetype = 2) +
  geom_segment(x = 0.5, y = 0, xend = 0.5, yend = 0.5, linetype = 2) +
  labs(x = "Predicted Republican Voteshare", y = "Actual Republican Voteshare") +
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

###################################################################
bluetored <- p2_merged_df8 %>%
  filter(is.rep.2012 == 0) %>%
  filter(rep.share > 0.50)
  
table1_b2r <- table(bluetored$state) %>%
  as.data.frame() %>%
  rename(State = Var1, `Counties D to R` = Freq)

bluetoredandhigher <- bluetored %>%
  filter(resid > 0)

table1_b2rh <- table(bluetoredandhigher$state) %>%
  as.data.frame() %>%
  rename(State = Var1, `Counties Underpredicted` = Freq)

table100 <- merge(table1_b2r, table1_b2rh)

#
redtored <- p2_merged_df8 %>%
  filter(is.rep.2012 == 1) %>%
  filter(rep.share > 0.50)

table1_r2r <- table(redtored$state) %>%
  as.data.frame() %>%
  rename(State = Var1, `Counties R to R` = Freq)

redtoredhigher <- redtored %>%
  filter(resid > 0)

table1_r2rh <- table(redtored$state) %>%
  as.data.frame() %>%
  rename(State = Var1, `Counties underpredicted` = Freq)

table200 <- merge(table1_r2r, table1_r2rh)
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



#New Model with Share of Manufacturing Jobs and Average Wage and LFPR:




maps_df <- p2_merged_df8 %>%
  mutate(rep.share.change = rep.share - repshare.lag)

map_theme <-   theme(
  legend.position = "none",
  panel.grid = element_blank(),
  axis.title = element_blank(),
  axis.text = element_blank()
)
  
map1 <- maps_df %>% filter(state != "HI") %>% county.heatmap("rep.share.change") + 
  scale_fill_gradient2(low = "#085BB2", high = "#FF2312", mid = "#4DAFFF", midpoint = -0.02) +
  map_theme

map2 <- maps_df %>% filter(state != "HI") %>% county.heatmap("flip") + 
  scale_fill_gradient2(low = "#085BB2", high = "#FF2312", mid = "#4DAFFF", midpoint = 0.5) +
  map_theme

map3 <- maps_df %>% filter(state != "HI") %>% county.heatmap("resid") + 
  scale_fill_viridis(option = "D", discrete = FALSE) +
  map_theme

map4 <- maps_df %>% filter(state != "HI") %>% county.heatmap("rep.share.change") + 
  scale_fill_viridis(option = "A", discrete = FALSE) +
  map_theme

#List the counties that flipped and put it in a table (all the names) for descriptive statistics:
#flipped_counties <- descrip_df2 %>%
 # filter(flip == 1)

#table(flipped_counties$state) #shows the number of counties in each state that flipped.
#Higest are: IA 33, MI 12, MN 19, NY 20, WI 23


m24 <- lm(resid ~ -1 + manu_share_gro + av_wage_gro + lfpr_male_gro + gini_gro + uneduc, p2_merged_df8)

#For swing states:
m25 <- lm(resid ~ -1 + manu_share_gro + av_wage_gro + lfpr_male_gro + gini_gro + uneduc, p2_merged_df8_swing)

#For rust belt states:
m26 <- lm(resid ~ -1 + manu_share_gro + av_wage_gro + lfpr_male_gro + gini_gro + uneduc, p2_merged_df8_rust)
      



################################# Appendix ######################################################


