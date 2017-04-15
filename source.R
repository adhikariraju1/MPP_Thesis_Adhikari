
#********************************************Pre-processing ritual************************************

#Loading all the necessary packages

packages <- c("bea.R", "acs", "magrittr", "httr", "tidyr", "blsAPI", "rjson", "readxl", "dplyr", "jsonlite",
              "stringr", "rJava", "xlsx", "qdap", "data.table", "plm", "rio", "Zelig")
load <- lapply(packages, require, character.only = T)

#Setting the working directory
setwd("C:/RajuPC/MPP Final Thesis/WorkingDirectory")

#All the keys for different APIs obtained from the respective websites
blskey <- "a9e62413e38741b5aeb814efc5a3d066"
beaKey <- 'C3812F4D-F498-40F8-9F36-9FF5AF65DBD7'
censusKey <- "c7ed765d1b03f4217ccc4b37d31b0dc3580db44e"
datagovkey <- "ubFrNGonfwMQm3lK04C6djaMcqFuIe5mvev4RooI"

#***************Link with other R files********************************************************

source("Statistics.R")
#********************************************Regressions************************************

m1_fe <- plm(rep.share ~ unemp_gro + PCI_gro, merged_df4)

summary(m1_fe)

m2_fe <- plm(rep.share ~ unemp_gro + unemp_gro:as.factor(rep_incumb) + repshare.lag + PCI_gro + 
               as.factor(rep_incumb) + Pop + as.factor(rural) + white.percent, merged_df4, model = 'within')

summary(m2_fe)

table(merged_df4$repshare.lag)

m3_fe <- plm(rep.share ~ unemp_gro + repshare.lag + PCI_gro + rep_incumb, merged_df4, model = 'within')


source("Statistics_Part2.R")


## CLean the education data
edu.data <- rio::import("ACS_15_5YR_B15003_with_ann.csv", skip = 1) %>%
  select(-matches("Margin"), -`Estimate; Total:`) %>%
  select(matches("school|Kinder|grade|id2")) %>%
  dplyr::rename(county.fips = Id2)

colnames(edu.data) <- colnames(edu.data) %>% 
  stringi::stri_replace_all_fixed("Estimate; Total: - ", "") %>%
  stringi::stri_replace_all_fixed(" ", "") %>%
  stringi::stri_replace_all_regex(",|'", "")

edu.data %<>%
  select(-Professionalschooldegree, -Regularhighschooldiploma, -`12thgradenodiploma`) %>%
  mutate(total = rowSums(.) - county.fips) %>%
  select(county.fips, total)
  
#********************************************Regressions************************************
p2_m1_ols <- lm(rep.share.gro ~ unemp_gro + pci_gro + total, p2_merged_df4)

summary(p2_m1_ols)


p2_m2_ols <- lm(rep.share.gro ~ unemp_gro + pci_gro + pop + white.percent + as.factor(rural) + 
                  white.percent:as.factor(rural), p2_merged_df4)
summary(p2_m2_ols)


logit.data <- p2_merged_df4 %>%
  mutate(flip = is.rep.2016 - is.rep.2012) %>%
  filter(flip != -1)

p2_m2_logit <- glm(flip ~ unemp_gro + pci_gro + pop +total, logit.data, family = binomial(link = "logit"))
summary(p2_m2_logit)


p2_m2_relogit <- zelig(flip ~ unemp_gro + pci_gro + pop + white.percent,
                       model = "relogit", data = logit.data, tau = 0.05)
summary(p2_m2_relogit)
t <- setx(p2_m2_relogit) %>% sim(p2_m2_relogit, .)


#********************Visualization***************************************
p2_merged_df4 %>% filter(state != "HI") %>% county.heatmap("rep.share") + 
  scale_fill_gradient2(low = "#085BB2", high = "#FF2312", mid = "#4DAFFF", midpoint = 0.40)