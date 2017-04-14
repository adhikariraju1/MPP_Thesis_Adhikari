
#********************************************Pre-processing ritual************************************

#Loading all the necessary packages

packages <- c("bea.R", "acs", "magrittr", "httr", "tidyr", "blsAPI", "rjson", "readxl", "dplyr", "jsonlite",
              "stringr", "rJava", "xlsx", "qdap", "data.table", "plm", "rio")
load <- lapply(packages, require, character.only = T)

#Setting the working directory
setwd("C:/RajuPC/MPP Final Thesis/WorkingDirectory")

#All the keys for different APIs obtained from the respective websites
blskey <- "a9e62413e38741b5aeb814efc5a3d066"
beaKey <- 'C3812F4D-F498-40F8-9F36-9FF5AF65DBD7'
censusKey <- "c7ed765d1b03f4217ccc4b37d31b0dc3580db44e"
datagovkey <- "ubFrNGonfwMQm3lK04C6djaMcqFuIe5mvev4RooI"

#***************Link with other R files********************************************************8888

source("Statistics.R")
#********************************************Regressions************************************

#Main economic variables only:
m1_fe <- plm(rep.share ~ unemp_gro + PCI_gro, merged_df4)

summary(m1_fe)

m2_fe <- plm(rep.share ~ unemp_gro + unemp_gro:rep_incumb + repshare.lag + PCI_gro + rep_incumb + Pop + rural + white.percent, merged_df4, model = 'within')

summary(m2_fe)

table(merged_df4$repshare.lag)

m3_fe <- plm(rep.share ~ unemp_gro + repshare.lag + PCI_gro + rep_incumb, merged_df4, model = 'within')


source("Statistics_Part2.R")