library(dplyr)
library(tidyr)
library(data.table)
library(xlsx)


race <- read.fwf("C:/RajuPC/MPP Final Thesis/MPP_Thesis_Adhikari/al.1990_2014.19ages.adjusted.txt", 
                                 widths = c(4,2,2,3,2,1,1,1,2,8), colClasses = "character")

colnames(race) <- c("year", "state", "state.fips", "county.fips", "registry", "race", "origin", 
                    "sex", "age.group", "population")

race$year <- as.numeric(race$year) 
race$population <- as.numeric(race$population)
race$age.group <- as.numeric(race$age.group)

#To concatenate:
race$county.fips <- paste(race$state.fips, race$county.fips, sep="")
race$county.fips <- as.numeric(race$county.fips)

race$race <- paste(race$race, race$origin, sep="")

race <- race[-c(3,7)] #Removing the columns from before we did the concatenate.

race <- race %>%
  filter(year %% 4 == 0) %>%
  filter(age.group >= 5)

#To concatenate:
#101 is white male, 201 is black male, 102 is white female, 202 is black female
#create hispanic male, hispanic female, other male, other female (301, 401 combined) (302, 402 combined)
#(111, 211 combined, and 112, 212 combined)
race$racebysex <- paste(race$race, race$sex, sep="")

race <- race[-c(5,6)]

#Now the column age group can be removed since we don't need to group by this category anymore.
race <- race[-c(5)]


race <- race[c(1,2,3,6,5)] #Subsetting into the final dataframe with only variables and in proper order.

race <- race %>%
  group_by(county.fips, state, year, racebysex) %>%
  summarize(population = sum(population))

race <- spread(race, racebysex, population)
  
setnames(race, old =c("county.fips", "state", "year", "101", "102", "111", "112", "201", 
                      "202", "211", "212", "301", "302", "311", "312", "401", "402", "411", "412"), 
         new = c("county.fips", "state", "year", "whitemale", "whitefemale", "white.his.male", "white.his.female", "blackmale", 
                 "blackfemale", "black.his.male", "black.his.female", "indmale", "indfemale", "ind.his.male", "ind.his.female", 
                 "asianmale", "asianfemale", "asian.his.male", "asian.his.female"))  


race$other.male <- race$white.his.male + race$black.his.male + 
  race$indmale + race$ind.his.male + race$asianmale + race$asian.his.male

race$other.female <- race$white.his.female + race$black.his.female + 
  race$indfemale + race$ind.his.female + race$asianfemale + race$asian.his.female


race <- race[c(1:5,8,9,20,21)]

#At the very end. But do all this in Command Prompt
write.xlsx(race, "C:/RajuPC/MPP Final Thesis/MPP_Thesis_Adhikari/race.xlsx")

 
  



  
  


