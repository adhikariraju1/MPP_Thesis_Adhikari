
#********************************************Election dataset************************************

#Loading Republican voting data:
election_df1 <- read.csv("full-us-presidential-counties-2012-2016-rep.csv")
#Renaming column names to merge them with other data later
names(election_df1)[names(election_df1)=="state.abb"] <- "state"
names(election_df1)[names(election_df1)=="county.name"] <- "county"
names(election_df1)[names(election_df1)=="ï..year"] <- "year"
names(election_df1)[names(election_df1)=="vote.percent"] <- "vote.percent.rep"


#Converting the county and state columns from factors into strings so that they can be merged with other dataframes later
election_df1$county <- as.character(election_df1$county)
election_df1$state <- as.character(election_df1$state)

#subsetting only the necessary columns for the final dataframe.
election_df1 <- election_df1[c(1,2,5,7,10)]


#Loading Democratic voting data:
election_df2 <- read.csv("full-us-presidential-counties-2012-2016-demo.csv")
#Renaming column names to merge them with other data later
names(election_df2)[names(election_df2)=="state.abb"] <- "state"
names(election_df2)[names(election_df2)=="county.name"] <- "county"
names(election_df2)[names(election_df2)=="ï..year"] <- "year"
names(election_df2)[names(election_df2)=="vote.percent"] <- "vote.percent.dem"


#Converting the county and state columns from factors into strings so that they can be merged with other dataframes later
election_df2$county <- as.character(election_df2$county)
election_df2$state <- as.character(election_df2$state)

#subsetting only the necessary columns for the final dataframe.
election_df2 <- election_df2[c(1,2,5,7,10)]

#Merging republican and democratic data to get the two-party voteshare:
election_df <- merge(election_df1, election_df2, by = c('county.fips', 'year', 'county', 'state'), 
                     all = TRUE)

#Creating a variable for republican two-party vote share:
election_df$rep.share <- election_df$vote.percent.rep / (election_df$vote.percent.rep + election_df$vote.percent.dem) 


#Add a lag of rep.voteshare and then remove year 2012: 
election_df <- election_df %>%
  arrange(county.fips, year) %>%
  group_by(county.fips) %>%
  mutate(repshare.lag = lag(rep.share)) %>%
  filter(year > 2012)

#Creating a variable to see whether a county is a democratic county or a republican county
election_df$is.rep.2016 <- ifelse(election_df$rep.share > 0.50, "1", "0")
election_df$is.rep.2012 <- ifelse(election_df$repshare.lag > 0.50, "1", "0")

election_df$is.rep.2016 <- as.numeric(election_df$is.rep.2016)
election_df$is.rep.2012 <- as.numeric(election_df$is.rep.2012)

election_df <- election_df[c(1:4,7:10)]



#********************************************BEA economic datasets************************************

#BEA Regional Income Data: Population by county 
##2016 Data is not available, so we will use 2015 data
beaPop <- list(
  'UserID' = beaKey ,
  'Method' = 'GetData',
  'datasetname' = 'RegionalIncome',
  'TableName' = 'CA1' ,
  'LineCode' = '2',
  'Year' = '2012, 2015' ,
  'GeoFips' = 'COUNTY' ,
  'ResultFormat' = 'json'
)
beaPop <- beaGet(beaPop, asString = T, asTable = F)
Population <- jsonlite::fromJSON(beaPop)$BEAAPI$Results$Data %>%
  mutate(DataValue = ifelse(DataValue == '(NA)', NA, DataValue),
         DataValue = as.numeric(DataValue)) %>%
  rename(Year = TimePeriod, Pop = DataValue) %>%
  mutate(Year = as.numeric(Year))

names(Population)[5]<-"Pop_Label"
names(Population)[7]<-"pop"
Population <- Population[, -c(1,5,6,8)]

#BEA Regional Income Data: Per Capita Income by county
beaPCI <- list(
  'UserID' = beaKey ,
  'Method' = 'GetData',
  'datasetname' = 'RegionalIncome',
  'TableName' = 'CA1' ,
  'LineCode' = '3',
  'Year' = '2012, 2015' ,
  'GeoFips' = 'COUNTY' ,
  'ResultFormat' = 'json'
);

beaPCI <- beaGet(beaPCI, asString = T, asTable = F)
PerCapitaIncome <- jsonlite::fromJSON(beaPCI)$BEAAPI$Results$Data %>%
  mutate(DataValue = ifelse(DataValue == '(NA)', NA, DataValue),
         DataValue = as.numeric(DataValue)) %>%
  rename(Year = TimePeriod, PCI = DataValue) %>%
  mutate(Year = as.numeric(Year))

names(PerCapitaIncome)[5]<-"PCI_Label"
names(PerCapitaIncome)[7]<-"pci"
PerCapitaIncome <- PerCapitaIncome[, -c(1,5,6,8)]

#BEA Regional Income Data: Current Transfer receipts of individuals from Governments
##API error, this data is not loading.
beaPercapita_Current_Transfer <- list(
  'UserID' = beaKey ,
  'Method' = 'GetData',
  'datasetname' = 'RegionalIncome',
  'TableName' = 'CA30' ,
  'LineCode' = '130',
  'Year' = '2012, 2015' ,
  'GeoFips' = 'COUNTY' ,
  'ResultFormat' = 'json'
);

beaPercapita_Current_Transfer <- beaGet(beaPercapita_Current_Transfer, asString = T, asTable = F)
PerCapitaCurrentTransfer <- jsonlite::fromJSON(beaPercapita_Current_Transfer)$BEAAPI$Results$Data %>%
  mutate(DataValue = ifelse(DataValue == '(NA)', NA, DataValue),
         DataValue = as.numeric(DataValue)) %>%
  rename(Year = TimePeriod, PCCT = DataValue) %>%
  mutate(Year = as.numeric(Year))

names(PerCapitaCurrentTransfer)[5]<-"PCCT_Label"
names(PerCapitaCurrentTransfer)[7]<-"pcct"
PerCapitaCurrentTransfer <- PerCapitaCurrentTransfer[, -c(1,5,6,8)]

#BEA Regional Income Data: Adjustment for Residence equals the inflows to that county minus the outflows from that county
beaAdjustment_Residence <- list(
  'UserID' = beaKey ,
  'Method' = 'GetData',
  'datasetname' = 'RegionalIncome',
  'TableName' = 'CA91' ,
  'LineCode' = '30',
  'Year' = '2012, 2015' ,
  'GeoFips' = 'COUNTY' ,
  'ResultFormat' = 'json'
);

beaAdjustment_Residence <- beaGet(beaAdjustment_Residence, asString = T, asTable = F)
AdjustmentResidence <- jsonlite::fromJSON(beaAdjustment_Residence)$BEAAPI$Results$Data %>%
  mutate(DataValue = ifelse(DataValue == '(NA)', NA, DataValue),
         DataValue = as.numeric(DataValue)) %>%
  rename(Year = TimePeriod, Adj_res = DataValue) %>%
  mutate(Year = as.numeric(Year))


names(AdjustmentResidence)[5]<-"Adj_res_Label"
names(AdjustmentResidence)[7]<-"adj_res"
AdjustmentResidence <- AdjustmentResidence[, -c(1,5,6,8)]



#Merging all BEA dataframes
bea_df <- merge(Population, PerCapitaIncome, by = c('GeoFips', 'Year', 'GeoName'), all=T)
bea_df <- merge(bea_df, PerCapitaCurrentTransfer, by = c('GeoFips', 'Year', 'GeoName'), all=T)
bea_df <- merge(bea_df, AdjustmentResidence, by = c('GeoFips', 'Year', 'GeoName'), all=T)

names(bea_df)[names(bea_df)=="GeoFips"] <- "county.fips"
bea_df$county.fips <- as.numeric(bea_df$county.fips)

## Custom function
geo_seperate <- function(text, digit = 1) {
  # This function is to use the text in the column GeoName and seperate them into two parts; the one with the state and the one with counties. Then use dplyr to mutate the columns respectivly
  
  temp.text <- str_split(text, ", ") %>% unlist() %>%
    # Split at the comma. and force the text to take between 1:3 parts
    str_replace_all("[*]", "")
  
  if (length(temp.text) == 3) {
    # Correcting the state and county names if more than one comma in their names
    temp.text <- c(paste(temp.text[1:2], collapse = ", "), temp.text[3])
  }
  
  if (length(temp.text) == 1) {
    return(NA) # For those with only US or state names
  } else {
    return(temp.text[digit])
  }
}

bea_df %<>%
  rowwise() %>%
  mutate(county = geo_seperate(GeoName, 1),
         state = geo_seperate(GeoName, 2)) %>%
  select(county.fips, Year, GeoName, state, county, everything())

# Only need to filter out the NAs from state and county now
bea_df <- bea_df %>% filter(county != "NA")
bea_df$county <- tolower(bea_df$county) #Changed the county names to lowercase

#Rename the Year variable to year.
names(bea_df)[names(bea_df)=="Year"] <- "year"

bea_df <- bea_df %>%
  arrange(county.fips, year) %>%
  group_by(county.fips) %>%
  mutate(pci_lag = lag(pci)) %>%
  mutate(pop_lag = lag(pop)) %>%
  mutate(pcct_lag = lag(pcct)) %>%
  mutate(adj_res_lag = lag(adj_res)) %>%
  filter(year %% 2 == 1)

#Calculate the one year percent change for all the economic variables from BEA
bea_df$pci_gro <- (bea_df$pci - bea_df$pci_lag)/bea_df$pci_lag
bea_df$pcct_gro <- (bea_df$pcct - bea_df$pcct_lag)/bea_df$pcct_lag
bea_df$adj_gro <- (bea_df$adj_res - bea_df$adj_res_lag)/bea_df$adj_res_lag

#subsetting only the necessary columns for the final dataframe.
bea_df2 <- bea_df[c(1:2,4:6,14:16)]

bea_df2 <- bea_df2 %>% 
  mutate(county = str_replace_all(county, " city", "")) %>%
  mutate(county = str_replace_all(county, "doña", "dona"))


#To remove, things inside the parenthesis such as fremont (includes yellowstone park) and baltimore (independent).
bea_df2$county <-  genX(bea_df2$county, " (", ")") #qdap package helped with this.

bea_df2 <- bea_df2 %>% 
  ungroup() %>%
  filter(state != "AK") %>%
  mutate(county = ifelse(county == "maui + kalawao", "maui", county)) %>%
  mutate(county.fips = ifelse(county.fips == 15901, 15009, county.fips))


#TOTO: Just saw that maui needs to have a fips code of 15009 instead of 15901 in order to merge with other datasets. I put the code above, but it didn"t work.

#TOTO: In terms of the counties in Virginia that appear with a plus sign(+), it is difficult to separate them since other variables would have to separated too and we have no means of doing that. So, what I did was that, when I did the merging at the end, I merged not by bea_df but by other dataframes. So it automatically removed the ones that had the plus sign. I guess I can say that I couldn"t incoroporate them in the model because of the complexity of the data and hence I removed them. What do you say?


#********************************************BLS unemployment datasets************************************

#BLS Data on Unemployment by county, clean data, remove unnecessary rows
unemp12 <- read_excel('laucnty12.xlsx')
unemp12 <- separate(unemp12, County, into = c("County.name", "State"), sep=",")
unemp12 <- unemp12[-c(3220:3222), ]

unemp15 <- read_excel('laucnty15.xlsx')
unemp15 <- separate(unemp15, County, into = c("County.name", "State"), sep=",")
unemp15 <- unemp15[-c(3221:3223), ]

#Combine all the dataframes from different years into one dataframe
unemployment_df <- bind_rows(unemp12, unemp15, .id = NULL)
names(unemployment_df)[names(unemployment_df)=="County.name"] <- "county"
names(unemployment_df)[names(unemployment_df)=="State"] <- "state"
names(unemployment_df)[names(unemployment_df)=="County Code"] <- "county.fips"
names(unemployment_df)[names(unemployment_df)=="State FIPS Code"] <- "state.fips"
names(unemployment_df)[names(unemployment_df)=="Year"] <- "year"

unemployment_df$county <- strsplit(unemployment_df$county, " County") #Remove the word County from the county names
unemployment_df$county <- tolower(unemployment_df$county) #Changed the county names to lowercase
unemployment_df <- unite(unemployment_df, state.fips, county.fips, col ="county.fips", sep="") #Combine two columns to make them into one.

#The states that appeared as NA in this dataframe represented DC, hence the NAs will be changed to DC
unemployment_df <- replace_na(unemployment_df, list(state="DC", unemployment_df$state))

#Converting from character to numeric for merging purposes
unemployment_df$county.fips <- as.numeric(unemployment_df$county.fips)
unemployment_df$year <- as.numeric(unemployment_df$year)


# There is an extra space in front or back of the state names. Need to remove them
unemployment_df <- unemployment_df %>% 
  mutate(state = str_replace_all(state, " ", "")) %>%
  filter(state != "AK" & state != "PR")

#Rename the variable "Unemployment" to "unemp"
names(unemployment_df)[names(unemployment_df)=="Unemployment Rate"] <- "unemp"

#Do the lag of unemloyment
unemployment_df <- unemployment_df %>%
  arrange(county.fips, year) %>%
  group_by(county.fips) %>%
  mutate(unemp_lag = lag(unemp)) %>%
  filter(year %% 2 == 1)

#Calculate the one year percent change for unemployment
unemployment_df$unemp_gro <- (unemployment_df$unemp - unemployment_df$unemp_lag)/unemployment_df$unemp_lag

#subsetting only the necessary columns for the final dataframe.
unemployment_df2 <- unemployment_df[c(2:5,11)]

#Renaming certain objects in the county names
unemployment_df2 <- unemployment_df2 %>% 
  mutate(county = str_replace_all(county, " parish", "")) %>%
  mutate(county = str_replace_all(county, " city", "")) %>%
  mutate(county = str_replace_all(county, " town", ""))

#********************************************Merging all datasets************************************

# Finally merging all the 3 dataframes: bea_df2, election_df, and unemployment_df2

beabls_df <- merge(unemployment_df2, bea_df2, by= c('county.fips'), all.x = TRUE) #Merging first two datasets
beablselec <- merge(beabls_df, election_df, by =c('county.fips'), all.x = TRUE)

merged_df1 <- beablselec[c(1:3,5,9:12,16:19)] #Subsetting into the final dataframe with only variables and in proper order.
names(merged_df1)[names(merged_df1)=="county.x"] <- "county"
names(merged_df1)[names(merged_df1)=="state.x"] <- "state"

#Rural dummy:
rural <- read.csv("rural.csv")
rural <- rural[c(1,4,5)]
names(rural)[names(rural)=="ï..county.fips"] <- "county.fips"

p2_merged_df <- merge(merged_df1, rural, by = 'county.fips', all.x = TRUE)

# White dummy
race <- read.csv("race2015.csv")
race <- race[-c(1:3)]
names(race)[names(race)=="FIPS"] <- "county.fips"

p2_merged_df2 <- merge(p2_merged_df, race, by = c('county.fips'), all.x = TRUE)

p2_merged_df2$white.percent <- p2_merged_df2$white / p2_merged_df2$pop

#Removing duplicate counties. There were 6 of them.
issue.data <- p2_merged_df2 %>%
  group_by(county.fips) %>%
  summarise(issue = n())

p2_merged_df2 %<>% merge(issue.data) %>%
  mutate(drop = ifelse(issue > 6 & is.na(pop), 1, 0)) %>%
  filter(drop == 0) %>%
  select(-drop, -issue)


#Creating the dv with change in rep voteshare from 2012 to 2016. And removing unnecessary columns:
p2_merged_df2$rep.share.gro = p2_merged_df2$rep.share - p2_merged_df2$repshare.lag
p2_merged_df3 <- p2_merged_df2[-c(15:17)]

## Load and clean the education data:
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

p2_merged_df4 <- merge(p2_merged_df3, edu.data)

#Creating the new variable for flip, which will be the dependent variable for the logit model.
logit.data <- p2_merged_df4 %>%
  mutate(flip = is.rep.2016 - is.rep.2012) %>%
  filter(flip != -1)

#Create education percentage and rename education variable:
logit.data$educ = logit.data$total / logit.data$pop


#Remove previous dataframes:
rm(election_df1, election_df2)
rm(beaPop, beaPCI, beaPercapita_Current_Transfer, beaAdjustment_Residence)
rm(Population, PerCapitaIncome, PerCapitaCurrentTransfer, AdjustmentResidence, bea_df)
rm(unemp12, unemp15, unemployment_df) 
rm(beabls_df, beablselec, election_df, unemployment_df2, bea_df2)
rm(issue.data, merged_df1, race, rural, p2_merged_df, p2_merged_df2, p2_merged_df3, edu.data)


export(logit.data, "part2data.csv")

