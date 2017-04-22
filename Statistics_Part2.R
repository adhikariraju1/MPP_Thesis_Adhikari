
#********************************************Election dataset************************************

#Loading Republican voting data:
election_df1 <- read.csv("full-us-presidential-counties-2012-2016-rep.csv")
#Renaming column names to merge them with other data later
election_df1 <- election_df1 %>%
  rename(state = state.abb,
         county = county.name,
         year = ï..year,
         vote.percent.rep = vote.percent)

#Converting the county and state columns from factors into strings so that they can be merged with other dataframes later
election_df1$county <- as.character(election_df1$county)
election_df1$state <- as.character(election_df1$state)

#subsetting only the necessary columns for the final dataframe.
election_df1 <- election_df1 %>%
  select(year, county.fips, county, state, vote.percent.rep)

#Loading Democratic voting data:
election_df2 <- read.csv("full-us-presidential-counties-2012-2016-demo.csv")
#Renaming column names to merge them with other data later
election_df2 <- election_df2 %>%
  rename(state = state.abb,
         county = county.name,
         year = ï..year,
         vote.percent.dem = vote.percent)

#Converting the county and state columns from factors into strings so that they can be merged with other dataframes later
election_df2$county <- as.character(election_df2$county)
election_df2$state <- as.character(election_df2$state)

#subsetting only the necessary columns for the final dataframe.
election_df2 <- election_df2 %>%
  select(year, county.fips, county, state, vote.percent.dem)

#Merging republican and democratic data to get the two-party voteshare:
election_df <- merge(election_df1, election_df2, by = c('county.fips', 'year', 'county', 'state'), 
                     all = TRUE)

#Creating a variable for republican two-party vote share:
election_df <- election_df %>%
  mutate(rep.share = vote.percent.rep / (vote.percent.rep + vote.percent.dem))

#Add a lag of rep.voteshare and then remove year 2012: 
election_df <- election_df %>%
  arrange(county.fips, year) %>%
  group_by(county.fips) %>%
  mutate(repshare.lag = lag(rep.share),
         year = as.numeric(year)) %>%
  filter(year != 2012)

#Creating a variable to see whether a county is a democratic county or a republican county
election_df$is.rep.2016 <- ifelse(election_df$rep.share > 0.50, "1", "0")
election_df$is.rep.2012 <- ifelse(election_df$repshare.lag > 0.50, "1", "0")

election_df$is.rep.2016 <- as.numeric(election_df$is.rep.2016)
election_df$is.rep.2012 <- as.numeric(election_df$is.rep.2012)

election_df <- election_df %>%
  select(-vote.percent.rep, -vote.percent.dem)


#********************************************BEA economic datasets************************************

#BEA Regional Income Data: Population by county 
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

Population <- Population %>%
  select(GeoFips, GeoName, Year, Pop)


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

PerCapitaIncome <- PerCapitaIncome %>%
  select(GeoFips, GeoName, Year, PCI)

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

PerCapitaCurrentTransfer <- PerCapitaCurrentTransfer %>%
  select(GeoFips, GeoName, Year, PCCT)

#BEA Regional Income Data: Manufacturing jobs
beaManufacturing <- list(
  'UserID' = beaKey ,
  'Method' = 'GetData',
  'datasetname' = 'RegionalIncome',
  'TableName' = 'CA25N' ,
  'LineCode' = '500',
  'Year' = '2012, 2015' ,
  'GeoFips' = 'COUNTY' ,
  'ResultFormat' = 'json'
);

beaManufacturing <- beaGet(beaManufacturing, asString = T, asTable = F)
Manufacturing <- jsonlite::fromJSON(beaManufacturing)$BEAAPI$Results$Data %>%
  mutate(DataValue = ifelse(DataValue == '(NA)', NA, DataValue),
         DataValue = as.numeric(DataValue)) %>%
  rename(Year = TimePeriod, manu = DataValue) %>%
  mutate(Year = as.numeric(Year))

Manufacturing <- Manufacturing %>%
  select(GeoFips, GeoName, Year, manu)

# Private non-farm employment(we need to divide manufacturing by this)
beaPrivate <- list(
  'UserID' = beaKey ,
  'Method' = 'GetData',
  'datasetname' = 'RegionalIncome',
  'TableName' = 'CA25N' ,
  'LineCode' = '90',
  'Year' = '2012, 2015' ,
  'GeoFips' = 'COUNTY' ,
  'ResultFormat' = 'json'
);

beaPrivate <- beaGet(beaPrivate, asString = T, asTable = F)
Private <- jsonlite::fromJSON(beaPrivate)$BEAAPI$Results$Data %>%
  mutate(DataValue = ifelse(DataValue == '(NA)', NA, DataValue),
         DataValue = as.numeric(DataValue)) %>%
  rename(Year = TimePeriod, private = DataValue) %>%
  mutate(Year = as.numeric(Year))

Private <- Private %>%
  select(GeoFips, GeoName, Year, private)


#BEA Regional Income Data: Employment(Total number of jobs) by county
beaEMP <- list(
  'UserID' = beaKey ,
  'Method' = 'GetData',
  'datasetname' = 'RegionalIncome',
  'TableName' = 'CA30' ,
  'LineCode' = '240',
  'Year' = '2012, 2015' ,
  'GeoFips' = 'COUNTY' ,
  'ResultFormat' = 'json'
);

beaEMP <- beaGet(beaEMP, asString = T, asTable = F)
Jobs <- jsonlite::fromJSON(beaEMP)$BEAAPI$Results$Data %>%
  mutate(DataValue = ifelse(DataValue == '(NA)', NA, DataValue),
         DataValue = as.numeric(DataValue)) %>%
  rename(Year = TimePeriod, jobs = DataValue) %>%
  mutate(Year = as.numeric(Year))
Jobs <- Jobs %>%
  select(GeoFips, GeoName, Year, jobs)

#Average wage by county
beaWage <- list(
  'UserID' = beaKey ,
  'Method' = 'GetData',
  'datasetname' = 'RegionalIncome',
  'TableName' = 'CA30' ,
  'LineCode' = '290',
  'Year' = '2012, 2015' ,
  'GeoFips' = 'COUNTY' ,
  'ResultFormat' = 'json'
);

beaWage <- beaGet(beaWage, asString = T, asTable = F)
Wage <- jsonlite::fromJSON(beaWage)$BEAAPI$Results$Data %>%
  mutate(DataValue = ifelse(DataValue == '(NA)', NA, DataValue),
         DataValue = as.numeric(DataValue)) %>%
  rename(Year = TimePeriod, av_wage = DataValue) %>%
  mutate(Year = as.numeric(Year))
Wage <- Wage %>%
  select(GeoFips, GeoName, Year, av_wage)

#Merging all BEA dataframes
bea_df <- merge(Population, PerCapitaIncome, by = c('GeoFips', 'Year', 'GeoName'), all=T)
bea_df <- merge(bea_df, PerCapitaCurrentTransfer, by = c('GeoFips', 'Year', 'GeoName'), all=T)
bea_df <- merge(bea_df, Jobs, by = c('GeoFips', 'Year', 'GeoName'), all=T)
bea_df <- merge(bea_df, Wage, by = c('GeoFips', 'Year', 'GeoName'), all=T)
bea_df <- merge(bea_df, Manufacturing, by = c('GeoFips', 'Year', 'GeoName'), all=T)
bea_df <- merge(bea_df, Private, by = c('GeoFips', 'Year', 'GeoName'), all=T)

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
bea_df <- bea_df %>% 
  filter(county != "NA")
bea_df$county <- tolower(bea_df$county) #Changed the county names to lowercase

#Rename the Year variable to year.
bea_df <- bea_df %>%
  rename(year = Year, pop = Pop, pci = PCI, pcct = PCCT)

bea_df <- bea_df %>%
  arrange(county.fips, year) %>%
  group_by(county.fips) %>%
  mutate(pci_lag = lag(pci)) %>%
  mutate(pop_lag = lag(pop)) %>%
  mutate(pcct_lag = lag(pcct)) %>%
  mutate(jobs_lag = lag(jobs)) %>%
  mutate(av_wage_lag = lag(av_wage)) %>%
  mutate(manu_share = manu / private) %>%
  mutate(manu_share_lag = lag(manu_share)) %>%
  filter(year %% 2 == 1)

#Calculate the one year percent change for all the economic variables from BEA
bea_df <- bea_df %>%
  mutate(pci_gro = (pci - pci_lag) / pci_lag, 
         pcct_gro = (pcct - pcct_lag) /pcct_lag, 
         jobs_gro = (jobs - jobs_lag) / jobs_lag,
         av_wage_gro = (av_wage - av_wage_lag) / av_wage_lag,
         manu_share_gro = (manu_share - manu_share_lag) / manu_share_lag)

#subsetting only the necessary columns for the final dataframe.
bea_df <- bea_df %>%
  mutate(pop_thou = pop / 1000) %>%
  select(county.fips, year, state, county, pop, pop_thou, pci_gro, pcct_gro, jobs_gro, av_wage_gro, manu_share_gro)
 
bea_df <- bea_df %>% 
  mutate(county = str_replace_all(county, " city", "")) %>%
  mutate(county = str_replace_all(county, "doña", "dona"))


#To remove, things inside the parenthesis such as fremont (includes yellowstone park) and baltimore (independent).
bea_df$county <-  genX(bea_df$county, " (", ")") #qdap package helped with this.

bea_df <- bea_df %>% 
  ungroup() %>%
  filter(state != "AK") %>%
  mutate(county = ifelse(county == "maui + kalawao", "maui", county)) %>%
  mutate(county.fips = ifelse(county.fips == 15901, 15009, county.fips))

#********************************************BLS unemployment datasets************************************

#BLS Data on Unemployment by county, clean data, remove unnecessary rows
unemp12 <- read_excel('laucnty12.xlsx')
unemp12 <- separate(unemp12, County, into = c("County.name", "State"), sep=",")

unemp15 <- read_excel('laucnty15.xlsx')
unemp15 <- separate(unemp15, County, into = c("County.name", "State"), sep=",")

#Combine all the dataframes from different years into one dataframe
unemployment_df <- bind_rows(unemp12, unemp15, .id = NULL)

unemployment_df <- unemployment_df %>%
  rename(county = County.name, state = State, 
         county.fips = `County Code`, 
         state.fips = `State FIPS Code`, 
         year = Year, 
         unemp = `Unemployment Rate`)

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

#Do the lag of unemloyment
unemployment_df <- unemployment_df %>%
  arrange(county.fips, year) %>%
  group_by(county.fips) %>%
  mutate(unemp_lag = lag(unemp)) %>%
  filter(year %% 2 == 1)

#Calculate the one year percent change for unemployment
unemployment_df <- unemployment_df %>%
  mutate(unemp_gro = (unemp - unemp_lag) / unemp_lag) %>%
  select(county.fips, county, state, year, unemp_gro)

#Renaming certain objects in the county names
unemployment_df <- unemployment_df %>% 
  mutate(county = str_replace_all(county, " parish", "")) %>%
  mutate(county = str_replace_all(county, " city", "")) %>%
  mutate(county = str_replace_all(county, " town", ""))

#********************************************Merging all datasets************************************

# Finally merging all the 3 dataframes: bea_df2, election_df, and unemployment_df2

beabls_df <- merge(unemployment_df, bea_df, by= c('county.fips'), all.x = TRUE) #Merging first two datasets
beablselec <- merge(beabls_df, election_df, by =c('county.fips'), all.x = TRUE)

p2_merged_df1 <- beablselec %>%
  select(county.fips, county.x, state.x, unemp_gro, pop, pop_thou, pci_gro, pcct_gro, 
         jobs_gro, av_wage_gro, manu_share_gro, rep.share, repshare.lag, is.rep.2012, is.rep.2016) %>%
  rename(county = county.x, state = state.x)
  
#Rural dummy:
rural <- read.csv("rural.csv")
rural <- rural %>%
  rename(county.fips = ï..county.fips) %>%
  select(county.fips, rural_percent, rural)

p2_merged_df2 <- merge(p2_merged_df1, rural, by = 'county.fips', all.x = TRUE)

# White dummy 18 years and above white
race <- read.csv("race2015.csv")
race <- race %>%
  rename(county.fips = FIPS) %>%
  select(county.fips, white)

p2_merged_df3 <- merge(p2_merged_df2, race, by = c('county.fips'), all.x = TRUE)

p2_merged_df3 <- p2_merged_df3 %>%
  mutate(white.percent = white / pop)

#Removing duplicate counties. There were 6 of them. #TOTO: this is not working since it is showing a total of 3118 counties instead of 3112.
issue.data <- p2_merged_df3 %>%
  group_by(county.fips) %>%
  summarise(issue = n())

p2_merged_df3 %<>% merge(issue.data) %>%
  mutate(drop = ifelse(issue > 1 & is.na(pop), 1, 0)) %>%
  filter(drop == 0) %>%
  select(-drop, -issue)

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

p2_merged_df4 <- p2_merged_df4 %>%
  mutate(uneduc = total / pop)

#Creating the new variable for flip.
p2_merged_df5 <- p2_merged_df4 %>%
  mutate(flip = is.rep.2016 - is.rep.2012)
#0 means didn't flip, 1 means flipped to republican and -1 means flipped to democratic.

p2_merged_df5 <- p2_merged_df5 %>%
  mutate(rep.share.gro = (rep.share - repshare.lag)/repshare.lag)



###############################################################Some playing around with employment data#########################
emp.data <- rio::import("ACS_15_5YR_S2301_with_ann.csv", skip = 1) %>%
  select(-matches("Margin")) %>%
  select(-matches("Total")) %>%
  select(-matches("Unemployment")) %>%
  select(-matches("Female")) %>%
  select(-matches("AGE")) %>%
  select(-matches("DISABILITY")) %>%
  select(-matches("Employment")) %>%
  select(-matches("RACE")) %>%
  select(-matches("EDUCATIONAL")) %>%
  select(-matches("POVERTY")) %>%
  select(-Id, -Geography)

names(emp.data)[names(emp.data)=="Labor Force Participation Rate; Estimate; White alone, not Hispanic or Latino"] <- "lfpr_white_2015"
names(emp.data)[names(emp.data)=="Labor Force Participation Rate; Estimate; Population 20 to 64 years - SEX - Male"] <- "lfpr_male_2015"
names(emp.data)[names(emp.data)=="Id2"] <- "county.fips"

emp.data <- emp.data %>%
  select(county.fips, lfpr_male_2015, lfpr_white_2015) 

emp.data2012 <- rio::import("ACS_12_5YR_S2301_with_ann.csv", skip = 1) %>%
  select(-matches("Margin")) %>%
  select(-matches("Total")) %>%
  select(-matches("Unemployment")) %>%
  select(-matches("Female")) %>%
  select(-matches("AGE")) %>%
  select(-matches("DISABILITY")) %>%
  select(-matches("Employment")) %>%
  select(-matches("RACE")) %>%
  select(-matches("EDUCATIONAL")) %>%
  select(-matches("POVERTY")) %>%
  select(-matches("Employed")) %>%
  select(-Id, -Geography)

names(emp.data2012)[names(emp.data2012)=="In labor force; Estimate; White alone, not Hispanic or Latino"] <- "lfpr_white_2012"
names(emp.data2012)[names(emp.data2012)=="In labor force; Estimate; SEX - Male"] <- "lfpr_male_2012"
names(emp.data2012)[names(emp.data2012)=="Id2"] <- "county.fips"

emp.data2012 <- emp.data2012 %>%
  select(county.fips, lfpr_male_2012, lfpr_white_2012) 

emp.df <- merge(emp.data, emp.data2012)

p2_merged_df6 <- merge(p2_merged_df5, emp.df, all.x = TRUE)

p2_merged_df6 <- p2_merged_df6 %>%
  mutate(lfpr_male_gro = lfpr_male_2015 - lfpr_male_2012) %>%
  mutate(lfpr_white_gro = lfpr_white_2015 - lfpr_white_2012)

########################################################################################333
#Gini Coefficient:
gini.data2015 <- rio::import("ACS_15_5YR_B19083_with_ann.csv", skip = 1)
names(gini.data2015)[names(gini.data2015)=="Estimate; Gini Index"] <- "gini_2015"
names(gini.data2015)[names(gini.data2015)=="Id2"] <- "county.fips"

gini.data2015 <- gini.data2015 %>%
  select(county.fips, gini_2015)


gini.data2012 <- rio::import("ACS_12_5YR_B19083_with_ann.csv", skip = 1)
names(gini.data2012)[names(gini.data2012)=="Estimate; Gini Index"] <- "gini_2012"
names(gini.data2012)[names(gini.data2012)=="Id2"] <- "county.fips"

gini.data2012 <- gini.data2012 %>%
  select(county.fips, gini_2012)

gini_df <- merge(gini.data2012, gini.data2015)

gini_df <- gini_df %>%
  mutate(gini_gro = gini_2015 - gini_2012)

####################################################

p2_merged_df7 <- merge(p2_merged_df6, gini_df, all.x = TRUE) 




#Remove previous dataframes:
rm(election_df1, election_df2)
rm(beaPop, beaPCI, beaPercapita_Current_Transfer, beaManufacturing, beaPrivate, beaEMP, beaWage)
rm(Population, PerCapitaIncome, PerCapitaCurrentTransfer, bea_df, Jobs, Wage, Manufacturing, Private)
rm(unemp12, unemp15, unemployment_df) 
rm(beabls_df, beablselec, election_df)
rm(issue.data, race, rural, p2_merged_df1, p2_merged_df2, p2_merged_df3, edu.data, p2_merged_df4, emp.data, emp.data2012, emp.df, gini_df, gini.data2012, gini.data2015, intercept, p2_merged_df6)


############################################################3333333
export(p2_merged_df7, "part2data.csv")

