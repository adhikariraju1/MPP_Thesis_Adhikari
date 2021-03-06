

#********************************************Election dataset************************************

#Loading Republican voting data:
election_df1 <- read.csv("full-us-presidential-counties-1988-2012-rep.csv")
#Renaming column names to merge them with other data later
names(election_df1)[names(election_df1)=="state.abb"] <- "state"
names(election_df1)[names(election_df1)=="county.name"] <- "county"
names(election_df1)[names(election_df1)=="�..year"] <- "year"
names(election_df1)[names(election_df1)=="vote.percent"] <- "vote.percent.rep"


#Converting the county and state columns from factors into strings so that they can be merged with other dataframes later
election_df1$county <- as.character(election_df1$county)
election_df1$state <- as.character(election_df1$state)

#subsetting only the necessary columns for the final dataframe.
election_df1 <- election_df1[c(1,2,5,7,10)]


#Loading Democratic voting data:
election_df2 <- read.csv("full-us-presidential-counties-1988-2012-demo.csv")
#Renaming column names to merge them with other data later
names(election_df2)[names(election_df2)=="state.abb"] <- "state"
names(election_df2)[names(election_df2)=="county.name"] <- "county"
names(election_df2)[names(election_df2)=="�..year"] <- "year"
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

#Creating a variable to see whether a county is a democratic county or a republican county
election_df$is.rep <- ifelse(election_df$rep.share > 0.50, "1", "0")
election_df$is.rep <- as.numeric(election_df$is.rep)

election_df <- election_df[c(1:4, 7:8)]

#Add a lag of rep.voteshare and then remove year 1988: 
election_df <- election_df %>%
  arrange(county.fips, year) %>%
  group_by(county.fips) %>%
  mutate(repshare.lag = lag(rep.share)) %>%
  filter(year >= 1992)

#********************************************BEA economic datasets************************************

#BEA Regional Income Data: Population by county 
##2016 Data is not available
beaPop <- list(
  'UserID' = beaKey ,
  'Method' = 'GetData',
  'datasetname' = 'RegionalIncome',
  'TableName' = 'CA1' ,
  'LineCode' = '2',
  'Year' = '1991, 1992, 1995, 1996, 1999, 2000, 2003, 2004, 2007, 2008, 2011, 2012' ,
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
Population <- Population[, -c(1,5,6,8)]

#BEA Regional Income Data: Per Capita Income by county
beaPCI <- list(
  'UserID' = beaKey ,
  'Method' = 'GetData',
  'datasetname' = 'RegionalIncome',
  'TableName' = 'CA1' ,
  'LineCode' = '3',
  'Year' = '1991, 1992, 1995, 1996, 1999, 2000, 2003, 2004, 2007, 2008, 2011, 2012' ,
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
PerCapitaIncome <- PerCapitaIncome[, -c(1,5,6,8)]

#The data is available until 2015 only. Let's see what can be done to match the election data. 


#BEA Regional Income Data: Current Transfer receipts of individuals from Governments
##API error, this data is not loading.
beaPercapita_Current_Transfer <- list(
  'UserID' = beaKey ,
  'Method' = 'GetData',
  'datasetname' = 'RegionalIncome',
  'TableName' = 'CA30' ,
  'LineCode' = '130',
  'Year' = '1991, 1992, 1995, 1996, 1999, 2000, 2003, 2004, 2007, 2008, 2011, 2012' ,
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
PerCapitaCurrentTransfer <- PerCapitaCurrentTransfer[, -c(1,5,6,8)]

#BEA Regional Income Data: Adjustment for Residence equals the inflows to that county minus the outflows from that county
beaAdjustment_Residence <- list(
  'UserID' = beaKey ,
  'Method' = 'GetData',
  'datasetname' = 'RegionalIncome',
  'TableName' = 'CA91' ,
  'LineCode' = '30',
  'Year' = '1991, 1992, 1995, 1996, 1999, 2000, 2003, 2004, 2007, 2008, 2011, 2012' ,
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
  mutate(PCI_lag = lag(PCI)) %>%
  mutate(Pop_lag = lag(Pop)) %>%
  mutate(PCCT_lag = lag(PCCT)) %>%
  mutate(Adj_res_lag = lag(Adj_res)) %>%
  filter(year %% 2 == 0)

#Calculate the one year percent change for all the economic variables from BEA
bea_df$PCI_gro <- (bea_df$PCI - bea_df$PCI_lag)/bea_df$PCI_lag
bea_df$PCCT_gro <- (bea_df$PCCT - bea_df$PCCT_lag)/bea_df$PCCT_lag
bea_df$Adj_gro <- (bea_df$Adj_res - bea_df$Adj_res_lag)/bea_df$Adj_res_lag

#subsetting only the necessary columns for the final dataframe.
bea_df <- bea_df[c(1,2,4:6,14:16)]

bea_df <- bea_df %>% 
  mutate(county = str_replace_all(county, " city", "")) %>%
  mutate(county = str_replace_all(county, "do�a", "dona"))


#To remove, things inside the parenthesis such as fremont (includes yellowstone park) and baltimore (independent).
bea_df$county <-  genX(bea_df$county, " (", ")") #qdap package helped with lllthis.

bea_df <- bea_df %>% 
  ungroup() %>%
  filter(state != "AK") %>%
  mutate(county = ifelse(county == "maui + kalawao", "maui", county)) %>%
  mutate(county.fips = ifelse(county.fips == 15901, 15009, county.fips))

#TOTO: Just saw that maui needs to have a fips code of 15009 instead of 15901 in order to merge with other datasets. I put the code above, but it didn"t work.

#TOTO: In terms of the counties in Virginia that appear with a plus sign(+), it is difficult to separate them since other variables would have to separated too and we have no means of doing that. So, what I did was that, when I did the merging at the end, I merged not by bea_df but by other dataframes. So it automatically removed the ones that had the plus sign. I guess I can say that I couldn"t incoroporate them in the model because of the complexity of the data and hence I removed them. What do you say?


#********************************************BLS unemployment datasets************************************

#BLS Data on Unemployment by county, clean data, remove unnecessary rows
unemp91 <- read_excel('laucnty91.xlsx')
unemp91 <- separate(unemp91, County, into = c("County.name", "State"), sep=",")
unemp91 <- unemp91[-c(3218:3220), ]

unemp92 <- read_excel('laucnty92.xlsx')
unemp92 <- separate(unemp92, County, into = c("County.name", "State"), sep=",")
unemp92 <- unemp92[-c(3217:3219), ]

unemp95 <- read_excel('laucnty95.xlsx')
unemp95 <- separate(unemp95, County, into = c("County.name", "State"), sep=",")
unemp95 <- unemp95[-c(3219:3221), ]

unemp96 <- read_excel('laucnty96.xlsx')
unemp96 <- separate(unemp96, County, into = c("County.name", "State"), sep=",")
unemp96 <- unemp96[-c(3218:3220), ]

unemp99 <- read_excel('laucnty99.xlsx')
unemp99 <- separate(unemp99, County, into = c("County.name", "State"), sep=",")
unemp99 <- unemp99[-c(3219:3221), ]

unemp00 <- read_excel('laucnty00.xlsx')
unemp00 <- separate(unemp00, County, into = c("County.name", "State"), sep=",")
unemp00 <- unemp00[-c(3218:3220), ]

unemp03 <- read_excel('laucnty03.xlsx')
unemp03 <- separate(unemp03, County, into = c("County.name", "State"), sep=",")
unemp03 <- unemp03[-c(3219:3221), ]

unemp04 <- read_excel('laucnty04.xlsx')
unemp04 <- separate(unemp04, County, into = c("County.name", "State"), sep=",")
unemp04 <- unemp04[-c(3218:3220), ]

unemp07 <- read_excel('laucnty07.xlsx')
unemp07 <- separate(unemp07, County, into = c("County.name", "State"), sep=",")
unemp07 <- unemp07[-c(3219:3221), ]

unemp08 <- read_excel('laucnty08.xlsx')
unemp08 <- separate(unemp08, County, into = c("County.name", "State"), sep=",")
unemp08 <- unemp08[-c(3218:3220), ]

unemp11 <- read_excel('laucnty11.xlsx')
unemp11 <- separate(unemp11, County, into = c("County.name", "State"), sep=",")
unemp11 <- unemp11[-c(3221:3223), ]

unemp12 <- read_excel('laucnty12.xlsx')
unemp12 <- separate(unemp12, County, into = c("County.name", "State"), sep=",")
unemp12 <- unemp12[-c(3220:3222), ]

#Combine all the dataframes from different years into one dataframe
unemployment_df <- bind_rows(unemp91, unemp92, unemp95, unemp96, unemp99, unemp00, unemp03, unemp04, unemp07,  unemp08, unemp11, unemp12, .id = NULL)
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
  filter(year %% 2 == 0)

#Calculate the one year percent change for unemployment
unemployment_df$unemp_gro <- (unemployment_df$unemp - unemployment_df$unemp_lag)/unemployment_df$unemp_lag

#subsetting only the necessary columns for the final dataframe.
unemployment_df <- unemployment_df[c(2:5,11)]

#Renaming certain objects in the county names
unemployment_df <- unemployment_df %>% 
  mutate(county = str_replace_all(county, " parish", "")) %>%
  mutate(county = str_replace_all(county, " city", "")) %>%
  mutate(county = str_replace_all(county, " town", ""))

#********************************************Merging all datasets************************************

# Finally merging all the 3 dataframes: bea_df, election_df, and unemployment_df

beabls_df <- merge(unemployment_df, bea_df, by= c('county.fips', 'year'), all.x = TRUE) #Merging first two datasets
beablselec <- merge(beabls_df, election_df, by =c('county.fips', 'year'), all.x = TRUE)
#filtering to remove AK from the dataset. After doing this, all the states matched between the two datasets. We check this using the unique function.
merged_df1 <- beablselec[c(1,2,12,13,8,14,15,16,5,9,10,11)] #Subsetting into the final dataframe with only variables and in proper order.

#Incumbency dummy:
incumbency <- read.csv("incumbency.csv") 
names(incumbency)[names(incumbency)=="�..year"] <- "year"

merged_df2 <- merge(merged_df1, incumbency, by = 'year', all.x = TRUE)  


#Rural dummy:
rural <- read.csv("rural.csv")
rural <- rural[c(1,4,5)]
names(rural)[names(rural)=="�..county.fips"] <- "county.fips"

merged_df3 <- merge(merged_df2, rural, by = 'county.fips', all.x = TRUE)


# White dummy
race <- read.csv("race1992_2012.csv")
race <- race[-c(1:2)]
names(race)[names(race)=="FIPS"] <- "county.fips"

merged_df4 <- merge(merged_df3, race, by = c('county.fips', 'year'), all.x = TRUE)

merged_df4$white.percent <- merged_df4$white / merged_df4$Pop

#Removing duplicate counties. There were 6 of them.
issue.data <- merged_df4 %>%
  group_by(county.fips) %>%
  summarise(issue = n())

merged_df4 %<>% merge(issue.data) %>%
  mutate(drop = ifelse(issue > 6 & is.na(Pop), 1, 0)) %>%
  filter(drop == 0) %>%
  select(-drop, -issue)

#Remove untidy dataframes from the environment
rm(election_df1, election_df2)
rm(beaPop, beaPCI, beaPercapita_Current_Transfer, beaAdjustment_Residence)
rm(Population, PerCapitaIncome, PerCapitaCurrentTransfer, AdjustmentResidence)
rm(unemp91, unemp92, unemp95, unemp96, unemp99, unemp00, unemp03, unemp04, unemp07,  unemp08, unemp11, unemp12) 
rm(bea_df, beabls_df, beablselec, election_df, incumbency, merged_df1, merged_df2, merged_df3, rural, unemployment_df)
rm(race, issue.data)


#Make Pop to Pop_thou
merged_df4 <- merged_df4 %>%
  mutate(Pop_thou = Pop / 1000) %>%
  mutate(log.Pop = log(Pop)) %>%
  mutate(log.Pop_thou = log(Pop_thou)) %>%
  mutate(log.unemp_gro = log(unemp_gro)) %>%
  mutate(log.white.percent = log(white.percent)) %>%
  mutate(log.rural_percent = log(rural_percent))

export(merged_df4, "part1data.csv")


#Correlation matrix for all the variables: independent and dependent
#cor(merged_df1[,c(6:10)], use="complete.obs", method="pearson")
