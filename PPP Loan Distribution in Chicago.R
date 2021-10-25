#PPP Loan Data
#Load in the packages
library(readr)
library(data.table)
library(ggplot2)
library(tidyr)
library(dplyr)
library(zoo)
library(lubridate)
library(scales)
library(sf)
library(gridExtra)
library(stringr)

#Clear the Environment
rm(list = ls())

#Read in the PPP Loan Data: first data set: all loans >$150K. 
df<- read.csv("https://data.sba.gov/dataset/8aa276e2-6cab-4f86-aca4-a7dde42adf24/resource/be89502c-1961-4a79-a44c-979eb3f411a8/download/public_150k_plus_210630.csv")

#Read in the other PPP Loan Data: all loans <$150K
df1<-read.csv("https://data.sba.gov/dataset/8aa276e2-6cab-4f86-aca4-a7dde42adf24/resource/e796a768-4785-46c1-98a7-8bd3a1ef0ab9/download/public_up_to_150k_1_210630.csv")

df2<-read.csv("https://data.sba.gov/dataset/8aa276e2-6cab-4f86-aca4-a7dde42adf24/resource/962261d5-bd48-469f-8977-4f516e686e45/download/public_up_to_150k_2_210630.csv")

df3<-read.csv("https://data.sba.gov/dataset/8aa276e2-6cab-4f86-aca4-a7dde42adf24/resource/3335ed9e-6097-46f5-b928-d3ee1ce32520/download/public_up_to_150k_3_210630.csv")

df4<-read.csv("https://data.sba.gov/dataset/8aa276e2-6cab-4f86-aca4-a7dde42adf24/resource/065378d0-73c5-4730-b1b2-b421c5c44c73/download/public_up_to_150k_4_210630.csv")

df5<-read.csv("https://data.sba.gov/dataset/8aa276e2-6cab-4f86-aca4-a7dde42adf24/resource/02417f74-713e-4b9b-b831-e3587ad7a2a7/download/public_up_to_150k_5_210630.csv")

df6<-read.csv("https://data.sba.gov/dataset/8aa276e2-6cab-4f86-aca4-a7dde42adf24/resource/d54eff57-3417-429f-a161-55b497fd2777/download/public_up_to_150k_6_210630.csv")

df7<-read.csv("https://data.sba.gov/dataset/8aa276e2-6cab-4f86-aca4-a7dde42adf24/resource/94d910a4-54fc-4c54-94b2-ed0bce001366/download/public_up_to_150k_7_210630.csv")

df8<-read.csv("https://data.sba.gov/dataset/8aa276e2-6cab-4f86-aca4-a7dde42adf24/resource/fc07af7a-6aee-413e-92f9-2abd1145ff25/download/public_up_to_150k_8_210630.csv")

df9<-read.csv("https://data.sba.gov/dataset/8aa276e2-6cab-4f86-aca4-a7dde42adf24/resource/64be68f5-4d4a-409d-91e9-3fb3fc1e603d/download/public_up_to_150k_9_210630.csv")

df10<-read.csv("https://data.sba.gov/dataset/8aa276e2-6cab-4f86-aca4-a7dde42adf24/resource/2efb5391-e77f-467b-9b1c-ea4e9690e94f/download/public_up_to_150k_10_210630.csv")

df11<-read.csv("https://data.sba.gov/dataset/8aa276e2-6cab-4f86-aca4-a7dde42adf24/resource/1935d2f9-f1c8-4b1b-99a7-f8cf9cd399bc/download/public_up_to_150k_11_210630.csv")

df12<-read.csv("https://data.sba.gov/dataset/8aa276e2-6cab-4f86-aca4-a7dde42adf24/resource/a5a4affd-de13-44ff-b0ec-2b2b30902ae3/download/public_up_to_150k_12_210630.csv")

#Combine the data sets 
dftotal<- dplyr::bind_rows(df, df1, df2, df3, df4, df5, df6, df7, df8, df9, df10, df11, df12)

#Convert to date format
dftotal$DateApproved <- as.Date(dftotal$DateApproved , format = "%m/%d/%Y")

#Convert City Names to Uniform Standard
dftotal<-dftotal%>%
  mutate(BorrowerCity = toupper(BorrowerCity))

#Filter only loans made to borrowers in Chicago
dffinal<-dftotal%>%
  filter(BorrowerCity == "CHICAGO")%>%
  filter(BorrowerState == "IL")

#Clean Zip Codes, remove extra -four digits, e.g. if a zip code says 60601-0004, this removes the -0004.  
dffinal <- dffinal%>%
  mutate(zip = gsub("-.*","",BorrowerZip))

#Convert loan numbers to numeric to sum them up
dffinal$CurrentApprovalAmount <- as.numeric(as.character(dffinal$CurrentApprovalAmount)) #Convert to Numeric from Character to Add 

#Sum total loan distribution by zip code
dffinal<-dffinal %>% 
  group_by(zip) %>% 
  summarise(Total = sum(CurrentApprovalAmount))%>% 
  select(zip, Total)

#Read in Chicago's Zip Code Boundaries
zipcodes<- read_sf("~/Downloads/Boundaries - ZIP Codes.geojson")

#Merge the two data sets together
map <- merge(zipcodes, dffinal, all.x = TRUE) 

#Read in Zip Code Race/Ethnicity Data (from NHGIS)
race<- read.csv("~/Downloads/nhgis0042_ds244_20195_2019_zcta.csv")

#Convert Populations to Percentages, e.g. percentB= Percent of Zip Code that is Black. 
race<-race%>%
  mutate(percentB = (ALUKE004/ALUKE001)*100)%>%
  mutate(percentW = (ALUKE003/ALUKE001)*100)%>%
  mutate(percentL = (ALUKE012/ALUKE001)*100)

#Change Column Names and Remove Unnecessary Text from the Rows
race <- race%>%
  mutate(NAME_E = str_remove_all(NAME_E, "ZCTA5 ")) #remove excess text from rows to later match to the geojson file

names(race)[names(race) == "NAME_E"] <- "zip" #rename the column

#Remove unnecessary columns
race <- race%>%
  select(zip, percentB, percentW, percentL, ALUKE001)

#Merge the data sets together
map1 <- merge(zipcodes, race, all.x = TRUE) #matches only  that are in both datasets

#Remove NAs
map1 <- map1[!is.na(map1$percentL), ] #remove blank and empty rows

#Find highest percentage across all three columns (percentB, percentW, percentL)
setDT(map1) 
map1[, majority := colnames(.SD)[max.col(.SD, ties.method="first")], .SDcols = c("percentB", "percentW", "percentL")]

#Rename "percentW" to White, "percentB" to Black, and "percentL" to Hispanic or Latino
map1$majority[map1$majority == "percentW"] <- "White" 
map1$majority[map1$majority == "percentB"] <- "Black" 
map1$majority[map1$majority == "percentL"] <- "Hispanic or Latino"

#Rearrange the Legend Order
map1$majority <- factor(map1$majority , levels = c("White", "Black", "Hispanic or Latino"))

#Convert back to Data.Frame
setDF(map1)

#Remove Unnecessary Columns
map1<-map1%>%
  select(zip, majority, ALUKE001, percentW, percentB, percentL)

#Read in the Zip Code Median Household Income Data (NHGIS)
dfincome<- read.csv("~/Downloads/nhgis0043_ds244_20195_2019_zcta.csv")

#Change Column Names and Remove Unnecessary Text from the Rows
dfincome <- dfincome%>%
  mutate(NAME_E = str_remove_all(NAME_E, "ZCTA5 ")) #remove excess text from rows to match geojson file

names(dfincome)[names(dfincome) == "NAME_E"] <- "zip" #rename the column

#Remove unnecessary columns
dfincome <- dfincome%>%
  select(zip, ALW1E001)

#Merge the data sets together
map2 <- merge(zipcodes, dfincome, all.x = TRUE) #matches only zip codes that are in both data sets
map2 <- na.omit(map2)

#Calculate Median Household Income Quintiles
range(map2$ALW1E001) #see the range $22,158 - $191,528. 
quantile(map2$ALW1E001)
quantile(map2$ALW1E001, probs = c(0.05, 0.95)) #what 5% of the numbers are less than and what 95% of the numbers are less than
#5% less than $26,891.2 and 95% less than $123,395.3. 
quantile(map2$ALW1E001, probs = seq(0, 1, 1/5))

#0%      20%      40%      60%      80%     100% 
#  22158.0  38634.6  51912.4  69295.2 101474.0 191528.0 

#create new categorical variables for discrete scale
map2 <- map2 %>% 
  mutate(med_income = case_when(
    ALW1E001 <= 38634.6 ~ "Lowest Quintile: <$38.6K",
    38634.6 < ALW1E001 & ALW1E001 <= 51912.4 ~ "Second Quintile: $38.6K - $51.9K",
    51912.4 < ALW1E001 & ALW1E001 <= 69295.2 ~ "Middle Quintile: $51.9K - $69.2K",
    69295.2 < ALW1E001 & ALW1E001 <= 101474.0 ~ "Fourth Quintile: $69.2K - $101.5K",
    101474.0  < ALW1E001  ~ "Top Quintile: >$101.5K"))

#Rearrange the Legend Order
map2$med_income <- factor(map2$med_income , levels = c("Lowest Quintile: <$38.6K", "Second Quintile: $38.6K - $51.9K", "Middle Quintile: $51.9K - $69.2K", "Fourth Quintile: $69.2K - $101.5K", "Top Quintile: >$101.5K"))

#Finally, bring map and map1 together
finalmap <- left_join(map, map1, by = "zip")
finalmap <- na.omit(finalmap)

#Remove Duplicated Rows
finalmap<-distinct(finalmap)

#Plot the Data
ggp11<- ggplot() +
  geom_sf(data = finalmap, aes(fill = majority)) +
  scale_fill_manual(labels = c("White", "Black", "Hispanic or Latino"), values = c("dodgerblue4", "lightsteelblue2", "dodgerblue2"))+
  coord_sf(datum = NA) + #removes Lat and Long Labels from the Figure
  theme(rect = element_blank()) +#removes gray background
  ggtitle("Racial and Ethnic Demographics of Zip Codes in Chicago")+
  labs(subtitle = "Figure 1-1: Majority Race/Ethnic Group by Zip Code", caption = "Source: City of Chicago;\nU.S. Census  2019 American Community Survey: 5-Year Data;\nIPUMS NHGIS, University of Minnesota, www.nhgis.org;\nAuthor's Calculations.\n\nNote: Since ethnicity is distinct from race, a zip code can be both\nmajority-Hispanic as well as majority-White or majority-Black. For the purposes\nof this map, if a zip code's Hispanic or Latino share of the population is\nlarger than Black or White population share, then it is shaded as\nmajority-Hispanic or Latino.\n\nBlake Rubey.")+  
  theme(legend.position = "right")+
  labs(fill = "")+
  theme_bw()

#Plot the Data
ggp12<- ggplot() +
  geom_sf(data = map2, aes(fill = med_income)) +
  scale_fill_manual(labels = c("Lowest Quintile: <$38.6K", "Second Quintile: $38.6K - $51.9K", "Middle Quintile: $51.9K - $69.2K", "Fourth Quintile: $69.2K - $101.5K", "Top Quintile: >$101.5K"), values = c("lightsteelblue2", "lightskyblue1", "deepskyblue2", "dodgerblue2", "dodgerblue4"))+
  coord_sf(datum = NA) + #removes Lat and Long Labels from the Figure
  theme(rect = element_blank()) +#removes gray background
  labs(subtitle = "Figure 1-2: Median Household Income in the Past 12 Months", caption = "Source: City of Chicago;\nU.S. Census  2019 American Community Survey: 5-Year Data;\nIPUMS NHGIS, University of Minnesota, www.nhgis.org;\nAuthor's Calculations.\n\nBlake Rubey") +
  ggtitle("Income Inequities Across Chicago's Zip Codes")+
  theme(legend.position = "right")+
  labs(fill = "")+
  theme_bw()

grid.arrange(ggp11, ggp12, ncol = 2) 

#Plot PPP Loan Map
ggp21<- ggplot() +
  geom_sf(data = finalmap, aes(fill = Total)) +
  scale_fill_continuous(low = "white", high = "dodgerblue4", labels = comma)+
  coord_sf(datum = NA) + #removes Lat and Long Labels from the Figure
  theme(rect = element_blank()) +#removes gray background
  ggtitle("Majority-White Zip Codes Recieved Majority of PPP Funds") +
  labs(subtitle = "Figure 2-1: Total PPP Dollar Distribution by Zip Code", caption = "Source: City of Chicago;\nU.S. Small Business Administration;\nAuthor's Calculations.\n\nBlake Rubey.")+  
  theme(legend.position = "right")+
  labs(fill = "U.S. Dollars")+
  theme_bw()

#Calculate total amount of money distributed to majority white, Black, and Hispanic or Latino zip codes
totalmoney<- finalmap 

#Remove Unnecessary Columns
setDT(totalmoney)

totalmoney<-totalmoney%>%
  select(zip, Total, majority, ALUKE001, percentW, percentB, percentL)

totalmoney<-totalmoney%>%
  group_by(majority)%>%
  summarise(Total2 = sum(Total), Population = sum(ALUKE001))%>%
  mutate(Percent = (Total2/sum(Total2)*100))%>% 
  mutate(PopPercent = (Population/sum(Population)*100))%>% 
  select(majority, Total2, Percent, Population, PopPercent)

#Check to make sure population number = 2.8 Million
sum(totalmoney$Population) 
#== 2,856, 218

#Filter only necessary columns
totalmoney<-totalmoney%>%
  select(majority, Percent, PopPercent)

#Prepare to Plot
totalmoney <- totalmoney %>%
  gather("Stat", "Value", -majority)

#Plot the Data
ggp22<- totalmoney %>% 
  ggplot(aes(x = reorder(majority, -Value), y = Value, fill = Stat)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.40)+
  scale_fill_manual(labels = c("Percent of\nPPP Dollars", "Percent of\nPopulation"), values = c("dodgerblue4", "dodgerblue2"))+
  geom_text(aes(label = round(Value,2)), position = position_dodge(width = .40), vjust = 0, hjust = .5)+
  ylab("Percent") + xlab("") +
  ggtitle("Majority-White Zip Codes Recieved More than 60%\nof all PPP Dollars Distributed in Chicago") +
  labs(subtitle = "Figure 2-2: Percent of All PPP Dollars Distributed by Majority\nRace/Ethnicity of Zip Code Relative to Population Size of Zip Code", caption = "Source: U.S. Small Business Administration;\nU.S. Census  2019 American Community Survey: 5-Year Data;\nIPUMS NHGIS, University of Minnesota, www.nhgis.org;\nAuthor's Calculations.\n\nBlake Rubey.") +
  labs(fill="") +
  theme_bw()

#Plot the Data Together
grid.arrange(ggp21, ggp22, ncol = 2)  

#Now, Analyze the Percent of Dollars Sent to Zip Codes in the Highest Income Quintiles relative to Population Size
#Need to merge map, map1, and map2 together
setDT(map2)
map2<-map2%>%
  select(zip, ALW1E001, med_income)
setDF(map2)

finalmap <- merge(finalmap, map2, all.x = TRUE) 

#Remove duplicated rows
finalmap<-distinct(finalmap)

#Sum Money Relative to Population Size
totalmoney2<-finalmap

totalmoney2<-totalmoney2%>%
  group_by(med_income)%>%
  summarise(Total = sum(Total), population = sum(ALW1E001))%>% 
  mutate(Percent = (Total/sum(Total)*100))%>% 
  mutate(PopPercent = (population/sum(population)*100))%>%
  select(med_income, Total, Percent, PopPercent)

#Remove Geometries 
setDT(totalmoney2)
totalmoney2<-totalmoney2%>%
  select(med_income, Total, Percent, PopPercent)

#Filter only necessary columns
totalmoney2<-totalmoney2%>%
  select(med_income, Percent, PopPercent)

totalmoney2 <- totalmoney2 %>%
  gather("Stat", "Value", -med_income)

#Plot the Data
totalmoney2 %>% 
  ggplot(aes(x = med_income, y = Value, fill = Stat)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.40)+
  scale_fill_manual(labels = c("Percent of\nPPP Dollars", "Percent of\nPopulation"), values = c("dodgerblue4", "dodgerblue2"))+
  geom_text(aes(label = round(Value,2)), position = position_dodge(width = .40), vjust = 0, hjust = .5)+
  ylab("Percent") + xlab("") +
  scale_x_discrete(labels=c("Lowest Quintile:\n<$38.6K", "Second Quintile:\n$38.6K - $51.9K", "Middle Quintile:\n$51.9K - $69.2K", "Fourth Quintile:\n$69.2K - $101.5K", "Top Quintile:\n>$101.5K")) +
  ggtitle("The Middle and Fourth Quintiles Account for More\nthan Half of the Population and Only 34% of PPP Dollars") +
  labs(subtitle = "Figure 3: Percent of All PPP Dollars Distributed\nby Zip Code Median Household Income Quintile Relative to Zip Code Population", caption = "Source: U.S. Small Business Administration;\nU.S. Census  2019 American Community Survey: 5-Year Data;\nIPUMS NHGIS, University of Minnesota, www.nhgis.org;\nAuthor's Calculations.\n\nBlake Rubey.") +
  labs(fill="") +
  theme_bw()

##NOW, REVIEW TIME SERIES DISTRIBUTION OF PAYMENTS

#Filter only loans made to borrowers in Chicago
dffinal2<-dftotal%>%
  filter(BorrowerCity == "CHICAGO")%>%
  filter(BorrowerState == "IL")

#Clean Zip Codes, remove extra -four digits, e.g. if a zip code says 60601-0004, this removes the -0004.  
dffinal2 <- dffinal2%>%
  mutate(zip = gsub("-.*","",BorrowerZip))%>%
  select(zip, CurrentApprovalAmount, DateApproved)

#Convert loan numbers to numeric to sum them up
dffinal2$CurrentApprovalAmount <- as.numeric(as.character(dffinal2$CurrentApprovalAmount)) #Convert to Numeric from Character to Add 

#Merge the Data
dffinal2 <- merge(map1, dffinal2, all.x = TRUE) #matches only zip codes that are in both datasets

#Break into two groups for purposes of plotting the data: 2020 data and 2021 data
dffinal2a<-setDT(dffinal2)[DateApproved %between% c('2020-1-01', '2020-12-31')] 

dffinal2b<-setDT(dffinal2)[DateApproved %between% c('2021-1-01', '2021-12-31')] 

#Sum up Lending by Total Dollars Lent to Each Majority Group by Date
dffinal2a<-dffinal2a%>%
  group_by(majority, DateApproved)%>%
  summarise(Total = sum(CurrentApprovalAmount))%>% 
  select(majority, DateApproved, Total)

dffinal2b<-dffinal2b%>%
  group_by(majority, DateApproved)%>%
  summarise(Total = sum(CurrentApprovalAmount))%>% 
  select(majority, DateApproved, Total)

#Rearrange Legend to Plot the Data
dffinal2a$majority <- factor(dffinal2a$majority , levels = c("White", "Black", "Hispanic or Latino"))

dffinal2b$majority <- factor(dffinal2b$majority , levels = c("White", "Black", "Hispanic or Latino"))

#Or if you want to plot both 2020 and 2021 data on one chart:
dffinal3<-dffinal2%>%
  group_by(majority, DateApproved)%>%
  summarise(Total = sum(CurrentApprovalAmount))%>% 
  select(majority, DateApproved, Total)

dffinal3$majority <- factor(dffinal3$majority , levels = c("White", "Black", "Hispanic or Latino"))

ggp41<-dffinal2a%>%
  ggplot(aes(x = DateApproved, y= Total, fill = majority))+
  geom_bar(stat = "identity")+
  ylab("Millions of Dollars") +
  xlab("") +
  scale_fill_manual(labels = c("White", "Black", "Hispanic or Latino"), values = c("dodgerblue4", "lightsteelblue2", "dodgerblue2"))+
  ggtitle("Majority-White Zip Codes Recieved Majority of Funds in Early 2020") +
  labs(subtitle = "Figure 4-1: PPP Distribution by Majority Race/Ethnicity of Zip Code in 2020", caption = "Source: U.S. Small Business Administration;\nU.S. Census 2019 American Community Survey: 5-Year Data;\nIPUMS NHGIS, University of Minnesota, www.nhgis.org;\nAuthor's Calculations.\n\nBlake Rubey") +
  labs(fill = "")+
  scale_y_continuous( labels=function(x)x/1000000)+
  scale_x_date(date_labels = "%B %Y") +
  theme_bw() 


ggp42<-dffinal2b%>%
  ggplot(aes(x = DateApproved, y= Total, fill = majority))+
  geom_bar(stat = "identity")+
  ylab("Millions of Dollars") +
  xlab("") +
  scale_fill_manual(labels = c("White", "Black", "Hispanic or Latino"), values = c("dodgerblue4", "lightsteelblue2", "dodgerblue2"))+
  ggtitle("Majority-White Zip Codes Recieved Majority of Funds in Early 2021") +
  labs(subtitle = "Figure 4-2: PPP Distribution by Majority Race/Ethnicity of Zip Code in 2021", caption = "Source: U.S. Small Business Administration;\nU.S. Census 2019 American Community Survey: 5-Year Data;\nIPUMS NHGIS, University of Minnesota, www.nhgis.org;\nAuthor's Calculations.\n\nBlake Rubey") +
  labs(fill = "")+
  scale_y_continuous( labels=function(x)x/1000000)+
  scale_x_date(date_labels = "%B %Y") +
  theme_bw() 

grid.arrange(ggp41, ggp42, ncol = 1) 
