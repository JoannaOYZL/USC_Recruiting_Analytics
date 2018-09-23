# Loading necessary packages and datasets

library(readr)
library(readxl)
library(stringr)
library(lubridate)
library(ggplot2)
library(plyr)
library(dplyr)
library(scales)

data = read_excel("Case #2 - USC Recruiting Strategy Case - DATA - sept 2018.xlsx")
codebook = read_excel("CASE #2 - Recruiting Strategy CodeBook9.5.18.xlsx")

data = as.data.frame(data)

# Replacing all spaces, "/" and "-" in the column names to underline "_" , and remove all brackets in column names
colnames(data) = gsub("-", "_", colnames(data))
colnames(data) = gsub(" ", "_", colnames(data))
colnames(data) = gsub("\\/", "_", colnames(data))
colnames(data) = gsub("\\(", "", colnames(data))
colnames(data) = gsub("\\)", "", colnames(data))

codebook$Field = gsub("-", "_", codebook$Field)
codebook$Field = gsub(" ", "_", codebook$Field)
codebook$Field = gsub("\\/", "_", codebook$Field)
codebook$Field = gsub("\\(", "", codebook$Field)
codebook$Field = gsub("\\)", "", codebook$Field)


# Replace "-" in "sip_score" and "sip_comments" with NA
data$sip_comments[data$sip_comments == "-"] = NA
data$sip_score[data$sip_score == "-"] = NA

# Data cleaning is conducted based on the results of EDA

#--------------------------------Ensure Value Naming Consistency---------------------------------
# Ensure naming consistency in column "billing country"
## Convert all billing country that should be in the US to "United States"
data$billing_country[data$billing_country %in% c("united States", "United", "USA", 
                                                 "United States of America", "CA")] = "United States"

# Ensure naming consistency in column "residency"
data$residency = as.factor(data$residency)
levels(data$residency) = c("US Citizen","International","Non Resident","Non Resident","None",
                           "Other","Permanent US Resident", "Permanent US Resident",
                           "Temporary US Resident","US Citizen","US Citizen")

# Remove records with erroneous post_code and post_code_account (very few)
data$post_code[data$post_code %in% c("1645;264", "264;1412", "264;1411", "264;264", "272;264")] = "264"
data$post_code_account[data$post_code_account %in% c("1645;264", "264;1412", "264;1411", "264;264", "272;264")] = "264"

#--------------------------------------Remove NAs---------------------------------------------
# Some billing countries have too few records, we decided to remove "Japan", "Canada", "South Korea", "United Kindom"
# And keep only "US" and "China"
data = data%>%
  filter(!billing_country %in% c("Japan", "Canada", "South Korea", "United Kingdom"))

#for 4 variables: 90041_mi, 91601_mi, 92122_mi, 94607_mi, fill in the median calculated by grouping by billing_country
bc_90041 = data%>%
  group_by(billing_country)%>%
  summarise(med = median(`90041_mi`, na.rm = TRUE))
data$`90041_mi`[data$billing_country == bc_90041$billing_country[1] & #China
                is.na(data$`90041_mi`)] = bc_90041$med[1]
data$`90041_mi`[data$billing_country == bc_90041$billing_country[2] & #US
                is.na(data$`90041_mi`)] = bc_90041$med[2]

bc_91601 = data%>%
  group_by(billing_country)%>%
  summarise(med = median(`91601_mi`, na.rm = TRUE))
data$`91601_mi`[data$billing_country == bc_91601$billing_country[1] & #China
                  is.na(data$`91601_mi`)] = bc_91601$med[1]
data$`91601_mi`[data$billing_country == bc_91601$billing_country[2] & #US
                  is.na(data$`91601_mi`)] = bc_91601$med[2]

bc_92122 = data%>%
  group_by(billing_country)%>%
  summarise(med = median(`92122_mi`, na.rm = TRUE))
data$`92122_mi`[data$billing_country == bc_92122$billing_country[1] & #China
                  is.na(data$`92122_mi`)] = bc_92122$med[1]
data$`92122_mi`[data$billing_country == bc_92122$billing_country[2] & #US
                  is.na(data$`92122_mi`)] = bc_92122$med[2]

bc_94607 = data%>%
  group_by(billing_country)%>%
  summarise(med = median(`94607_mi`, na.rm = TRUE))
data$`94607_mi`[data$billing_country == bc_94607$billing_country[1] & #China
                  is.na(data$`94607_mi`)] = bc_94607$med[1]
data$`94607_mi`[data$billing_country == bc_94607$billing_country[2] & #US
                  is.na(data$`94607_mi`)] = bc_94607$med[2]

# set all NAs in needed numerical columns to its median
collist = c("age", "distance_to_campus", "first_review_exp","first_review_gpa",
  "first_review_lor","first_review_sop","first_review_total","graduate_gpa",
  "last_60_90_gpa","registered_units_account","second_review_exp",
  "second_review_gpa","second_review_sop","second_review_total","self_reported_gpa",
  "undergraduate_gpa")

for (i in collist){
  data[, i][is.na(data[, i])] = median(data[, i], na.rm = TRUE)
}

# Fill "blank" to NAs in categorical columns
# residency
data$residency = as.character(data$residency)
data$residency[is.na(data$residency)]="blank"
data$residency = as.factor(data$residency)

# column number of variables that has NA needed to be change to "blank"
NA_fill <- c(56,57,2,25,27,37,47,22,62,42,9,41,35,33,58,59,60,61,38,36,
             31,39,29,43,81,67)
for (i in NA_fill) {
  data[,i][is.na(data[, i])] = "blank"
}

# billing zip
data$`billing_zip/postal_code`[is.na(data$billing_zip_postal_code)] = 99999


#--------------------------------------Remove Outliers------------------------------------------
# 
data = data%>%
  filter(degree_posted < "2019-01-01" &
         file_status_completed_date < "2019-01-01" &
         decision_date < "2019-01-01" &
         undergraduate_gpa != 0 &
         self_reported_gpa <= 4 & self_reported_gpa != 0 &
         birth_date < "2000-01-01" &
         date_returned_from_second_review < "2019-01-01" &
         gia_review_date < "2019-01-01" &
         age >= 15)

data = data[data$post_code != "Fall 2015" & data$post_code_account != "Fall 2015", ]

#-------------------------------------Data Type------------------------------------------------
# convert the data type to factor and reorder the level to match actual semesters timeline
data$application_term = as.factor(data$application_term)
levels(data$application_term) = levels(data$application_term)[c(10, 1, 11, 2, 7, 12, 3, 8, 13, 4, 5, 9, 14, 6)]

data$program_length = as.factor(data$program_length)
levels(data$program_length) = levels(data$program_length)[c(2, 4, 6, 7, 1, 8, 3, 5)]

fwrite(data, "data_cleaned.csv")
