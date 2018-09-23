# Loading necessary packages and datasets
library(readr)
library(readxl)
library(stringr)
library(lubridate)
library(ggplot2)
library(data.table)
library(plyr)
library(dplyr)

data = read_excel("Case #2 - USC Recruiting Strategy Case - DATA - sept 2018.xlsx")
codebook = read_excel("CASE #2 - Recruiting Strategy CodeBook9.5.18.xlsx")

###############################################################################################################################
# --------------------------------------------- PART 1: Dataset Overview ------------------------------------------------------
###############################################################################################################################

# Replacing all spaces in the column names to underline "_"
colnames(data) = gsub(" ", "_", colnames(data))
codebook$Field = gsub(" ", "_", codebook$Field)

# Creating summary statistics table
summary = data.frame(Variable = colnames(data))
summary$DataType = sapply(data, class)                             # Creating column "DataType" to indicate all variables' data type
summary$NAnum = sapply(data, function(x){sum(is.na(x))})           # Creating column "NAnum" to count the number of NAs in each variable
summary$uniqueNum = sapply(data, function(x){length(unique(x))})   # Creating column "uniqueNum" to count the number of unique values in each variable
summary$min = sapply(data, min, na.rm = TRUE)                      # Creating column "min" to indicate all variables' minimum value (only meaningful for numeric values)
summary$max = sapply(data, max, na.rm = TRUE)                      # Creating column "max" to indicate all variables' maximum value (only meaningful for numeric values)
summary$mean = sapply(data, mean, na.rm = TRUE)                    # Creating column "meab" to indicate all variables' mean (only meaningful for numeric values)

# Join variable definitions in codebook to summary statistics table
summary = merge(summary, codebook, by.x = "Variable", by.y = "Field")

# List 5 examples of the field values for each variable
example = sapply(data, function(x){sort(unique(x)[1:5])})         # extract and sort unique values for each column
example = ldply(example, rbind)                                   # transform the list into a dataframe
colnames(example) = c("Variable", "e1", "e2", "e3", "e4", "e5")   # rename the columns
example$examples = apply(example[,-1], 1, paste, collapse = "//") # paste all the 5 examples for each variable into one column
summary = merge(summary, example[, c(1, 7)], by = "Variable")     # join the examples to the summary table

# Write the summary statistics into csv file
fwrite(summary, "summary.csv")


###############################################################################################################################
# ------------------------------------------- PART 1: Column-by-column Cleaning -----------------------------------------------
###############################################################################################################################

# There are altogether 81 variables in the dataset
# We will go over each column one by one to understand its details and then decide the corresponding cleaning method

sum(!is.na(data$sip_comments))
data$sip_comments[data$opportunity_id == "006a000001BhaUvAAJ"]









