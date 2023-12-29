# set the working directory and read the data file to R
setwd("C:\\Users\\OHOstin\\Desktop\\Portfolio\\project 3- R(Data cleaning)")
data <- read.csv("Salary Survey 2021.csv")
data

# noticed that it had reached max memory capacity. 
# Therefore, I want to increase to custom value and retry printing the data.
options(max.print=1000000)
data

# Explore dataset imported.
head(data)

# Headers needs to be modified due to organization.
colnames(data) <- c("Timestamp","Age Group", "Industry", "Job Title", "Job title detail", "Salary", 
              "Bonus", "Currency","Other Currency", "Salary detail", "Country", "State", "City", 
              "Experience Level", "Experience Level in the field", "Education Level", "Gender", "Race")
head(data)

# Noticing that some columns are mostly empty and irrelevant to my interest, I will go ahead and omit few columns.
drops <- c("Timestamp","Job title detail","Other Currency", "Salary detail") 
data <- data[,!(names(data) %in% drops)]
head(data)

# Another minor errors can be found in Country column. Many of rows have different variation of spelling United States for this column
# Therefore, Those needs be corrected for easier categorization.
# I will go ahead replace those cells with US, U.S., United States of America, United States as US
library(stringr)
data$Country <- str_trim(data$Country, "left")
data$Country <- str_replace_all(data$Country, c("U.S.A." = "US", "USA" = "US", "Usa" = "US", "usa" = "US",
                                                "United States of America" = "US", "United States" = "US", 
                                                "United states" = "US", "united states" = "US",  
                                                "U.S." = "US", "U.S" = "US"))


# Now I will select all entries that have US in Country column and have at least 10000 USD annual salary.
USentry <- data[data$Country == 'US' & data$Salary > 10000,]
nrow(USentry)
str(USentry)

# Looking at the summary of the dataframe, I noticed that data has been reduced to 21000 rows and Salary column is in chr type. 
# I will be changing it to numeric type by removing comma between numbers.
# In addition, I switch gender column to categorical type that has three category(Male, Female, Non-binary)
USentry$Salary <- as.numeric(gsub(",","",USentry$Salary))
USentry$Gender <- as.factor(ifelse(USentry$Gender == "Man", "Male",
                                   ifelse(USentry$Gender == "Woman", "Female","Non-Binary")))

# There seems to be frequent missing values on Bonus column.
# This can be corrected by replacing NA values to 0 instead
USentry$Bonus[is.na(USentry$Bonus)] <- 0

# Race and Industry column consists of multiple categories of value.But, some Race rows got mixed up selecting multiple options. 
# I would remove rows that has less than 10 occurrence and those with empty value.
# Stored them under newUSentry  
sort(table(USentry$Race), decreasing = TRUE)
USentry <- USentry[!(is.na(USentry$Race) | USentry$Race == ""),]
USentry <- USentry[!(is.na(USentry$Industry) | USentry$Industry == ""),]

names(which(table(USentry$Race) > 10))
newUSentry <- USentry[USentry$Race %in% names(which(table(USentry$Race) > 10)), ]
sort(table(newUSentry$Race), decreasing = TRUE)

# Data preparation step is done and now I will do basic visualization based off of data I have cleaned.
library(ggplot2)
ggplot(newUSentry, aes(x=Gender)) + 
  geom_bar(fill = "cornflowerblue", 
           color="black") +
  labs(x = "Race", 
       y = "Frequency", 
       title = "Participants by Gender")


# Extract rows that has general industry field by excluding those rows that has fewer than 10 participants in its industry.
USentrywcommonIndus <- newUSentry[newUSentry$Industry %in% names(which(table(newUSentry$Industry) > 10)), ]
sort(table(USentrywcommonIndus$Industry), decreasing = TRUE)

# With extracted data, I will plot the frequency of each industries and order them. 
library(forcats)
ggplot(USentrywcommonIndus, aes(x = fct_rev(fct_infreq(Industry)))) + 
  geom_bar() +
  labs(x = "Industry",
       y = "Frequency",
       title = "Frequency by Industry") +
  coord_flip()

# With cleaned data set, more of visualization and statistical analysis can be done through R
# Main purpose of this project was to demonstrate and learn method of data cleansing.
# Hence, I am going to stop here and continue exploration later on.
# Thank you for following up to this data cleaning project.

