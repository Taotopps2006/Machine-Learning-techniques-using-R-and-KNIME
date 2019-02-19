########## ANALYSIS OF CENSUS DATASET CALLED Adult
#########  BY TAWOSE OLAMIDE TIMOTHY
#########  UB_NUMBER: 16000522
########   M.Sc Big Data Science and Technology
########   Faculty of Engineering and Informatics
#######    University of Bradford. 
#######    Session: 2017

########################################## SECTION 2: LIBRARY INSTALLATION AND DATA IMPORTATION ####################################################
## Loading the Dataset and Installing required packages
# R environment preparation
rm(list=ls())    # clears all variables declared in the memory   
# rm(list = ls(all.names = TRUE))
# Installing and loading R packages
require(dplyr)  # provides data manipulating functions. # library(dplyr) would also work 
library(magrittr)  # provides pipe operator
library(ggplot2)
install.packages('plotrix')  # for 3D pie chart
library(plotrix)
library(car)   # provides "Boxplot" function,leveneTest from the {car} package which labels outliers and scatter3d function
install.packages('data.table')   # installs as.data.table function
library(data.table)
install.packages('psych')
library(psych)
install.packages('sm')
library(sm)

### DATA IMPORTATION IN R IN CSV FORMAT
# the parameter na.strings is equal to c("") so that each missing value is coded as a NA.
# importing the training data of the default partition using ML++ CVFiles
TRAIN <- read.csv('M:\\Rstudio codes\\adult_train.csv',header=T,na.strings=c(""))
dim(TRAIN) # dimension of the dataset
# importing the test data of the default partition using ML++ CVFiles
TEST <- read.csv('M:\\Rstudio codes\\adult_test.csv',header=T,na.strings=c(""))
dim(TEST)
COMBINE <- rbind(TRAIN,TEST)   # rbind was used to combine the two datasets namely train and test rowwise
dim(COMBINE)
########################################## SECTION 3: MANAGING AND UNDERSTANDING DATA ####################################################
### 2. Data Exploration and Preprocessing   Exploratory data avalysis
## Data Description
######################################################

################ SECTION 3.1 : BASIC EXPLORATION ##################
## Exploration Data Analysis and Handling Missing Data
#Obtaining first several rows of the data frame using head. tail is used to obtain the last several rows
# To check if the data is loaded properly in the workspace
## Basic Exploration
head(TRAIN)   # training dataset
head(TEST)    # test dataset
head(COMBINE)  # combined dataset

tail(TRAIN)
tail(TEST)
tail(COMBINE)

###################################################################

################ SECTION 3.2 : DESCRIPTIVE DATA SUMMARIZATION ##################

nrow(COMBINE)  # returns the number of rows
ncol(COMBINE)  #returns the number of columns
str(COMBINE)  #returns the structure of the data frame  # understand the covariates
summary(COMBINE)   # returns summary statistics
describe(COMBINE)
################################################################################

################ SECTION 3.3 :  DATA PRE-PROCESSING / PREPARATION ##################
########################### 
######## Step 3.3.1: Data Labelling and Cleaning and Preparation
# This preprocessing step often is crucial for obtaining a good fit of the model and better predictive ability.
#################################################
# changing "?" to NA
levels(COMBINE$WorkClass)[levels(COMBINE$WorkClass)==" ?"] <- NA
levels(COMBINE$Occupation)[levels(COMBINE$Occupation)==" ?"] <- NA
levels(COMBINE$NativeCountry)[levels(COMBINE$NativeCountry)==" ?"] <- NA
levels(TRAIN$WorkClass)[levels(TRAIN$WorkClass)==" ?"] <- NA
levels(TRAIN$Occupation)[levels(TRAIN$Occupation)==" ?"] <- NA
levels(TRAIN$NativeCountry)[levels(TRAIN$NativeCountry)==" ?"] <- NA
levels(TEST$WorkClass)[levels(TEST$WorkClass)==" ?"] <- NA
levels(TEST$Occupation)[levels(TEST$Occupation)==" ?"] <- NA
levels(TEST$NativeCountry)[levels(TEST$NativeCountry)==" ?"] <- NA

# changing "<=50k" and ">50k" to <=50k and >50k respectively
levels(COMBINE$Class)[levels(COMBINE$Class)==" <=50K."] <- " <=50K"
levels(COMBINE$Class)[levels(COMBINE$Class)==" >50K."] <- " >50K"
levels(TEST$Class)[levels(TEST$Class)==" <=50K."] <- " <=50K"
levels(TEST$Class)[levels(TEST$Class)==" >50K."] <- " >50K"
str(COMBINE)
str(TEST)

#### REMOVING THE SPACE BEFORE THE CLASS LABEL
levels(COMBINE$Class)[levels(COMBINE$Class)==" <=50K"] <- "<=50K"
levels(COMBINE$Class)[levels(COMBINE$Class)==" >50K"] <- ">50K"
levels(TEST$Class)[levels(TEST$Class)==" <=50K"] <- "<=50K"
levels(TEST$Class)[levels(TEST$Class)==" >50K"] <- ">50K"
levels(TRAIN$Class)[levels(TRAIN$Class)==" <=50K"] <- "<=50K"
levels(TRAIN$Class)[levels(TRAIN$Class)==" >50K"] <- ">50K"
str(COMBINE)
str(TEST)


## Check missing values
sapply(COMBINE, function(x) sum(is.na(x)))  # total NA in each column
sapply(TRAIN, function(x) sum(is.na(x)))  # total NA in each column
sapply(TEST, function(x) sum(is.na(x)))  # total NA in each column

## Handle important columns with few missing values: deleting rows with missing values
# WorkClass, Occupation, NativeCountry
# since there is few rows, we will discard/remove these rows (we could also have replaced the missing values with the mode and keep the datapoints)
COMBINE<- COMBINE[!is.na(COMBINE$WorkClass),]
COMBINE<- COMBINE[!is.na(COMBINE$Occupation),]
COMBINE<- COMBINE[!is.na(COMBINE$NativeCountry),]
dim(COMBINE)

TRAIN<- TRAIN[!is.na(TRAIN$WorkClass),]
TRAIN<- TRAIN[!is.na(TRAIN$Occupation),]
TRAIN<- TRAIN[!is.na(TRAIN$NativeCountry),]
dim(TRAIN)

TEST<- TEST[!is.na(TEST$WorkClass),]
TEST<- TEST[!is.na(TEST$Occupation),]
TEST<- TEST[!is.na(TEST$NativeCountry),]
dim(TEST)

## Check TO CONFIRM REMOVAL OF missing values
sapply(COMBINE, function(x) sum(is.na(x)))  # total NA in each column
sapply(TRAIN, function(x) sum(is.na(x)))  # total NA in each column
sapply(TEST, function(x) sum(is.na(x)))  # total NA in each column


## DATA REDUCTION
## Drop columns with many missing values and redundant data
# droping column numbers 3 and 5. These are columns are: Finalwgt, Edu-Num
COMBINE<- subset(COMBINE,select=c(1,2,4,6,7,8,9,10,11,12,13,14,15))
TRAIN<- subset(TRAIN,select=c(1,2,4,6,7,8,9,10,11,12,13,14,15))
TEST<- subset(TEST,select=c(1,2,4,6,7,8,9,10,11,12,13,14,15))

############################################## HANDLING AND EXPLORING CATEGORICAL VARIABLES #############################
# class distribution for categorical variables
w = table(COMBINE$WorkClass)   
e = table(COMBINE$Edu_Level)
m = table(COMBINE$MaritalStat)
o = table(COMBINE$Occupation)
re = table(COMBINE$Relationship)
ra = table(COMBINE$Race)
se = table(COMBINE$Sex)
na =  table(COMBINE$NativeCountry)
label = table(COMBINE$Class)

cbind(freq=w, percentage=prop.table(w) * 100)
cbind(freq=m, percentage=prop.table(m) * 100)
cbind(freq=o, percentage=prop.table(o) * 100)
cbind(freq=re, percentage=prop.table(re) * 100)
cbind(freq=ra, percentage=prop.table(ra) * 100)
cbind(freq=se, percentage=prop.table(se) * 100)
cbind(freq=e, percentage=prop.table(e) * 100)
cbind(freq=na, percentage=prop.table(na) * 100)
cbind(freq=label, percentage=prop.table(label) * 100)

# Visualizing distribution of categorical variables using Bar plots before concept hierarchy generation
qplot(COMBINE$WorkClass, xlab = 'Work Class', ylab = 'Count',  main='Bar Plot: Distribution of Work Class before Transformation')
qplot(COMBINE$Edu_Level, xlab = 'Education Level', ylab = 'Count',  main='Bar Plot: Distribution of Education Level before Transformation')
qplot(COMBINE$MaritalStat, xlab = 'Marital Status', ylab = 'Count',  main='Bar Plot: Distribution of Marital Status before Transformation')
qplot(COMBINE$Occupation, xlab = 'Occupation', ylab = 'Count',  main='Bar Plot: Distribution of Occupation before Concept Hierarchy')
qplot(COMBINE$Relationship, xlab = 'Relationship', ylab = 'Count',  main='Bar Plot: Distribution of Relationship')
qplot(COMBINE$Race, xlab = 'Race', ylab = 'Count',  main='Bar Plot: Distribution of Race')
qplot(COMBINE$Sex, xlab = 'Sex', ylab = 'Count',  main='Bar Plot: Distribution of Sex')
qplot(COMBINE$NativeCountry, xlab = 'Native Country', ylab = 'Count',  main='Bar Plot: Distribution of Native Country before Concept Hierarchy')

######################################################
########### DATA TRANSFORMATION / CONCEPT HIERARCHY
#############   1)   FOR COMBINED DATASET (TEST + TRAIN)
#Because i am going to be modifying the text directly, 
#i converted them to character strings. 
# This is done for all the text variables i intend to work with.
COMBINE$Edu_Level = as.character(COMBINE$Edu_Level)
COMBINE$MaritalStat = as.character(COMBINE$MaritalStat)
COMBINE$Occupation = as.character(COMBINE$Occupation)
COMBINE$WorkClass= as.character(COMBINE$WorkClass)
COMBINE$NativeCountry= as.character(COMBINE$NativeCountry)


COMBINE$Edu_Level[COMBINE$Edu_Level==" Preschool"] = "Dropout"
COMBINE$Edu_Level[COMBINE$Edu_Level==" 1st-4th"] = "Dropout"
COMBINE$Edu_Level[COMBINE$Edu_Level==" 5th-6th"] = "Dropout"
COMBINE$Edu_Level[COMBINE$Edu_Level==" 7th-8th"] = "Dropout"
COMBINE$Edu_Level[COMBINE$Edu_Level==" 9th"] = "Dropout"
COMBINE$Edu_Level[COMBINE$Edu_Level==" 10th"] = "Dropout"
COMBINE$Edu_Level[COMBINE$Edu_Level==" 11th"] = "Dropout"
COMBINE$Edu_Level[COMBINE$Edu_Level==" 12th"] = "Dropout"
COMBINE$Edu_Level[COMBINE$Edu_Level==" HS-grad"] = "HS-Graduate"
COMBINE$Edu_Level[COMBINE$Edu_Level==" Some-college"] = "HS-Graduate"
COMBINE$Edu_Level[COMBINE$Edu_Level==" Assoc-acdm"] = "Associates"
COMBINE$Edu_Level[COMBINE$Edu_Level==" Assoc-voc"] = "Associates"
COMBINE$Edu_Level[COMBINE$Edu_Level==" Bachelors"] = "Bachelors"
COMBINE$Edu_Level[COMBINE$Edu_Level==" Masters"] = "Masters"
COMBINE$Edu_Level[COMBINE$Edu_Level==" Doctorate"] = "Doctorate"
COMBINE$Edu_Level[COMBINE$Edu_Level==" Prof-school"] = "Prof-school"
#I'm done modifying the Edu_Level categorical variables, so I'm converting them back to factors. 
COMBINE$Edu_Level = factor(COMBINE$Edu_Level)
levels(COMBINE$Edu_Level)

COMBINE$MaritalStat[COMBINE$MaritalStat==" Never-married"] = "Never-married"
COMBINE$MaritalStat[COMBINE$MaritalStat==" Married-AF-spouse"] = "Married"
COMBINE$MaritalStat[COMBINE$MaritalStat==" Married-civ-spouse"] = "Married"
COMBINE$MaritalStat[COMBINE$MaritalStat==" Married-spouse-absent"] = "Married"
COMBINE$MaritalStat[COMBINE$MaritalStat==" Separated"] = "Separated"
COMBINE$MaritalStat[COMBINE$MaritalStat==" Divorced"] = "Divorced"
COMBINE$MaritalStat[COMBINE$MaritalStat==" Widowed"] = "Widowed"
#I'm done modifying the Marital Status categorical variables, so I'm converting them back to factors. 
COMBINE$MaritalStat = factor(COMBINE$MaritalStat)
levels(COMBINE$MaritalStat)

COMBINE$WorkClass[COMBINE$WorkClass==" Federal-gov"] = "Federal-gov"
COMBINE$WorkClass[COMBINE$WorkClass==" Local-gov"] = "Local-gov"
COMBINE$WorkClass[COMBINE$WorkClass==" State-gov"] = "State-gov"
COMBINE$WorkClass[COMBINE$WorkClass==" Private"] = "Private"
COMBINE$WorkClass[COMBINE$WorkClass==" Self-emp-inc"] = "Self-Employed"
COMBINE$WorkClass[COMBINE$WorkClass==" Self-emp-not-inc"] = "Self-Employed"
COMBINE$WorkClass[COMBINE$WorkClass==" Without-pay"] = "Without-pay"
#I'm done modifying the Work Class categorical variables, so I'm converting them back to factors. 
COMBINE$WorkClass = factor(COMBINE$WorkClass)
levels(COMBINE$WorkClass)

COMBINE$Occupation[COMBINE$Occupation==" Adm-clerical"] = "Adm-clerical"
COMBINE$Occupation[COMBINE$Occupation==" Armed-Forces"] = "Armed-Forces"
COMBINE$Occupation[COMBINE$Occupation==" Exec-managerial"] = "White-Collar"
COMBINE$Occupation[COMBINE$Occupation==" Craft-repair"] = "Blue-Collar"
COMBINE$Occupation[COMBINE$Occupation==" Farming-fishing"] = "Blue-Collar"
COMBINE$Occupation[COMBINE$Occupation==" Handlers-cleaners"] = "Blue-Collar"
COMBINE$Occupation[COMBINE$Occupation==" Machine-op-inspct"] = "Blue-Collar"
COMBINE$Occupation[COMBINE$Occupation==" Transport-moving"] = "Blue-Collar"
COMBINE$Occupation[COMBINE$Occupation==" Priv-house-serv"] = "Service"
COMBINE$Occupation[COMBINE$Occupation==" Other-service"] = "Service"
COMBINE$Occupation[COMBINE$Occupation==" Protective-serv"] = "Service"
COMBINE$Occupation[COMBINE$Occupation==" Prof-specialty"] = "Prof-specialty"
COMBINE$Occupation[COMBINE$Occupation==" Sales"] = "Sales"
COMBINE$Occupation[COMBINE$Occupation==" Tech-support"] = "Tech-support"
#I'm done modifying the Occupation categorical variables, so I'm converting them back to factors. 
COMBINE$Occupation = factor(COMBINE$Occupation)
levels(COMBINE$Occupation)


COMBINE$NativeCountry[COMBINE$NativeCountry==" Cambodia"] = "non-USA"
COMBINE$NativeCountry[COMBINE$NativeCountry==" Canada"] = "non-USA"
COMBINE$NativeCountry[COMBINE$NativeCountry==" China"] = "non-USA"
COMBINE$NativeCountry[COMBINE$NativeCountry==" Columbia"] = "non-USA"
COMBINE$NativeCountry[COMBINE$NativeCountry==" Cuba"] = "non-USA"
COMBINE$NativeCountry[COMBINE$NativeCountry==" Dominican-Republic"] = "non-USA"
COMBINE$NativeCountry[COMBINE$NativeCountry==" Ecuador"] = "non-USA"
COMBINE$NativeCountry[COMBINE$NativeCountry==" El-Salvador"] = "non-USA"
COMBINE$NativeCountry[COMBINE$NativeCountry==" England"] = "non-USA"
COMBINE$NativeCountry[COMBINE$NativeCountry==" France"] = "non-USA"
COMBINE$NativeCountry[COMBINE$NativeCountry==" Germany"] = "non-USA"
COMBINE$NativeCountry[COMBINE$NativeCountry==" Greece"] = "non-USA"
COMBINE$NativeCountry[COMBINE$NativeCountry==" Guatemala"] = "non-USA"
COMBINE$NativeCountry[COMBINE$NativeCountry==" Haiti"] = "non-USA"
COMBINE$NativeCountry[COMBINE$NativeCountry==" Holand-Netherlands"] = "non-USA"
COMBINE$NativeCountry[COMBINE$NativeCountry==" Honduras"] = "non-USA"
COMBINE$NativeCountry[COMBINE$NativeCountry==" Hong"] = "non-USA"
COMBINE$NativeCountry[COMBINE$NativeCountry==" Hungary"] = "non-USA"
COMBINE$NativeCountry[COMBINE$NativeCountry==" India"] = "non-USA"
COMBINE$NativeCountry[COMBINE$NativeCountry==" Iran"] = "non-USA"
COMBINE$NativeCountry[COMBINE$NativeCountry==" Ireland"] = "non-USA"
COMBINE$NativeCountry[COMBINE$NativeCountry==" Italy"] = "non-USA"
COMBINE$NativeCountry[COMBINE$NativeCountry==" Jamaica"] = "non-USA"
COMBINE$NativeCountry[COMBINE$NativeCountry==" Japan"] = "non-USA"
COMBINE$NativeCountry[COMBINE$NativeCountry==" Laos"] = "non-USA"
COMBINE$NativeCountry[COMBINE$NativeCountry==" Mexico"] = "non-USA"
COMBINE$NativeCountry[COMBINE$NativeCountry==" Nicaragua"] = "non-USA"
COMBINE$NativeCountry[COMBINE$NativeCountry==" Outlying-US(Guam-USVI-etc)"] = "non-USA"
COMBINE$NativeCountry[COMBINE$NativeCountry==" Peru"] = "non-USA"
COMBINE$NativeCountry[COMBINE$NativeCountry==" Philippines"] = "non-USA"
COMBINE$NativeCountry[COMBINE$NativeCountry==" Poland"] = "non-USA"
COMBINE$NativeCountry[COMBINE$NativeCountry==" Portugal"] = "non-USA"
COMBINE$NativeCountry[COMBINE$NativeCountry==" Puerto-Rico"] = "non-USA"
COMBINE$NativeCountry[COMBINE$NativeCountry==" Scotland"] = "non-USA"
COMBINE$NativeCountry[COMBINE$NativeCountry==" South"] = "non-USA"
COMBINE$NativeCountry[COMBINE$NativeCountry==" Taiwan"] = "non-USA"
COMBINE$NativeCountry[COMBINE$NativeCountry==" Thailand"] = "non-USA"
COMBINE$NativeCountry[COMBINE$NativeCountry==" Trinadad&Tobago"] = "non-USA"
COMBINE$NativeCountry[COMBINE$NativeCountry==" Vietnam"] = "non-USA"
COMBINE$NativeCountry[COMBINE$NativeCountry==" Yugoslavia"] = "non-USA"
COMBINE$NativeCountry[COMBINE$NativeCountry==" United-States"] = "USA"
#I'm done modifying the Native Country categorical variables, so I'm converting them back to factors. 
COMBINE$NativeCountry = factor(COMBINE$NativeCountry)
levels(COMBINE$NativeCountry)



#############   2)  CONCEPT HIERARCHY FOR ML++ GenCVFiles TRAIN DATASET
#Because i am going to be modifying the text directly, 
#i converted them to character strings. 
# This is done for all the text variables i intend to work with.
TRAIN$Edu_Level = as.character(TRAIN$Edu_Level)
TRAIN$MaritalStat = as.character(TRAIN$MaritalStat)
TRAIN$Occupation = as.character(TRAIN$Occupation)
TRAIN$WorkClass= as.character(TRAIN$WorkClass)
TRAIN$NativeCountry= as.character(TRAIN$NativeCountry)


TRAIN$Edu_Level[TRAIN$Edu_Level==" Preschool"] = "Dropout"
TRAIN$Edu_Level[TRAIN$Edu_Level==" 1st-4th"] = "Dropout"
TRAIN$Edu_Level[TRAIN$Edu_Level==" 5th-6th"] = "Dropout"
TRAIN$Edu_Level[TRAIN$Edu_Level==" 7th-8th"] = "Dropout"
TRAIN$Edu_Level[TRAIN$Edu_Level==" 9th"] = "Dropout"
TRAIN$Edu_Level[TRAIN$Edu_Level==" 10th"] = "Dropout"
TRAIN$Edu_Level[TRAIN$Edu_Level==" 11th"] = "Dropout"
TRAIN$Edu_Level[TRAIN$Edu_Level==" 12th"] = "Dropout"
TRAIN$Edu_Level[TRAIN$Edu_Level==" HS-grad"] = "HS-Graduate"
TRAIN$Edu_Level[TRAIN$Edu_Level==" Some-college"] = "HS-Graduate"
TRAIN$Edu_Level[TRAIN$Edu_Level==" Assoc-acdm"] = "Associates"
TRAIN$Edu_Level[TRAIN$Edu_Level==" Assoc-voc"] = "Associates"
TRAIN$Edu_Level[TRAIN$Edu_Level==" Bachelors"] = "Bachelors"
TRAIN$Edu_Level[TRAIN$Edu_Level==" Masters"] = "Masters"
TRAIN$Edu_Level[TRAIN$Edu_Level==" Doctorate"] = "Doctorate"
TRAIN$Edu_Level[TRAIN$Edu_Level==" Prof-school"] = "Prof-school"
#I'm done modifying the Edu_Level categorical variables, so I'm converting them back to factors. 
TRAIN$Edu_Level = factor(TRAIN$Edu_Level)
levels(TRAIN$Edu_Level)

TRAIN$MaritalStat[TRAIN$MaritalStat==" Never-married"] = "Never-married"
TRAIN$MaritalStat[TRAIN$MaritalStat==" Married-AF-spouse"] = "Married"
TRAIN$MaritalStat[TRAIN$MaritalStat==" Married-civ-spouse"] = "Married"
TRAIN$MaritalStat[TRAIN$MaritalStat==" Married-spouse-absent"] = "Married"
TRAIN$MaritalStat[TRAIN$MaritalStat==" Separated"] = "Separated"
TRAIN$MaritalStat[TRAIN$MaritalStat==" Divorced"] = "Divorced"
TRAIN$MaritalStat[TRAIN$MaritalStat==" Widowed"] = "Widowed"
#I'm done modifying the Marital Status categorical variables, so I'm converting them back to factors. 
TRAIN$MaritalStat = factor(TRAIN$MaritalStat)
levels(TRAIN$MaritalStat)

TRAIN$WorkClass[TRAIN$WorkClass==" Federal-gov"] = "Federal-gov"
TRAIN$WorkClass[TRAIN$WorkClass==" Local-gov"] = "Local-gov"
TRAIN$WorkClass[TRAIN$WorkClass==" State-gov"] = "State-gov"
TRAIN$WorkClass[TRAIN$WorkClass==" Private"] = "Private"
TRAIN$WorkClass[TRAIN$WorkClass==" Self-emp-inc"] = "Self-Employed"
TRAIN$WorkClass[TRAIN$WorkClass==" Self-emp-not-inc"] = "Self-Employed"
TRAIN$WorkClass[TRAIN$WorkClass==" Without-pay"] = "Without-pay"
#I'm done modifying the Work Class categorical variables, so I'm converting them back to factors. 
TRAIN$WorkClass = factor(TRAIN$WorkClass)
levels(TRAIN$WorkClass)

TRAIN$Occupation[TRAIN$Occupation==" Adm-clerical"] = "Adm-clerical"
TRAIN$Occupation[TRAIN$Occupation==" Armed-Forces"] = "Armed-Forces"
TRAIN$Occupation[TRAIN$Occupation==" Exec-managerial"] = "White-Collar"
TRAIN$Occupation[TRAIN$Occupation==" Craft-repair"] = "Blue-Collar"
TRAIN$Occupation[TRAIN$Occupation==" Farming-fishing"] = "Blue-Collar"
TRAIN$Occupation[TRAIN$Occupation==" Handlers-cleaners"] = "Blue-Collar"
TRAIN$Occupation[TRAIN$Occupation==" Machine-op-inspct"] = "Blue-Collar"
TRAIN$Occupation[TRAIN$Occupation==" Transport-moving"] = "Blue-Collar"
TRAIN$Occupation[TRAIN$Occupation==" Priv-house-serv"] = "Service"
TRAIN$Occupation[TRAIN$Occupation==" Other-service"] = "Service"
TRAIN$Occupation[TRAIN$Occupation==" Protective-serv"] = "Service"
TRAIN$Occupation[TRAIN$Occupation==" Prof-specialty"] = "Prof-specialty"
TRAIN$Occupation[TRAIN$Occupation==" Sales"] = "Sales"
TRAIN$Occupation[TRAIN$Occupation==" Tech-support"] = "Tech-support"
#I'm done modifying the Occupation categorical variables, so I'm converting them back to factors. 
TRAIN$Occupation = factor(TRAIN$Occupation)
levels(TRAIN$Occupation)


TRAIN$NativeCountry[TRAIN$NativeCountry==" Cambodia"] = "non-USA"
TRAIN$NativeCountry[TRAIN$NativeCountry==" Canada"] = "non-USA"
TRAIN$NativeCountry[TRAIN$NativeCountry==" China"] = "non-USA"
TRAIN$NativeCountry[TRAIN$NativeCountry==" Columbia"] = "non-USA"
TRAIN$NativeCountry[TRAIN$NativeCountry==" Cuba"] = "non-USA"
TRAIN$NativeCountry[TRAIN$NativeCountry==" Dominican-Republic"] = "non-USA"
TRAIN$NativeCountry[TRAIN$NativeCountry==" Ecuador"] = "non-USA"
TRAIN$NativeCountry[TRAIN$NativeCountry==" El-Salvador"] = "non-USA"
TRAIN$NativeCountry[TRAIN$NativeCountry==" England"] = "non-USA"
TRAIN$NativeCountry[TRAIN$NativeCountry==" France"] = "non-USA"
TRAIN$NativeCountry[TRAIN$NativeCountry==" Germany"] = "non-USA"
TRAIN$NativeCountry[TRAIN$NativeCountry==" Greece"] = "non-USA"
TRAIN$NativeCountry[TRAIN$NativeCountry==" Guatemala"] = "non-USA"
TRAIN$NativeCountry[TRAIN$NativeCountry==" Haiti"] = "non-USA"
TRAIN$NativeCountry[TRAIN$NativeCountry==" Holand-Netherlands"] = "non-USA"
TRAIN$NativeCountry[TRAIN$NativeCountry==" Honduras"] = "non-USA"
TRAIN$NativeCountry[TRAIN$NativeCountry==" Hong"] = "non-USA"
TRAIN$NativeCountry[TRAIN$NativeCountry==" Hungary"] = "non-USA"
TRAIN$NativeCountry[TRAIN$NativeCountry==" India"] = "non-USA"
TRAIN$NativeCountry[TRAIN$NativeCountry==" Iran"] = "non-USA"
TRAIN$NativeCountry[TRAIN$NativeCountry==" Ireland"] = "non-USA"
TRAIN$NativeCountry[TRAIN$NativeCountry==" Italy"] = "non-USA"
TRAIN$NativeCountry[TRAIN$NativeCountry==" Jamaica"] = "non-USA"
TRAIN$NativeCountry[TRAIN$NativeCountry==" Japan"] = "non-USA"
TRAIN$NativeCountry[TRAIN$NativeCountry==" Laos"] = "non-USA"
TRAIN$NativeCountry[TRAIN$NativeCountry==" Mexico"] = "non-USA"
TRAIN$NativeCountry[TRAIN$NativeCountry==" Nicaragua"] = "non-USA"
TRAIN$NativeCountry[TRAIN$NativeCountry==" Outlying-US(Guam-USVI-etc)"] = "non-USA"
TRAIN$NativeCountry[TRAIN$NativeCountry==" Peru"] = "non-USA"
TRAIN$NativeCountry[TRAIN$NativeCountry==" Philippines"] = "non-USA"
TRAIN$NativeCountry[TRAIN$NativeCountry==" Poland"] = "non-USA"
TRAIN$NativeCountry[TRAIN$NativeCountry==" Portugal"] = "non-USA"
TRAIN$NativeCountry[TRAIN$NativeCountry==" Puerto-Rico"] = "non-USA"
TRAIN$NativeCountry[TRAIN$NativeCountry==" Scotland"] = "non-USA"
TRAIN$NativeCountry[TRAIN$NativeCountry==" South"] = "non-USA"
TRAIN$NativeCountry[TRAIN$NativeCountry==" Taiwan"] = "non-USA"
TRAIN$NativeCountry[TRAIN$NativeCountry==" Thailand"] = "non-USA"
TRAIN$NativeCountry[TRAIN$NativeCountry==" Trinadad&Tobago"] = "non-USA"
TRAIN$NativeCountry[TRAIN$NativeCountry==" Vietnam"] = "non-USA"
TRAIN$NativeCountry[TRAIN$NativeCountry==" Yugoslavia"] = "non-USA"
TRAIN$NativeCountry[TRAIN$NativeCountry==" United-States"] = "USA"
#I'm done modifying the Native Country categorical variables, so I'm converting them back to factors. 
TRAIN$NativeCountry = factor(TRAIN$NativeCountry)
levels(TRAIN$NativeCountry)



#############   3)  CONCEPT HIERARCHY FOR TEST DATASET
#Because i am going to be modifying the text directly, 
#i converted them to character strings. 
# This is done for all the text variables i intend to work with.
TEST$Edu_Level = as.character(TEST$Edu_Level)
TEST$MaritalStat = as.character(TEST$MaritalStat)
TEST$Occupation = as.character(TEST$Occupation)
TEST$WorkClass= as.character(TEST$WorkClass)
TEST$NativeCountry= as.character(TEST$NativeCountry)


TEST$Edu_Level[TEST$Edu_Level==" Preschool"] = "Dropout"
TEST$Edu_Level[TEST$Edu_Level==" 1st-4th"] = "Dropout"
TEST$Edu_Level[TEST$Edu_Level==" 5th-6th"] = "Dropout"
TEST$Edu_Level[TEST$Edu_Level==" 7th-8th"] = "Dropout"
TEST$Edu_Level[TEST$Edu_Level==" 9th"] = "Dropout"
TEST$Edu_Level[TEST$Edu_Level==" 10th"] = "Dropout"
TEST$Edu_Level[TEST$Edu_Level==" 11th"] = "Dropout"
TEST$Edu_Level[TEST$Edu_Level==" 12th"] = "Dropout"
TEST$Edu_Level[TEST$Edu_Level==" HS-grad"] = "HS-Graduate"
TEST$Edu_Level[TEST$Edu_Level==" Some-college"] = "HS-Graduate"
TEST$Edu_Level[TEST$Edu_Level==" Assoc-acdm"] = "Associates"
TEST$Edu_Level[TEST$Edu_Level==" Assoc-voc"] = "Associates"
TEST$Edu_Level[TEST$Edu_Level==" Bachelors"] = "Bachelors"
TEST$Edu_Level[TEST$Edu_Level==" Masters"] = "Masters"
TEST$Edu_Level[TEST$Edu_Level==" Doctorate"] = "Doctorate"
TEST$Edu_Level[TEST$Edu_Level==" Prof-school"] = "Prof-school"
#I'm done modifying the Edu_Level categorical variables, so I'm converting them back to factors. 
TEST$Edu_Level = factor(TEST$Edu_Level)
levels(TEST$Edu_Level)

TEST$MaritalStat[TEST$MaritalStat==" Never-married"] = "Never-married"
TEST$MaritalStat[TEST$MaritalStat==" Married-AF-spouse"] = "Married"
TEST$MaritalStat[TEST$MaritalStat==" Married-civ-spouse"] = "Married"
TEST$MaritalStat[TEST$MaritalStat==" Married-spouse-absent"] = "Married"
TEST$MaritalStat[TEST$MaritalStat==" Separated"] = "Separated"
TEST$MaritalStat[TEST$MaritalStat==" Divorced"] = "Divorced"
TEST$MaritalStat[TEST$MaritalStat==" Widowed"] = "Widowed"
#I'm done modifying the Marital Status categorical variables, so I'm converting them back to factors. 
TEST$MaritalStat = factor(TEST$MaritalStat)
levels(TEST$MaritalStat)

TEST$WorkClass[TEST$WorkClass==" Federal-gov"] = "Federal-gov"
TEST$WorkClass[TEST$WorkClass==" Local-gov"] = "Local-gov"
TEST$WorkClass[TEST$WorkClass==" State-gov"] = "State-gov"
TEST$WorkClass[TEST$WorkClass==" Private"] = "Private"
TEST$WorkClass[TEST$WorkClass==" Self-emp-inc"] = "Self-Employed"
TEST$WorkClass[TEST$WorkClass==" Self-emp-not-inc"] = "Self-Employed"
TEST$WorkClass[TEST$WorkClass==" Without-pay"] = "Without-pay"
#I'm done modifying the Work Class categorical variables, so I'm converting them back to factors. 
TEST$WorkClass = factor(TEST$WorkClass)
levels(TEST$WorkClass)

TEST$Occupation[TEST$Occupation==" Adm-clerical"] = "Adm-clerical"
TEST$Occupation[TEST$Occupation==" Armed-Forces"] = "Armed-Forces"
TEST$Occupation[TEST$Occupation==" Exec-managerial"] = "White-Collar"
TEST$Occupation[TEST$Occupation==" Craft-repair"] = "Blue-Collar"
TEST$Occupation[TEST$Occupation==" Farming-fishing"] = "Blue-Collar"
TEST$Occupation[TEST$Occupation==" Handlers-cleaners"] = "Blue-Collar"
TEST$Occupation[TEST$Occupation==" Machine-op-inspct"] = "Blue-Collar"
TEST$Occupation[TEST$Occupation==" Transport-moving"] = "Blue-Collar"
TEST$Occupation[TEST$Occupation==" Priv-house-serv"] = "Service"
TEST$Occupation[TEST$Occupation==" Other-service"] = "Service"
TEST$Occupation[TEST$Occupation==" Protective-serv"] = "Service"
TEST$Occupation[TEST$Occupation==" Prof-specialty"] = "Prof-specialty"
TEST$Occupation[TEST$Occupation==" Sales"] = "Sales"
TEST$Occupation[TEST$Occupation==" Tech-support"] = "Tech-support"
#I'm done modifying the Occupation categorical variables, so I'm converting them back to factors. 
TEST$Occupation = factor(TEST$Occupation)
levels(TEST$Occupation)


TEST$NativeCountry[TEST$NativeCountry==" Cambodia"] = "non-USA"
TEST$NativeCountry[TEST$NativeCountry==" Canada"] = "non-USA"
TEST$NativeCountry[TEST$NativeCountry==" China"] = "non-USA"
TEST$NativeCountry[TEST$NativeCountry==" Columbia"] = "non-USA"
TEST$NativeCountry[TEST$NativeCountry==" Cuba"] = "non-USA"
TEST$NativeCountry[TEST$NativeCountry==" Dominican-Republic"] = "non-USA"
TEST$NativeCountry[TEST$NativeCountry==" Ecuador"] = "non-USA"
TEST$NativeCountry[TEST$NativeCountry==" El-Salvador"] = "non-USA"
TEST$NativeCountry[TEST$NativeCountry==" England"] = "non-USA"
TEST$NativeCountry[TEST$NativeCountry==" France"] = "non-USA"
TEST$NativeCountry[TEST$NativeCountry==" Germany"] = "non-USA"
TEST$NativeCountry[TEST$NativeCountry==" Greece"] = "non-USA"
TEST$NativeCountry[TEST$NativeCountry==" Guatemala"] = "non-USA"
TEST$NativeCountry[TEST$NativeCountry==" Haiti"] = "non-USA"
TEST$NativeCountry[TEST$NativeCountry==" Holand-Netherlands"] = "non-USA"
TEST$NativeCountry[TEST$NativeCountry==" Honduras"] = "non-USA"
TEST$NativeCountry[TEST$NativeCountry==" Hong"] = "non-USA"
TEST$NativeCountry[TEST$NativeCountry==" Hungary"] = "non-USA"
TEST$NativeCountry[TEST$NativeCountry==" India"] = "non-USA"
TEST$NativeCountry[TEST$NativeCountry==" Iran"] = "non-USA"
TEST$NativeCountry[TEST$NativeCountry==" Ireland"] = "non-USA"
TEST$NativeCountry[TEST$NativeCountry==" Italy"] = "non-USA"
TEST$NativeCountry[TEST$NativeCountry==" Jamaica"] = "non-USA"
TEST$NativeCountry[TEST$NativeCountry==" Japan"] = "non-USA"
TEST$NativeCountry[TEST$NativeCountry==" Laos"] = "non-USA"
TEST$NativeCountry[TEST$NativeCountry==" Mexico"] = "non-USA"
TEST$NativeCountry[TEST$NativeCountry==" Nicaragua"] = "non-USA"
TEST$NativeCountry[TEST$NativeCountry==" Outlying-US(Guam-USVI-etc)"] = "non-USA"
TEST$NativeCountry[TEST$NativeCountry==" Peru"] = "non-USA"
TEST$NativeCountry[TEST$NativeCountry==" Philippines"] = "non-USA"
TEST$NativeCountry[TEST$NativeCountry==" Poland"] = "non-USA"
TEST$NativeCountry[TEST$NativeCountry==" Portugal"] = "non-USA"
TEST$NativeCountry[TEST$NativeCountry==" Puerto-Rico"] = "non-USA"
TEST$NativeCountry[TEST$NativeCountry==" Scotland"] = "non-USA"
TEST$NativeCountry[TEST$NativeCountry==" South"] = "non-USA"
TEST$NativeCountry[TEST$NativeCountry==" Taiwan"] = "non-USA"
TEST$NativeCountry[TEST$NativeCountry==" Thailand"] = "non-USA"
TEST$NativeCountry[TEST$NativeCountry==" Trinadad&Tobago"] = "non-USA"
TEST$NativeCountry[TEST$NativeCountry==" Vietnam"] = "non-USA"
TEST$NativeCountry[TEST$NativeCountry==" Yugoslavia"] = "non-USA"
TEST$NativeCountry[TEST$NativeCountry==" United-States"] = "USA"
#I'm done modifying the Native Country categorical variables, so I'm converting them back to factors. 
TEST$NativeCountry = factor(TEST$NativeCountry)
levels(TEST$NativeCountry)
#################################################


#########  COMBINED DATASET (TEST AND TRAIN) GRAPHICAL NREPRESENTATIONS
# Visualizing Distribution of categorical variables in combined dataset using Bar plots
qplot(COMBINE$Relationship, xlab = 'Relationship', ylab = 'Count',  main='Bar Plot: Distribution of Relationship')
#3D piechart showing how many relationship in each category
slices <-table(COMBINE$Relationship)
lbls <- levels(COMBINE$Relationship)
pct <- round(slices/sum(slices)*100)
lbls <- paste0(lbls, ": ", pct, "%") # add percents to labels
pie3D(slices,
      labels = lbls,
      col=rainbow(length(lbls)),
      main="\n \n \n \n \n \n \n \n \n \n \n 3D pie Chart showing Distribution of Relationship",cex.main=0.8)


qplot(COMBINE$Race, xlab = 'Race', ylab = 'Count',  main='Bar Plot: Distribution of Race')
#3D piechart showing how many race in each category
slices <-table(COMBINE$Race)
lbls <- levels(COMBINE$Race)
pct <- round(slices/sum(slices)*100)
lbls <- paste0(lbls, ": ", pct, "%") # add percents to labels
pie3D(slices,
      labels = lbls,
      col=rainbow(length(lbls)),
      main="\n \n \n \n \n \n \n \n \n \n \n \n \n \n 3D pie Chart showing Distribution of Race",cex.main=0.8)

qplot(COMBINE$Sex, xlab = 'Sex', ylab = 'Count',  main='Bar Plot: Distribution of Sex')
#3D piechart showing how many sex in each category
slices <-table(COMBINE$Sex)
lbls <- levels(COMBINE$Sex)
pct <- round(slices/sum(slices)*100)
lbls <- paste0(lbls, ": ", pct, "%") # add percents to labels
pie3D(slices,
      labels = lbls,
      col=rainbow(length(lbls)),
      main="\n \n \n \n \n \n \n \n \n \n \n \n \n \n 3D pie Chart showing Distribution of Sex",cex.main=0.8)


qplot(COMBINE$WorkClass, xlab = 'Work Class', ylab = 'Count',  main='Bar Plot: Distribution of Work Class after Transformation')

qplot(COMBINE$Edu_Level, xlab = 'Education Level', ylab = 'Count',  main='Bar Plot: Distribution of Education Level after Transformation')
qplot(COMBINE$MaritalStat, xlab = 'Marital Status', ylab = 'Count',  main='Bar Plot: Distribution of Marital Status after Transformation')
qplot(COMBINE$Occupation, xlab = 'Occupation', ylab = 'Count',  main='Bar Plot: Distribution of Occupation after Transformation')
qplot(COMBINE$NativeCountry, xlab = 'Native Country', ylab = 'Count',  main='Bar Plot: Distribution of Native Country after Transformation')
#3D piechart showing how many countries in each category
slices <-table(COMBINE$NativeCountry)
lbls <- levels(COMBINE$NativeCountry)
pct <- round(slices/sum(slices)*100)
lbls <- paste0(lbls, ": ", pct, "%") # add percents to labels
pie3D(slices,
      labels = lbls,
      col=rainbow(length(lbls)),
      main="\n \n \n \n \n \n \n \n \n \n \n \n \n \n \n 3D Pie Chart of Native Country after transforamtion",cex.main=0.8)

qplot(COMBINE$Class, xlab = 'Class', ylab = 'Count',  main='Bar Plot: Distribution of Class label attribute')
#3D piechart showing how many relationship in each category
slices <-table(COMBINE$Class)
lbls <- levels(COMBINE$Class)
pct <- round(slices/sum(slices)*100)
lbls <- paste0(lbls, ": ", pct, "%") # add percents to labels
pie3D(slices,
      labels = lbls,
      col=rainbow(length(lbls)),
      main="\n \n \n \n \n \n \n \n \n \n \n \n \n \n  3D pie Chart showing Distribution of Class label",cex.main=0.8)

# Visualizing Distribution of continous variables using Bar plots
# Histogram: To see some continous variables distribution
nt <- COMBINE[,c(1,9,10,11)]
library(e1071)  # for using skewness function
apply(nt,2,skewness)
hist(COMBINE$Age,cex.main=0.8,xlab = 'Age',ylab='Frequency',main = 'Histogram: Distribution of Age')
hist(COMBINE$HPW,cex.main=0.8,xlab = 'Hours per week',ylab='Frequency',main = 'Histogram: Distribution of Hours per week')

######################################         
################### BIVARIATE DATA ANALYSIS   #####################
######### i.	Numeric-to-categorical relationship
# box plots for each category/group to understand data distribution and identify outliers 
par(mfrow=c(1,2))
Boxplot(Age~Class,data=COMBINE,main = "Box plot of Age versus Income level",cex.main=1.0, xlab = "Income level",col = (c("green","blue")), ylab = "Age",id.method="none")
Boxplot(HPW~Class,data=COMBINE,main = "Box plot of Hours/week vs Income level", cex.main=1.0,xlab = "Imcome level",col = (c("green","blue")), ylab = "HPW",id.method="none")
par(mfrow=c(1,1))         

# mean for each category using aggregate function / Descriptive statistics by group using aggregate()
aggregate(Age~Class,data=COMBINE,FUN = mean)
aggregate(HPW~Class,data=COMBINE,FUN = mean)

######### ii.	Numeric-to-numeric relationship: 
# Scatterplot matrix : to check relationship between continous variables
panel.cor <- function(x, y, digits = 2, prefix = "r = ", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient
  r <- (cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt) 
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt,cex = 2.5)

}

pairs(nt, lower.panel = panel.smooth, upper.panel = panel.cor,main="Correlation Plot of continous variables")  # with smooth line

########## iii.	Categorical-to-categorical relationship
##grouped barplot of the Cars93 dataset by origin (USA/non USA) broken down by car transmission (automatic/manual).
#legend indicates the color coding of the categorical variables
par(mfrow=c(1,1))
#cross-tabulation of  WorkClass and Income level
total <- table(COMBINE$WorkClass, COMBINE$Class) 
cbind(freq=total, percentage=prop.table(total) * 100)
colors<- c("darkblue","red",'green',"pink","yellow","brown",'purple','orange')      
title=" Distribution by WorkClass and Income level"  
barplot(total,main=title,xlab="Income level", ylab="count from dataset", col=colors,
        legend=rownames(total),beside=TRUE)

#cross-tabulation of  Occupation and Income level
total <- table(COMBINE$Occupation, COMBINE$Class) 
cbind(freq=total, percentage=prop.table(total) * 100)
colors<- c("darkblue","red",'green',"pink","yellow","brown",'purple','orange')      
title=" Distribution by Occupation and Income level"  
barplot(total,main=title,xlab="Income level", ylab="count from dataset", col=colors,
        legend=rownames(total),beside=TRUE)

#cross-tabulation of marital status and Income level
total <- table(COMBINE$MaritalStat, COMBINE$Class) 
cbind(freq=total, percentage=prop.table(total) * 100)
colors<- c("darkblue","red",'green',"pink","yellow","brown")      
title=" Distribution by Marital Status and Income level"  
barplot(total,main=title,xlab="Income level", ylab="count from dataset", col=colors,
        legend=rownames(total),beside=TRUE)

#cross-tabulation of relationship and Income level
total <- table(COMBINE$Relationship, COMBINE$Class) 
cbind(freq=total, percentage=prop.table(total) * 100)
colors<- c("darkblue","red",'green',"pink","yellow","brown")      
title=" Distribution by Relationship and Income level"  
barplot(total,main=title,xlab="Income level", ylab="count from dataset", col=colors,
        legend=rownames(total),beside=TRUE)

#cross-tabulation of education level and Income level
total <- table(COMBINE$Edu_Level, COMBINE$Class) 
cbind(freq=total, percentage=prop.table(total) * 100)
colors<- c("darkblue","red",'green',"pink","yellow","brown",'purple','orange')      
title=" Distribution by Education level and Income level"  
barplot(total,main=title,xlab="Income level", ylab="count from dataset", col=colors,
        legend=rownames(total),beside=TRUE)


# writing the pre-processed dataset to a CSV file for performing classification in KNIME
write.csv(COMBINE, file = "M:\\Rstudio codes\\COMBINE_PREPROCESS.csv")
write.csv(TEST, file = "M:\\Rstudio codes\\ML_TEST_PREPROC.csv")
write.csv(TRAIN, file = "M:\\Rstudio codes\\ML_TRAIN_PREPROC.csv")

