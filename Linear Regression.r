# Remove all objects in the environment
rm(list=ls())

# Load packages in bulk
pacman::p_load(tidyverse, reshape2, car, corrplot, magrittr, lubridate, BaylorEdPsych, mvnmle, lmtest, caret, sandwich, caTools, MLmetrics)

# Initialise Working Directory and get started
setwd("C:\\Users\\raymo\\OneDrive\\Desktop\\EB5101 - Foundations of Business Analytics\\Assignment")
DIR = "Assignment 1"
file = "2015_Air_quality_in_northern_Taiwan.csv"
data = read.csv(file.path(DIR,file))

# Set Time column as a POSIXct object
data$time = as.POSIXct(data$time)

# Check for observations/rows that have missing values across the entire sequence
missing = data %>%
  filter(!complete.cases(.))
nrow(missing)
nrow(missing)/nrow(data)

# View structure of dataframe
str(data)

# ------------------- DATA PRE-PROCESSING
cleandata <- data

# It is believed that any relationship between PM2.5 and location / time should 
# already be captured by the remaining 22 variables. As such, location and time
# should be removed from further analysis.
cleandata$time <- NULL
cleandata$station <- NULL

# Removal of fields with invalid values
cleandata = as.data.frame(apply(cleandata,2, function(x) ifelse(grepl("x|#|\\*",x),"", x)))

# Replacement of NR values to 0
cleandata$RAINFALL <- ifelse(cleandata$RAINFALL=="NR", 0, cleandata$RAINFALL)

# Removal of fields with "NR"
cleandata = as.data.frame(apply(cleandata,2, function(x) ifelse(x=="NR","", x)))

# Update classes of all fields to numeric
cleandata=apply(cleandata, c(1,2),function(x) as.numeric(x))
cleandata=as.data.frame(cleandata)

#Check for missing data
missing = cleandata %>%
  filter(!complete.cases(.))
nrow(missing)

#Removal of empty fields
cleandata <- cleandata %>%
  filter(complete.cases(.))

file1 = "cleandata.csv"
write.csv(cleandata, file.path(DIR,file1),row.names=F)


# ------------------- SPLIT SAMPLE TO TRAIN AND TEST
set.seed(1234)

splitData <- sample.split(cleandata$PM2.5, SplitRatio = 0.7)
head(splitData); class(splitData)

# Create train dataset
traindata <- cleandata[splitData,]
nrow(traindata)/nrow(cleandata)

# Create test dataset
testdata <- cleandata[!splitData,]
nrow(testdata)/nrow(cleandata)

# -------------------------- MODELLING
#initial model (include all input variables)
model <- lm(formula = PM2.5 ~ . , data=traindata)
summary(model)

#final model
final_model <- lm(formula = PM2.5 ~ . 
                  -NO -NMHC -RAINFALL -RAIN_COND
                  -WIND_SPEED -WIND_DIREC -CH4
                  -NO2 -THC -WD_HR -NOx, 
                  data=traindata)
summary(final_model)
vif(final_model)  #inspect multi-colinearity

#Residual Check
par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(final_model,sub.caption = "Residual Check")
bptest(final_model)

#Transformation of Dependent Variable
traindata = traindata[-which(traindata$PM2.5 == 0),]
PM2.5BCMod = BoxCoxTrans(traindata$PM2.5)
print(PM2.5BCMod)

traindata = cbind(traindata, PM2.5_new = predict(PM2.5BCMod,traindata$PM2.5))
head(traindata)

model_trans = lm(PM2.5_new ~ . -PM2.5 -WIND_DIREC 
                 -NO -WIND_SPEED -RH -CH4 -THC -RAINFALL -RAIN_COND 
                 -WD_HR -NOx -NO2 -UVB, 
                 data = traindata)
summary(model_trans)
bptest(model_trans)
plot(model_trans,sub.caption = "Residual Check")
vif(model_trans)

# Dropping a variable due to multi-collinearity
model_trans = lm(PM2.5_new ~ . -PM2.5 -WIND_DIREC 
                 -NO -WIND_SPEED -RH -CH4 -THC -RAINFALL -RAIN_COND 
                 -WD_HR -NOx -NO2 -UVB -NMHC, 
                 data = traindata)
summary(model_trans)
vif(model_trans)
plot(model_trans,sub.caption = "Residual Check")

#Model Validation
testdata = testdata[-which(testdata$PM2.5==1),]

#Transformation of PM2.5
testdata$PM2.5_new = (testdata$PM2.5^0.4-1)/0.4
predicted_data = predict(model_trans,newdata=testdata)
MAPE(predicted_data, testdata$PM2.5_new)

# MAPE of Original Model
testdata <- cleandata[!splitData,]
testdata = testdata[-which(testdata$PM2.5==0),]
predicted_data = predict(final_model, newdata = testdata)
MAPE(predicted_data, testdata$PM2.5)

file2 = "predicted_data.csv"
write.csv(predicted_data, file.path(DIR,file2),row.names=F)
file3 = "testdata.csv"
write.csv(testdata, file.path(DIR,file3),row.names=F)
