# Step 0. Save XLSX file as CSV

setwd("/Users/jessicalinder/Desktop/Data to Analyze/French Data/")
# Step 1. load in the FrenchData
FrenchData = read.csv("/Users/jessicalinder/Desktop/Data to Analyze/French_Data.csv")
str(FrenchData)
# change subject "CCF" field to factor
FrenchData$Subject.Number = factor(FrenchData$Subject.Number)
FrenchData$Sex = factor(FrenchData$Sex)
levels(FrenchData$Sex) = c("M","F")
str(FrenchData)


# quick and dirty scatter plot #####
# plot the FrenchData as points
plot(FrenchData$Sex, FrenchData$Stickers.Shared)
# run a linear regression to get the trendline
Gender.lm = lm(Stickers.Shared ~ Sex, data=FrenchData)
summary(Gender.lm)
# Use the linear regression to plot the best-fit line
abline(fit)
# Age Gender Interaction Model
GenderAge.lm = lm(Stickers.Shared ~ age_months, data=FrenchData)
summary(GenderAge.lm)
# get correlation coefficient
t.test(FrenchData$Stickers.Shared ~ FrenchData$age_months)


