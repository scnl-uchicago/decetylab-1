p +geom_bar(stat="identity")
p<-ggplot(data2=df, aes(SharedWith, NumberShared)) +geom_bar(stat="identity")
p<-ggplot(data2, aes(SharedWith, NumberShared)) +geom_bar(stat="identity")
ggplot(data2, aes(SharedWith, NumberShared)) +geom_bar(stat="identity")
install.packages("gmailr")
#install.packages("mailR")


# Step 1. load in the data
data = read.csv("/Users/jessicalinder/Desktop/Data to Analyze/French_DataC.csv")
str(data)
# change subject "CCF" field to factor
data$CCF = factor(data$CCF)
data$child_gender = factor(data$child_gender)
levels(data$child_gender) = c("M","F")
str(data)

# quick and dirty scatter plot #####

# plot the data as points
plot(data$age_months, data$dictator_stickers_shared)
# run a linear regression to get the trendline
fit = lm(age_months, dictator_stickers_shared, data=data)
# Use the linear regression to plot the best-fit line
abline(fit)
# get correlation coefficient
cor.test(data$age_months,data$dictator_stickers_shared)

fit = lm(DOB, Stickers.Shared, data=data)



data = read.csv("/Users/jessicalinder/Desktop/Data to Analyze/CCF behavioral task data_JMC.csv")
str(data)
fit = lm(age_months, dictator_stickers_shared, data=data)
abline(fit)
# get correlation coefficient
cor.test(data$age_months,data$dictator_stickers_shared)

t.test(data$Stickers.Shared ~ data$Sex)
fit = lm(dictator_stickers_shared ~ child_gender, data=data)
cor.test(data$child_gender, data$dictator_stickers_shared)



# fancy scatterplots ####
#install.packages('ggplot2')
require('ggplot2')
# the syntax is a little strange at first
# load the data into ggplotw, then show points #####
# + geom_point()
ggplot(data, aes(x=age_months, y=dictator_stickers_shared)) +
  geom_point()
# add a linear trendline
# + geom_smooth(method=lm)

ggplot(data, aes(x=age_months, y=dictator_stickers_shared)) +
  geom_point() + geom_smooth(method=lm)
# add gender as a grouping variable
# aes(..., group=child_gender)

ggplot(data, aes(x=age_months, y=dictator_stickers_shared, group=child_gender)) +
  geom_point() + geom_smooth(method=lm)
# add gender as a grouping variable and use difference colors
# aes(... color=child_gender)

ggplot(data, aes(x=age_months, y=dictator_stickers_shared, color=child_gender)) +
  geom_point() + geom_smooth(method=lm)

# separate plots by gender
ggplot(data, aes(x=age_months, y=dictator_stickers_shared)) +
  geom_point() + geom_smooth(method=lm) + facet_wrap(~child_gender)