# Step 0. Save XLSX file as CSV

# Step 1. load in the data
data = read.csv("/Users/jessicalinder/Desktop/Data to Analyze/CCF behavioral task data_JMC.csv")
str(data)
# change subject "CCF" field to factor
data$CCF = factor(data$CCF)
data$child_gender = factor(data$child_gender)
levels(data$child_gender) = c("M","F")
str(data)

data$wealth_poor

# quick and dirty scatter plot #####
# plot the data as points
plot(data$wealth_poor, data$wealth_rich)
# run a linear regression to get the trendline
fit = lm(wealth_poor, wealth_rich, data=data)

# Use the linear regression to plot the best-fit line
abline(fit)

# get correlation coefficient
t.test(data$wealth_poor,data$wealth_rich,paired = TRUE)
