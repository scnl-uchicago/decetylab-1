# Install packages
install.packages("corrplot")
install.packages("ggplot2")
install.packages("plyr")
library(ggplot2)
library(plyr)
library(corrplot)

# Prepare workspace
rm(list=ls())

# Read and prepare data
#usa_data <- read.csv('/Volumes/decetylab/lab_members/kozloff/decetylab/analysis/cross_cultural_fairness/data/clean_data/usa_behavioral_task_data.csv')
usa_data <- read.csv('/Volumes/decetylab/lab_members/kozloff/decetylab/analysis/cross_cultural_fairness/data/clean_data/usa_combined_data.csv')

# Remove outliers
usa_data <- usa_data[which(!is.na(usa_data$child_gender)),]
usa_data <- usa_data[which(usa_data$age_years<9),]
usa_data <- usa_data[which(usa_data$Subject!=146),]

# Correct data types
usa_data$age_years <- as.factor(usa_data$age_years)
usa_data$wealth_poor<-as.numeric(usa_data$wealth_poor)
usa_data$merit_lazy<-as.numeric(usa_data$merit_lazy)
usa_data$empathy_hurt<-as.numeric(usa_data$empathy_hurt)
usa_data$dictator_stickers_shared<-as.numeric(usa_data$dictator_stickers_shared)
usa_data$dictator_stickers_shared<-as.numeric(usa_data$dictator_stickers_shared)
usa_data$child_gender[which(usa_data$child_gender==1)]<-"gender1"
usa_data$child_gender[which(usa_data$child_gender==2)]<-"gender2"


# Dictator game by age and gender
dictator_anova = aov(dictator_stickers_shared~age_years*child_gender,data =usa_data)
summary(dictator_anova)

# Without outliers
# Df Sum Sq Mean Sq F value   Pr(>F)    
# age_years                4  180.0   44.99   7.184 3.79e-05 ***
#   child_gender             1   33.2   33.23   5.307   0.0232 *  
#   age_years:child_gender   4   53.8   13.46   2.149   0.0800 .  
# Residuals              104  651.3    6.26                     
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#+++++++++++++++++++++++++
# Function to calculate the mean and the standard deviation
# for each group
# Created by http://www.sthda.com/english/wiki/ggplot2-line-plot-quick-start-guide-r-software-and-data-visualization
#+++++++++++++++++++++++++
# data : a data frame
# varname : the name of a column containing the variable
#to be summariezed
# groupnames : vector of column names to be used as
# grouping variables
data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

dictator_summary <- data_summary(usa_data, varname="dictator_stickers_shared", 
                                 groupnames=c("child_gender", "age_years"))
head(dictator_summary)


ggplot(dictator_summary, aes(x=age_years, y=dictator_stickers_shared, group=child_gender, color=child_gender)) + 
  # geom_errorbar(aes(ymin=dictator_stickers_shared-sd, ymax=dictator_stickers_shared+sd), width=.1) +
  geom_line() + geom_point()+
  scale_color_brewer(palette="Paired")+theme_minimal() +
  xlab("Age (years)") + ylab("Number stickers") +
  theme(axis.text.x = element_text(color = "grey20", size = 12, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 12, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12))


# Distributive justice (empathy) by age and gender
hurt_anova = aov(empathy_hurt~age_years*child_gender,data =usa_data)
summary(hurt_anova)

# Without outliers:
# Df Sum Sq Mean Sq F value Pr(>F)
# age_years                4   2.80  0.6996   1.011  0.405
# child_gender             1   0.29  0.2931   0.424  0.517
# age_years:child_gender   4   2.08  0.5206   0.752  0.559
# Residuals              104  71.95  0.6918   

empathy_hurt_summary <- data_summary(usa_data, varname="empathy_hurt", 
                                     groupnames=c("child_gender", "age_years"))
head(empathy_hurt_summary)


ggplot(empathy_hurt_summary, aes(x=age_years, y=empathy_hurt, group=child_gender, color=child_gender)) + 
  # geom_errorbar(aes(ymin=empathy_hurt-sd, ymax=empathy_hurt+sd), width=.1) +
  geom_line() + geom_point()+
  scale_color_brewer(palette="Paired")+theme_minimal() +
  xlab("Age (years)") + ylab("Number stickers") +
  theme(axis.text.x = element_text(color = "grey20", size = 12, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 12, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12))






# Distributive justice (wealth) by age and gender
wealth_anova = aov(wealth_poor~age_years*child_gender,data =usa_data)
summary(wealth_anova)

# Without outliers:
# Df Sum Sq Mean Sq F value Pr(>F)  
# age_years                4   6.15  1.5374   3.137 0.0176 *
#   child_gender             1   0.06  0.0634   0.129 0.7199  
# age_years:child_gender   4   1.81  0.4534   0.925 0.4524  
# Residuals              104  50.96  0.4900                 
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

wealth_poor_summary <- data_summary(usa_data, varname="wealth_poor", 
                                    groupnames=c("child_gender", "age_years"))
head(wealth_poor_summary)


ggplot(wealth_poor_summary, aes(x=age_years, y=wealth_poor, group=child_gender, color=child_gender)) + 
  # geom_errorbar(aes(ymin=wealth_poor-sd, ymax=wealth_poor+sd), width=.1) +
  geom_line() + geom_point()+
  scale_color_brewer(palette="Paired")+theme_minimal() +
  xlab("Age (years)") + ylab("Number stickers") +
  theme(axis.text.x = element_text(color = "grey20", size = 12, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 12, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12))


# Distributive justice (merit) by age and gender
merit_anova = aov(merit_lazy~age_years*child_gender,data =usa_data)
summary(merit_anova)

# Df Sum Sq Mean Sq F value   Pr(>F)    
# age_years                4  17.88   4.471   6.639 8.47e-05 ***
#   child_gender             1   0.00   0.000   0.000    0.998    
# age_years:child_gender   4   1.83   0.458   0.680    0.608    
# Residuals              104  70.04   0.673                     
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

merit_lazy_summary <- data_summary(usa_data, varname="merit_lazy", 
                                   groupnames=c("child_gender", "age_years"))
head(merit_lazy_summary)


ggplot(merit_lazy_summary, aes(x=age_years, y=merit_lazy, group=child_gender, color=child_gender)) + 
  # geom_errorbar(aes(ymin=merit_lazy-sd, ymax=merit_lazy+sd), width=.1) +
  geom_line() + geom_point()+
  scale_color_brewer(palette="Paired")+theme_minimal() +
  xlab("Age (years)") + ylab("Number stickers") +
  theme(axis.text.x = element_text(color = "grey20", size = 12, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 12, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12))


# Pearson's correlations
usa_data$age_years<- as.numeric(usa_data$age_years)


# Test correlations
cor.test(usa_data$age_years, usa_data$dictator_stickers_shared)
# Pearson's product-moment correlation
# 
# Pearson's product-moment correlation
# 
# data:  usa_data$age_years and usa_data$dictator_stickers_shared
# t = 4.5103, df = 112, p-value = 1.603e-05
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.2243256 0.5372412
# sample estimates:
#       cor 
# 0.3920651 

cor.test(usa_data$age_years, usa_data$wealth_poor)
# Pearson's product-moment correlation
# 
# data:  usa_data$age_years and usa_data$wealth_poor
# t = 2.3936, df = 112, p-value = 0.01835
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.03823994 0.38874579
# sample estimates:
#       cor 
# 0.2206033

cor.test(usa_data$age_years, usa_data$merit_lazy)

# Pearson's product-moment correlation
# Pearson's product-moment correlation
# 
# data:  usa_data$age_years and usa_data$merit_lazy
# t = -4.9204, df = 112, p-value = 2.991e-06
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  -0.5619382 -0.2576580
# sample estimates:
#        cor 
# -0.4215947 

cor.test(usa_data$wealth_poor, usa_data$merit_lazy)
# Pearson's product-moment correlation
# 
# data:  usa_data$wealth_poor and usa_data$merit_lazy
# t = -2.3635, df = 112, p-value = 0.01983
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  -0.38638396 -0.03546441
# sample estimates:
#        cor 
# -0.2179576 



# Visualize correlations
col_names <- c("age_years", "dictator_stickers_shared", "wealth_poor", "merit_lazy", "empathy_hurt")
correlation_subset <- as.matrix(usa_data[col_names])

colnames(correlation_subset) <- c("Age", "Dictator", "Wealth", "Merit", "Empathy")
res2 <- cor.mtest(correlation_subset, conf.level = .95)
M <- cor(correlation_subset)


corrplot(M, p.mat = res2$p, tl.col="black", insig = "blank")

# age_dictator_corr<-cor.test(usa_data$age_years,usa_data$dictator_stickers_shared)
# age_dictator_corr

# Descriptive
# Number of each gender
gender_1 <- length(usa_data$child_gender[which(usa_data$child_gender=="gender1")])
gender_1
gender_2 <- length(usa_data$child_gender[which(usa_data$child_gender=="gender2")])
gender_2
# Dictator mean and SD
dictator_mean = mean(usa_data$dictator_stickers_shared)
dictator_mean
dictator_sd = sd(usa_data$dictator_stickers_shared)
dictator_sd
# Distributive justice tasks: mean and SD
hurt_mean = mean(usa_data$empathy_hurt)
hurt_mean
hurt_sd = sd(usa_data$empathy_hurt)
hurt_sd
poor_mean = mean(usa_data$wealth_poor)
poor_mean
poor_sd = sd(usa_data$wealth_poor)
poor_sd
lazy_mean = mean(usa_data$merit_lazy)
lazy_mean
lazy_sd = sd(usa_data$merit_lazy)
lazy_sd
