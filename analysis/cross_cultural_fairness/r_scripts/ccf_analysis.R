# Install packages ####
# install.packages("tidyverse")
# install.packages("plyr")
# install.packages("GeneNet")
# install.packages("ggcorrplot")
#install.packages("questionr")
require(Hmisc)
require(tidyverse)
require(corrplot)
require(GeneNet)
require(ggcorrplot)
require(questionr)

# Prepare workspace ####
rm(list=ls())

# Read and prepare data
usa_data <- read.csv('/Volumes/decetylab/lab_members/kozloff/projects/ccf/data/clean_data/usa_combined_data.csv', stringsAsFactors = FALSE)

# Correct data types ####
usa_data$child_gender[which(usa_data$child_gender==1)]<-"male"
usa_data$child_gender[which(usa_data$child_gender==2)]<-"female"
usa_data$child_gender<- as.factor(usa_data$child_gender)
usa_data$wealth_poor<-as.numeric(usa_data$wealth_poor)
usa_data$merit_lazy<-as.numeric(usa_data$merit_lazy)
usa_data$empathy_hurt<-as.numeric(usa_data$empathy_hurt)
usa_data$dictator_stickers_shared<-as.numeric(usa_data$dictator_stickers_shared)
usa_data$dictator_stickers_shared<-as.numeric(usa_data$dictator_stickers_shared)

# Rename IRI scores ####
colnames(usa_data)[75:102] <- c("iri_01", "iri_02", "iri_03", "iri_04", "iri_05", "iri_06", "iri_07", "iri_08", 
                                "iri_09", "iri_10", "iri_11", "iri_12", "iri_13", "iri_14", "iri_15", "iri_16", 
                                "iri_17", "iri_18", "iri_19", "iri_20", "iri_21", "iri_22", "iri_23", "iri_24", 
                                "iri_25", "iri_26", "iri_27", "iri_28")

# Find IRI subscores ####
usa_data$iri_fs <- (usa_data$iri_01+usa_data$iri_05+abs(usa_data$iri_07-5)+abs(usa_data$iri_12-5)+usa_data$iri_16
                    +usa_data$iri_23+usa_data$iri_26-5)

usa_data$iri_pt <- (abs(usa_data$iri_03-5)+usa_data$iri_08+usa_data$iri_11+abs(usa_data$iri_15-5)+usa_data$iri_21
                    +usa_data$iri_25+usa_data$iri_28-5)

usa_data$iri_ec <- (usa_data$iri_02-1+abs(usa_data$iri_04-5)+usa_data$iri_09-1+abs(usa_data$iri_14-5)
                    +abs(usa_data$iri_18-5)+usa_data$iri_20-1+usa_data$iri_22-1)

usa_data$iri_pd <- (usa_data$iri_06-1+usa_data$iri_10-1+abs(usa_data$iri_13-5)+usa_data$iri_17-1
                    +abs(usa_data$iri_19-5)+usa_data$iri_24-1+usa_data$iri_27-1)


# Find number of siblings
colnames(usa_data)[colnames(usa_data)=="Child.s.Birth.Order..out.of.____.children"] <- "total_children"
usa_data$siblings <- usa_data$total_children-1

# Find justice sensitivity subscores ####

# Rename justice sensitivity scores 
colnames(usa_data)[147:154] <- c("js_1","js_2","js_3","js_4","js_5","js_6","js_7","js_8")

# Find justice sensitivity subscores
usa_data$js_victim <- (usa_data$js_1 + usa_data$js_2)
usa_data$js_observer <- (usa_data$js_3 + usa_data$js_4)
usa_data$js_beneficiary <- (usa_data$js_5 + usa_data$js_6)
usa_data$js_perpetrator <- (usa_data$js_7 + usa_data$js_8)

# Find total justice sensitivity
usa_data$js_total <- (usa_data$js_victim+usa_data$js_observer+usa_data$js_beneficiary+usa_data$js_perpetrator)/4

# Code lying
usa_data$lying <- NA
# These people admitted to peeking (deception_correct is if they peeked. deception_peeked is if they said they peeked)
usa_data[which(usa_data$deception_correct == 1 & usa_data$deception_peeked==1),]$lying <- 0
# These people peeked, but lied
usa_data[which(usa_data$deception_correct == 1 & usa_data$deception_peeked==2),]$lying <- 1
# These people did not peek or report peeking
usa_data[which(usa_data$deception_correct == 2 & usa_data$deception_peeked==2),]$lying <- 0
usa_data$lying <- as.factor(usa_data$lying)

# Rename peeking column
colnames(usa_data)[colnames(usa_data)=="deception_correct"] <- "peeking"

# 7 was considered "other" for maternal education. Find these values 
maternal_ed_other <- (cbind(usa_data$Subject, usa_data$Level.of.formal.education.mother.completed, usa_data$Please.specify)[which(usa_data$Level.of.formal.education.mother.completed==7),])
# Hand-convert those who selected 7 to a scale of 1-6 based on their response — these ones all seem to have completed some college
usa_data[which(usa_data$Subject%in%c(152,156,160, 201)),]$Level.of.formal.education.mother.completed <- 3

# Maternal education was scored from 1(highest) to 6 (lowest), while SES was 1(lowest) to 9 (highest). Reverse coding for maternal education
usa_data$corrected_ed <- NA
if(length(usa_data[which(usa_data$Level.of.formal.education.mother.completed==6),]$corrected_ed) > 0){
  usa_data[which(usa_data$Level.of.formal.education.mother.completed==6),]$corrected_ed <- 1}
if(length(usa_data[which(usa_data$Level.of.formal.education.mother.completed==5),]$corrected_ed) > 0){
  usa_data[which(usa_data$Level.of.formal.education.mother.completed==5),]$corrected_ed <- 2}
usa_data[which(usa_data$Level.of.formal.education.mother.completed==4),]$corrected_ed <- 3
usa_data[which(usa_data$Level.of.formal.education.mother.completed==3),]$corrected_ed <- 4
usa_data[which(usa_data$Level.of.formal.education.mother.completed==2),]$corrected_ed <- 5
usa_data[which(usa_data$Level.of.formal.education.mother.completed==1),]$corrected_ed <- 6

hist(usa_data$Total.family.income.last.year, xlab = "Total Family Income", main = "Frequency of Total Family Income")
hist(usa_data$corrected_ed, xlab = "Maternal Education", main = "Frequency of Maternal Education")

# z-score SES measures
usa_data$maternal_education <- scale(usa_data$corrected_ed)
usa_data$income <- scale(usa_data$Total.family.income.last.year)

# Average z-scored maternal education and income to create combined SES variable
usa_data$ses <- rowMeans(usa_data[c('maternal_education', 'income')], na.rm=TRUE)

hist(usa_data$corrected_ed, xlab = "SES Rank", main = "Frequency of Composite SES Rank")


# Subset only the variables of interest
usa_data <- usa_data %>% select (Subject, child_gender, age_years, age_months, peeking, lying, maternal_education, income, ses, siblings,
                                 iri_ec, iri_pd, iri_pt, iri_fs, js_victim, js_perpetrator, js_observer, js_beneficiary, js_total, 
                                 dictator_stickers_shared, wealth_poor, wealth_rich, empathy_hurt, empathy_unhurt, merit_lazy, merit_work)

# Descriptive ####

# Number of each gender
tapply(usa_data$age_months, usa_data$child_gender, length)

# Find mean and standard deviation of non-scaled income and maternal education
(mean(usa_data$Total.family.income.last.year, na.rm = TRUE))
(sd(usa_data$Total.family.income.last.year, na.rm = TRUE))
(mean(usa_data$corrected_ed, na.rm = TRUE))
(sd(usa_data$corrected_ed, na.rm = TRUE))
# Find mean and standard deviation of scaled and averaged SES
(mean(usa_data$ses, na.rm = TRUE))
(sd(usa_data$ses, na.rm = TRUE))

# Find how many had at least a high school education
length(usa_data$corrected_ed[which(usa_data$corrected_ed>3)])/ length(usa_data$corrected_ed)

# Find mean and standard deviation of scaled and averaged SES
(mean(usa_data$ses, na.rm = TRUE))
(sd(usa_data$ses, na.rm = TRUE))

# Find mean and standard deviation of siblings
mean(usa_data$siblings, na.rm = TRUE)
sd(usa_data$siblings, na.rm = TRUE)

# Histogram of SES measures
hist(usa_data$age_months)

# Find mean and standard deviation of parent IRI scores
mean(usa_data$iri_ec, na.rm = TRUE)
sd(usa_data$iri_ec, na.rm = TRUE)
mean(usa_data$iri_fs, na.rm = TRUE)
sd(usa_data$iri_fs, na.rm = TRUE)
mean(usa_data$iri_pd, na.rm = TRUE)
sd(usa_data$iri_pd, na.rm = TRUE)
mean(usa_data$iri_pt, na.rm = TRUE)
sd(usa_data$iri_pt, na.rm = TRUE)

# Find mean and standard deviation of child JS scores
mean(usa_data$js_total, na.rm = TRUE)
sd(usa_data$js_total, na.rm = TRUE)
mean(usa_data$js_beneficiary, na.rm = TRUE)
sd(usa_data$js_beneficiary, na.rm = TRUE)
mean(usa_data$js_perpetrator, na.rm = TRUE)
sd(usa_data$js_perpetrator, na.rm = TRUE)
mean(usa_data$js_victim, na.rm = TRUE)
sd(usa_data$js_victim, na.rm = TRUE)
mean(usa_data$js_observer, na.rm = TRUE)
sd(usa_data$js_observer, na.rm = TRUE)

# Dictator mean and SD
dictator_mean = mean(usa_data$dictator_stickers_shared, na.rm = TRUE)
dictator_mean
dictator_sd = sd(usa_data$dictator_stickers_shared, na.rm = TRUE)
dictator_sd

# Distributive justice tasks: mean and SD
lazy_mean = mean(usa_data$merit_lazy, na.rm = TRUE)
lazy_mean
lazy_sd = sd(usa_data$merit_lazy, na.rm = TRUE)
lazy_sd

poor_mean = mean(usa_data$wealth_poor, na.rm = TRUE)
poor_mean
poor_sd = sd(usa_data$wealth_poor, na.rm = TRUE)
poor_sd

hurt_mean = mean(usa_data$empathy_hurt, na.rm = TRUE)
hurt_mean
hurt_sd = sd(usa_data$empathy_hurt, na.rm = TRUE)
hurt_sd

#Find how many peeked/lied
npeeked<-length(usa_data[which(usa_data$peeking==1),]$lying)
percent_peeked <- npeeked/(length(usa_data$Subject))
length(usa_data[which(usa_data$lying==1&usa_data$peeking==1),]$lying)/npeeked


# Remove outliers ###

# Remove outliers based on age — removes 2 participants
age_months_sd <- sd(usa_data$age_months, na.rm = TRUE)
age_months_sd
age_months_mean <- mean(usa_data$age_months, na.rm = TRUE)
age_months_mean
usa_data_clean <- usa_data[which(usa_data$age_months > age_months_mean-(2.5*age_months_sd) & usa_data$age_months < age_months_mean+(2.5*age_months_sd)),]

# Remove outliers based on maternal education - removes 3
maternal_education_sd <- sd(usa_data$maternal_education, na.rm = TRUE)
maternal_education_sd
maternal_education_mean <- mean(usa_data$maternal_education, na.rm = TRUE)
maternal_education_mean
usa_data_clean <- usa_data_clean[which(usa_data_clean$maternal_education > maternal_education_mean-(2.5*maternal_education_sd) & usa_data_clean$maternal_education < maternal_education_mean+(2.5*maternal_education_sd)),]

# Remove outliers based on income - removes 4
income_sd <- sd(usa_data$income, na.rm = TRUE)
income_sd
income_mean <- mean(usa_data$income, na.rm = TRUE)
income_mean
usa_data_clean <- usa_data_clean[which(usa_data_clean$income > income_mean-(2.5*income_sd) & usa_data_clean$income < income_mean+(2.5*income_sd)),]

# Remove one person who did not understand task and did not complete it
usa_data_clean <- usa_data_clean[which(usa_data_clean$Subject!=146),]

# Remove outliers based on ses
# ses_sd <- sd(usa_data$ses, na.rm = TRUE)
# ses_sd
# ses_mean <- mean(usa_data$ses, na.rm = TRUE)
# ses_mean
# usa_data_clean <- usa_data_clean[which(usa_data_clean$ses > ses_mean-(2.5*ses_sd) & usa_data_clean$ses < ses_mean+(2.5*ses_sd)),]

# Remove outliers based on iri_ec
# iri_ec_sd <- sd(usa_data$iri_ec, na.rm = TRUE)
# iri_ec_sd
# iri_ec_mean <- mean(usa_data$iri_ec, na.rm = TRUE)
# iri_ec_mean
# usa_data_clean <- usa_data_clean[which(usa_data_clean$iri_ec > iri_ec_mean-(2.5*iri_ec_sd) & usa_data_clean$iri_ec < iri_ec_mean+(2.5*iri_ec_sd)),]

# Remove outliers based on iri_pd
# iri_pd_sd <- sd(usa_data$iri_pd, na.rm = TRUE)
# iri_pd_sd
# iri_pd_mean <- mean(usa_data$iri_pd, na.rm = TRUE)
# iri_pd_mean
# usa_data_clean <- usa_data_clean[which(usa_data_clean$iri_pd > iri_pd_mean-(2.5*iri_pd_sd) & usa_data_clean$iri_pd < iri_pd_mean+(2.5*iri_pd_sd)),]

# Remove outliers based on iri_pt
# iri_pt_sd <- sd(usa_data$iri_pt, na.rm = TRUE)
# iri_pt_sd
# iri_pt_mean <- mean(usa_data$iri_pt, na.rm = TRUE)
# iri_pt_mean
# usa_data_clean <- usa_data_clean[which(usa_data_clean$iri_pt > iri_pt_mean-(2.5*iri_pt_sd) & usa_data_clean$iri_pt < iri_pt_mean+(2.5*iri_pt_sd)),]

# Remove outliers based on iri_fs
# iri_fs_sd <- sd(usa_data$iri_fs, na.rm = TRUE)
# iri_fs_sd
# iri_fs_mean <- mean(usa_data$iri_fs, na.rm = TRUE)
# iri_fs_mean
# usa_data_clean <- usa_data_clean[which(usa_data_clean$iri_fs > iri_fs_mean-(2.5*iri_fs_sd) & usa_data_clean$iri_fs < iri_fs_mean+(2.5*iri_fs_sd)),]

# Remove outliers based on dictator_stickers_shared
# dictator_stickers_shared_sd <- sd(usa_data$dictator_stickers_shared, na.rm = TRUE)
# dictator_stickers_shared_sd
# dictator_stickers_shared_mean <- mean(usa_data$dictator_stickers_shared, na.rm = TRUE)
# dictator_stickers_shared_mean
# usa_data_clean <- usa_data_clean[which(usa_data_clean$dictator_stickers_shared > dictator_stickers_shared_mean-(2.5*dictator_stickers_shared_sd) & usa_data_clean$dictator_stickers_shared < dictator_stickers_shared_mean+(2.5*dictator_stickers_shared_sd)),]

# Remove outliers based on wealth_poor
# wealth_poor_sd <- sd(usa_data$wealth_poor, na.rm = TRUE)
# wealth_poor_sd
# wealth_poor_mean <- mean(usa_data$wealth_poor, na.rm = TRUE)
# wealth_poor_mean
# usa_data_clean <- usa_data_clean[which(usa_data_clean$wealth_poor > wealth_poor_mean-(2.5*wealth_poor_sd) & usa_data_clean$wealth_poor < wealth_poor_mean+(2.5*wealth_poor_sd)),]

# Remove outliers based on empathy_hurt
# empathy_hurt_sd <- sd(usa_data$empathy_hurt, na.rm = TRUE)
# empathy_hurt_sd
# empathy_hurt_mean <- mean(usa_data$empathy_hurt, na.rm = TRUE)
# empathy_hurt_mean
# usa_data_clean <- usa_data_clean[which(usa_data_clean$empathy_hurt > empathy_hurt_mean-(2.5*empathy_hurt_sd) & usa_data_clean$empathy_hurt < empathy_hurt_mean+(2.5*empathy_hurt_sd)),]

# Remove outliers based on merit_lazy
# merit_lazy_sd <- sd(usa_data$merit_lazy, na.rm = TRUE)
# merit_lazy_sd
# merit_lazy_mean <- mean(usa_data$merit_lazy, na.rm = TRUE)
# merit_lazy_mean
# usa_data_clean <- usa_data_clean[which(usa_data_clean$merit_lazy > merit_lazy_mean-(2.5*merit_lazy_sd) & usa_data_clean$merit_lazy < merit_lazy_mean+(2.5*merit_lazy_sd)),]

# age_years_sd <- sd(as.integer(usa_data$age_years))
# age_years_sd
# age_years_mean <- mean(as.integer(usa_data$age_years))
# age_years_mean
# usa_data_clean <- usa_data_clean[which(as.integer(usa_data_clean$age_years) > age_years_mean-(2.5*age_years_sd) & as.integer(usa_data_clean$age_years < age_years_mean+(2.5*age_years_sd))),]

# Remove one person who did not report gender
# usa_data_clean <- usa_data_clean[which(!is.na(usa_data_clean$child_gender)),]

# Second Set of Descriptive, With Outliers Removed ####

# Number of each gender
tapply(usa_data_clean$age_months, usa_data_clean$child_gender, length)
summary(usa_data_clean$age_years)
summary(usa_data_clean$age_months)

# Find mean and standard deviation of non-scaled income and maternal education
(mean(usa_data_clean$Total.family.income.last.year, na.rm = TRUE))
(sd(usa_data_clean$Total.family.income.last.year, na.rm = TRUE))
(mean(usa_data_clean$corrected_ed, na.rm = TRUE))
(sd(usa_data_clean$corrected_ed, na.rm = TRUE))

# Find how many had at least a high school education
length(usa_data_clean$corrected_ed[which(usa_data_clean$corrected_ed>3)])/ length(usa_data_clean$corrected_ed)

# Find mean and standard deviation of scaled and averaged SES
(mean(usa_data_clean$ses, na.rm = TRUE))
(sd(usa_data_clean$ses, na.rm = TRUE))

# Find mean and standard deviation of scaled and averaged SES
(mean(usa_data_clean$ses, na.rm = TRUE))
(sd(usa_data_clean$ses, na.rm = TRUE))

# Find mean and standard deviation of siblings
mean(usa_data_clean$siblings, na.rm = TRUE)
sd(usa_data_clean$siblings, na.rm = TRUE)

# Histogram of SES measures
hist(usa_data_clean$age_months)

# Find mean and standard deviation of parent IRI scores
mean(usa_data_clean$iri_ec, na.rm = TRUE)
sd(usa_data_clean$iri_ec, na.rm = TRUE)
mean(usa_data_clean$iri_fs, na.rm = TRUE)
sd(usa_data_clean$iri_fs, na.rm = TRUE)
mean(usa_data_clean$iri_pd, na.rm = TRUE)
sd(usa_data_clean$iri_pd, na.rm = TRUE)
mean(usa_data_clean$iri_pt, na.rm = TRUE)
sd(usa_data_clean$iri_pt, na.rm = TRUE)

#Find how many peeked/lied
npeeked_clean<-length(usa_data_clean[which(usa_data_clean$peeking==1),]$lying)
percent_peeked_clean <- npeeked_clean/(length(usa_data_clean$Subject))
length(usa_data_clean[which(usa_data_clean$lying==1&usa_data_clean$peeking==1),]$lying)/npeeked_clean


# Find mean and standard deviation of child JS scores
mean(usa_data_clean$js_total, na.rm = TRUE)
sd(usa_data_clean$js_total, na.rm = TRUE)
mean(usa_data_clean$js_beneficiary, na.rm = TRUE)
sd(usa_data_clean$js_beneficiary, na.rm = TRUE)
mean(usa_data_clean$js_observer, na.rm = TRUE)
sd(usa_data_clean$js_observer, na.rm = TRUE)
mean(usa_data_clean$js_perpetrator, na.rm = TRUE)
sd(usa_data_clean$js_perpetrator, na.rm = TRUE)
mean(usa_data_clean$js_victim, na.rm = TRUE)
sd(usa_data_clean$js_victim, na.rm = TRUE)

# Dictator mean and SD
dictator_mean_clean = mean(usa_data_clean$dictator_stickers_shared, na.rm = TRUE)
dictator_mean_clean
dictator_sd_clean = sd(usa_data_clean$dictator_stickers_shared, na.rm = TRUE)
dictator_sd_clean

# Distributive justice tasks: mean and SD
lazy_mean_clean = mean(usa_data_clean$merit_lazy, na.rm = TRUE)
lazy_mean_clean
lazy_sd_clean = sd(usa_data_clean$merit_lazy, na.rm = TRUE)
lazy_sd_clean

poor_mean_clean = mean(usa_data_clean$wealth_poor, na.rm = TRUE)
poor_mean_clean
poor_sd_clean = sd(usa_data_clean$wealth_poor, na.rm = TRUE)
poor_sd_clean

hurt_mean = mean(usa_data_clean$empathy_hurt, na.rm = TRUE)
hurt_mean
hurt_sd = sd(usa_data_clean$empathy_hurt, na.rm = TRUE)
hurt_sd



# Pearson's correlations for age by months ####

# Correlations
cor.test(usa_data_clean$age_months, usa_data_clean$dictator_stickers_shared, use = "complete.obs")
# Pearson's product-moment correlation
# 
# data:  usa_data_clean$age_months and usa_data_clean$dictator_stickers_shared
# t = 3.9432, df = 105, p-value = 0.0001453
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.1816737 0.5139578
# sample estimates:
#       cor 
# 0.3591444 


cor.test(usa_data_clean$age_months, usa_data_clean$merit_lazy, use = "complete.obs")

# Pearson's product-moment correlation
# 
# data:  usa_data_clean$age_months and usa_data_clean$merit_lazy
# t = -4.4703, df = 106, p-value = 1.965e-05
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  -0.5461426 -0.2263338
# sample estimates:
#        cor 
# -0.3982731

cor.test(usa_data_clean$age_months, usa_data_clean$wealth_poor, use = "complete.obs")
# Pearson's product-moment correlation
# 
# data:  usa_data_clean$age_months and usa_data_clean$wealth_poor
# t = 2.6144, df = 106, p-value = 0.01024
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.05993957 0.41576195
# sample estimates:
#       cor 
# 0.2461256 

cor.test(usa_data_clean$age_months, usa_data_clean$empathy_hurt, use = "complete.obs")
# Pearson's product-moment correlation
# 
# data:  usa_data_clean$age_months and usa_data_clean$empathy_hurt
# t = 1.7065, df = 106, p-value = 0.09084
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  -0.02626335  0.34192999
# sample estimates:
#       cor 
# 0.1635222 

cor.test(usa_data_clean$wealth_poor, usa_data_clean$merit_lazy, use = "complete.obs")
# Pearson's product-moment correlation
# 
# data:  usa_data_clean$wealth_poor and usa_data_clean$merit_lazy
# t = -2.2526, df = 106, p-value = 0.02634
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  -0.38707711 -0.02580591
# sample estimates:
#        cor 
# -0.2137375 

cor.test(usa_data_clean$income, usa_data_clean$wealth_poor, use = "complete.obs")

# Pearson's product-moment correlation
# 
# data:  usa_data_clean$income and usa_data_clean$wealth_poor
# t = 3.0158, df = 106, p-value = 0.003209
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.09730036 0.44636772
# sample estimates:
#       cor 
# 0.2811055 

cor.test(usa_data_clean$maternal_education, usa_data_clean$empathy_hurt, use = "complete.obs")

# Pearson's product-moment correlation
# 
# data:  usa_data_clean$maternal_education and usa_data_clean$empathy_hurt
# t = 2, df = 106, p-value = 0.04806
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.001784151 0.366461865
# sample estimates:
#       cor 
# 0.1906938 

cor.test(usa_data_clean$income, usa_data_clean$empathy_hurt, use = "complete.obs")
# Pearson's product-moment correlation
# 
# data:  usa_data_clean$income and usa_data_clean$empathy_hurt
# t = 2.4522, df = 106, p-value = 0.01583
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.0446813 0.4030263
# sample estimates:
#       cor 
# 0.2316989 

# Behavioral Correlation visualizations ####
col_names_1 <- c("age_months", "income", "maternal_education", "dictator_stickers_shared", "wealth_poor", "merit_lazy", "empathy_hurt")

correlation_subset <- as.matrix(usa_data_clean[col_names_1])


colnames(correlation_subset) <- c("Age months", "Income", "Maternal Education",
                                  "Dictator", "DJ Wealth", "DJ Merit", "DJ Empathy")

p_values <- cor.mtest(correlation_subset, conf.level = .95)
corr_values <- cor(correlation_subset, use = "complete.obs")

ggcorrplot(corr_values, ggtheme = ggplot2::theme_light,
           colors = c("#BB4444", "white", "#4477AA"),
           hc.order = TRUE,
           p.mat = p_values$p, lab = TRUE, type = "full", insig = "blank")

# Dispositional Correlations
cor.test(usa_data_clean$js_perpetrator, usa_data_clean$iri_pt, use = "complete.obs")
# Pearson's product-moment correlation
# 
# data:  usa_data_clean$js_perpetrator and usa_data_clean$iri_pt
# t = 2.4746, df = 94, p-value = 0.01513
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.04926388 0.42663970
# sample estimates:
#       cor 
# 0.2473072 

cor.test(usa_data_clean$js_perpetrator, usa_data_clean$iri_ec, use = "complete.obs")
# Pearson's product-moment correlation
# 
# data:  usa_data_clean$js_perpetrator and usa_data_clean$iri_ec
# t = 3.6139, df = 94, p-value = 0.000487
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.1599841 0.5137771
# sample estimates:
#       cor 
# 0.3492674 

cor.test(usa_data_clean$js_observer, usa_data_clean$iri_pd, use = "complete.obs")
# Pearson's product-moment correlation
# 
# data:  usa_data_clean$js_observer and usa_data_clean$iri_pd
# t = 2.2797, df = 93, p-value = 0.02491
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.0299006 0.4124752
# sample estimates:
#       cor 
# 0.2300572


# Dispositional Correlations Visualizations

col_names_2 <- c("js_total", "js_beneficiary", "js_observer", "js_victim", "js_perpetrator",
                 "iri_fs", "iri_pt", "iri_ec", "iri_pd")

correlation_subset_2 <- as.matrix(usa_data_clean[col_names_2])


colnames(correlation_subset_2) <- c("Child Justice Sensitivity", "Child JS Beneficiary", "Child JS Observer", "Child JS Victim", "Child JS Perpetrator", 
                                    "Parent IRI Fantasy", "Parent IRI Perspective-Taking", "Parent IRI Empathic Concern", "Parent IRI Personal Distress")

p_values_2 <- cor.mtest(correlation_subset_2, conf.level = .95)
corr_values_2 <- cor(correlation_subset_2, use = "complete.obs")

ggcorrplot(corr_values_2, ggtheme = ggplot2::theme_light,
           hc.order = TRUE,
           colors = c("#BB4444", "white", "#4477AA"),
           p.mat = p_values_2$p, lab = TRUE, type = "full", insig = "blank")

# Helper function to calculate the mean and the standard deviation for each group ####
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
  data_sum <- plyr::rename(data_sum, c("mean" = varname))
  return(data_sum)
}

# Age in years: Dictator game by age and gender ####

usa_data$age_years <- as.factor(usa_data$age_years)

dictator_anova = aov(dictator_stickers_shared~age_years*child_gender,data =usa_data_clean)
summary(dictator_anova)

# Df Sum Sq Mean Sq F value   Pr(>F)    
# age_years                1   79.5   79.53  13.507 0.000379 ***
#   child_gender             1    3.9    3.90   0.663 0.417337    
# age_years:child_gender   1    9.6    9.58   1.627 0.204929    
# Residuals              103  606.5    5.89                     
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 2 observations deleted due to missingness
# > 

# Same results from ANCOVA with age in months as covariate
dictator_anova_months = aov(dictator_stickers_shared~age_months*child_gender,data =usa_data_clean)
summary(dictator_anova_months)

# Df Sum Sq Mean Sq F value   Pr(>F)    
# age_months                1   90.2   90.23  15.583 0.000145 ***
#   child_gender              1    4.3    4.25   0.734 0.393497    
# age_months:child_gender   1    8.7    8.68   1.499 0.223666    
# Residuals               103  596.4    5.79                     
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 2 observations deleted due to missingness

dictator_summary <- data_summary(usa_data_clean, varname="dictator_stickers_shared", 
                                 groupnames=c("child_gender", "age_years"))
head(dictator_summary)


ggplot(dictator_summary, aes(x=age_years, y=dictator_stickers_shared, group=child_gender, color=child_gender)) + 
#  geom_errorbar(aes(ymin=dictator_stickers_shared-sd, ymax=dictator_stickers_shared+sd), width=.1) +
  geom_line() + geom_point()+
  scale_color_brewer(palette="Paired")+theme_minimal() +
  xlab("Age (years)") + ylab("Number stickers") +
  labs(colour = "Child Gender") +
  theme(axis.text.x = element_text(color = "grey20", family = "Times New Roman", size = 20, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", family = "Times New Roman", size = 20, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", family = "Times New Roman", size = 36, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", family = "Times New Roman", size = 36, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        legend.title = element_text(family = "Times New Roman", size=26),
        legend.text = element_text(family = "Times New Roman", size=20))


# Distributive justice (merit) by age and gender ####
merit_anova = aov(merit_lazy~age_years*child_gender,data =usa_data_clean)
summary(merit_anova)

#                        Df Sum Sq Mean Sq F value   Pr(>F)    
# age_years                1  14.66  14.657  22.109 7.94e-06 ***
#   child_gender             1   0.00   0.001   0.001    0.978    
# age_years:child_gender   1   1.31   1.312   1.979    0.162    
# Residuals              104  68.95   0.663                     
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


# Same results from ANCOVA with age in months as covariate
merit_anova_months = aov(merit_lazy~age_months*child_gender,data =usa_data_clean)
summary(merit_anova_months)


merit_lazy_summary <- data_summary(usa_data_clean, varname="merit_lazy", 
                                   groupnames=c("child_gender", "age_years"))
head(merit_lazy_summary)

ggplot(merit_lazy_summary, aes(x=age_years, y=merit_lazy, group=child_gender, color=child_gender)) + 
  #geom_errorbar(aes(ymin=merit_lazy-sd, ymax=merit_lazy+sd), width=.1) +
  geom_line() + geom_point()+
  scale_color_brewer(palette="Paired")+theme_minimal() +
  xlab("Age (years)") + ylab("Number stickers") +
  labs(colour = "Child Gender") +
  theme(axis.text.x = element_text(color = "grey20", family = "Times New Roman", size = 20, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", family = "Times New Roman", size = 20, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", family = "Times New Roman", size = 36, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", family = "Times New Roman", size = 36, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        legend.title = element_text(family = "Times New Roman", size=26),
        legend.text = element_text(family = "Times New Roman", size=20))




# Distributive justice (wealth) by age and gender ####
wealth_anova = aov(wealth_poor~age_years*child_gender,data =usa_data_clean)
summary(wealth_anova)

# Df Sum Sq Mean Sq F value Pr(>F)  
# age_years                1   2.78  2.7788   5.242 0.0241 *
#   child_gender             1   0.05  0.0504   0.095 0.7584  
# age_years:child_gender   1   0.04  0.0390   0.074 0.7866  
# Residuals              104  55.13  0.5301                 
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


# Same results from ANCOVA with age in months as covariate
wealth_anova_months = aov(wealth_poor~age_months*child_gender,data =usa_data_clean)
summary(wealth_anova_months)

wealth_poor_summary <- data_summary(usa_data_clean, varname="wealth_poor", 
                                    groupnames=c("child_gender", "age_years"))
head(wealth_poor_summary)

ggplot(wealth_poor_summary, aes(x=age_years, y=wealth_poor, group=child_gender, color=child_gender)) + 
  # geom_errorbar(aes(ymin=wealth_poor-sd, ymax=wealth_poor+sd), width=.1) +
  geom_line() + geom_point()+
  scale_color_brewer(palette="Paired")+theme_minimal() +
  xlab("Age (years)") + ylab("Number stickers") +
  labs(colour = "Child Gender") +
  theme(axis.text.x = element_text(color = "grey20", family = "Times New Roman", size = 20, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", family = "Times New Roman", size = 20, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", family = "Times New Roman", size = 36, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", family = "Times New Roman", size = 36, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        legend.title = element_text(family = "Times New Roman", size=26),
        legend.text = element_text(family = "Times New Roman", size=20))



# Distributive justice (empathy) by age and gender ####
empathy_anova = aov(empathy_hurt~age_years*child_gender,data =usa_data_clean)
summary(empathy_anova)

#                         Df Sum Sq Mean Sq F value Pr(>F)
# age_years                1   1.55  1.5492   2.343  0.129
# child_gender             1   0.36  0.3617   0.547  0.461
# age_years:child_gender   1   0.00  0.0026   0.004  0.950
# Residuals              104  68.75  0.6611 

# Same results from ANCOVA with age in months as covariate
empathy_anova_months = aov(empathy_hurt~age_months*child_gender,data =usa_data_clean)
summary(empathy_anova_months)


empathy_hurt_summary <- data_summary(usa_data_clean, varname="empathy_hurt", 
                                     groupnames=c("child_gender", "age_years"))
head(empathy_hurt_summary)

ggplot(empathy_hurt_summary, aes(x=age_years, y=empathy_hurt, group=child_gender, color=child_gender)) + 
  #geom_errorbar(aes(ymin=empathy_hurt-sd, ymax=empathy_hurt+sd), width=.1) +
  geom_line() + geom_point()+
  scale_color_brewer(palette="Paired")+theme_minimal() +
  xlab("Age (years)") + ylab("Number stickers") +
  labs(colour = "Child Gender") +
  theme(axis.text.x = element_text(color = "grey20", family = "Times New Roman", size = 20, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", family = "Times New Roman", size = 20, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", family = "Times New Roman", size = 36, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", family = "Times New Roman", size = 36, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        legend.title = element_text(family = "Times New Roman", size=26),
        legend.text = element_text(family = "Times New Roman", size=20))


# Lying by age (months) and gender ####

usa_data_glm <- usa_data_clean %>% 
  select(age_years, age_months, child_gender, lying, peeking, iri_ec, iri_pd, iri_fs, iri_pt, js_total) %>%
  drop_na() %>% as_tibble()

usa_data_glm$peeking<- as.factor(usa_data_glm$peeking)

lying_glm_months <- glm(lying ~ age_months *child_gender, data = usa_data_glm, family = "binomial")

# lying_glm_iri <- glm(lying ~ age_months * child_gender + iri_ec, data = usa_data_glm, family = "binomial")
lying_glm_iri <- glm(lying ~ age_months * child_gender + iri_ec+ iri_pd+ iri_fs+ iri_pt + js_total, data = usa_data_glm, family = "binomial")

summary(lying_glm_months)


# Call:
#  glm(formula = lying ~ age_months * child_gender, family = "binomial", 
#      data = usa_data_glm)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -2.0275  -0.8943   0.5298   0.9101   1.8526  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)   
# (Intercept)                  5.65831    1.79146   3.158  0.00159 **
#   age_months                  -0.07195    0.02210  -3.255  0.00113 **
#   child_gendermale            -1.11017    2.51502  -0.441  0.65891   
# age_months:child_gendermale  0.01904    0.03052   0.624  0.53279   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 120.50  on 86  degrees of freedom
# Residual deviance:  98.14  on 83  degrees of freedom
# AIC: 106.14


# 0.07195 



# Report Odds ratio, z-stat and p-value
odds.ratio(lying_glm_months)

summary(lying_glm_iri)

anova(lying_glm_months, lying_glm_iri)

# change in deviance, df
# gives p-value: was deviance significantly reduced by adding 1 factor

pchisq(6.265, 5, lower.tail = FALSE)


#Peeking GLM
peeking_glm_months <- glm(peeking ~ age_months *child_gender, data = usa_data_glm, family = "binomial")

peeking_glm_iri <- glm(peeking ~ age_months * child_gender + iri_ec+ iri_pd+ iri_fs+ iri_pt + js_total, data = usa_data_glm, family = "binomial")

summary(peeking_glm_months)


summary(peeking_glm_iri)
anova(peeking_glm_months, peeking_glm_iri)


# Report Odds ratio, z-stat and p-value?
# odds.ratio(peeking_glm_months)


# change in deviance, df
# gives p-value: was deviance significantly reduced by adding X factors?

pchisq(2.651, 5, lower.tail = FALSE)



# tested by adding quadratic (non-linear effects for age), didn't improve model fit 
# lying_glm_months_quadratic <- glm(lying ~ age_months *child_gender + I(age_months^2),  data = usa_data_glm, family = "binomial")
# anova(lying_glm_months, lying_glm_months_quadratic, lying_glm_iri)



