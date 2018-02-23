# Section 1
bankData <-read.table(file ="C:/Users/graha/Documents/4th year/EDT/ca 1/BANKINGrel.csv", stringsAsFactors=FALSE, sep =",", header=TRUE)

## A
str(bankData)

## B
perc.age <- sum(is.na(bankData$age)) / length(bankData$age) * 100
perc.age

perc.job <- sum(is.na(bankData$job)) / length(bankData$job) * 100
perc.job

perc.marital <- sum(is.na(bankData$marital)) / length(bankData$marital) * 100
perc.marital

perc.education <- sum(is.na(bankData$education)) / length(bankData$education) * 100
perc.education

perc.housing <- sum(is.na(bankData$housing)) / length(bankData$housing) * 100
perc.housing

perc.loan <- sum(is.na(bankData$loan)) / length(bankData$loan) * 100
perc.loan

perc.contact  <- sum(is.na(bankData$contact )) / length(bankData$contact ) * 100
perc.contact 

perc.month <- sum(is.na(bankData$month)) / length(bankData$month) * 100
perc.month

perc.day_of_week <- sum(is.na(bankData$day_of_week)) / length(bankData$day_of_week) * 100
perc.day_of_week

perc.duration <- sum(is.na(bankData$duration)) / length(bankData$duration) * 100
perc.duration

perc.campaign <- sum(is.na(bankData$campaign)) / length(bankData$campaign) * 100
perc.campaign

perc.pdays <- sum(is.na(bankData$pdays)) / length(bankData$pdays) * 100
perc.pdays

perc.previous <- sum(is.na(bankData$previous)) / length(bankData$previous) * 100
perc.previous

perc.poutcome <- sum(is.na(bankData$poutcome)) / length(bankData$poutcome) * 100
perc.poutcome

perc.cons.price.idx <- sum(is.na(bankData$cons.price.idx)) / length(bankData$cons.price.idx) * 100
perc.cons.price.idx

perc.cons.conf.idx <- sum(is.na(bankData$cons.conf.idx)) / length(bankData$cons.conf.idx) * 100
perc.cons.conf.idx

perc.euribor3m <- sum(is.na(bankData$euribor3m)) / length(bankData$euribor3m) * 100
perc.euribor3m


perc.nr.employed <- sum(is.na(bankData$nr.employed)) / length(bankData$nr.employed) * 100
perc.nr.employed

## C
######################################################
Mode <- function(x) {
  uni <- unique(x)
  uni[which.max(tabulate(match(x, uni)))]
}
######################################################
summary(bankData$age)
Mode(bankData$age)
sd(bankData$age, na.rm = TRUE)

summary(bankData$job)
Mode(bankData$job)
sd(bankData$job, na.rm = TRUE)

Mode(bankData$marital)

Mode(bankData$education)

Mode(bankData$housing)

Mode(bankData$loan)

Mode(bankData$contact)

Mode(bankData$month)

Mode(bankData$day_of_week)

summary(bankData$duration)
Mode(bankData$duration)
sd(bankData$duration, na.rm = TRUE)

summary(bankData$campaign)
Mode(bankData$campaign)
sd(bankData$campaign, na.rm = TRUE)

summary(bankData$pdays)
Mode(bankData$pdays)
sd(bankData$pdays, na.rm = TRUE)

summary(bankData$previous)
Mode(bankData$previous)
sd(bankData$previous, na.rm = TRUE)

Mode(bankData$poutcome)

summary(bankData$cons.price.idx)
Mode(bankData$cons.price.idx)
sd(bankData$cons.price.idx, na.rm = TRUE)

summary(bankData$cons.conf.idx)
Mode(bankData$cons.conf.idx)
sd(bankData$cons.conf.idx, na.rm = TRUE)

summary(bankData$euribor3m)
Mode(bankData$euribor3m)
sd(bankData$euribor3m, na.rm = TRUE)

summary(bankData$nr.employed)
Mode(bankData$nr.employed)
sd(bankData$nr.employed, na.rm = TRUE)

## D
# Using Shapiro wilks test for normaliy

shapiro.test(bankData$age)
shapiro.test(bankData$duration)
shapiro.test(bankData$campaign)
shapiro.test(bankData$pdays)
shapiro.test(bankData$previous)
shapiro.test(bankData$cons.price.idx)
shapiro.test(bankData$cons.conf.idx)
shapiro.test(bankData$euribor3m)
shapiro.test(bankData$nr.employed)

## E

##################################
# Skewness
#             3(mean - median)
# Skewness = -------------------
#             standard deviation
#
# perfect = 0, left = -, right = +
##################################

skewness.age <- 3*(mean(bankData$age, na.rm = TRUE) - median(bankData$age, na.rm = TRUE)) / sd(bankData$age, na.rm = TRUE)
skewness.age

skewness.duration <- 3*(mean(bankData$duration, na.rm = TRUE) - median(bankData$duration, na.rm = TRUE)) / sd(bankData$duration, na.rm = TRUE)
skewness.duration

skewness.campaign <- 3*(mean(bankData$campaign, na.rm = TRUE) - median(bankData$campaign, na.rm = TRUE)) / sd(bankData$campaign, na.rm = TRUE)
skewness.campaign

skewness.pdays <- 3*(mean(bankData$pdays, na.rm = TRUE) - median(bankData$pdays, na.rm = TRUE)) / sd(bankData$pdays, na.rm = TRUE)
skewness.pdays

skewness.previous <- 3*(mean(bankData$previous, na.rm = TRUE) - median(bankData$previous, na.rm = TRUE)) / sd(bankData$previous, na.rm = TRUE)
skewness.previous

skewness.cons.price.idx <- 3*(mean(bankData$cons.price.idx, na.rm = TRUE) - median(bankData$cons.price.idx, na.rm = TRUE)) / sd(bankData$cons.price.idx, na.rm = TRUE)
skewness.cons.price.idx

skewness.cons.conf.idx <- 3*(mean(bankData$cons.conf.idx, na.rm = TRUE) - median(bankData$cons.conf.idx, na.rm = TRUE)) / sd(bankData$cons.conf.idx, na.rm = TRUE)
skewness.cons.conf.idx

skewness.euribor3m <- 3*(mean(bankData$euribor3m, na.rm = TRUE) - median(bankData$euribor3m, na.rm = TRUE)) / sd(bankData$euribor3m, na.rm = TRUE)
skewness.euribor3m

skewness.nr.employed <- 3*(mean(bankData$nr.employed, na.rm = TRUE) - median(bankData$nr.employed, na.rm = TRUE)) / sd(bankData$nr.employed, na.rm = TRUE)
skewness.nr.employed

## F
cor(bankData$age, bankData$duration, use="complete.obs")
cor(bankData$age, bankData$campaign, use="complete.obs")
cor(bankData$age, bankData$pdays, use="complete.obs")
cor(bankData$age, bankData$previous, use="complete.obs")
cor(bankData$age, bankData$cons.price.idx, use="complete.obs")
cor(bankData$age, bankData$cons.conf.idx, use="complete.obs")
cor(bankData$age, bankData$euribor3m, use="complete.obs")
cor(bankData$age, bankData$nr.employed, use="complete.obs")

cor(bankData$duration, bankData$age, use="complete.obs")
cor(bankData$duration, bankData$campaign, use="complete.obs")
cor(bankData$duration, bankData$pdays, use="complete.obs")
cor(bankData$duration, bankData$previous, use="complete.obs")
cor(bankData$duration, bankData$cons.price.idx, use="complete.obs")
cor(bankData$duration, bankData$cons.conf.idx, use="complete.obs")
cor(bankData$duration, bankData$euribor3m, use="complete.obs")
cor(bankData$duration, bankData$nr.employed, use="complete.obs")

cor(bankData$campaign, bankData$age, use="complete.obs")
cor(bankData$campaign,bankData$duration, use="complete.obs")
cor(bankData$campaign, bankData$pdays, use="complete.obs")
cor(bankData$campaign, bankData$previous, use="complete.obs")
cor(bankData$campaign, bankData$cons.price.idx, use="complete.obs")
cor(bankData$campaign, bankData$cons.conf.idx, use="complete.obs")
cor(bankData$campaign, bankData$euribor3m, use="complete.obs")
cor(bankData$campaign, bankData$nr.employed, use="complete.obs")

cor(bankData$pdays, bankData$age, use="complete.obs")
cor(bankData$pdays,bankData$duration, use="complete.obs")
cor(bankData$pdays, bankData$campaign, use="complete.obs")
cor(bankData$pdays, bankData$previous, use="complete.obs")
cor(bankData$pdays, bankData$cons.price.idx, use="complete.obs")
cor(bankData$pdays, bankData$cons.conf.idx, use="complete.obs")
cor(bankData$pdays, bankData$euribor3m, use="complete.obs")
cor(bankData$pdays, bankData$nr.employed, use="complete.obs")

cor(bankData$previous, bankData$age, use="complete.obs")
cor(bankData$previous,bankData$duration, use="complete.obs")
cor(bankData$previous, bankData$campaign, use="complete.obs")
cor(bankData$previous, bankData$pdays, use="complete.obs")
cor(bankData$previous, bankData$cons.price.idx, use="complete.obs")
cor(bankData$previous, bankData$cons.conf.idx, use="complete.obs")
cor(bankData$previous, bankData$euribor3m, use="complete.obs")
cor(bankData$previous, bankData$nr.employed, use="complete.obs")

cor(bankData$cons.price.idx, bankData$age, use="complete.obs")
cor(bankData$cons.price.idx,bankData$duration, use="complete.obs")
cor(bankData$cons.price.idx, bankData$campaign, use="complete.obs")
cor(bankData$cons.price.idx, bankData$pdays, use="complete.obs")
cor(bankData$cons.price.idx,bankData$previous, use="complete.obs")
cor(bankData$cons.price.idx, bankData$cons.conf.idx, use="complete.obs")
cor(bankData$cons.price.idx, bankData$euribor3m, use="complete.obs")
cor(bankData$cons.price.idx, bankData$nr.employed, use="complete.obs")

cor(bankData$cons.conf.idx, bankData$age, use="complete.obs")
cor(bankData$cons.conf.idx,bankData$duration, use="complete.obs")
cor(bankData$cons.conf.idx, bankData$campaign, use="complete.obs")
cor(bankData$cons.conf.idx, bankData$pdays, use="complete.obs")
cor(bankData$cons.conf.idx,bankData$previous, use="complete.obs")
cor(bankData$cons.conf.idx, bankData$cons.price.idx, use="complete.obs")
cor(bankData$cons.conf.idx, bankData$euribor3m, use="complete.obs")
cor(bankData$cons.conf.idx, bankData$nr.employed, use="complete.obs")

cor(bankData$euribor3m, bankData$age, use="complete.obs")
cor(bankData$euribor3m,bankData$duration, use="complete.obs")
cor(bankData$euribor3m, bankData$campaign, use="complete.obs")
cor(bankData$euribor3m, bankData$pdays, use="complete.obs")
cor(bankData$euribor3m,bankData$previous, use="complete.obs")
cor(bankData$euribor3m, bankData$cons.price.idx, use="complete.obs")
cor( bankData$euribor3m, bankData$cons.conf.idx, use="complete.obs")
cor(bankData$euribor3m, bankData$nr.employed, use="complete.obs")


cor(bankData$cons.conf.idx, bankData$age, use="complete.obs")
cor(bankData$cons.conf.idx,bankData$duration, use="complete.obs")
cor(bankData$cons.conf.idx, bankData$campaign, use="complete.obs")
cor(bankData$cons.conf.idx, bankData$pdays, use="complete.obs")
cor(bankData$cons.conf.idx,bankData$previous, use="complete.obs")
cor(bankData$cons.conf.idx, bankData$cons.price.idx, use="complete.obs")
cor(bankData$cons.conf.idx, bankData$euribor3m, use="complete.obs")
cor(bankData$cons.conf.idx, bankData$nr.employed, use="complete.obs")


# Section 2
## A

install.packages("ggplot2", dependencies=TRUE)
library(ggplot2)

ggplot(bankData, aes(x = age, fill = y)) + geom_histogram(binwidth = 1) 

ggplot(bankData, aes(x = duration, fill = y)) + geom_histogram(binwidth = 10)

ggplot(bankData, aes(x = campaign, fill = y)) + geom_histogram(binwidth = 1) 
## look into
ggplot(bankData, aes(x = pdays, fill = y)) + geom_histogram(binwidth = 250) 

ggplot(bankData, aes(x = previous, fill = y)) + geom_histogram(binwidth = 1) 

ggplot(bankData, aes(x = cons.price.idx, fill = y)) + geom_histogram(binwidth = 1) 

ggplot(bankData, aes(x = cons.conf.idx, fill = y)) + geom_histogram(binwidth = 1) 

ggplot(bankData, aes(x = euribor3m, fill = y)) + geom_histogram(binwidth = 1) 

ggplot(bankData, aes(x = nr.employed, fill = y)) + geom_histogram(binwidth = 10) 

# Section 3
## A

ggplot(bankData, aes(x = job, fill = y)) + geom_bar()

ggplot(bankData, aes(x = marital, fill = y)) + geom_bar()

ggplot(bankData, aes(x = education, fill = y)) + geom_bar()

ggplot(bankData, aes(x = housing, fill = y)) + geom_bar()

ggplot(bankData, aes(x = loan, fill = y)) + geom_bar()

ggplot(bankData, aes(x = contact, fill = y)) + geom_bar()

ggplot(bankData, aes(x = month, fill = y)) + geom_bar()

ggplot(bankData, aes(x = day_of_week, fill = y)) + geom_bar()

ggplot(bankData, aes(x = poutcome, fill = y)) + geom_bar()


# Section 4

# age
ggplot(bankData, aes(x = age)) + geom_histogram(binwidth = 1)

##################################
# IQR = Q3 - Q1
summary(bankData$age)
# Q1 = 32, Q3 = 47
#47 - 32 = 15
# IQR Q1 = 32 - 1.5(15) = 9.5
# IQR Q3 = 47 + 1.5(15) = 69.5
bankData$age[which(bankData$age < 9.5)]
bankData$age[which(bankData$age > 69.5)]
# OUTLIERS TEMP (none)
##################################

#
#       x - mean(X)
# X* = --------------
#       SD(X)
#
#zScore Standardisation

zscore.age <-(bankData$age -
                     mean(bankData$age, na.rm = TRUE))/sd(bankData$age, na.rm = TRUE)
zscore.age[which((zscore.age < -3))]
zscore.age[which((zscore.age > 3))]

# duration

ggplot(bankData, aes(x = duration)) + geom_histogram(binwidth = 1)

##################################
# IQR = Q3 - Q1
summary(bankData$duration)
# Q1 = 101.5, Q3 = 319.5
# 319.5 - 101.5 = 218
# IQR Q1 = 101.5 - 1.5(218) = -225.5
# IQR Q3 = 319.5 + 1.5(218) = 646.5
bankData$duration[which(bankData$duration < -225.5)]
bankData$duration[which(bankData$duration > 646.5)]
# OUTLIERS TEMP (none)
##################################

#
#       x - mean(X)
# X* = --------------
#       SD(X)
#
#zScore Standardisation

zscore.duration <-(bankData$duration -
                mean(bankData$duration, na.rm = TRUE))/sd(bankData$duration, na.rm = TRUE)
zscore.duration[which((zscore.duration < -3))]
zscore.duration[which((zscore.duration > 3))]

# campaign 

ggplot(bankData, aes(x = campaign )) + geom_histogram(binwidth = 1)

##################################
# IQR = Q3 - Q1
summary(bankData$campaign)
# Q1 = 1, Q3 = 3
# 3 - 1 = 2
# IQR Q1 = 1 - 1.5(2) = -2
# IQR Q3 = 3 + 1.5(2) = 6
bankData$campaign[which(bankData$campaign < -2)]
bankData$campaign[which(bankData$campaign > 6)]
# OUTLIERS TEMP (none)
##################################

#
#       x - mean(X)
# X* = --------------
#       SD(X)
#
#zScore Standardisation

zscore.campaign <-(bankData$campaign -
                     mean(bankData$campaign, na.rm = TRUE))/sd(bankData$campaign, na.rm = TRUE)
zscore.campaign[which((zscore.campaign < -3))]
zscore.campaign[which((zscore.campaign > 3))]

# pdays 

ggplot(bankData, aes(x = pdays )) + geom_histogram(binwidth = 100)

##################################
# IQR = Q3 - Q1
summary(bankData$pdays)
# Q1 = 999, Q3 = 999
# 999 - 999 = 0
# IQR Q1 = 999 - 1.5(0) = 999
# IQR Q3 = 999 + 1.5(0) = 999
bankData$pdays[which(bankData$pdays < 999)]
bankData$pdays[which(bankData$pdays > 999)]
# OUTLIERS TEMP (none)
##################################

#
#       x - mean(X)
# X* = --------------
#       SD(X)
#
#zScore Standardisation

zscore.pdays <-(bankData$pdays -
                     mean(bankData$pdays, na.rm = TRUE))/sd(bankData$pdays, na.rm = TRUE)
zscore.pdays[which((zscore.pdays < -3))]
zscore.pdays[which((zscore.pdays > 3))]

# previous 

ggplot(bankData, aes(x = previous )) + geom_histogram(binwidth = 1)

##################################
# IQR = Q3 - Q1
summary(bankData$previous)
# Q1 = 0, Q3 = 0
# 0 - 0 = 0
# IQR Q1 = 0- 1.5(0) = 0
# IQR Q3 = 0 + 1.5(0) = 0
bankData$previous[which(bankData$previous < 0)]
bankData$previous[which(bankData$previous > 0)]
# OUTLIERS TEMP (none)
##################################

#
#       x - mean(X)
# X* = --------------
#       SD(X)
#
#zScore Standardisation

zscore.previous <-(bankData$previous -
                  mean(bankData$previous, na.rm = TRUE))/sd(bankData$previous, na.rm = TRUE)
zscore.previous[which((zscore.previous < -3))]
zscore.previous[which((zscore.previous > 3))]

# cons.price.idx 

ggplot(bankData, aes(x = cons.price.idx )) + geom_histogram(binwidth = 1)

##################################
# IQR = Q3 - Q1
summary(bankData$cons.price.idx)
# Q1 = 93.09, Q3 = 93.99
# 93.99 - 93.08 = .91
# IQR Q1 = 93.08 - 1.5(0.91) = 91.75
# IQR Q3 = 93.99 + 1.5(0.91) = 95.355
bankData$cons.price.idx[which(bankData$cons.price.idx < 91.75)]
bankData$cons.price.idx[which(bankData$cons.price.idx > 95.355)]
# OUTLIERS TEMP (none)
##################################

#
#       x - mean(X)
# X* = --------------
#       SD(X)
#
#zScore Standardisation

zscore.cons.price.idx <-(bankData$cons.price.idx -
                     mean(bankData$cons.price.idx, na.rm = TRUE))/sd(bankData$cons.price.idx, na.rm = TRUE)
zscore.cons.price.idx[which((zscore.cons.price.idx < -3))]
zscore.cons.price.idx[which((zscore.cons.price.idx > 3))]


# cons.conf.idx

ggplot(bankData, aes(x = cons.conf.idx )) + geom_histogram(binwidth = 1)

##################################
# IQR = Q3 - Q1
summary(bankData$cons.conf.idx)
# Q1 = -42.70, Q3 = -36.40
# -36.40 - -42.70 = 6.3
# IQR Q1 = -42.70 - 1.5(6.3) = -52.15
# IQR Q3 = -36.40 + 1.5(6.3) = -26.95
bankData$cons.conf.idx[which(bankData$cons.conf.idx < -52.15)]
bankData$cons.conf.idx[which(bankData$cons.conf.idx > -26.95)]
# OUTLIERS TEMP (none)
##################################

#
#       x - mean(X)
# X* = --------------
#       SD(X)
#
#zScore Standardisation

zscore.cons.conf.idx <-(bankData$cons.conf.idx -
                           mean(bankData$cons.conf.idx, na.rm = TRUE))/sd(bankData$cons.conf.idx, na.rm = TRUE)
zscore.cons.conf.idx[which((zscore.cons.conf.idx < -3))]
zscore.cons.conf.idx[which((zscore.cons.conf.idx > 3))]


# euribor3m

ggplot(bankData, aes(x = euribor3m )) + geom_histogram(binwidth = 1)

##################################
# IQR = Q3 - Q1
summary(bankData$euribor3m)
# Q1 = 1.334, Q3 = 4.961
# 4.961 - 1.334 = 3.627
# IQR Q1 = 1.334 - 1.5(3.627) = -4.1065
# IQR Q3 = 4.961 + 1.5(3.627) = 10.4015
bankData$euribor3m[which(bankData$euribor3m < -4.1065)]
bankData$euribor3m[which(bankData$euribor3m > 10.4015)]
# OUTLIERS TEMP (none)
##################################

#
#       x - mean(X)
# X* = --------------
#       SD(X)
#
#zScore Standardisation

zscore.euribor3m <-(bankData$euribor3m -
                          mean(bankData$euribor3m, na.rm = TRUE))/sd(bankData$euribor3m, na.rm = TRUE)
zscore.euribor3m[which((zscore.euribor3m < -3))]
zscore.euribor3m[which((zscore.euribor3m > 3))]


# nr.employed

ggplot(bankData, aes(x = nr.employed )) + geom_histogram(binwidth = 1)

##################################
# IQR = Q3 - Q1
summary(bankData$nr.employed)
# Q1 = 5099, Q3 =5228
# 5228 - 5099 = 189
# IQR Q1 = 5099 - 1.5(189) = 4815.5
# IQR Q3 = 5228 + 1.5(189) = 5511.5
bankData$nr.employed[which(bankData$nr.employed < 4815.5)]
bankData$nr.employed[which(bankData$nr.employed > 5511.5)]
# OUTLIERS TEMP (none)
##################################

#
#       x - mean(X)
# X* = --------------
#       SD(X)
#
#zScore Standardisation

zscore.nr.employed <-(bankData$nr.employed -
                      mean(bankData$nr.employed, na.rm = TRUE))/sd(bankData$nr.employed, na.rm = TRUE)
zscore.nr.employed[which((zscore.nr.employed < -3))]
zscore.nr.employed[which((zscore.nr.employed > 3))]



# Section 5

ggplot(bankData, aes(x = age , y = duration)) + geom_point()

ggplot(bankData, aes(x = age , y = duration)) + geom_point()
ggplot(bankData, aes(x = age , bankData$campaign)) + geom_point()
ggplot(bankData, aes(x = age , bankData$pdays)) + geom_point()
ggplot(bankData, aes(x = age , bankData$previous)) + geom_point()
ggplot(bankData, aes(x = age , bankData$cons.price.idxv)) + geom_point()
ggplot(bankData, aes(x = age , bankData$cons.conf.idx)) + geom_point()
ggplot(bankData, aes(x = age , bankData$euribor3m)) + geom_point()
ggplot(bankData, aes(x = age , bankData$job)) + geom_point()
ggplot(bankData, aes(x = age , bankData$marital)) + geom_point()
ggplot(bankData, aes(x = age , bankData$education)) + geom_point()
ggplot(bankData, aes(x = age , bankData$housing)) + geom_point()
ggplot(bankData, aes(x = age , bankData$loan)) + geom_point()
ggplot(bankData, aes(x = age , bankData$contact)) + geom_point()
ggplot(bankData, aes(x = age , bankData$month)) + geom_point()
ggplot(bankData, aes(x = age , bankData$day_of_week)) + geom_point()


ggplot(bankData, aes(x = age , y = duration)) + geom_point()
ggplot(bankData, aes(x = age , bankData$campaign)) + geom_point()
ggplot(bankData, aes(x = age , bankData$pdays)) + geom_point()
ggplot(bankData, aes(x = age , bankData$previous)) + geom_point()
ggplot(bankData, aes(x = age , bankData$cons.price.idxv)) + geom_point()
ggplot(bankData, aes(x = age , bankData$cons.conf.idx)) + geom_point()
ggplot(bankData, aes(x = age , bankData$euribor3m)) + geom_point()
ggplot(bankData, aes(x = age , bankData$job)) + geom_point()
ggplot(bankData, aes(x = age , bankData$marital)) + geom_point()
ggplot(bankData, aes(x = age , bankData$education)) + geom_point()
ggplot(bankData, aes(x = age , bankData$housing)) + geom_point()
ggplot(bankData, aes(x = age , bankData$loan)) + geom_point()
ggplot(bankData, aes(x = age , bankData$contact)) + geom_point()
ggplot(bankData, aes(x = age , bankData$month)) + geom_point()
ggplot(bankData, aes(x = age , bankData$day_of_week)) + geom_point()



