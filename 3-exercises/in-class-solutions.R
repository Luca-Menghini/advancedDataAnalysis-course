# ###################################################################################################################
# IN-CLASS EXERCISES SOLUTION
# This script includes the solutions to the exercises that we saw in class, during the lectures
# ###################################################################################################################

# DAY 2 (Introduction to R) 
#####################################################################################################

# Exercise 1 (Arithmetic operations, slide 7)
# ___________________________________________________________________________________________________

# 1
((45+21)^3+3/4)/sqrt(32-12/17)

# 2
(sqrt(7-pi)/(3*(45-34)))

# 3
(12-exp(1)^2)^(1/3)+log(10*pi)

# 4
(sin(3/4*pi)^2+cos(3/2*pi))/log(exp(1)^(3/2),base = 7)

# 5
sum(1:10)/10

# Exercise 2 (Logical and relational operators, slide 8)
# ___________________________________________________________________________________________________

# preposition: (x >= -4 & x <= -2) | (x >= 2 & x <= 4)

# two TRUE and two FALSE relationship to test the prosposition
(-5 >= -4 & -5 <= -2) | (-5 >= 2 & -5 <= 4) # FALSE
(-3 >= -4 & -3 <= -2) | (-3 >= 2 & -3 <= 4) # TRUE
(0 >= -4 & 0 <= -2) | (0 >= 2 & 0 <= 4)     # FALSE
(3 >= -4 & 3 <= -2) | (3 >= 2 & 3 <= 4)     # TRUE

# last point
4 ^ 3 %in% c(2,3,4) # the exponentiation is executed before the relational operator "%in%" (i.e., is 4^3 included in 2,3,4?)
4 * 3 %in% c(2,3,4) # the product is executed after the relational operator "%in%" (i.e., how much is the relational result multiplied by 4?)
# note: the relationship "3 %in% c(2,3,4)" is automatically converted into numeric (TRUE=1, FALSE=0)

# Exercise 3 (R objects, slide 23)
# ___________________________________________________________________________________________________

# emptying the workspace (always a good choice to start a script with this)
rm(list=ls()) 

# 1. create the vectors
x1 <- c("Gina","Pina","Tina","Maria","Lina")
x2 <- c(507,535,566,955,515)
x3 <- c(19,27,28,25,25)

# 2. create a dataframe
tony <- cbind(x1,x2,x3)
tony <- as.data.frame(tony)
tony <- data.frame(x1,x2,x3)
tony <- data.frame(names = x1,
                   numbers = x2,
                   scores = x3)

# 3. delete objects from ws
rm(x1,x2,x3)

# 4. select Maria's grade
tony[tony$names=="Maria","scores"]
tony[tony$names=="Maria",3] # alternative solution

# 5. select's Pina's rows
tony[tony$names=="Pina",]

# 6. select all students with score = 25
tony[tony$scores==25,]

# 7. selet score higher than 20
tony[tony$scores>20,]

# 8. mean and sd of scores
mean(tony$scores)
sd(tony$scores)

# Exercise 4 (R workspace, slide 30)
# ___________________________________________________________________________________________________

# emptying the workspace (always a good choice to start a script with this)
rm(list=ls()) 

# 1. Download the files from Moodle or Github

# 2. Read them, identify their class and that of the objects inside
load("data/studqs.RData") # reading files (note: I saved them in the "data" subfolder of my working directory)
studqs2 <- read.csv("data/studqs.csv")
class(qs)
class(studqs2) # both of them are data.frame
str(qs)
str(studqs2) # the only difference is that variables Q01-Q05 are character in the CSV files and factors in the RData file
# trick: if you put the argument stringsAsFactors = TRUE, all character variables are converted to factors
str(read.csv("data/studqs.csv",stringsAsFactors = TRUE)) # now the two datasets are identical

# 3. descriptive statistics and histogram of the variable NumVar
library(psych) # loding pkg - if not yet installed, run install.packages("psych")
describe(qs$numVar)
hist(qs$numVar)

# 4. frequency table of the variable Q02
table(qs$Q02)

# 5. same thing but only considering those that replied “Sì” (yes) to the question Q01
table(qs[qs$Q01=="Sì","Q02"])
table(qs$Q02[qs$Q01=="Sì"]) # alternative solution

# 6. crossed frequencies
table(qs[,c("Q02","Q03")])
table(qs[,c(3,4)]) # alternative solution
table(qs$Q02,qs$Q03) # alternative solution

# Exercise 5 (R graphics, slide 44)
# ___________________________________________________________________________________________________

# emptying the workspace (always a good choice to start a script with this)
rm(list=ls()) 

# 1. download and read the data
load("data/studqs.RData") # reading file (note: I saved them in the "data" subfolder of my working directory)

# 2. hist of numVar separately by Q01
hist(qs[qs$Q01=="Sì","numVar"])
hist(qs[qs$Q01=="No","numVar"]) # this returns an error: invalid number of 'breaks' (why is that?)
summary(qs$Q01) # here's why: the levels of Q01 are "Sì" and "Non a tutti" (there is no level called "No")
hist(qs[qs$Q01=="Non a tutti","numVar"]) # now it works!
# trick to have both histograms in one plot (run the following 3 lines)
par(mfrow=c(1,2))
hist(qs[qs$Q01=="Sì","numVar"])
hist(qs[qs$Q01=="Non a tutti","numVar"])
par(mfrow=c(1,1)) # this is needed to come back to visualizing 1 plot at time

# 3. Normal QQ plot of the variable numVar. Is it normally distributed?
qqnorm(qs$numVar) # looks like normally distributed because it follows the normal quantiles distribution (streight line)
qqline(qs$numVar, col="red") # you can also add the line which should be followed under normality

# 4. Box plot of numVar by Q01: which group has the highest median? Which has the hugest variability?
boxplot(numVar ~ Q01, data=qs) # those with Q01 = "Sì" have a slightly higher median and substantially lower variability
boxplot(qs$numVar ~ qs$Q01) # alternative solution

# 5. create numVar2 corresponding to numVar2 - 2 * rnorm(nrow(qs), sd = 20)
qs$numVar2 <- qs$numVar - 2 * rnorm(nrow(qs), sd = 20)

# 6. scatter plot of numVar2 vs numVar with regression line + correlation coeff.
plot(numVar2 ~ numVar, data=qs)
plot(qs$numVar2 ~ qs$numVar) # alternative
abline( lm(numVar2 ~ numVar, data=qs) ) # adding regression line (note: abline() adds a line, whereas lm() fits a linear model that provides the intercept "a" and the slope "b" to be used in abline())
cor(qs$numVar,qs$numVar2) # this returns NA ('not available') because the dataset has a missing value
cor(qs$numVar,qs$numVar2, use = "complete.obs") # if we set the argument "use" equal to "complete.obs", missing values are ignored


# DAY 4 (Linear model recap) 
#####################################################################################################

# emptying the workspace (always a good choice to start a script with this)
rm(list=ls())

# 1. loading data
# install.packages("osfr") # only the first time if not already installed
library(osfr) # package to interact with the Open Science Framework platform
proj <- "https://osf.io/ha5dp/" # link to the OSF project
osf_download(osf_ls_files(osf_retrieve_node(proj))[2, ],conflicts="overwrite") # download
preg <- na.omit(read.csv("OSFData_Upload_2023_Mar30.csv",stringsAsFactors=TRUE)) # read data
colnames(preg)[c(2,5,12,14)] <- c("age","depr","NICU","threat") # set variable names

# 2. Explore the variables
# mean and SD of quantitative variables
mean(preg$age) 
sd(preg$age)
mean(preg$depr) 
sd(preg$depr)
mean(preg$threat) 
sd(preg$threat)
# frequency for categorical variables
table(preg$NICU)
100 * table(preg$NICU)/nrow(preg) # from frequency to percentage
# correlation between 2 quantitative variables
cor(x=preg$age, y=preg$depr)
# correlation matrix among 3+ quantitative variables
cor(x=preg[,c("age","depr","threat")])
hist(preg$age) # histograms of quantitative variables
hist(preg$depr)
hist(preg$threat)
plot(preg$NICU) # barplot of categorical variables

# 3. fit a null model m0 predicting depr
m0 <- lm(formula = depr ~ 1, data = preg)

# 4. Fit a simple regression model m1 with depr being predicted by threat
m1 <- lm(formula = depr ~ threat, data = preg)

# 5. Fit a multiple regression model m2 also controlling for NICU and age
m2 <- lm(formula = depr ~ threat + NICU + age, data = preg)

# 6. Fit an interactive model m3 to check whether age moderates the relationship between threat and depr.
m3 <- lm(formula = depr ~ threat + NICU + age + age:threat, data = preg)

# 7. Compare the models with AIC and likelihood ratio test: which is the best model?
AIC(m0,m1,m2,m3) # AIC: the lower the better (m2 is the best model)
lmtest::lrtest(m0,m1,m2,m3) # LRT: m2 is the best model (comparison between m2 and m3 is not significant)

# 8. Print & interpret the coefficients estimated by the selected model
coefficients(m2)
# Intercept = 9.73 --> when threat and age = 0 and NICU = No, the expected value of depr is 9.73
# threat = 0.06 --> when age = 0, within the same NICU category, a 1-unit increase in threat predicts an increase of 0.06 in depr
# NICUYes = 1.01 --> when threat and age = 0, the expected difference between NICU=Yes and NICU=No is 1.01 (i.e., mothers with NICU=Yes have a mean depr score higher than mothers with NICU=No)
# age = -0.08 --> when threat = 0, within the same NICU category, a 1-unit increase in age predicts an decrease of 0.08 in depr
summary(m2)$sigma^2 # residual variance (difficult to interpret without comparing it with alternative models)

# 9. Print & interpret the statistical significance of the estimated coefficients
summary(m2) # all coefficients are significant!
# Intercept: depression levels are significantly higher than zero, controlling for threat, NICU, and age
# threat is positively and significantly related to depr, controlling for NICU and age
# depr is significantly higher than when NICU = Yes than when NICU = No, controlling for threat and age
# age is negatively and significantly related to depr, controlling for threat and NICU

# 10. Plot the effects of the selected model
library(effects) # plotting effects with the effects package
plot(allEffects(m2))
library(sjPlot) # plotting effects with the sjPlot package
plot_model(m2,type="pred",terms=c("threat"))
plot_model(m2,type="pred",terms=c("age"))
plot_model(m2,type="pred",terms=c("NICU"))

# 11. Compute the determination coefficient of the selected model
summary(m2)$r.squared # R2 = 0.10 --> the model explains 10% of the variance in depr (the 90% of depr variance remains unexplained)

# DAY 6 (Multilevel data processing) 
#####################################################################################################

# emptying the work environment
rm(list=ls())

# 1. loading files from Github
repo <- "https://github.com/SRI-human-sleep/INSA-home" # loading datasets from GitHub
load(url(paste0(repo,"/raw/main/Appendix%20D%20-%20Data/emaFINAL.RData")))
load(url(paste0(repo,"/raw/main/Appendix%20D%20-%20Data/demosFINAL.RData")))

# alternatively, you can download the files (from Github or Moodle), save them in your working directory - getwd() - and runt the followng lines
load("demosFINAL.RData")
load("emaFINAL.RData")

# selecting columns
ema <- ema[,c("ID","dayNr","stress","TST")] # ema = time-varying variables
demos <- demos[,c("ID","insomnia")] # demos = time-invariant variables

# 2. Print the first rows
head(ema) # How many rows per subject? many :)
head(demos) # How many rows per subject? one row per subject

# 3. Which variable includes individual observations? Individual observations are collected each day and indexed by the dayNr davriable
# which is the cluster variable? The clusters are subjects, indexed by the subject identification code (ID variable)
# which is the predictor? Based on the previous slide, the predictor is stress

# 4. Which variable(s) at the within-cluster level (Level 1)? stress and TST (and dayNr)
# Which variable(s) at the between-cluster level (Level 2)? ID and insomnia

# 5. Explore (descript., correlations, plots)
mean(ema$TST, na.rm=TRUE) # TST
sd(ema$TST, na.rm=TRUE)
mean(ema$stress, na.rm=TRUE) # stress
sd(ema$stress, na.rm=TRUE)
hist(ema$TST) # plotting
hist(ema$stress)
table(demos$insomnia) # frequency table
plot(demos$insomnia) # barplot
cor(ema$stress,ema$TST,use="complete.obs") # correlations

# 6. Compute the cluster mean for each level-1 variable using aggregate()
tst.between <- aggregate(TST ~ ID, data=ema, FUN=mean) # cluster means of TST
stress.between <- aggregate(stress ~ ID, data=ema, FUN=mean) # cluster means of stress

# 7. Join the cluster means to the demos dataset using cbind()
demos$ID <- as.factor(as.character(demos$ID)) # re-level the variable
demos <- cbind(demos,tst.between$TST,stress.between$stress) # joining
colnames(demos)[3:4] <- c("TST.m","stress.m") # changing column names

# 8. Join the cluster means to the ema dataset using plyr::join()
# install.packages("plyr") # if not already installed
library(plyr)
ema <- join(ema,demos,by="ID") # joining demos to ema

# 9. Subtract individual obs. from cluster means
ema$TST.cmc <- ema$TST - ema$TST.m
ema$stress.cmc <- ema$stress - ema$stress.m

# Extra: Compute the grand-mean-centered & the cluster-mean-centered values of stress and TST 
# Then, compute their Pearson’s correlation with the cor() function
demos$TST.gmc <- demos$TST.m - mean(demos$TST.m) # grand mean centering
demos$stress.gmc <- demos$stress.m - mean(demos$stress.m)
# note: cluster mean centering has been already done at point #9
cor(ema$TST.cmc,ema$stress.cmc, use="complete.obs") # correlation at the within-cluster level
cor(demos$TST.gmc,demos$stress.gmc) # correlation at the between-cluster level
cor(demos$TST.m,demos$stress.m) # note: at the between level, the correlation between gmc values is identical to the correlation between non-centered values

