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
rm(list=ls()) # remove the list of objects saved in the workspace

# 1. loading data
# install.packages("osfr") # only the first time if not already installed
library(osfr) # package to interact with the Open Science Framework platform
proj <- "https://osf.io/ha5dp/" # link to the OSF project
osf_download(osf_ls_files(osf_retrieve_node(proj))[2, ],conflicts="overwrite") # download and save in the current working directory
preg <- na.omit(read.csv("OSFData_Upload_2023_Mar30.csv",stringsAsFactors=TRUE)) # read data and omit NA values (not available = missing data)
colnames(preg)[c(2,5,12,14)] <- c("age","depr","NICU","threat") # set variable names (i.e., I subset the column names number 2,5,12,14 and make them equal to a new character vector)

# 2. Explore the variables
# mean and SD of quantitative variables
mean(preg$age) # mean of age
sum(preg$age)/nrow(preg) # the mean is the sum of all values divided by the number of values
sd(preg$age) # mean of SD
sqrt(var(preg$age)) # the SD is the squared root of the variance
sqrt(
  sum((preg$age-mean(preg$age))^2)/nrow(preg) # the variance is the sum of squared deviations from the mean divided by the number of values
     )
mean(preg$depr) 
sd(preg$depr)
mean(preg$threat) 
sd(preg$threat)
# frequency for categorical variables
table(preg$NICU) # table returns the frequency table (i.e., the number of values in each level of a variable)
100 * table(preg$NICU)/nrow(preg) # from frequency to percentage
# correlation between 2 quantitative variables
cor(x=preg$age, y=preg$depr) # the function cor returns the Pearson correlation between the x and the y variables
# correlation matrix among 3+ quantitative variables
cor(x=preg[,c("age","depr","threat")]) # here, x is a data.frame that I create by subsetting the three variables from the preg dataset (there is no y)
hist(preg$age) # histograms of quantitative variables (i.e., the taller the bars the more the cases with that value)
hist(preg$depr)
hist(preg$threat)
plot(preg$NICU) # barplot of categorical variables (i.e., the taller the bars, the higher the number of cases with that value)

# 3. fit a null model m0 predicting depr
m0 <- lm(formula = depr ~ 1, data = preg) # depr 'tilde' (predicted by/regressed on) 1 (intercept)

# 4. Fit a simple regression model m1 with depr being predicted by threat
m1 <- lm(formula = depr ~ threat, data = preg) # depr predicted by threat (the intercept is still there, even if not specified)

# 5. Fit a multiple regression model m2 also controlling for NICU and age
m2 <- lm(formula = depr ~ threat + NICU + age, data = preg) # depr predicted by the sum of the three variables (plus the intercept)

# 6. Fit an interactive model m3 to check whether age moderates the relationship between threat and depr.
m3 <- lm(formula = depr ~ threat + NICU + age + age:threat, data = preg) # the fourth predictor is the product of age and threat, standing for how much age *moderates* the relationship between threat and depr

# 7. Compare the models with AIC and likelihood ratio test: which is the best model?
AIC(m0,m1,m2,m3) # AIC: the lower the better (m2 is the best model)
lmtest::lrtest(m0,m1,m2,m3) # LRT: m2 is the best model (comparison between m2 and m3 is not significant: p = 0.7185, which is higher than 0.05)

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

# 4. Which variable(s) at the within-cluster level (Level 1)? stress and TST (and dayNr) because they variate both between and within cluster
# Which variable(s) at the between-cluster level (Level 2)? ID and insomnia because they only variate between clusters

# 5. Explore (descript., correlations, plots)
mean(ema$TST, na.rm=TRUE) # TST (note: I put na.rm=TRUE because the dataset includes several missing data)
sd(ema$TST, na.rm=TRUE)
mean(ema$stress, na.rm=TRUE) # stress
sd(ema$stress, na.rm=TRUE)
hist(ema$TST) # histogram of TST (the taller the bar the higher the number of cases with that value)
hist(ema$stress)
table(demos$insomnia) # frequency table (i.e., number of cases per level of a categorical variable)
plot(demos$insomnia) # barplot (the taller the bar the higher the number of cases with that value)
cor(ema$stress,ema$TST,use="complete.obs") # correlations

# 6. Compute the cluster mean for each level-1 variable using aggregate()
tst.between <- aggregate(TST ~ ID, data=ema, FUN=mean) # cluster means of TST (i.e., computing the mean of TST per each cluster)
stress.between <- aggregate(stress ~ ID, data=ema, FUN=mean) # cluster means of stress

# 7. Join the cluster means to the demos dataset using cbind()
demos$ID == tst.between$ID # check whether the ID variable is identical between the two datasets
# I got an Error saying "level sets of factors are different"
# let's see how many levels in each variable
nlevels(demos$ID) # 107 levels (subjects) in the demos datset
nlevels(tst.between$ID) # 93 levels (subjects) in the tst.between datset
# the problem is that we removed subjects with less than 30 complete days but the demos dataset still consider a factor with 107 subjects
demos$ID <- as.factor(as.character(demos$ID)) # re-level the variable (i.e., avoid considering levels with no cases)
demos$ID == tst.between$ID # now they all correspond and we can join the three datsets
demos <- cbind(demos,
               tst.between$TST,
               stress.between$stress) # joining
colnames(demos)[3:4] <- c("TST.m","stress.m") # changing column names (i.e., subsetting the third and fourth column names and making them equal to a new character vector)

# 8. Join the cluster means to the ema dataset using plyr::join()
# install.packages("plyr") # if not already installed
library(plyr)
ema <- join(ema,demos,by="ID") # joining demos to ema by the ID variable (i.e., all rows associated with a given ID value are attached to the rows associated with the same ID value)

# 9. Subtract individual obs. from cluster means
ema$TST.cmc <- ema$TST - ema$TST.m # cluster-mean-centered TST is equal to day-specific TST minus the corresponding cluster mean (i.e., the TST value measured on a specific time point from a specific subject minus the mean TST of that subject across all time points)
ema$stress.cmc <- ema$stress - ema$stress.m

# Extra: Compute the grand-mean-centered & the cluster-mean-centered values of stress and TST 
# Then, compute their Pearson’s correlation with the cor() function
demos$TST.gmc <- demos$TST.m - mean(demos$TST.m) # grand mean centering (i.e., subtracting the grand average from all values). Note: I'm doing it in the wide-form dataset, with one row per cluster
demos$stress.gmc <- demos$stress.m - mean(demos$stress.m)
# note: cluster mean centering has been already done at point #9
cor(ema$TST.cmc,ema$stress.cmc, use="complete.obs") # correlation at the within-cluster level (i.e., TST is lower than usual in those days when stress is higher than usual compared to days when stress is lower than usual)
cor(demos$TST.gmc,demos$stress.gmc) # correlation at the between-cluster level (i.e., TST is lower in subjects with higher stress compared to subjects with lower stress)
cor(demos$TST.m,demos$stress.m) # note: at the between level, the correlation between gmc values is identical to the correlation between non-centered values

# DAY 8 (Descriptives) 
#####################################################################################################

rm(list=ls()) # emptying the working environment

# 1. load data
getwd() # finding where my working directory is and pasting the file in there
studData <- read.csv("4-data/studentData.csv") # reading the file
# note: it is a comma separated value (CSV) file

# 2. mean, SD, frequencies
mean(studData$math_grade)
sd(studData$math_grade)
mean(studData$anxiety)
sd(studData$anxiety)
psych::describe(studData[,c("anxiety","math_grade")]) # the describe function from the psych package returns a set of descriptive stats for a given data.frame (here, I only considered the anxiety and math_grade variables)
table(studData$classID) # frequency table: number of cases per cluster

# 3. cluster means for anxiety
wide <- aggregate(anxiety ~ classID, data=studData, FUN=mean) # computing the mean anxiety for each classID

# 4. join anxiety cluster means to long-form dataset
studData <- plyr::join(studData,wide,by="classID") # attaching wide-form dataset values to the rows of the long-form dataset with the corresponding classID value
colnames(studData)[6] <- "anxiety.cm" # changing the name of the sixth variable to avoid having two variables with the same name

# 5. compute cluster-mean-center anxiety
studData$anxiety.cmc <- studData$anxiety - studData$anxiety.cm
# cluster-mean-centered anxiety is equal to each student's anxiety minus the average anxiety level of that student's class (i.e., cluster mean)

# 6. Repeat points 3-5 for math_grade
wide <- aggregate(math_grade ~ classID, data=studData, FUN=mean)
studData <- plyr::join(studData,wide,by="classID")
colnames(studData)[8] <- "math_grade.cm"
studData$math_grade.cmc <- studData$math_grade - studData$math_grade.cm

# 7. level-2 correlation (between-cluster correlation)
wide <- studData[!duplicated(studData$classID),] # moving to wide form (i.e., removing all rows whose classID value has been already taken in previous rows - that is, removing all classID duplicates so that we only have one row per each classID)
# note: between-cluster correlations are computed from the wide-form dataset to avoid overestimating the sample size (i.e., level-2 variable do not variate within clusters, so we don't wanna count two observations from the same cluster as two different cases)
cor(wide$anxiety.cm, wide$math_grade.cm)
# interpretation: classes with higher average anxiety levels show lower average math grades than classes with lower average anxiety levels

# 8. level-1 correlation (within-cluster correlation)
cor(studData$anxiety.cmc, studData$math_grade.cmc)
# interpretation: within a given class, students with higher anxiety than their class' mean anxiety show lower math grade than their class' mean math grade compared to sutdents with lower anxiety than their class' mean anxiety

# 9. fit a null LMER model with the lme4 package and get sigma2 and tau2_00
library(lme4)
fit <- lmer(math_grade ~ (1|classID), data = studData) # null LMER model where the synthax "(1|classID)" is used to indicate the random intercept based on the class ID variable
summary(fit)

# 10. compute and interpret the ICC
ICC <- 0.1115 / (0.1115 + 0.3278)
# ICC is equal to the variance of the random intercept divided by the sum between the variance of the random intercept and the residual variance
# note: the denominator is the model estimate of the total variance
# so the ICC is equal to the variance of the random intercept (between-cluster) divided by the residual variance
# so the ICC indexes the proportion of between-cluster variance over the total variance
ICC

# alternative way to compute the ICC (slightly different result only due to decimal rounding)
tau2 <- summary(fit)$varcor$classID[[1]] # extracting the variance of the random intercept
sigma2 <- summary(fit)$sigma^2 # extracting the residual variance
ICC <- tau2 / (tau2 + sigma2) # computing ICC
ICC

# interpretation:
# ICC = 0.25 means that the 25% of the variance in math_grade is between-cluster variance, whereas the remaining 75% is within-cluster variance
# this means that math_grade varies more between students than between classes
# possibly suggesting that student characteristics (e.g., student anxiety) are more important predictors of math_grade than class characteristics (e.g., teacher self-efficacy)
# yet, the 25% of the variance is accounted by local dependencies due to students being nested within classes, so we should use a multilevel approach

# DAY 9/10 (LMER model fit)
#####################################################################################################

# 1. Download & read the pre-processed dataset insa.RData (omitting missing data)
rm(list=ls()) # emptying the work environment
getwd() # get where your working directory is and save the data file in there
load("insa.RData") # read data from your working directory

# 2. Mean, SD, correlations & plots
mean(insa$TST) # mean (note: in the insa dataset there are no missing values, so we can omit "na.rm=TRUE")
sd(insa$TST)
mean(insa$stress) # note: although we work on cluster-mean-centered stress (stress.cmc), descriptives are usually computed from the original variable (more interpretable)
sd(insa$stress)
table(insa$insomnia) # these are the number of observations from each group, but since insomnia is a level-2 (between-cluster) variable, we should use the wide-form dataset
table(insa[!duplicated(insa$ID),"insomnia"]) # now we have the number of participants in each group
100*table(insa[!duplicated(insa$ID),"insomnia"])/nlevels(insa$ID) # extra: this is to compute the percentage of participants in each group

# 3. Fit a null LMER model `m0` of `TST` and compute the ICC
library(lme4) # open the lme4 package
m0 <- lmer(TST ~ (1|ID), data = insa)

# let's compute the ICC
tau2 <- summary(m0)$varcor$ID[[1]] # tau_00 squared = variance of the random intercept
sigma2 <- summary(m0)$sigma^2 # sigma squared = variance of the residuals
ICC <- tau2 / (tau2 + sigma2) # ICC = random intercept variance / total variance = random intercept variance / (random intercept variance + residual variance)
ICC # ICC = 0.19 --> the 19% of the variance in TST is at the between-cluster level (i.e., TST mainly varies within cluster than between clusters)

# 4. Fit a model `m1` with `TST` being predicted by `stress.cmc`
m1 <- lmer(TST ~ stress.cmc + (1|ID), data = insa)

# 5. Fit a model `m2` with a random slope for `stress.cmc`
m2 <- lmer(TST ~ stress.cmc + (stress.cmc|ID), data = insa)

# 6. Inspect the `summary()` of each model
summary(m0) # fixed effects only include the fixed intercept (410.838), while random effects include the random intercept variance (1183) and the residual variance (5158)
summary(m1) # fixed effects include the fixed intercept (410.849) and the fixed slope (-4.921), while random effects include the random intercept variance (1186) and the residual variance (5138)
# note that the residual variance has slightly diminished compared to model m0 because we added one predictor, and so we explain some of the unexplained (residual) variance
summary(m2) # fixed effects include the fixed intercept (410.909) and the fixed slope (-5.868), while random effects include the random intercept variance (1183.71), the random slope variance (87.26), and the residual variance (5071.19)
# note that the residual variance has further diminished due to the same reason

# Is there a 'substantial' within-individual relationship between TST and stress?
# yes, from the summary of model m1 we can see that the t-value for stress.cmc (-4.132) is higher than |1.96| (i.e., it is lower than -1.96), so the relationship is negative and 'substantially' lower than zero 
# even when including the random slope (model m2), the t-value for stress.cmc (-3.562) is 'substantially' lower than -1.96
# so we can say that there is a 'substantial' negative relationship between stress and TST at the within-cluster level (level 1)

# extra: we can even compute 95% confidence intervals for the stress.cmc slope:
(fixeffs <- summary(m2)$coefficients) # taking the fixed coefficient table
(stress_slope <- fixeffs[2,1]) # selecting fixed slope for stress.cmc
(stress_slope_SE <- fixeffs[2,2]) # selecting fixed slope standard error for stress.cmc
stress_slope + 1.96*stress_slope_SE # upper CI: estimate + 1.96 X standard error
stress_slope - 1.96*stress_slope_SE # lower CI: estimate - 1.96 X standard error
# since zero is not included within the 95% confidence intervals, we might conclude that the estimated coefficient is significantly lower than zero

# 7. Fit a model `m3` that also includes `insomnia` group differences
m3 <- lmer(TST ~ stress.cmc + insomnia + (stress.cmc|ID), data = insa)
summary(m3) # fixed effects include the fixed intercept (409.624), the fixed slope for stress.cmc (-5.684), and the fixed slope for insomnia (2.524), while random effects include the random intercept variance (1195.95), the random slope variance (87.22), and the residual variance (5071.23)

# interpretation:
# - intercept: the predicted ('mean') TST value when stress.cmc = 0 (i.e., average stress level) and insomnia = 0 (i.e., control group) is 409.6 minutes
# - stress slope: considering both control and insomnia, a 1-unit increase in stress.cmc (i.e., higher stress than usual) predicts a decrease in TST by -5.68 minutes
# note: we say "considering both controls and insomnia" because the interaction is not yet included - so the model makes a sort of average between the two groups
# - insomnia slope: when stress.cmc = 0 (i.e., average stress levels), the insomnia group is predicted to show an average TST of 2.52 minutes higher than the average TST in the control group (i.e., 409.6 + 2.52 = 412.12 minutes)

# Any group differences? 
# no, or well there is a positive difference such that insomnia have a higher average TST than the control group
# yet, this difference is not 'substantial' since the t-value is lower than 1.96

# Does it change the effect of `stress`?
# the negative relationship between stress.cmc and TST has increased from -5.686 (model m2) to -5.684 (model m3)
# so we can say that the inclusion of insomnia did not greatly impact the slope estimated for stress.cmc (i.e., it did not greatly impact the relationship between stress.cmc and TST)
  
# 8. Fit a model `m4` that also includes the interaction between `insomnia` and `stress.cmc`
m4 <- lmer(TST ~ stress.cmc * insomnia + (stress.cmc|ID), data = insa) # to do that, we can just substitute the "+" symbol with the "*" symbol
m4 <- lmer(TST ~ stress.cmc + insomnia + stress.cmc:insomnia + (stress.cmc|ID), data = insa) # alternative way to write the same model

# 9. Inspect the `summary()` of of model `m4`
summary(m4) # fixed effects include the fixed intercept (409.505), the fixed slope for stress.cmc (-7.187), the fixed slope for insomnia (2.759), and the interaction between stress.cmc and insomnia (2.923), while random effects include the random intercept variance (1196.32), the random slope variance (86.44), and the residual variance (5071.75)

# interpretation (notice how it changes from the additive model m3 to the interactive model m4):
# - intercept: the predicted ('mean') TST value when stress.cmc = 0 (i.e., average stress level) and insomnia = 0 (i.e., control group) is 409.505 minutes ---> same intepretation than in the additive model m3
# - stress slope: in the control group, a 1-unit increase in stress.cmc (i.e., higher stress than usual) predicts a decrease in TST of -7.187 minutes
# note: we say "in the control group" because the interaction is included in the model - so the model focuses this coefficient in the control group and quantifies the difference between the two groups in the interactive term below
# - insomnia slope: when stress.cmc = 0 (i.e., average stress levels), the insomnia group is predicted to show an average TST of 2.76 minutes higher than the average TST in the control group (i.e., 409.505 + 2.759 = 412.264 minutes) ---> same interpretation than in the additive model m3
# - interaction (first interpretation): the relationship between stress.cmc and TST in the insomnia group is 2.923 minutes-per-stress higher than in the control group (i.e., -7.187 + 2.923 = -4.26); in other words, a 1-unit increase in stress.cmc (higher stress than usual) predicts a TST reduction of -7.187 minutes in the control group and a decrease  4.26 minutes in the insomnia group
# - interaction (second interpretation): the TST difference between insomnia and controls is 2.923 minutes larger when stress.cmc increases by 1 unit (i.e., higher stress than usual) compared to when stress.cmc = 0 (i.e., average stress level); in other words, stress enlarges the differences beween the two groups, with insomnia sleeping further more hours than controls

# Does `insomnia` moderate the within-individual relationship between `stress` and `TST`?
# no, or well there is a positive interaction (2.923), such that stress.cmc decreases TST more in the control than in the insomnia group
# yet, this interaction is not 'substantial' since the t-value is lower than 1.96

# DAY 11 (Reading the Results section of a paper - pt1)
#####################################################################################################

# Graham et al (2020) - Table 3
# ____________________________________________________________________________

# 1. Which variable identifies individual observations and which is the cluster variable?
## individual observations are identified by children (i.e., one observation per child), whereas neighborhoods identify the cluster variable
## note: this can be inferred from the Random Effects section of the table or by looking at the Analytical strategy section of the paper 

# 2. Which predictors are at level 1 (within-cluster)? Which at level 2 (between-cluster)?
## level-1 predictors are those shown under the 'Individual characteristics' section of the table: Race, Sex, Family type, Income, Mother's educational attainment, Perceived neighborhood disorder
## level-2 predictors are those shown under the 'Neighborhoods characteristics' section of the table: Percent non-Hispanic Black,  Percent foreign born, Percent female head of household with children, Percent age 25+ with less than high school degree, Percent age 25+ with a master's degree or greater, Neighborhood economic stress scale	

# 3. Do the authors report the random effects? Which ones?
## Yes, the authors report the random intercept variance ("Variance among neighborhoods") and the residual variance ("Variance among individuals")

# 4. Does the model include 1+ random slopes? For which predictor(s)?
## No, the model does not include any random slope (just the random intercept).

# 5. Do the authors report estimate SE, t-value, 95% CI?
## no, the table only reports estimate and SE, but no t-values or 95% CI


# Ersan & Rodriguez (2020) - Table 5
# ____________________________________________________________________________

# 1. Which variable identifies individual observations and which is the cluster variable?
## individual observations are identified by students (i.e., one observation per student), whereas schools idenfity the cluster variable
## note: you need to take a look at the paper to answer that (particularly the Analysis subsection of the method: "Our models have two levels: ...")

# 2. Which predictors are at level 1 (within-cluster)? Which at level 2 (between-cluster)?
## level-1 predictors are student-level predictors: ses (socioeconomic status), Engage, Litnum, Liking, and Preschool
## level-2 predictors are M_ses (mean ses in each school) Instruction, Readiness, Conditions, Emphasis, and Safe
## How to get that from Table 5: all level-1 predictor coefficients are reported with the Greek letter 'beta' and are decomposed into a 'slope intercept' (fixed slope) and a random slope (under the "random effects" section of the table), whereas level-2 predictors don't have the 'beta' coefficient and don't variate within cluster (note: only level-1 variables can variate within clusters)
## How to get that more easily: just look at model 4 in Table 2 :)

# 3. Do the authors report the random effects? Which ones?
## Yes, the table reports the following random coefficients: the variance of the random intercept (Mean achievement, u_0j), the variance of the random slope for any level-1 predictor (u_1j, u_2j, u_3j, u_4j), and the residual variance (r_ij)
## Yet, the table does not seem to report the correlations among random effects (i.e., the correlation between the random intercept and any random slope, the correlations among random slopes)

# 4. Does the model include 1+ random slopes? For which predictor(s)?
## Yes, the models include all possible random slopes, i.e., the random slope of any level-1 predictor included in a given model
## e.g., since no level-1 predictors are included in Model 1, no random slope are included and reported
## e.g., in Model 2, ses is the only level-1 predictor, so only its random slope is included and reported
## e.g., in Model 4, four level-1 predictors are included and all random slopes are included and reported

# 5. Do the authors report estimate SE, t-value, 95% CI?
## No, the table only reports coefficient and standard error


# Juvrud et al (2021) - supplementary table S2
# ____________________________________________________________________________

# 1. Which variable identifies individual observations and which is the cluster variable?
## individual observations are identified by trials (i.e., one observation per trial), whereas infants (subjects) identify the cluster variable
## note: you need to take a look at the paper (method section) to answer this

# 2. Which predictors are at level 1 (within-cluster)? Which at level 2 (between-cluster)?
## level-1 predictors are trial-level predictors: emotion manipulation (i.e., angry vs. fearful, etc.), SetSize (1 vs. 5), and face manipulation (familiar vs. stranger)
## level-2 predictors are the mother ratings at the PANAS (i.e., since each infant only has 1 mother)
## note: the table alone is not sufficient to answer these questions, i.e., you need to take a look at the paper (particularly the Stimuli and the Procedure sections)

# 3. Do the authors report the random effects? Which ones?
## No, the table only shows fixed effects (the authors did not report the variance of the random intercept or that of the residuals)

# 4. Does the model include 1+ random slopes? For which predictor(s)?
## No, the model does not include any random slope (just the random intercept).
## note: again, this cannot be answered based on the Table because it doesn't show the random effects. You need to look at the paper (Data analysis subsection of the Method).
## In this case, the authors say "We computed a LMM with participant as a random effect to allow for individual variability in baseline eye movement reaction times"
## "individual variability in baseline RT" means random intercept
## but they do not specify that the effects/relationships/differences of interest were allowed to randomly variate as well (i.e., no random slope)

# 5. Do the authors report estimate SE, t-value, 95% CI?
## yes, all these indices are reported in the table


# DAY 12 (Infants’ pupil dilation - model fit and diagnostics)
#####################################################################################################

rm(list=ls()) # emptying the work environment

# 1. Download and read the dataset
library(osfr) # package to interact with the OSF platform
proj <- "https://osf.io/p8nfh/" # link to the OSF project (see protocol paper & data dictionary)
osf_download(osf_ls_files(osf_retrieve_node(proj))[5,],conflicts="overwrite") # download
infants <- read.csv2("data/multiverse.csv",stringsAsFactors=TRUE) # read dataset
colnames(infants)[c(18,17,19)] <- c("id","fam","pupil") # shortening variable names
infants$pupil <- as.numeric(infants$pupil) # pupil as numeric

infants <- read.csv2("infantPupil.csv") # alternatively you can download the data from Moodle/Github and read it from you WD
colnames(infants)[c(18,17,19)] <- c("id","fam","pupil") # shortening variable names
infants$pupil <- as.numeric(infants$pupil) # pupil as numeric

# 2. explore the variables: mean, sd, freq., and correlations
mean(infants$pupil)
sd(infants$pupil)
boxplot(infants$pupil)
hist(infants$pupil)

# computing the No. of observations per infant id
table(infants$id) # number of individual observations per cluster

# computing the No. of observations per fam category --> more or less 50/50
table(infants$fam)

# computing the No. of observation per fam and id cateogry --> all infants performed both labeled and unlabeled trials
table(infants$fam,infants$id)

# 3. Which variable identifies individual observations and which is the cluster variable? How many clusters?
## Individual observations are the single pupul measures within each 1-sec trial (e.g., 1 each ms)
## Clusters are infants (id). And we have 16 clusters
 
## the cluster variables are the infants, indexed by the 'id' variable
nlevels(as.factor(infants$id)) # number of clusters = 16

# Which predictor(s) at lv1, which at lv2?
# id is the only level-2 predictor (only varying across clusters)

# fam is neither an individual-obs-level (level-1) variable (because it does not change within trials)
# nor a cluster-level (level-2) variable (because it changes within clusters)
# it is something in the middle

# Fit a null LMER model m0 and compute the ICC for the variable pupil
library(lme4)
#      lmer(Yij   ~ (1|cluster), data) # null RI model synthax
m0 <- lmer(pupil ~ (1|id), data=infants  ) # null model (or intercept-only model)
sigma2 <- summary(m0)$sigma^2
tau2 <- summary(m0)$varcor$id[[1]]
ICC <- tau2 / (tau2 + sigma2)
ICC 
ICC*100 # icc in percentage
# interpretation: pupil is a more within-level variable, it variates more within than between clusters
# specifically, we estimated that the 31% of the variance in pupil is at the between-cluster level
# whereas the remaining quote (1-31% = 69%) is within

# 6. Fit a random-intercept model m1 that includes the fixed effect 'fam'
m1 <- lmer(pupil ~ fam + (1|id), data=infants)

# 7. Fit a random-slope model m2 (i.e., ‘free’ the random slope for fam)
m2 <- lmer(pupil ~ fam + (fam|id), data=infants)

# 8. check assumptions of model m2
## a) normality and linearity of residuals
hist(residuals(m2)) # they seem centered on zero (ok linearity)
qqnorm(residuals(m2)); qqline(residuals(m2)) # they seem normally distributed (ok normality)

## b) independence and homoscedasticity considering the residuals
plot(m2) # we can see a negative trend (ko independence) but no trends in the variability (ok homoscedasticity)

## c) normality and linearity of random intercept
RI <- ranef(m2)[[1]][,1] # extracting random intercept
hist(RI) # centered on zero (ok linearity)
qqnorm(RI); qqline(RI) # not very normally distributed (ko normality)
RS <- ranef(m2)[[1]][,2] # extracting random slope
hist(RS) # centered on zero (ok linearity)
qqnorm(RS); qqline(RS) # more normally distributed than the random intercept (ok normality)
## note: we cannot really evaluate linearity and normality because we have no level-2 predictors others than the cluster variable 'id'

## d) absence multicollinearity
car::vif(m2) # I got this error: Error in vif.merMod(m2) : model contains fewer than 2 terms. Why so?
## that's because we only have 1 predictor (fam) so we cannot talk about excessively correlated predictors (that would require having 2+ predictors)
## e) absence of influential cases
## first, let's inspect the Cook's distance of each individual observation
boxplot(cooks.distance(m2)) # we can see 1/2 individual observations with extreme Cook's distance (we should try removing them)
##second, let's inspect the Cook's distance of each cluster
library(influence.ME)
plot(influence(m2, group="id"), which="cook") # we can see that participant #29 has an extreme Cook's distance (we should try removing her/him)

# 9. Print, visualize, & interpret the fixed effects estimated by model m2: Is hypothesis HP1 confirmed?
summary(m2)$coefficients # fixed effects with SE and t-values
# we can say that the difference between labeled and unlabeled (-579) is not 'substantial' because |t| < 1.96
library(sjPlot)
plot_model(m2) # forest plot: we can say that the difference between labeled and unlabeled is not significant because zero is included in the confidence intervals 
plot_model(m2,type="pred",terms="fam") # effect plot: we can say that the two conditions do not significantly differ because the mean of the first one is included in the confindence intervals of second one, and viceversa


# DAY 13 (Infants’ pupil dilation model comparison)
#####################################################################################################

# 1. Fit models m0, m1, and m2 as in slide #66
## see and run the code from the previous exercise

# 2. We want to account the habituation effect on pupil dilation: fit a third model m3 that also includes time (time in ms over the trial), and a fourth model m4 including session (reflecting time on task)
m3 <- lmer(pupil ~ fam + time + (fam|id), data=infants)
m4 <- lmer(pupil ~ fam + time + session + (fam|id), data=infants)

## note: I got a Warning message: Some predictor variables are on very different scales: consider rescaling 
## Warning messages are not like Errors (i.e., only the latter means that the code did not work), but we should be aware of their meanings
## In this case, it says that the variable time is measured on a very different scale (from 0 to 15776 milliseconds) than the variable fam (with just 2 levels) and session (from 1 to 9)
summary(infants[,c("fam","time","session")]) # summary of the range of the variable scales
## because of these huge scale differences, some parameters might be difficult to interpret (e.g., because they are like on the 0.000001 scale)
summary(m4)$coefficients # note how small is the Estimate for time and its standard error, this is due to the very large scale of this variable compared to other variables
## if these were our real analyses, we might have rescaled time, e.g., to express it in seconds rather than milliseconds
## just for curiosity, let's take a look on how that would change the coefficients:
infants$time_sec <- infants$time/1000 # time in seconds
m4_sec <- lmer(pupil ~ fam + time_sec + session + (fam|id), data=infants) # refitting model m4 (note that now I'm not getting the Warning message)
summary(m4_sec)$coefficients # now Estimate and Std. Error values are much more readable
## note: in the original model, Estimate for time is the predicted change in pupil dilation for a 1-millisecond increase in time, whereas in model m4_sec is the predicted change for a 1-second increase in time!

# 3. Evaluate model m4 diagnostics
## normality and linearity of residuals
hist(residuals(m4)) # normality: not perfectly symmetric but almost there; linearity: the distribution is centered on zero (ok)
qqnorm(residuals(m4)); qqline(residuals(m4)) # qqplot shows some strong deviation from normality (if these were our real data we should have considered alternative family distributions, e.g., Gamma)
## homoscedasticity and independence of residuals
plot(m4) # the plot of residuals vs. fitted (i.e., predicted) values clearly shows a violation of the assumption of independence because we can see a clear negative trend --> this model might be wrong!
# in contrast, we don't see strong deviations from homoscedasticity (i.e., the range/spreadness of the residuals is quite uniform over the range of fitted values)
## optional: let's check the same assumptions by considering each predictor
boxplot(residuals(m4)~infants$fam) # fam: no trends, no different variance (ok)
plot(residuals(m4)~infants$time) # time: positive trend, lower variance for higher time values (ko)
boxplot(residuals(m4)~infants$session) # time: no strong linear trend (ok) but some sessions (2 and 8) have lower variance (not ok with homoscedasticity)
## we can conclude that the homoscedasticity and the independence are not met due to time and session 
## extracting random effects and wide dataset
wide <- infants[!duplicated(infants$id),] # wide dataset (1 row per infant)
RI <- ranef(m4)[[1]][,1] # r. intercept
RS <- ranef(m4)[[1]][,2] # r. slope
## normality and linearity of random effects
qqnorm(RI); qqline(RI) # random intercept shows some deviation from normality but is ok. It is also centered on zero (linearity ok)
qqnorm(RS); qqline(RS) # more or less the same for the random slope
## homoscedasticity and independence of random effects
## In this case, it make no sense to plot random effects against predictors because all predictors are at level 1 and cannot be averaged by cluster (infant)
## that is, it make no sense to compute the average session or time per infant; it also does not make sense to 'average' the fam variable by infant
## so we don't need to test this assumption here

## note: in real analyses, we should have found different ways to model these data (e.g., Gamma GLMER, GAM) because diagnostics are bad
## but here we don't care about it because it's just an exercise :)

# 4. Compare all models with the likelihood ratio test and the AIC: which is the best model?
library(lmtest)
lrtest(m0,m1,m2,m3,m4) # likelihood ratio test: all comparisons are significant, thus we select the most complex model m4
## note: that means that the inclusion of each predictor is associated with a significant increase in the model likelihood compared to the model without that predictor
## so all predictors significantly contribute to the model fit (its ability to predict data similar to the observed data)
AIC(m0,m1,m2,m3,m4) ## AIC: the best model (with the lowest AIC) is model m4
## note: here is how to select the lowest value: AIC(m0,m1,m2,m3,m4)[which(AIC(m0,m1,m2,m3,m4)$AIC==min(AIC(m0,m1,m2,m3,m4)$AIC)),]

# 5. Print, visualize, and interpret the coefficients estimated by the selected model: which fixed effects are significant?
summary(m4)$coefficients # here's the summary table, it shows t values higher than 1.96 for the intercept and the session slope, and a t value lower than -1.96 for time
## although t-values are not enough to establish statistical significance, we can say that pupil dilation seems to 'substantially' decrease over time and 'substantially' increase over session, whereas the unlabeled-labeled difference is not 'substantial'
library(sjPlot)
plot_model(m4) # the forest plot shows a very small (but 'significant') negative effect of time (red = negative) and positive effect of session (blue = positive)
## note: only confidence intervals of fam can be clearly shown because they are wide (so wide that they include zero, meaning that the differences by fam are not 'significant')
## in contrast, the confidence intervals for time and session are very small (basically not visible in the plot); that's because we used very different scales for the predictors
plot_model(m4,type="pred")[1] # effect plot of fam: since each mean (dots) is included in the other condition confidence intervals (dashed lines), we can conclude that the difference is not 'significant'
plot_model(m4,type="pred")[2] # effect plot of time: since the mean effect (red line) when time is 0 is not included in the confidence intervals (pink band) corresponding to higher time values, and viceversa, we can conclude that this is a 'significant' negative relationship
plot_model(m4,type="pred")[3] # effect plot of session: here, confidence intervals are very wide and the effect of session does not look 'significant' at all
## note: the third plot is inconsistent with the summary table above: this is another confirmation that computing confidence intervals from LMER models can be very unreliable (see slide #69)

# 6. Print and interpret the coefficient of determination R2 of the selected model
library(MuMIn)
r.squaredGLMM(m4)
## marginal R2: fixed effects only predict the 1.6% of the variance in pupil dilation
## conditional R2: fixed and random effects together predict the 28.8% of the variance in pupil dilation


# DAY 13 (Reading the Results section of a paper - pt2)
#####################################################################################################

# Graham et al (2020) - Table 3
# ____________________________________________________________________________

# 1. Did the authors compare multiple models? Based on which criteria?
## yes, the table shows 5 models, which are compared based on the AIC and the likelihood ratio test (chi2) - see the bottom of the table

# 2. Do the authors report the likelihood ratio test of their models? Which is the best model?
## yes, they report the likelihood ratio test, supporting Model 5 as the best model (i.e., the best model is the most complex model showing a significant 'chi2' statistics; since here all models have a significant 'chi2', as shown by the asterisks ***, we can conclude that the best model is the last model, model 5)

# 3. Do the authors report the AIC and BIC indicators? Which is the best model?
## the authors only report the AIC, supporting Model 5 a the best model (i.e., the model with the lowest AIC value: 151,848)

# 4. Do the authors report the statistical significance of the estimated parameters? Which fixed effect is significant?
## yes, they indicate the statistical significance of the parameters by using the asterisk
## for instance, in model 1 there is a significant intercept and a significant slope for Race (all levels significantly differ from the reference) and sex but not for Family type

# 5. Do the authors report the coefficient of determination? If yes, what proportion the y variance is explained by the models?
## no, coefficeints of determinations are not reported in the table


# Ersan & Rodriguez (2020) - Table 5
# ____________________________________________________________________________

# 1. Did the authors compare multiple models? Based on which criteria?
## yes, the table shows 5 models, which are compared based on the explained variance (R squared) - see the bottom of the table

# 2. Do the authors report the likelihood ratio test of their models? Which is the best model?
## no, the table don't report the likelihood ratio test statistics

# 3. Do the authors report the AIC and BIC indicators? Which is the best model?
## no, the table don't report the AIC or the BIC

# 4. Do the authors report the statistical significance of the estimated parameters? Which fixed effect is significant?
## yes, they wrote "All fixed-effects parameters were significant at the alpha level of .01, except (†)" (see table notes)
## so let's say they highlighted non-significant parameters because most of them were significant

# 5. Do the authors report the coefficient of determination? If yes, what proportion the y variance is explained by the models?
## not exactly, they report an estimated of the percent explained variance (which is the coeff. of determination) at each level, although they do not explicitly refer to the R2 coefficient of determination
## the models explain from 4 to 18% of level-1 variance and from 16 to 72% of level-2 variance, respectively


# Juvrud et al (2021) - supplementary table S2
# ____________________________________________________________________________

# 1. Did the authors compare multiple models? Based on which criteria?
## no, the table only shows 1 model (and the paper does not mention any comparison)

# 2. Do the authors report the likelihood ratio test of their models? Which is the best model?
## no, the table don't report the likelihood ratio test statistics

# 3. Do the authors report the AIC and BIC indicators? Which is the best model?
## no, the table don't report the AIC or the BIC

# 4. Do the authors report the statistical significance of the estimated parameters? Which fixed effect is significant?
## yes, the table report the exact p-values under the "Sig." column
# the significant fixed effects are those with a p-value lower than 0.05 (or with 95% CI excluding zero, but the two information are equivalent): Blurred, Happy*SetSize5, Blurred*SetSize5, and Fearful*PANAS*SetSize5,  

# 5. Do the authors report the coefficient of determination? If yes, what proportion the y variance is explained by the models?
## no, the table does not report the coefficient of determination (and the paper does not mention it)
