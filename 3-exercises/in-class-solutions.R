# ####################################################################################################################
# IN-CLASS EXERCISES SOLUTION
# This script includes the solutions to the exercises that we saw in class, during the lectures
# ####################################################################################################################

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