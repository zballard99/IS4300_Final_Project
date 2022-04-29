##Welcome to my Final Project for IS 4300
##Zachary Ballard
library(dslabs)
library(rpart)
library(tidyverse)
library(caret)
library(ggthemes)

# Get the Default Data from GitHub
##put my github URL for this
# This is the URL for the data that will be analyzed.
urlfile <- "https://github.com/zballard99/IS4300_Final_Project/blob/main/Default.csv"
# Read the data into R and name it cricketers.
default_data <- read_csv(url(urlfile))


#default_data <- read_csv(file = 'default.csv')
head(default_data)
str(default_data)



dd <- as.data.frame(default_data)
##Visualize
dd <- dd %>% ggplot()
dd1 <- dd + geom_point(aes(income,balance, col=default)) + ##graph the variables
  labs(x = "Income", y = "Avg Balance", title = "Defaults by Income and Average Balance",
       color="Default") ##Adjust the titles for x&y axis, title, and legends.

dd2 <- dd + geom_point(aes(income,balance, col=default, shape = student),alpha=.4, size = 2) + ##graph the variables
  labs(x = "Income", y = "Avg Balance",title = "Defaults by Income and Average Balance",
       color="Default", shape = "Student") ##Adjust the titles for x&y axis, title, and legends.

dd3 <- dd + geom_point(aes(income,balance, col=default, shape = student),alpha=.7, size = 2) + ##graph the variables
  labs(x = "Income", y = "Avg Balance",title = "Extract of Default Data ",
       color="Default", shape = "Student") + ##Adjust the titles for x&y axis, title, and legends.
  ylim(1500,2500) + #Filter for just those with average balance greater than 1500 and less than 2000. 
  xlim(5000,70000) #Filter for just those with income greater than 5000 and less than 70000. 
dd1 + theme_solarized() ##Themes not needed but it is nice to get variety. 
dd2 + theme_pander()
dd3 + theme_economist()


##Analysis
defaultd <- default_data
##Standard Data:

#S for statistic 1-5
#Max Income:
S1 <-max(defaultd$income) #highest income represented is 73554.23
#Min Income:
S2 <- min(defaultd$income) #lowest income represented is 771.97
#Avg Income:
S3 <- mean(defaultd$income) #Average income is 33516.98
#Highest Avg Bal:
S4 <- max(defaultd$balance)  #Max avg bal after monthly payment is 2654.32
#Lowest Avg Bal:
S5 <- min(defaultd$balance) #Lowest avg bal is 0

SStat <- c("Highest Income","Lowest Income","Average Income",
            "Highest Monthly Balance", "Lowest Monthly Balance") #Name the stats
SCalc <- c(S1,S2,S3,S4,S5) #insert stat values
data.frame(Statistic = SStat,Value = SCalc) #create stats table

#Number of students in data set:
table(defaultd$student)
#2944 of the observations in this dataset are students. 
perS <- 2944/10000  #or 29.44% of the data set is made up of students
paste(round(100*perS, 2), "%", sep="")



#Separate table, prob stats
#PS is for Probability statistic/percentage

##Prob of default for income < 20000
dil20 <- defaultd %>% filter(income < 20000)
table(dil20$default)
PS1 <- 93/(2069+93)  ##4.3% of individuals with under 20k income defaulted
PS1 <- paste(round(100*PS1, 2), "%", sep="")

##probability of default for income between 20000 and 40000
dil4020 <- defaultd %>% filter(income < 40000 & income > 20000)
table(dil4020$default)
PS2 <- 135/(4206+135)  #3.11% of individuals with between 20k and 40k income defaulted
PS2 <- paste(round(100*PS2, 2), "%", sep="")

#Probability of default for income > 40000
dig40 <- defaultd %>% filter(income > 40000)
table(dig40$default)
PS3 <- 105/(3392+105) #3% of individuals with more than 40k income defaulted
PS3 <- paste(round(100*PS3, 2), "%", sep="")

##Probability of default if student
dsy <- defaultd %>% filter(student == 'Yes')
table(dsy$default)
PS4 <- 127/(2817+127) #4.31% of students defaulted
PS4 <- paste(round(100*PS4, 2), "%", sep="")

##Odds of default if not student
dsn <- defaultd %>% filter(student == 'No')
table(dsn$default)
PS5 <- 206/(6850+206) #2.92% of non students defaulted. 
PS5 <- paste(round(100*PS5, 2), "%", sep="")

#probability of default if average balance > 1500
dbg15 <- defaultd %>% filter(balance > 1500)
table(dbg15$default)
PS6 <- 258/(653+258) #28.32% of clients with over a 1500 average monthly balance defaulted
PS6 <- paste(round(100*PS6, 2), "%", sep="")

#Probability of default if average balance < 500
dbl5 <- defaultd %>% filter(balance <500)
table(dbl5$default) #0% of clients with under a 500 avg monthly balance defaulted. 
PS7 <- 0
PS7 <- paste(round(100*PS7, 2), "%", sep="")

ProbN <- c("Income Less than 20k","Income between 20k and 40k","Income greater than 40k",
           "Student", "Non Student",
           "Average Balance greater than 1500", "Average Balance < 500") #Name the probabilities
ProbS <- c(PS1,PS2,PS3,PS4,PS5,PS6,PS7) #insert the values, already in % and rounded
data.frame(Probability_Of_Default_if = ProbN, Value = ProbS)


##nearest neighbors
defaultd$default <- as.factor(defaultd$default)
set.seed(2022,sample.kind = "Rounding") ##put in the model so we can duplicate results
testIndex <- createDataPartition(defaultd$default, times = 1, p = .2, list = FALSE)   
##Make p = .2 for larger dataset
train_set <- defaultd[-testIndex,] ##not the test index
test_set <- defaultd[testIndex,]  ##uses the text index

knn_5 <- knn3(default ~ .,data = train_set,k=5) # use the 5 nearest neighbors on 
# the train set.

# Now use the knn_5 model to predict what would happen on the test set.

Default_predict <- predict(knn_5, test_set,type = "class") %>%
  factor(levels = levels(train_set$default))

T <- table(test_set$default,Default_predict)
##Create a new table showing you a total table with predicted
##versus actual for the Class variable. Compute accuracy from here
##as well as sensitivity and specificity.
T
T <- as.vector(T) #convert to vector
# Accuracy of the prediction is (num. correctly predicted)/(total)
accuracy <- (T[1]+T[4])/(T[1]+T[2]+T[3]+T[4]) # correct/total,to show the accuracy,
#use this to calc
# Sensitivity is Pr(predict 1 given actually 1) = 
sensitivity <- T[4]/(T[2]+T[4])   #we are focusing on the data that is actually Yes, how well did we predict it
# Specificity is Pr(predict 0 given actually 0) = 
specificity <- T[1]/(T[1]+T[3]) ##How good are we at predicting no default when its actually no (the other variable)

metric <- c("Accuracy","Sensitivity","Specificity") #create the metric of three measurements
value <- c(accuracy,sensitivity,specificity)
data.frame(Metric = metric,Value = round(value,3)) #This outputs our three measurements rounded
##Other than sensitivity, we get good accuracy and specificity metrics. 
##Unfortuntately, sensitivity is how well we predict defaults, so this line of
##testing provides really poor results. We are great at predicting no default (specificity). 
##But considering how massive the proportion of no default clients is, this is no surprise. 



##Tune of knn3
kv <- seq(1,15,1)  # try neighbors 1-15
# The following function computes an F score for each k.
F_1 <- sapply(kv, function(x){  #helps us identify which set of neighbors is the best predictor
  knn_ht <- knn3(default ~ .,data = train_set,k=x)
  Default_hat_kh <- predict(knn_ht, test_set,type = "class") %>%
    factor(levels = levels(train_set$default))
  F_meas(data = Default_hat_kh, reference = test_set$default)
})
kv[which.max(F_1)] # we can go back and run the model with k = 6
plot(kv,F_1,type = "l") #shows us a plot of the results

#Now use k=6
knn_6 <- knn3(default ~ .,data = train_set,k=6) # use the 6 nearest neighbors on 
# the train set.

# Now use the knn_6 model to predict what would happen on the test set.

Default_predict <- predict(knn_6, test_set,type = "class") %>%
  factor(levels = levels(train_set$default))
T <- table(test_set$default,Default_predict)
##Create a new table showing you a total table with predicted
##versus actual for the Class variable. Compute accuracy from here
##as well as sensitivity and specificity.
T
T <- as.vector(T) #convert to vector
# Accuracy of the prediction is (num. correctly predicted)/(total)
accuracy <- (T[1]+T[4])/(T[1]+T[2]+T[3]+T[4]) # correct/total,to show the accuracy,
#use this to calc
# Sensitivity is Pr(predict 1 given actually 1) = 
sensitivity <- T[4]/(T[2]+T[4])   #we are focusing on the data that is actually Yes, how well did we predict it
# Specificity is Pr(predict 0 given actually 0) = 
specificity <- T[1]/(T[1]+T[3]) ##How good are we at predicting no default when its actually no (the other variable)

metric <- c("Accuracy","Sensitivity","Specificity") #create the metric of three measurements
value <- c(accuracy,sensitivity,specificity)
data.frame(Metric = metric,Value = round(value,3)) #This outputs our three measurements rounded
##Other than sensitivity, we get good accuracy and specificity metrics again.
##our accuracy ends up being the almost same, with trade offs for no/yes. 
##our sensitivity falls even farther, while our specificity gets even closer to perfect. 
#Tuning this data actually yields slightly worse results even though k = 6 had the highest F score. 
#Since our whole goal is to predict who will default, the nearest neighbors test really does
##not yield the results we want. 



##Predict by two variables
defd <- defaultd
defd$predicted <- ifelse(defd$balance > 1500,1,
                         ifelse(defd$income < 20000, 1, 0))  ##Make a new variable and then set it with 
#a nested ifelse operator to predict yes (1) or no (0) for defaults


T <- table(Predict = defd$predicted,Actual = defd$default)  ##Create a new table showing you a total table with predicted
##versus actual for the vs variable. Compute accuracy from here
##as well as sensitivity and specificity.
T
T <- as.vector(T) #convert to vector
# Accuracy of the prediction is (num. correctly predicted)/(total)
accuracy <- (T[1]+T[4])/(T[1]+T[2]+T[3]+T[4]) # correct/total, you use T[1] to show the accuracy, use this to calc
# Sensitivity is Pr(predict 1 given actually 1) = 
sensitivity <- T[4]/(T[2]+T[4])   #we are focusing on the data that is actually Yes, how well did we predict it
# Specificity is Pr(predict 0 given actually 0) = 
specificity <- T[1]/(T[1]+T[3]) ##How good are we at predicting no default when its actually no (the other variable)

metric <- c("Accuracy","Sensitivity","Specificity") #create the metric of three measurements
value <- c(accuracy,sensitivity,specificity)
data.frame(Metric = metric,Value = round(value,3)) #This outputs our three measurements rounded
##with this predictor, we see that our accuracy is around 75%, our specificity is 
##good with 99.2%, but our sensitivity is very low. 

#rpart
fit <- rpart(default ~ income + balance + student
               , data = defaultd)
plot(fit,margin = .5) ##Here we see when x.radius_worst < 16.8
text(fit,cex = .75, use.n = TRUE)

defd <- defaultd
defd$predicted <- ifelse(defd$balance < 1800 ,0,  ##nested if's to test for each of the three variables
                      ifelse(defd$balance > 1972,1, 
                             ifelse(defd$income < 27400,0,1)))  ##creates a new variable (predicted)
##That will show if we correctly predicted whether or not the customer would default based upon these three
##variables

T <- table(Predict = defd$predicted,Actual = defd$default)  ##Create a new table showing you a total table with predicted
##versus actual for the vs variable. Compute accuracy from here
##as well as sensitivity and specificity.
T
T <- as.vector(T) #convert to vector
# Accuracy of the prediction is (num. correctly predicted)/(total)
accuracy <- (T[1]+T[4])/(T[1]+T[2]+T[3]+T[4]) # correct/total, you use T[1] to show the accuracy, use this to calc
# Sensitivity is Pr(predict 1 given actually 1) = 
sensitivity <- T[4]/(T[2]+T[4])   #we are focusing on the data that is actually Yes, how well did we predict it
# Specificity is Pr(predict 0 given actually 0) = 
specificity <- T[1]/(T[1]+T[3]) ##How good are we at predicting no default when its actually no (the other variable)

metric <- c("Accuracy","Sensitivity","Specificity") #create the metric of three measurements
value <- c(accuracy,sensitivity,specificity)
data.frame(Metric = metric,Value = round(value,3)) #This outputs our three measurements rounded
##So far this is the best fit we have from a metrics perspective. We see the highest sensitivity 
##metric, which predicts when a customer will default. 


##based on this rpart test, we can conclude that a customer with a balance greater than 
##1972 is likely to default, and if their balance is greater than 1800, but less than 1972,
##then they are likely to default if their income is greater than 27400. 
