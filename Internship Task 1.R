# Data Science and Buiseness Analytics Internship 
# Author : Mridula
# Perdiction using Supervised ML (GRIP-TSF-Task - #1)
# TO perdict the percentage of an student based on the no. of study hours

# STEP 1.Generating inputs using csv file
# STEP 2.Import the required libraries
# STEP 3.Splitting the dataset into train and test
# STEP 4.Apply the linear regression on the given 2 variables
# STEP 5.Validate the model
 

# Reading data from the given file
stu_data <- read.csv("http://bit.ly/w-data")  #stu_data can be read as student data

View(stu_data)
str(stu_data)   # to display structure of the given data
plot(stu_data)
cor(stu_data$Hours,stu_data$Scores)
head(stu_data)


# Using the required libraries
library(ggplot2)
library(caTools)
library(Metrics)

ggplot(data = stu_data,aes(Hours,Scores)) +
  geom_count(colour = 'red',size = 3)+
  labs(title = "Hours vs Scores(%)",x="Number of Hours",y="Scores(%)")
# From graph we can clearly observe that there is Linear relationship between no.of hours and scores(%)

set.seed(3)

#Splitting the data into training and test set
split <- sample.split(stu_data,SplitRatio = 0.9)
split
train <- subset(stu_data,split = "TRUE")
test <- subset(stu_data,split = "FALSE")


#creating a trained model
Model = lm(train$Scores ~ train$Hours,data = train)
Model
summary(Model)

# Plotting REGRESSION LINE on Hours vs Scores
ggplot(test,aes(x = Hours, y= Scores))+
  geom_point(colour="red",size = 3)+ geom_smooth(formula = y~x,method = lm)+ 
  labs(title = "Hours vs Scores(%)",x="Number of Hours", y="Scores(%)")

#Prediction over testing data
pred <- predict(Model,test)
pred


#comparing actual and predicted scores
actual_scores = train$Scores
actual_scores
predicting_scores = predict(Model,data.frame(actual_scores))
predicting_scores


comparing_scores = data.frame(actual_scores,predicting_scores)
comparing_scores

#Defining Linear regression model
lr = lm(Scores~Hours,data = stu_data)


#Calculating marks of student if he/she studies for 9.25 hours
given_data <- data.frame(Hours=9.25)
predict(lr,given_data)

#Validate the model
# Finding Accuracy by finding rmse (Root mean square error)
result= rmse(actual_scores,predicting_scores)
result

  
