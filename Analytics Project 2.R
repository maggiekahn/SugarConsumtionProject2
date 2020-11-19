##Analytics  Project 2###
library(ggplot2) #for ggplot system and preloaded datasets
library(plyr) #for ddply()
library(tseries) #for the J-B test
install.packages("readxl") #install packages 
library("readxl") #loading xl packages

##Sugar and Life expectancy####
SugarvLife<-read_excel("AnalyticsProject2.xlsx", sheet = "SugarvLife") #accessing data
na.omit(SugarvLife)#getting rid of varibles that were NA
SugarvLife_1<-na.omit(SugarvLife)
View(SugarvLife_1) #viewing variables 
ggplot(SugarvLife_1, aes(x = Life, y = Sugar )) + #making a scatter plot 
  geom_point()  +
  ggtitle("Linear Regression Relation Between \nSugar Consumption and Life Expectancy") +
  labs(y = "Sugar (gm)", x = "Life Expectancy (yrs.)") +
  geom_smooth(method ='lm') +
  theme(plot.title = element_text(hjust = 0.5))

##Training Data for Sugar and Life expectancy ###
dim(Training)#checking dimensions of training
p<-.7 #setting the percent for the testing sample
obs_count<-dim(SugarvLife_1)[1] #counting observations in the data frame
training_size <- floor(p * obs_count)
training_size
set.seed(1234) #setting the seed for partitioning 
train_ind <- sample(obs_count, size = training_size)
Training <- SugarvLife_1[train_ind, ] #pulls random rows for training
MODEL1<-lm(Life ~ Sugar, Training) #Building a model for training data
MODEL1$coefficients #Beta estimates 
MODEL1$residuals #residuals 
MODEL1$fitted.values #fitted redicted values 
hist(MODEL1$residuals) #plotting values 
jarque.bera.test(MODEL1$residuals) #Checking normality 
M1 <- lm(Sugar~ Life, Training) #Building Model for training data 
summary(M1) #summary for model 1 
PRED_1_IN <- predict(M1, Training) #generating predictions for in-sample data
PRED_1_OUT<- predict(M1, Training) #generating predictions for out-of-sample data
View(PRED_1_IN) #viewing predictions for training data
plot(PRED_1_IN, col= 'dark blue') #plotting predictions for training data
View(PRED_1_OUT)
plot(PRED_1_OUT, col = 'pink')
plot(Sugar ~ Life, Training, col ='purple') #Plots the In-sample training partition
points(Training$Life, Training$Sugar, col='orange') #Plots the Out-of sample training partition
RMSE_1_IN<-sqrt(sum((PRED_1_IN-Training$Sugar)^2)/length(PRED_1_IN))  #computes in-sample error trianing 
View(RMSE_1_IN) #viewing in-sample error
RMSE_1_OUT<-sqrt(sum((PRED_1_OUT-Training$Sugar)^2)/length(PRED_1_OUT)) #computes out-of-sample 

###Testing Data for Sugar and Life expectancy##
Testing <- SugarvLife_1[-train_ind, ] #pulls random rows for testing
dim(Testing)#checking dimensions of testing
MODEL2<-lm(Life ~ Sugar, Testing) #Building a model for training data
MODEL2$coefficients #Beta estimates 
MODEL2$residuals #residuals 
MODEL2$fitted.values #fitted redicted values 
hist(MODEL2$residuals) #plotting values 
jarque.bera.test(MODEL2$residuals) #Checking normality 
M2 <- lm(Sugar~ Life, Testing) #Building Model for training data
summary(M2)
PRED_2_IN<- predict(M2, Testing) #predicting in sample for testing 
PRED_2_OUT <- predict(M2, Testing)#predicting out sample for testing 
RMSE_2_IN<-sqrt(sum((PRED_2_IN-Testing$Sugar)^2)/length(PRED_2_IN))  #computes in-sample error
RMSE_2_OUT<-sqrt(sum((PRED_2_OUT-Testing$Sugar)^2)/length(PRED_2_OUT)) #computes out-of-sample 

##Comparing Testing and training for Sugar and Life expectancy##
x_grid <- seq(0,90) #CREATES GRID OF X-AXIS VALUES
predictions <- predict(M1, list(Life=x_grid))
plot(Training$Sugar ~ Training$Life, col='blue')
lines(x_grid, predictions, col='green', lwd=3)
points(Testing$Sugar ~ Testing$Life, col='red', pch=3)

##Sugar and GDP ($)###
GDPvSugar<-read_excel("AnalyticsProject2.xlsx", sheet = "GDPvSugar") #accessing data
na.omit(GDPvSugar) #getting rid of varibles that were NA
GDPvSugar_1 <-na.omit(GDPvSugar)
View(GDPvSugar_1) #viewing variables 
ggplot(GDPvSugar_1, aes(x = GDP, y = Sugar )) + #making a scatter plot 
  geom_point()  +
  ggtitle("Linear Regression Relation Between \nSugar Consumption and GDP ($)") +
  labs(y = "Sugar (gm)", x = "GDP ($)") +
  geom_smooth(method ='lm') +
  theme(plot.title = element_text(hjust = 0.5))

##Training data for Sugar and GDP ($)###
p<-.7 #setting the percent for the testing sample
dim(Training)#checking dimensions of training
obs_count<-dim(GDPvSugar_1)[1] #counting observations in the data frame
train_ind <- sample(obs_count, size = training_size) #setting training sample size 
training_size #checking what it looks like 
set.seed(1234) #setting the seed for partitioning 
train_ind <- sample(obs_count, size = training_size) #saving sample under train_ind
Training <- GDPvSugar_1[train_ind, ] #pulls random rows for training
MODEL3$coefficients #Beta estimates 
MODEL3$residuals #residuals 
MODEL3$fitted.values #fitted redicted values 
hist(MODEL3$residuals) #plotting values 
jarque.bera.test(MODEL3$residuals) #Checking normality 
M3<-lm(Sugar~ GDP, Training) #Building Model for training data 
summary(M3)
View(M3$fitted.values)
PRED_3_IN <- predict(M3, Training) #generating predictions for in-sample data
PRED_3_OUT <- predict(M3, Training) #generating predictions for out-of-sample data
View(PRED_3_IN) #viewing prediction 
plot(PRED_3_IN, col= 'dark blue') #plotting predictions for training data
plot(PRED_3_OUT, col= 'dark blue') #plotting predictions for training data
RMSE_3_IN<-sqrt(sum((PRED_3_IN-Training$Sugar)^2)/length(PRED_3_IN))  #computes in-sample error
RMSE_3_OUT<-sqrt(sum((PRED_3_IN-Training$Sugar)^2)/length(PRED_3_IN))  #computes in-sample error
plot(Sugar ~ GDP, Training, col ='purple') #Plots the In-sample training partition
points(Training$GDP, Training$Sugar, col='orange') #Plots the Out-of sample partition

##Testing data for Sugar and GDP($)##
Testing <- GDPvSugar_1[-train_ind, ] #pulls random rows for testing
dim(Testing)#checking dimensions of testing 
MODEL4<-lm(GDP ~ Sugar,Testing) 
MODEL4$coefficients #Beta estimates 
MODEL4$residuals #residuals 
MODEL4$fitted.values #fitted redicted values 
hist(MODEL4$residuals) #plotting values 
jarque.bera.test(MODEL4$residuals) #Checking normality 
M4<-lm(Sugar ~ Life, Testing)#Building model for testing data 
summary(M4) #Checking summary 
PRED_4_IN<-predict(M4, Testing) #Saving predictions for testing 
PRED_4_OUT<-predict(M4, Testing) #Saving prediciton for testing 
RMSE_4_IN<-sqrt(sum((PRED_4_IN-Testing$Sugar)^2)/length(PRED_4_IN)) #computes out-of-sample
RMSE_4_OUT<-sqrt(sum((PRED_4_OUT-Testing$Sugar)^2)/length(PRED_4_OUT)) #computes out-of-sample
 plot(Sugar ~ GDP, GDPvSugar_1) #Ploting entire dataset 
plot(Sugar ~ GDP, Testing,  col ='red', pch=3) #Plots the out-of-sample testing partition
points(Testing$GDP, Testing$Sugar, col='blue', pch=3) #Plots the out-of sample testing partition

#Comparing Testing and training for Sugar and GDP($)##
x_grid <- seq(0,13000) #CREATES GRID OF X-AXIS VALUES
predictions <- predict(M2, list(GDP=x_grid))
plot(Training$Sugar ~ Training$GDP, col='blue')
lines(x_grid, predictions, col='green', lwd=3)
points(Testing$Sugar ~ Testing$GDP, col='red', pch=3)


##Sugar and GDP Per Capita ###
CapvSugar<-read_excel("AnalyticsProject2.xlsx", sheet = "CapvSugar") #accessing data
na.omit(CapvSugar) #getting rid of varibles that were NA
CapvSugar_1<-na.omit(CapvSugar)
View(CapvSugar_1) #viewing variables 
ggplot(CapvSugar_1, aes(x = Cap, y = Sugar)) + #making a scatter plot 
  geom_point()  +
  ggtitle("Linear Regression Relation Between \nSugar Consumption and GDP Per Capita") +
  labs(y = "Sugar (gm)", x = "GDP per Capita (%)") +
  geom_smooth(method ='lm') +
  theme(plot.title = element_text(hjust = 0.5))
#Training Data for Sugar and GDP Per Capita ###
dim(Training)#checking dimensions of training
p<-.7 #setting the percent for the testing sample
obs_count<-dim(CapvSugar_1)[1] #counting observations in the data frame
training_size <- floor(p * obs_count) #saving training size 
training_size #checking training size 
set.seed(1234) #setting the seed for partitioning 
train_ind <- sample(obs_count, size = training_size)
Training <- CapvSugar_1[train_ind, ] #pulls random rows for training
MODEL5$coefficients #Beta estimates 
MODEL5$residuals #residuals 
MODEL5$fitted.values #fitted redicted values 
hist(MODEL5$residuals) #plotting values 
jarque.bera.test(MODEL5$residuals) #Checking normality 
M5 <- lm(Sugar~ Cap, Training) #Building Model for training data 
summary(M5)
PRED_5_IN <- predict(M5, Training) #generating predictions for in-sample data
PRED_5_OUT <- predict(M5, Training) #generate predictions on the (out-of-sample) training data
View(PRED_5_IN) #viewing predition 
View(M5$fitted.values)
plot(Sugar ~ Cap, Training, col ='purple') #Plots the In-sample training partition
points(Training$Cap, Training$Sugar, col='yellow') #Plots the Out-of sample partition
RMSE_5_IN<-sqrt(sum((PRED_5_IN-Training$Sugar)^2)/length(PRED_5_IN))  #computes in-sample error
RMSE_5_OUT<-sqrt(sum((PRED_5_OUT-Training$Sugar)^2)/length(PRED_5_OUT))  #computes in-sample error
RMSE_5_IN #In-sample error
RMSE_5_OUT #Out- of sample error 

###Testing Data for Sugar and GDP Per Capita###
Testing <- CapvSugar_1[-train_ind, ] #pulls random rows for testing
dim(Testing)#checking dimensions of testing 
MODEL6<-lm(Sugar ~ Cap,Testing) 
MODEL6$coefficients #Beta estimates 
MODEL6$residuals #residuals 
MODEL6$fitted.values #fitted redicted values 
hist(MODEL6$residuals) #plotting values 
jarque.bera.test(MODEL6$residuals) #Checking normality
plot(Sugar ~ Cap, Testing,  col ='red', pch=3) #Plots the out-of-sample testing partition
points(Testing$Cap, Testing$Sugar, col='blue', pch=3) #Plots the out-of sample testing partition
M6<-lm(Sugar ~ Cap, Testing) #saving testing data in M6
PRED_6_IN <- predict(M6, Testing) #generate predictions on the (out-of-sample) testing data
PRED_6_OUT <- predict(M6, Testing) #generate predictions on the (out-of-sample) testing data
RMSE_6_IN<-sqrt(sum((PRED_6_IN-Training$Sugar)^2)/length(PRED_6_IN))  #computes in-sample error
RMSE_6_OUT<-sqrt(sum((PRED_6_OUT-Testing$Sugar)^2)/length(PRED_6_OUT)) #computes out-of-sample 
RMSE_6_IN #In-sample error
RMSE_6_OUT #Out- of sample error 

#Comparing Testing and training for Sugar and Grams ($)##
x_grid <- seq(0,70) #CREATES GRID OF X-AXIS VALUES
predictions <- predict(M6, list(Cap=x_grid))
plot(Training$Sugar ~ Training$Cap, col='blue')
lines(x_grid, predictions, col='green', lwd=3)
points(Testing$Sugar ~ Testing$Cap, col='red', pch=3)

##Multiple Regression with sugar, GDP per capita, pov###
PovCapvSugar<-read_excel("AnalyticsProject2.xlsx", sheet = "PovCapvSugar") #accessing data
na.omit(PovCapvSugar) #getting rid of varibles that were NA
PovCapvSugar_1<-na.omit(PovCapvSugar)
View(PovCapvSugar_1) #viewing variables 
PovCapvSugar_1$Poverty<-NA #Creating a new column for the poverty variable 
for (i in 1:length(PovCapvSugar_1$POV)) { #creating a for/if statement 
  if (PovCapvSugar_1$POV[i] == 'Above the poverty line') { #label above if the number is higher than a certain number
    PovCapvSugar_1$Poverty[i]  <- "Above"
  }else{ 
    PovCapvSugar_1$Poverty[i] <- "Below"
  }
}
View(PovCapvSugar_1) #viewing the results to make sure everything looks okay
PovCapvSugar_1$Poverty<-as.factor(PovCapvSugar_1$Poverty) #converted to a factor variable
ggplot(PovCapvSugar_1, aes(x = Cap, y = Sugar, color = Poverty )) +
  geom_point() +
  geom_smooth(method ='lm', color = "black") +
  ggtitle("Linear Regression Relation Between \nSugar Consumption, Poverty, and GDP per capita")

#Training Data for sugar, GDP per capita, pov###
dim(Testing)
p<-.7 #fraction of sample I will use for training
obs_count<-dim(PovCapvSugar_1)[1] #number of observations in the data frame
training_size <- floor(p * obs_count) #round to the nearest integer 
training_size #checking number 
set.seed(1234) #making partition reproducible
train_ind <- sample(obs_count, size = training_size) #creating a vector with shuffled rows 
Training <- PovCapvSugar_1[train_ind, ] #pulls random rows for training
MODEL7 <- lm(Sugar ~ Cap + Poverty, Training)
MODEL7$coefficients #Beta estimates 
MODEL7$residuals #residuals 
MODEL7$fitted.values #fitted redicted values 
hist(MODEL7$residuals) #plotting values 
jarque.bera.test(MODEL7$residuals) #Checking normality
summary(MODEL7) #generates summary diagnostic output
PRED_7_IN <- predict(M7, Training) #predictions on the in-sample training data 
View(PRED_7_IN) #viewing the prediction i just made 
PRED_7_OUT <- predict(M7, Training) #prediction on the out-sample testing data
plot(PRED_7_IN) #Plotting results for predictions training 
Plot(PRED_7_OUT) #Plotting results for predictions training 
RMSE_7_IN<-sqrt(sum((PRED_7_IN-Training$Sugar)^2)/length(PRED_7_IN))  #computes in-sample error
RMSE_7_OUT<-sqrt(sum((PRED_7_OUT-Training$Sugar)^2)/length(PRED_7_OUT))  #computes in-sample error

 
#Testing for Data for sugar, GDP per capita, pov###
Testing <- PovCapvSugar_1[-train_ind, ] #pulls random rows for testing
dim(Testing) #checking the dimensions of the testing data 
MODEL8<-lm(Sugar ~ Cap + Pov, data = PovCapvSugar_1)
MODEL8$coefficients #Beta estimates 
MODEL8$residuals #residuals 
MODEL8$fitted.values #fitted redicted values 
hist(MODEL8$residuals) #plotting values 
jarque.bera.test(MODEL8$residuals) #Checking normality 
summary(MODEL8) #checking summary
M8<-lm(Sugar ~ Cap + Cap2, Testing)
PRED_8_IN <- predict(M8, Testing) #prediction on the out-sample testing data
PRED_8_OUT <- predict(M8, Testing) #prediction on the out-sample testing data
RMSE_8_IN<-sqrt(sum((PRED_8_IN-Testing$Sugar)^2)/length(PRED_8_IN)) #computes in-sample error 
RMSE_8_OUT<-sqrt(sum((PRED_8_OUT-Testing$Sugar)^2)/length(PRED_8_OUT)) #computes out-of-sample 
RMSE_8_IN #IN-SAMPLE ERROR
RMSE_8_OUT #OUT-OF-SAMPLE ERROR

#Plotting  Both Data Partitions ##
x_grid <- seq(0,70) #CREATES GRID OF X-AXIS VALUES
predictions <- predict(M8, list(Cap=x_grid, Cap2=x_grid^2))
plot(Training$Sugar ~ Training$Cap, col='blue')
lines(x_grid, predictions, col='green', lwd=3)
points(Testing$Sugar ~ Testing$Cap, col='red', pch=3)




