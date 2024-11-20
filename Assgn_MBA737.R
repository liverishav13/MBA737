# ABC Case Study problem
##Load the relevant packages
install.packages("readxl", "lubridate", "moments", "car",
                 "lmtest", "sandwich", "tidyverse"  )
library(readxl)
library(lubridate)
library(moments)
library(car)
library(lmtest)
library(sandwich)
library(urca)
library(tidyverse)
library(Metrics)
#################################################
#set your working directory where you data is saved
setwd("D:/MBA737/Assignment2")

##Read the data
Data= read_excel("ABC.xlsx")

##Check the data
head(Data) #Date contacts security ABC price, returns, sensex returns,
#dividend announcement, sentiment, and Nifty returns
dim(Data)
class(Data$Date) ##Date object seems to be in date/time
Data$Date=as.Date(Data$Date) ##Converting date/time to date object
class(Data$Date)
Data=na.omit(Data)

################################################
#Visualaze the data: Returns on ABC and Nifty
plot(Data$Date, Data$ABC, xlab="Data", ylab="", main="ABC Returns",
     type="l")


###Compute cummulative returns
Data$Cum_Ret= Data$Price/Data$Price[1]-1

#Plot cumulative returns
plot(Data$Date, Data$Cum_Ret, xlab="Data", ylab="", main="Cummulative Returns",
     type="l")

#############Market or Nifty Returns
plot(Data$Date, Data$Nifty, xlab="Data", ylab="", main="Market Returns",
     type="l")


#Cummulative market returns
Data$Cum_Ret_Nifty= cumsum(Data$Nifty)

###Plot cummulative returns
plot(Data$Date, Data$Cum_Ret_Nifty, xlab="Data", ylab="",
     main="Cummulative Market Returns",
     type="l")


###Basic properties of Data
###Summmarizing the data
summary(Data$ABC)
summary(Data$Nifty)
summary(Data$Sensex)

##########Plot ABC vs Nifty
plot(density(Data$ABC), main="Density-Graph",
     xlab="Data", col="red", lwd=2, lty=1, xlim=c(-0.6, 0.6))


lines(density(Data$Nifty), col="blue", lwd=2)
legend("topleft", c("ABC", "Nifty"),fill= c("red", "blue"))

###################################################################
##Normality and Staionerity of the data

####Symmetry 
skewness(Data$ABC)  #Large Negative skewness in ABC returns
skewness(Data$Nifty) #Moderate Negative skewness in ABC returns

agostino.test(Data$ABC)  #Negative skewness is statistically signifcant
agostino.test(Data$Nifty) #Negative skewness is statistically signifcant but small in magnitude
?kurtosis
##Kurtosis (fat tails and excess peakedness)
kurtosis(Data$ABC) ##very large kurtosis indicating fat tails and excess peaked ness
kurtosis(Data$Nifty) ##Moderately large kurtosis indicating fat tails and excess peaked ness


####The test compares kurtosis with normal distribution kurtosis (3)
anscombe.test(Data$ABC) ##very large and statistically signifcant kurtosis
#indicating fat tails and excess peaked ness
anscombe.test(Data$Nifty) ## statistically signifcant kurtosis
#indicating fat tails and excess peaked ness
#######################Testing the normality


####Overall test of data normality 
jarque.test(Data$ABC)  ##excessive deviation from normality due to 
##large negative skewness and kurtosis
jarque.test(Data$Nifty)  ##excessive deviation from normality due to 
##large negative skewness and kurtosis


#####
##Stationerity of the data
###Stationarity is an important requirement of regression or any mathematical time
#series model; it indicates whether the mean and variance of the process are 
##constant or changing with time
#stationarity is more imp that normality. Even if the data is not normal
#the sampling distribution may be normal due to central limit theorem

##ADF Test: H0 Null: Non stationarity 
summary(ur.df(Data$ABC)) ##Null rejected; Data Stationary 
summary(ur.df(Data$Nifty))##Null rejected; Data Stationary 

##PP test: H0: Null of Non stationarity
summary(ur.pp(Data$ABC)) ##Null rejected: Data Stationary 
summary(ur.pp(Data$Nifty)) ##Null rejected: Data Stationary 


###KPSS test H0: test of  stationarity 
summary(ur.kpss(Data$ABC)) #Fail to reject null: Data Staionary 
summary(ur.kpss(Data$Nifty)) #Fail to reject null: Data Staionary 

####

##############################Segregating the data into training and test data
##Create training data set from year 2007-2016
train= Data[year(Data$Date)<2017  & year(Data$Date)>2006,]
head(train)
tail(train)
dim(train)

test= Data[year(Data$Date)>2016,]
head(test)
tail(test)
dim(test)
######################################Simple linear regression model
### Examining the ABC returns and Nifty returns relationship on the train data

Slr=lm(ABC ~ Nifty, data= train)
model= summary(Slr)
model #examine the Rsq, coefficient and t-values: what do you infer
################Residual Diagnostics
densityPlot(model$residuals) ##density plot of raw residuals
densityPlot(rstudent(Slr)) ##density plot of raw residuals
qqPlot(Slr, id=list(n=10)) ##qqplot for normality of residuals
outlierTest(Slr) ###identify extreme outliers

###identify extreme outliers and remove them then rerun the model

#################Heteroscedasticity 
par(mfrow=c(2,2))
#create residual vs fitted, normal qq
# scale location and residual vs leverage plot
##This will help examine the goodness of fit and idenfity outliers
plot(Slr)

####Check with model fitted values/model
ncvTest(Slr) 
#Seems that error terms have heteroscedastic
######Breusch-Pagan Test of heteroscedasticity

bptest(model, studentize = F) ##with raw residuals
bptest(model, studentize = T) ###with studentized residuals
##It seems with studentize residuals we do not have the problem of 
#heteroscedasticity; often the heteroscedasticity is just an artifact
#of raw residual size variation, which goes away with log transformation
#or using studentized errors


####Autocorrelation################
#Durbin watson
dwtest(Slr)
##No autcorrelation in error 

###Breusch-Godfrey Test
bgtest(Slr)

##seems that no autocorrelation


##there are different versions of robust standard errors to account for 
##so that appropriate test stats are obtained
#only t-stat will change but coefficient will remain the same
############################Robust Standard Errors
#Different variants of robust standard errors and subsequent
##t and p values are reported
coeftest(Slr, vcov.=hccm) ##Heteroscedasticity-Corrected Covariance Matrices
coeftest(Slr, vcov.=vcovHAC) #Heteroscedasticity and Autocorrelation Consistent (HAC) Covariance Matrix
coeftest(Slr, vcov.=vcovHC) #Heteroscedasticity-Consistent Covariance Matrix Estimation
coeftest(Slr, vcov.=NeweyWest) #Newey-West HAC Covariance Matrix

###################Prediction using the test data
Pred_Slr=predict(Slr, test) #using the Slr model, predicted values with the test data

###Plotting the actual 
par(mfrow= c(1,1))
plot(test$Date, test$ABC, xlab="Date", ylab="Returns", 
     main="Predicted Returns vs Actual Returns"
    , ylim=c(-0.1, 0.1), pch=20, col="red", type="l", lwd=1)


##Adding the predicted values to the plot
lines(test$Date, Pred_Slr, lwd=1, col="green", type="l")

#add legends
legend( "topright", c("Actual Return", "Predicted Return"), fill=c("red", "green"))
##visually it seems to be a decent prediction 

##Correlation between actual and projected values
cor(test$ABC, Pred_Slr) ###correlation seems to be high
cor.test(test$ABC, Pred_Slr) ##correlation seems to be statistically signifcant



###Multiple Linear Regression model
Mlr= lm(ABC ~ Sensex+Sentiment+Nifty+DividendAnnounced, data=train)
 ###The model includes two variable including S
model=summary(Mlr)
model
###Notice the insignifcantly small and negative coefficient of Nifty
###Ideally it should be positive and signifcant; this appears to be happening 
##on account of multicollinearity between Nifty and Sensex
##Let us explore that
##########################################Multicollinearity 
##Compute the correlation 
cor(train[, c("ABC", "Sensex", "Nifty", "Sentiment")])
####correlation across these variables seems to be pretty high
##A more direct measure of multicollinearty is variance inflation factor VIF
vif(Mlr)
###Variance inflation measures of higher than 2 is undersirable
###As we can see it is higher for Sensex and Nifty, which is expected as both of them 
##are highly correlated
##So we can remove either one of them to improve the model as follows

Mlr= lm(ABC ~ Sensex+Sentiment+DividendAnnounced, data=train)
model=summary(Mlr)
model
############Prediction object with test data
Pred_Mlr= predict(Mlr, test)


###Correlation between the predicted object and actual data
cor(Pred_Mlr, test$ABC)

####Actual vs Predicted plot
plot(test$Date, test$ABC, xlab="Date", ylab="Returns",
     main="Actual vs Predicted Returns",
     ylim=c(-0.20, 0.20), col="red", type="l")

#Adding the predicted values
lines(test$Date, Pred_Mlr, col="green", type="l")
 #add legend to the plot
legend("topright", c("Actual Returns", "Predicted Returns"), fill=c("red", "green"))
#######################################################
####In addition we are adding to more models

##1: Naive model
naive=lm(ABC~1, data=train) ##Naive model which is based on unconditional prediction
##without considering any other variable
Pred_Naive= predict(naive, test)


####Another model to add Nifty with Nifty beta
Slr_2=lm(ABC ~ Nifty+I(Nifty*Nifty), data= train)
Pred_Slr_2=predict(Slr_2, test)
###############################
##################Let us compare the correlaton of these four prediction models
##with actual data

cor(cbind.data.frame(test$ABC,Pred_Slr,Pred_Slr_2, Pred_Mlr))
##highest correlation with MLR model, correlations for Pred naive can not be
#used as it predictions unconditional forecasts (not changing with values)

#############################################################
####################Let us construct an error object to compare these and 
##choose the best fit
Error= data.frame(matrix(nrow=10, ncol=4))
colnames(Error)=c("Naive", "SLR_Mod","SLR2_Mod", "MLR_Mod")
rownames(Error)=c("MSE", "RMSE", "RAE", "MAE",  "SMAPE","MSLE",
                  "RMSLE",  "RSE", "RRSE", "Complex")

#Compute and assign the error values for all the errors and error models
Error$Naive[1]=mse(test$ABC, Pred_Naive)
Error$SLR_Mod[1]=mse(test$ABC, Pred_Slr)
Error$SLR2_Mod[1]=mse(test$ABC, Pred_Slr_2)
Error$MLR_Mod[1]=mse(test$ABC, Pred_Mlr)


Error$Naive[2]=rmse(test$ABC, Pred_Naive)
Error$SLR_Mod[2]=rmse(test$ABC, Pred_Slr)
Error$SLR2_Mod[2]=rmse(test$ABC, Pred_Slr_2)
Error$MLR_Mod[2]=rmse(test$ABC, Pred_Mlr)


Error$Naive[3]=rae(test$ABC, Pred_Naive)
Error$SLR_Mod[3]=rae(test$ABC, Pred_Slr)
Error$SLR2_Mod[3]=rae(test$ABC, Pred_Slr_2)
Error$MLR_Mod[3]=rae(test$ABC, Pred_Mlr)


Error$Naive[4]=mae(test$ABC, Pred_Naive)
Error$SLR_Mod[4]=mae(test$ABC, Pred_Slr)
Error$SLR2_Mod[4]=mae(test$ABC, Pred_Slr_2)
Error$MLR_Mod[4]=mae(test$ABC, Pred_Mlr)


Error$Naive[5]=smape(test$ABC, Pred_Naive)
Error$SLR_Mod[5]=smape(test$ABC, Pred_Slr)
Error$SLR2_Mod[5]=smape(test$ABC, Pred_Slr_2)
Error$MLR_Mod[5]=smape(test$ABC, Pred_Mlr)

Error$Naive[6]=msle(test$ABC, Pred_Naive)
Error$SLR_Mod[6]=msle(test$ABC, Pred_Slr)
Error$SLR2_Mod[6]=msle(test$ABC, Pred_Slr_2)
Error$MLR_Mod[6]=msle(test$ABC, Pred_Mlr)

Error$Naive[7]=rmsle(test$ABC, Pred_Naive)
Error$SLR_Mod[7]=rmsle(test$ABC, Pred_Slr)
Error$SLR2_Mod[7]=rmsle(test$ABC, Pred_Slr_2)
Error$MLR_Mod[7]=rmsle(test$ABC, Pred_Mlr)

Error$Naive[8]=rmsle(test$ABC, Pred_Naive)
Error$SLR_Mod[8]=rmsle(test$ABC, Pred_Slr)
Error$SLR2_Mod[8]=rmsle(test$ABC, Pred_Slr_2)
Error$MLR_Mod[8]=rmsle(test$ABC, Pred_Mlr)

Error$Naive[9]=rmsle(test$ABC, Pred_Naive)
Error$SLR_Mod[9]=rmsle(test$ABC, Pred_Slr)
Error$SLR2_Mod[9]=rmsle(test$ABC, Pred_Slr_2)
Error$MLR_Mod[9]=rmsle(test$ABC, Pred_Mlr)

###The last error object is the objective to be minimized/maximized
##sort of gain/loss function; here we have considered a simple gain loss function
##that is average of all measures. This measure can be used to select the model with 
##best out of sample prediction
#MLR model appears to be best; 
Error$Naive[10]=mean(Error$Naive[1:9])
Error$SLR_Mod[10]=mean(Error$SLR_Mod[1:9])
Error$SLR2_Mod[10]=mean(Error$SLR2_Mod[1:9])
Error$MLR_Mod[10]=mean(Error$MLR_Mod[1:9])



Rank= data.frame(matrix(nrow=10, ncol=4))
colnames(Rank)=c("Naive", "SLR_Mod","SLR2_Mod", "MLR_Mod")
rownames(Rank)=c("MSE", "RMSE", "RAE", "MAE", "SMAPE",
                 "MSLE", "RMSLE", "RSE", "RRSE", "Complex")


Rank= apply(Error,1, rank)
print(Rank)




















