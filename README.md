# Boston-home-price-in-R
A  model to predict the factors that determines the prices of houses in Boston.

## i). Summary   
Our group looked at Housing dataset taken form the StatLib which is maintained at the Carnegie Mellon University. This dataset was collected from the suburbs of Boston and mainly looked at the values of homes in that particular area. So Our goal was to look at what affects the housing values around the suburbs in Boston In our data we have 506 observations and 14 different variables. The different variables and their meaning will be listed below the summary.  There are no missing values in our data which allowed us to thoroughly analyze the data. 

From the initial plotting( include all the variables), we finalized the variables(Crime + CHAS + Room + Dist + PTRATIO + Black + LSTAT) as our independents variables for building the model. 
Based on our calculation, our model met the linear assumption the observations are 70% match our model. There are about 12 outliners that have extremely high or low values. There are two high leverage point that effect the model.
### Conclusion: From our analysis, we determined that the percentage of lower class citizen in the population, Number of room, students teacher ratio, percentage of black residents, distance to employment centers, and charles index are significant in determining the median price of house in the studied area. However,we found the number of rooms and population of lower class citizens in a localitiy ot be highly significant in determinig the price of house in the Boston Suburbs.

## ii). About Dataset  
####Goal: what effect the housing values in suburbs of Boston
#### explaination of dataset: 
1. CRIM: per capita crime rate by town 
2. ZN: proportion of residential land zoned for lots over 25,000 sq.ft. 
3. INDUS: proportion of non-retail business acres per town 
4. CHAS: Charles River dummy variable (= 1 if tract bounds river; 0 otherwise) 
5. NOX: nitric oxides concentration (parts per 10 million) 
6. RM: average number of rooms per dwelling 
7. AGE: proportion of owner-occupied units built prior to 1940 
8. DIS: weighted distances to five Boston employment centres 
9. RAD: index of accessibility to radial highways 
10. TAX: full-value property-tax rate per $10,000 
11. PTRATIO: pupil-teacher ratio by town 
12. B: 1000(Bk - 0.63)^2 where Bk is the proportion of blacks by town 
13. LSTAT: % lower status of the population 
14. MEDV: Median value of owner-occupied homes in $1000's(dependent variable)  



```{r}

##setwd("C:/Users/Agyemang/Documents/R Projects/BANA 7038/Final")
##getwd()
##rm(list=ls())
```

##  1). Import the data into R for analysis and Cleaning the data

```{r}
home = read.table("home.txt", h=T)


```
The dataset was already formated so there was no need for much cleaning. However, we had to scale the dataset for easy comparison of the variables and to remove multicolinearity about covariates.

##  2). Explore the dataset

```{r}

names(home)
head(home, 4)
dim(home)
n=nrow(home)
n
```

The dataset has *506* data points and *14* variables

## 3).Data Visualization

Scatterplot matrix
 
```{r, fig.height=12, fig.width=15}
library(car)
##scatterplotMatrix(home, spread=FALSE, smoother.args=list(lty=2),
##                  main="Scatter Plot Matrix for Home Dataset")
pairs(home)
```  
From the Scatterplot, there is a noticable relationship between Medve and istat, distance, age,room,crime. There is a positive polynormial regression relationship between Medv and lstat, Crime. There is a negative polynormial regression between Medv and age, distance. There is a positive linear regression between Medv and Room. 

### 3.1  Plot histogram for the dataset

```{r, fig.height=15, fig.width=20}
par(mfrow =c(3,5))
hist(home$Medv, main = "Median Home value", col = "grey")
hist(home$Crime, main = "Crime", col = "grey")
hist(home$Zone, main = "Zone", col = "grey")
hist(home$Industry, main = "Industry", col = "grey")
hist(home$Charle, main = "Charles Indicator", col = "grey")
hist(home$Nos, main = "Notros Oxide", col = "grey")
hist(home$Room, main = "Room", col = "grey")
hist(home$Age, main = "Age", col = "grey")
hist(home$Dist, main = "Dist", col = "grey")
hist(home$Rad, main = "Rad", col = "grey")
hist(home$Tax, main = "Tax", col = "grey")
hist(home$PtRatio, main = "PtRadio", col = "grey")
hist(home$Black, main = "Black", col = "grey")
hist(home$lstat, main = "lstat", col = "grey")
```  
 The normal distribution is Room. The left skewed is Crime, ZN, Nos, Dist, and LSTAT. 

## 4). Model Development-part 1 explore the model

### 4.1). Fit the model with all variables and test for significant

```{r}
fit = lm(Medv ~ Crime+ Zone+Industry+Nos+ Charles+Nos+Room+Age+Dist+Rad+Tax+PtRatio+Black+lstat, data=home)
summary(fit)
```    
As we can see from the summary, we notice that the industry and age are insiginificant.we might need to remove those two variables for our next calculation. However, at this stage, we will continous explore our dataset. 
The coefficients of the model are verify the scatter plot model regarding of positive/negative linear relationship. 

### 4.2). Obtain the confident interval estimation for the coefficient estimates

```{r}
confint(fit)
```

Explanation : As we can see, the result suggest that we can be 95% confident that the interval shown above

### 4.3). Obtain the residuals for the model

```{r}
plot(residuals(fit))
```
As shown in the plot above, the residual of the regression is spread evenly without showing any pattern. This mean that the model is a good fit.

### 4.4). Perform Anova Test - F-Test

```{r}
anova(fit)
```  

As shown above, the F -test was shown to be the radio and Nos are insignificant, which we might need to remove them later.

### Obtain SST, SSR, SSRes

```{r, echo=TRUE}

SST = sum((home$Medv-mean(home$Medv))^2)
SST
SSRes = sum((home$Medv - fit$fitted.values)^2)
SSRes
SSR = sum((fit$fitted.values-mean(home$Medv))^2)
SSR
SSRes = SST -SSR
SSRes
F=SSR/2 / (SSRes/(n-2-1))
F
```
 We can see from the above calculation that SST is equal to (42716.3) , which means that total variability in the observed values is (11078.78).
SSR of (31637.51) means that the amount of variability in the observed values explained by the regression line is (11078.78)
The value for SSRes was calculated to be (11078.78), which means that the amount of the residual variation is explained by the regression line (11078.78) all these means that the regression equation is a very good fit in predicting aircraft landing distance. However, still have at least 2 variables (ages and room) that are insignificants; and the model only have 74% match. Therefore, we need to proceed our next step which is select the variables.
 

## 5). Variable selection
In the process we used R function called the stepAIC to determine the best combinations of independent variables that produce the best model fit.

```{r}
library(MASS)
home.fit = lm(Medv ~ Crime+ Zone+Industry+ Charles+Nos+Room+Age+Dist+Rad+Tax+PtRatio+Black+lstat, data=home)
stepAIC(home.fit, direction = "backward")

```

From the stepAIC above, it observed that crime, zone, charles index, Nos, room, dist, rad, black, lstat produces the best model.

## Fit a new model based on the stepAIC recommendation

```{r}
home.fit1 = lm(Medv ~ Crime + Zone + Charles + Nos + Room + Dist + Rad + Tax + 
        PtRatio + Black + lstat, data=home)
```  
 Based on the recommendation, we remove the "Age" and "industry", and obtain the new model (home.fit1)

### 5.1). Obtain model parameters

```{r}
summary(home.fit1)

```  
the summary show us all variables are significant. And the R^2 value went up, which indicate the model fitting slightly goes up.

### 5.2). Residual plot
```{r}
plot(residuals(home.fit1))

```   

Explanation : The residual plot are evenly separate out, except several outliners. 

### 5.3). Test for multicolinearity

```{r}
library(car)
vif(home.fit1)
sqrt(vif(home.fit1))>2
```  

 The multicollinearity test shows that there is a multicollinearity existing in the model (Rad and Tax). This will be investigated in later section.

### 6). All subset Regression
Like the stepAIC, the leaps does permutations of variables to determine the best combination to produce the best model. It uses the regsubset() function.

```{r}
library(leaps)
leaps=regsubsets(Medv ~ Crime + Zone + Charles + Nos + Room + Dist + Rad + Tax + PtRatio + Black + lstat, data=home, nbest=11)
```
### Plot the leap

```{r, fig.height=15, fig.width=15}
plot(leaps, scale="adjr2")

```
Based on the recommendation of the leaps analysis, crime, LSTAT, PTRATIO, Room, DIS, CHAS, black will be used as covariates to fit new model
 
## 7).model development - part 2 determine the final model  

### 7.1. Scaling the dataset
the scale fuction in R was used to standardized the dataset. This standardized each variable to a mean of 0 and standard deviation of 1. The resulting dataset was used for creating the final model.

```{r}
zhome = as.data.frame(scale(home))
```  
### 7.2). Fit a new model based on leaps selection  
```{r}
fit2 = lm(Medv ~ Crime + Charles + Room + Dist +PtRatio + Black + lstat, data=zhome)
summary(fit2)
```

### 7.3). Plot the residual
Ploting the residual to determine if there is any pattern in the observation.

```{r}
plot(residuals(fit2))
```

From the residual plot above, it could be observed that the data points are spread randomly without any apparent pattern. This means that the model is correct.

### 7.4).Test for multicolinearity
We used the variance inflation factor (vif) to determine if any of the independent variables correlate with each other.

```{r}

library(car)
vif(fit2)
sqrt(vif(fit2))>2
confint(fit2)

```
Based on the variable inflation factor, there is no multicollinearity in the model. 

### 7.5). Plot the model

```{r}
par(mfrow= c(2, 2))
plot(fit2)
```  

As shown in the plot above, the residual of the regression is spread evenly without showing any pattern. This mean that the model is a good fit.

### 7.6). Perform t-test the coefficients estimate

```{r, echo=TRUE}
library(lmtest)
coeftest(fit2, vcov = hccm)
2*(1-pt(abs(summary(fit2)$coef[,1]/summary(fit2)$coef[,2]), n-2))
```
Bsed on the test above, crime and charles index are insignificant.

### 7.7). Perform Anova Test - F-Test

```{r}
anova(fit2)
```

As shown above, the F -test was shown to be significant. However, distance was shown to be insignificant.

### 7.8). Obtain SST, SSR, SSRes

```{r, echo=TRUE}

SST = sum((zhome$Medv-mean(zhome$Medv))^2)
SST
SSRes = sum((zhome$Medv - fit2$fitted.values)^2)
SSRes
SSR = sum((fit2$fitted.values-mean(zhome$Medv))^2)
SSR

SSRes = SST -SSR
SSRes


F=SSR/2 / (SSRes/(n-2-1))
F
```
As shown above, the F -test was shown to be significant.  We can see from the above calculation that SST is equal to 505, which means that total variability in the observed values is 146.61.
SSR of 358.39 means that the amount of variability in the observed values explained by the regression line is 146.6051
The value for SSRes was calculated to be 146.61, which means that the amount of the residual variation is explained by the regression line 146.61


### 7.9). Obtain the R-squared
```{r}
Rsquared = summary(fit2)$r.squared
Rsquared
```

### Open adjusted R-squared

```{r}
Adj_R_Squared_model = summary(fit2)$adj.r.squared
Adj_R_Squared_model
```

From the calculation above, the R-Squared was 0.7096928 while the adjusted R-Squared was 0.7056122.
This means that about 70.6% of the variability of median home is predicted by the model, which is pretty significant.


### 8).Plot the model to determine if the model violate any statistical assumption

```{r,echo=TRUE, fig.height=10, fig.width=15}
confint(fit2)
par(mfrow=c(2,2))
plot(fit2)
```

### 8.1).  QQ plot of the model

```{r, echo=TRUE, , fig.height=10, fig.width=15}
library(car)
qqPlot(fit2, labels=row.names(home), id.method ="identify", simulate=TRUE,
			 main= "QQ plot of the life Distance data")
```

The qq plot also shows the same observations. Most of the observation are spread along a linear path,  but deviate towards the end. This shows shows the presence of outliers in the dataset.


### 8.2).Studentized Residual Plot
This plot is to observe the distribution of the errors. The code for this was taken from R in Action, by Robert I. Kabacoff.

```{r, echo=TRUE, fig.height=8, fig.width=14}
residplot <- function(fit, nbreaks=10) {
z <- rstudent(fit)
hist(z, breaks=nbreaks, freq=FALSE,
xlab="Studentized Residual",
main="Distribution of Errors")
rug(jitter(z), col="brown")
curve(dnorm(x, mean=mean(z), sd=sd(z)),
add=TRUE, col="blue", lwd=2)
lines(density(z)$x, density(z)$y,
col="red", lwd=2, lty=2)
legend("topright",
legend = c( "Normal Curve", "Kernel Density Curve"),
lty=1:2, col=c("blue","red"), cex=.7)
}
residplot(fit2)

```  
As we can see from the plot, the error follow the normal distribution, however, there are some outliners that will be investigate later on.

### 8.3).Linearity Test  

This test is to determine if the model is violate linearity assumption. You can use the component plus residual to plots identify signs of nonlinearity in the relationship between the dependent variable and the independent variables. 

```{r, fig.height=10, fig.width=15}
library(car)
crPlots(fit2)

```
From the plots above you can see that the linearity assumption is met by the model.

### 8.4). Homoscedasticity test

This test is to determine if the variance of the model is constant

```{r, fig.height=10, fig.width=15}
library(car)
ncvTest(fit2)
spreadLevelPlot(fit2)
```
From the observation above, it seems that constant variance assumption is met, since the p-value is insignificant ( p-value = 0.19)

### 8.5).  Outliers
Run this test to determine if any observations has extremely high or low value. This means that may not be predicted well by the model. 

```{r, fig.height=10, fig.width=15}
library(car)
outlierTest(fit2)
```
From the outlier test above, we can see that observations 369, 372, 272,413 are outliers.

### 8.6).  High Leverage Points

This test is to identify the observation with very high predictor values. I used a hat.plot function taken from R in Action book, by Robert I. Kabacoff.

```{r, fig.height=10, fig.width=15}
hat.plot <- function(fit) {
p <- length(coefficients(fit2))
n <- length(fitted(fit2))
plot(hatvalues(fit), main="Index Plot of Hat Values")
abline(h=c(2,3)*p/n, col="red", lty=2)
identify(1:n, hatvalues(fit2), names(hatvalues(fit2)))
}
hat.plot(fit2)

```
From the hat.plot function result above, you can observe that six observations have high leverage points.

### 8.7). Influential Observations

This test is to identify the observations that have unbalanced impact on the model's parameter values.
In this test, the Cook's D distance to find those influential points.

```{r, fig.height=10, fig.width=15}
cutoff <- 4/(nrow(home)-length(fit2$coefficients)-2)
plot(fit2, which=4, cook.levels=cutoff)
abline(h=cutoff, lty=2, col="red")

``` 

###  8.8). Cook's D plot to identify influential observations

```{r, fig.height=10, fig.width=15}
library(car)
avPlots(fit2, ask = FALSE, id.method="identify")


influencePlot(fit2, id.method="identify", main="Influence Plot",
sub="Circle size is proportional to Cook's distance")
```  
From the Cook's D distance plot above,in our particular case, we can see that point 369, 373,381 are influencial points. Hence removing these observations may affect the model. 

### 8.9). Cross Validation

```{r}
library(bootstrap)
shrinkage <- function(fit, k=10){
require(bootstrap)
theta.fit <- function(x,y){lsfit(x,y)}
theta.predict <- function(fit,x){cbind(1,x)%*%fit$coef}
x <- fit$model[,2:ncol(fit$model)]
y <- fit$model[,1]
results <- crossval(x, y, theta.fit, theta.predict, ngroup=k)
r2 <- cor(y, fit$fitted.values)^2
r2cv <- cor(y, results$cv.fit)^2
cat("Original R-square =", r2, "\n")
cat(k, "Fold Cross-Validated R-square =", r2cv, "\n")
cat("Change =", r2-r2cv, "\n")
}

```

## 9). Relative Importance
This test is done to determine which of the significant independent variables that was included in the final model is very useful for predicting the regression response.

```{r,echo=TRUE}

coef(fit2)

```  
We also used a fuction called relweights(), which was also taken from "R in Action Book". This function plot all the points and show which is more important. The diagrams are shown below for the first and final model.


### Using the Relative weight function to plot the regression model

```{r, echo=FALSE}

relweights <- function(fit,...){
R <- cor(fit$model)
nvar <- ncol(R)
rxx <- R[2:nvar, 2:nvar]
rxy <- R[2:nvar, 1]
svd <- eigen(rxx)
evec <- svd$vectors
ev <- svd$values
delta <- diag(sqrt(ev))
lambda <- evec %*% delta %*% t(evec)
lambdasq <- lambda ^ 2
beta <- solve(lambda) %*% rxy
rsquare <- colSums(beta ^ 2)
rawwgt <- lambdasq %*% beta ^ 2
import <- (rawwgt / rsquare) * 100
import <- as.data.frame(import)
row.names(import) <- names(fit$model[2:nvar])
names(import) <- "Weights"
import <- import[order(import),1, drop=FALSE]
dotchart(import$Weights, labels=row.names(import),
xlab="% of R-Square", pch=19,
main="Relative Importance of Predictor Variables",
sub=paste("Total R-Square=", round(rsquare, digits=3)),
...)
return(import)
}

```


### 9.1). Plot the model by using the relweights() function.

```{r, echo=TRUE, fig.height=10, fig.width=15}
relweights(fit2, col="blue")
```  
From the plotting above, it is obvious that population of lower class citizens and number of rooms are very sigificant as far as the median price of houses is concern.The black and crime also can determine the MEDV, but they have less weight compare to Room and LSTAT    

## Conclusion
From our analysis, we determined that the percentage of lower class citizen in the population, Number of room, people teacher ratio, percentage of black residents, distance to employment centers, and charles index are significant in determining the median price of house in the studied area. However,we found the number of rooms and population of lower class citizens in a localitiy to be highly significant in determinig the price of house in the Boston Suburbs.
