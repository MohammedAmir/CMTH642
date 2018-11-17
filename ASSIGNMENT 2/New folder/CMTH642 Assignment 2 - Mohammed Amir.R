---
title: "CMTH642 - Assignment # 2"
author: "Mohammed Amir"
date: "November 26, 2016"
output: word_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# Read the csv files in the folder
```{r}
USDAClean <- read.csv(file="D:/Big Data/CMTH642 - DATA ANALYTICS ADVANCED METHODS/ASSIGNMENT 2/USDAClean_MohammedAmir.csv",head=TRUE,sep=",")

```

# 1. Create a visualization to illustrate the distribution of values for Total Fat, Protein and Carbohydrate. (12 p)
```{r}
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
hist(USDAClean$TotalFat, main="Histogram for Total Fat",     xlab="Total Fat", col="Green" )
hist(USDAClean$Protein, main="Histogram for Protein",     xlab="Protein", col="Blue")
hist(USDAClean$Carbohydrate, main="Histogram for Carbohydrate",     xlab="Carbohydrate" , col="Yellow")

```

# 2. Create a visualization to illustrate the relationship between a food's Total Fat content and its calorie content. (12 p)
```{r}
Calories <- USDAClean$Calories
TotalFat <- USDAClean$TotalFat
plot (TotalFat, Calories,  col = c("green", "red"), xlab="TotalFat", ylab="Calories", main="Total Fat vs Calories realtionship graphy")
legend("topright", legend=c("TotalFat", "Calories"), col=c("green", "red"), lty=1:2, cex=1, box.lty=1)

```

# 3. Create a logistic regression model, using High Calories as the dependent variable, and Carbohydrate, Protein, Total Fat and Sodium as independent variables. (18 p)
```{r}
Carbohydrate <- USDAClean$Carbohydrate
Protein <- USDAClean$Protein
TotalFat <- USDAClean$TotalFat
Sodium <- USDAClean$Sodium
HighCalories <- USDAClean$HighCalories

logreg_model <- glm (HighCalories ~ Carbohydrate + Protein + TotalFat + Sodium, data=USDAClean, family = binomial, )
summary(logreg_model)

```

# 4. Which independent variable is not significant? (10 p)
```{r}
# As per the logistic regression value Sodium is not significant because its p value of 0.347 is greater than 0.05 and has no star rating associated to it.
```

# 5. Which independent variable has the strongest positive predictive power in the model? (10 p)
```{r}
# Comparing the coefficient number of all dependent variables, TotalFat has the highest coefficient value making it the highest positive predictive variable in the model

```

# 6. Create a script for a HealthCheck function to detect unhealthy foods. (18 p) 
```{r}
HealthCheck <- function(sodium, sugar, fat)
{
    if(sodium == 0)
    {
        print ("1")
    }
        else
        {
            if(sugar == 0 && sodium == 1)
            {
              print ("1")  
            }
              else
                {
                  if(fat == 0 && sugar == 1 && sodium == 1)
                    {
                      print ("1")  
                    }
                      else
                        {
                          print ("0")
                        }
                
                }
          }
  }
```

# 7. Add a new column called HealthCheck to the USDAclean data frame using the output of the function. (10 p)
```{r}
USDAClean$HealthCheck <- mapply(HealthCheck,USDAClean$HighSodium,USDAClean$HighSugar,USDAClean$TotalFat)

```

# 8. How many foods in the USDAcle an data frame fail the HealthCheck? (10 p)
```{r}
# 512 foods have failed the health check test were as 6101 foods have passed the test.

table(USDAClean$HealthCheck)
```