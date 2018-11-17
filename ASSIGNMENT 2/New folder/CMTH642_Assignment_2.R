---
  title: "CMTH642 - Assignment #2 "
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
Protein <- USDAClean$Protein
Carbohydrate <- USDAClean$Carbohydrate
TotalFat <- USDAClean$TotalFat

layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
hist(Protein)
hist(Carbohydrate)
hist(TotalFat)

```

# 2. Create a visualization to illustrate the relationship between a food's Total Fat content and its calorie content. (12 p)
```{r}
Calories <- USDAClean$Calories
plot (TotalFat, Calories,  col = c("green", "red"), xlab="TotalFat", ylab="Calories", main="Total Fat vs Calories realtionship graphy")
legend("topright", legend=c("TotalFat", "Calories"), col=c("green", "red"), lty=1:2, cex=1, box.lty=0)

```

# 3. Create a logistic regression model (18 p)
```{r}
model <- glm (HighCalories ~ Protein + TotalFat + Sodium, data=USDAClean, family = binomial, )
summary(model)
model.pred <- predict(model, type="response")
summary (model.pred)
```
# 4. Which independent variable is not significant? (10 p)
```{r}

```


# 5. Which independent variable has the strongest positive predictive power in the model? (10 p)
```{r}
# Based on coefficient number, total fat has the highest positive value indicating that the total fat has the hightest positive rate of change
for change of value in y. 
```

# 6. Create a script for a HealthCheck function to detect unhealthy foods. (18 p) 
```{r}
y <- function(sodium, sugar, fat)
{
    if(sodium == 0)
    {
        print ("Low Sodium , Passed")
    }
        else
        {
            if(sugar == 0 && sodium == 1)
            {
              print ("Low Sugar , Passed")  
            }
              else
                {
                  if(fat == 0 && sugar == 1 && sodium == 1)
                    {
                      print ("Low Fat , Passed")  
                    }
                      else
                        {
                          print ("Food Failed")
                        }
                
                }
          }
  }
  
#y (1,0,1)
```
# 7. Add a new column called HealthCheck to the USDAclean data frame using the output of the function. (10 p)
```{r}

```
# 8. How many foods in the USDAclean data frame fail the HealthCheck? (10 p)
```{r}

```
