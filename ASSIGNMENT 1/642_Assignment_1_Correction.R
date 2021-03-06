---
  title: "CMTH642 - Assignment 1"
author: "Mohammed Amir"
date: "October 16, 2016"
output: word_document
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


1. Read the csv files in the folder
```{r}
macro <- read.csv(file="D:/Big Data/CMTH642 - DATA ANALYTICS ADVANCED METHODS/ASSIGNMENT 1/USDA_Macronutrients.csv",head=TRUE,sep=",")
micro <- read.csv(file="D:/Big Data/CMTH642 - DATA ANALYTICS ADVANCED METHODS/ASSIGNMENT 1/USDA_Micronutrients.csv",head=TRUE,sep=",")
```

2. Merge the data frames using the variable "ID". Name the Merged Data Frame "USDA"
```{r}
USDA <- merge (macro, micro, by="ID")
```

3. Prepare the dataset for analysis
```{r}
# ----- Check data set structure
str(USDA)

# ----- Check for missing data
# is.na(USDA)

# ----- Check head
head(USDA)

# ----- Check column name
colnames(USDA)

# ----- Check data set summary
summary(USDA)

# ----- Number of rows and columns
dim(USDA)

# ----- Change Sodium & Potassium from factor to numeric
USDA$Sodium <- as.numeric(USDA$Sodium)
USDA$Potassium <- as.numeric(USDA$Potassium)
```

4. Remove records with missing values in 4 or more vectors
```{r}
USDA <- USDA[rowSums(is.na(USDA)) < 4, ]


```

5. How many records remain in the data frame?
```{r}
rowCount <- nrow(USDA)
rowCount
```

6. For records with missing values for Sugar, Vitamin E and Vitamin D, replace missing values with mean value for the respective vector
```{r}

Sugar    <- replace(USDA$Sugar,which(is.na(USDA$Sugar)),mean(USDA$Sugar, na.rm = TRUE))
VitaminC <- replace(USDA$VitaminC,which(is.na(USDA$VitaminC)),mean(USDA$VitaminC, na.rm = TRUE))
VitaminD <- replace(USDA$VitaminD,which(is.na(USDA$VitaminD)),mean(USDA$VitaminD, na.rm = TRUE))
VitaminE <- replace(USDA$VitaminE,which(is.na(USDA$VitaminE)),mean(USDA$VitaminE, na.rm = TRUE))

USDA <- data.frame(ID=USDA$ID,Description=USDA$Description,Calories=USDA$Calories, Protein=USDA$Protein, TotalFat=USDA$TotalFat, Carbohydrate=USDA$Carbohydrate, Sodium=USDA$Sodium, Cholesterol=USDA$Cholesterol, Calcium=USDA$Calcium, Iron=USDA$Iron, Potassium=USDA$Potassium, Sugar, VitaminC, VitaminD, VitaminE)



```

7. With a single line of code, remove all remaining records with missing values. Name the new Data Frame "USDAclean"
```{r}
#USDAClean <- na.omit(USDA)
USDAClean=USDA[complete.cases(USDA),]

```

8. How many records remain in the data frame?
```{r}
USDAClean_Count <- nrow(USDAClean)
USDAClean_Count
```

9. Which food has the highest sodium level?
```{r}
Highest_Sodium <- USDAClean[which.max(USDAClean$Sodium),]
Highest_Sodium

 which.max(USDAClean$Sodium)
 USDAClean$Description[262]
```

10. Create a scatter plot using Protein and Fat, with the plot title "Fat vs Protein", labeling the axes "Fat" and "Protein", and making the data points red
```{r, echo=FALSE}
plot ( USDAClean$TotalFat, USDAClean$Protein, main="Fat vs Protein", xlab = "Fat",ylab = "Protein", col="Red")

```

11. Create a histogram of Vitamin C distribution in foods, with a limit of 0 to 100 on the x-axis and breaks of 100
```{r}
hist(USDAClean$VitaminC, breaks = 100, xlim=c(0,100), main="Vitamin C distribution in food", xlab="Vitamin C")

```

12. Add a new variable to the data frame that takes value 1 if the food has higher sodium than average, 0 otherwise. Call this variable HighSodium
```{r}
#   ------ High Sodium
USDAClean$HighSodium <- ifelse(USDAClean$Sodium > mean(USDAClean$Sodium),1,0)
str(USDAClean)
```

13. Do the same for HighCalories, HighProtein, HighSugar, and HighFat
```{r}

#   ------ High Calories
USDAClean$HighCalories <- ifelse(USDAClean$Calories > mean(USDAClean$Calories),1,0)

#   ------ High Protein
USDAClean$HighProtein <- ifelse(USDAClean$Protein > mean(USDAClean$Protein),1,0)

#   ------ High Sugar
USDAClean$HighSugar <- ifelse(USDAClean$Sugar > mean(USDAClean$Sugar),1,0)

#   ------ High Fat
USDAClean$HighTotalFat <- ifelse(USDAClean$TotalFat > mean(USDAClean$TotalFat),1,0)

```

14. How many foods have both high sodium and high fat?
```{r}
High_Sodium_TotalFat <- USDAClean[USDAClean$HighSodium == 1,]
High_Sodium_TotalFat  <- High_Sodium_TotalFat[High_Sodium_TotalFat$HighTotalFat == 1,]
# High_Sodium_TotalFat

```

15. Calculate the average amount of iron by high and low protein (i.e. average amount of iron in foods with high protein and average amount of iron in foods with low protein)
```{r}
#    -- Average Iron for High protein
AverageIron_HighProtein <- USDAClean[USDAClean$HighProtein == 1,]
AverageIron_HighProtein <- mean(AverageIron_HighProtein$Iron)
AverageIron_HighProtein

#    -- Average Iron for low protein
AverageIron_LowProtein <- USDAClean[USDAClean$HighProtein == 0,]
AverageIron_LowProtein <- mean(AverageIron_LowProtein$Iron)
AverageIron_LowProtein


write.csv(USDAClean, file = "D:/USDA_CLEAN_DATA.csv", row.names= TRUE)
```

