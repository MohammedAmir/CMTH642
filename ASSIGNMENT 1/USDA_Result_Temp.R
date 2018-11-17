
# 1 - Read two data sets
macro <- read.csv(file="D:/Big Data/CMTH642 - DATA ANALYTICS ADVANCED METHODS/ASSIGNMENT 1/USDA_Macronutrients.csv",head=TRUE,sep=",")
micro <- read.csv(file="D:/Big Data/CMTH642 - DATA ANALYTICS ADVANCED METHODS/ASSIGNMENT 1/USDA_Micronutrients.csv",head=TRUE,sep=",")

# 2 - Merge data sets based on ID
USDA <- merge (macro, micro, by="ID")
USDA
#write.csv(USDA, file = "D:/USDA_MERGE.csv", row.names= TRUE)

# 3 - Prepare data set for analysis
str(USDA)
is.na(USDA)
# Change Sodium & Potassium from factor to numeric
USDA$Sodium <- as.numeric(USDA$Sodium)
USDA$Potassium <- as.numeric(USDA$Potassium)
str(USDA)


# 4 - Removed columns with 4 or more NA
USDA <- USDA[rowSums(is.na(USDA)) < 4, ]
USDA
write.csv(USDA, file = "D:/USDA_MERGE_WITHOUT_4NA.csv", row.names= TRUE)

# columns with 4 or more NA
#USDA1 <- USDA[rowSums(is.na(USDA)) >= 4, ]
#USDA1
#write.csv(USDA1, file = "D:/USDA_MERGE_MORE_NA.csv", row.names= TRUE)


# 5 - Number of records in data frame
nrow(USDA)

# 6 - For missing values in Sugar and Vitamins replace with mean value 
# Sugar
# s <- USDA$Sugar
# s
# s1 <- mean(USDA$Sugar, na.rm = TRUE)
# s1
s2 <- replace(USDA$Sugar,which(is.na(USDA$Sugar)),mean(USDA$Sugar, na.rm = TRUE))
s2

#  Vitamin D
# VE <- USDA$VitaminE
# VE
# VE1 <- mean(USDA$VitaminE, na.rm = TRUE)
# VE1
VE2 <- replace(USDA$VitaminE,which(is.na(USDA$VitaminE)),mean(USDA$VitaminE, na.rm = TRUE))
VE2

# Vitamin C
# VC <- USDA$VitaminC
# VC
# VC1 <- mean(USDA$VitaminC, na.rm = TRUE)
# VC1
VC2 <- replace(USDA$VitaminC,which(is.na(USDA$VitaminC)),mean(USDA$VitaminC, na.rm = TRUE))
VC2

# Vitamin C
# VD <- USDA$VitaminD
# VD
# VD1 <- mean(USDA$VitaminD, na.rm = TRUE)
# VD1
VD2 <- replace(USDA$VitaminD,which(is.na(USDA$VitaminD)),mean(USDA$VitaminD, na.rm = TRUE))
VD2

m <- data.frame(USDA$ID,USDA$Description,USDA$Calories, USDA$Protein, USDA$TotalFat, USDA$Carbohydrate, USDA$Sodium, USDA$Cholesterol, USDA$Calcium, USDA$Iron, USDA$Potassium,VC2, VE2, VD2, s2)
m
write.csv(m, file = "D:/USDA_MERGE_WITH_MEAN1.csv", row.names= TRUE)

# 7 - Remove all records with missing values
USDAClean <- na.omit(m)
USDAClean
write.csv(USDAClean, file = "D:/USDA_CLEAN1.csv", row.names= TRUE)

#data <- m[!apply(is.na(m) | m == "", 1, all),]
#write.csv(data, file = "D:/data.csv", row.names= TRUE)

#str(USDAClean$USDA.Sodium)

# 8 - Number of remaining data counts
nrow(USDAClean)

# 9 - Highest sodium level
minSodium <- USDAClean[which.max(USDAClean$USDA.Sodium),]
minSodium
MAXsodium1 <- max(as.integer(USDAClean$USDA.Sodium))
MAXsodium1
#TT <- which.max(USDAClean$USDA.Sodium)
#TT

# 10 - Protein vs Fat Scatter plot
plot ( USDAClean$USDA.TotalFat, USDAClean$USDA.Protein, main="Fat vs Protein", xlab = "Fat",ylab = "Protein", col="Red")

# 11 - Vitamin C histogram
hist(USDAClean$VC2, breaks = 100, xlim=c(0,100), main="Vitamin C distribution in food", xlab="Vitamin C")

# 12 - High Sodium
a <- as.numeric(USDAClean$USDA.Sodium)
b <- mean(as.numeric(USDAClean$USDA.Sodium))
USDAClean$HighSodium <- ifelse(a > b,1,0)
write.csv(USDAClean, file = "D:/data.csv", row.names= TRUE)

# 13 - High Calories
c <- as.numeric(USDAClean$USDA.Calories)
d <- mean(as.numeric(USDAClean$USDA.Calories))
USDAClean$HighCalories <- ifelse(c > d,1,0)
write.csv(USDAClean, file = "D:/data.csv", row.names= TRUE)

------ High Protein
e <- as.numeric(USDAClean$USDA.Protein)
f <- mean(as.numeric(USDAClean$USDA.Protein))
USDAClean$HighProtein <- ifelse(e > f,1,0)
write.csv(USDAClean, file = "D:/data.csv", row.names= TRUE)

------ High Sugar
g <- as.numeric(USDAClean$s2)
h <- mean(as.numeric(USDAClean$s2))
USDAClean$HighSugar <- ifelse(g > h,1,0)
write.csv(USDAClean, file = "D:/data.csv", row.names= TRUE)


------ High fat
i <- as.numeric(USDAClean$USDA.TotalFat)
j <- mean(as.numeric(USDAClean$USDA.TotalFat))
USDAClean$HighFat <- ifelse(i > j,1,0)
write.csv(USDAClean, file = "D:/data1.csv", row.names= TRUE)

# 14 - how many foods with high sodium and fat
yt <- USDAClean[USDAClean$HighSodium == 1,]
yt
#yt <- nrow(USDAClean[USDAClean$HighSodium == 1,])
#yt <- nrow(yt)
yt <- yt[yt$HighFat == 1,]
yx <- nrow(yt)
yx
nrow(filter(USDAClean, USDAClean$HighSodium == 1))

# 15 -- Average Iron for high protein

AverageIron_HighProtein <- USDAClean[USDAClean$HighProtein == 1,]
AverageIron_HighProtein
AverageIron_HighProtein <- mean(AverageIron_HighProtein$USDA.Iron)
AverageIron_HighProtein
#    -- Average Iron for low protein
AverageIron_LowProtein <- USDAClean[USDAClean$HighProtein == 0,]
AverageIron_LowProtein
AverageIron_LowProtein <- mean(AverageIron_LowProtein$USDA.Iron)
AverageIron_LowProtein

#-----------------------------------------------------------------------
meanSodium <- mean(as.numeric(USDAClean$USDA.Sodium))
meanSodium
USDAClean$HighSodium <- ifelse(1116 > meanSodium,1,0)
USDAClean$HighSodium

# Remove all NA VALUES
USDA1 <- data.frame(USDA$ID,USDA$Description,USDA$Calories, USDA$Protein, USDA$TotalFat, USDA$Carbohydrate, USDA$Sodium, USDA$Cholesterol, USDA$Sugar,USDA$Calcium, USDA$Iron, USDA$Potassium, USDA$VitaminC, USDA$VitaminD,USDA$VitaminE)
USDA1

USDA2 <- data.frame(USDA$ID,USDA$Description,USDA$Calories, USDA$Protein, USDA$TotalFat, USDA$Carbohydrate, USDA$Sodium, USDA$Cholesterol, USDA$Calcium, USDA$Iron, USDA$Potassium)
USDA2

y <- na.omit(USDA)
nro, w(y)
y
