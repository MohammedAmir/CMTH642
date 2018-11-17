# read data source
clean_data <- read.csv(file="D:/Big Data/CMTH642 - DATA ANALYTICS ADVANCED METHODS/ASSIGNMENT 2/USDAClean_MohammedAmir.csv",head=TRUE,sep=",")

# 1. draw distribution graph
x <- clean_data$Protein
y <- clean_data$Carbohydrate
z <- clean_data$TotalFat
w <- clean_data$Calories

plot (x, col = "Red")
lines(y, col="Blue")
lines(z, col = "Green")

layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
hist(x)
hist(y)
hist(x)

# 2. relationship between 2 variables
plot (z,w,  col = c("dark green", "red"), xlab="TotalFat", ylab="Calories", main="Protein vs Total Fat realtionship grphy")
legend("topright", legend=c("Protein", "TotalFat"), col=c("dark green", "red"), lty=1:2, cex=0.8, box.lty=0)


# 3. logistic regression model
glm()
#logistic regression model
model <- glm (HighCalories ~ Protein + TotalFat + Sodium, data=clean_data, family = binomial)
summary(model)
model.pred <- predict(model, type="response")
summary (model.pred)
xyplot(x ~ y, data = clean_data, groups = state)



cuse <- read.table("http://data.princeton.edu/wws509/datasets/cuse.dat",header=TRUE)
summary(cuse)
head(cuse)
attach(cuse)
glm(dependent variable ~ )
model <- glm( cbind(using, notUsing) ~  age + education + wantsMore , family = binomial)
model <- glm( age  ~ . , data = cuse, family = binomial)
summary(model)


# if function
x <- function(sodium, sugar, fat)
{
  if(sodium == 1)
  {
    print ("High Sodium , Failed")
    if(sugar == 1 && sodium == 1)
    {
      print ("High Sugar, Failed")
      if (fat == 1 && sugar == 1 && sodium == 1)
      {
        print ("Low Fat, Failed")
      }
      else
      {
        print ("Low Fat, Pass")
      }
    }
    else
    {
      print ("Low Sugar, Pass")
    }
  }
  else
  {print ("Low Sodium, Pass")}
}

x(1,1,0)

