library(ggvis)
library(class)
library(gmodels)


#Step 1 : Get Data
data("iris")
head(iris)


#Step 2 : Know Data; scatterplot
iris %>% ggvis(~Sepal.Length, ~Sepal.Width, fill = ~Species) %>% layer_points()
iris %>% ggvis(~Petal.Length, ~Petal.Width, fill = ~Species) %>% layer_points()
cor(iris$Petal.Length, iris$Petal.Width)

# Return values of `iris` levels 
x=levels(iris$Species)

# Print Setosa correlation matrix
print(x[1])
cor(iris[iris$Species==x[1],1:4])

# Print Versicolor correlation matrix
print(x[2])
cor(iris[iris$Species==x[2],1:4])

# Print Virginica correlation matrix
print(x[3])
cor(iris[iris$Species==x[3],1:4])

str(iris)

round(prop.table(table(iris$Species))*100, digits = 1)


#Step 3 
summary(iris[c("Petal.Width", "Sepal.Width")])

#Step 4: Prepare Workspace
#Step 5: Prepare Data


normalize <- function(x) {
  num <- x - min(x)
  denom <- max(x) - min(x)
  return(num/denom)
}

#normalization
iris_norm <- as.data.frame(lapply(iris[1:4], normalize))

summary(iris_norm)

#training and test dataset

set.seed(1234)

ind <- sample(2, nrow(iris), replace = TRUE, prob = c(0.67,0.33))

iris.training <- iris[ind == 1, 1:4]

head(iris.training)

iris.test <- iris[ind == 2, 1:4]

head(iris.test)

iris.trainLabels <- iris[ind == 1, 5]

summary(iris.trainLabels)

iris.testLabels <- iris[ind == 2, 5]

summary(iris.testLabels)

#kNN

iris_pred <- knn(train = iris.training, test = iris.test, cl = iris.trainLabels, k = 3)

summary(iris_pred)

#Step 7: Evaluate

irisTestLabels <- data.frame(iris.testLabels)

merge <- data.frame(iris_pred, irisTestLabels)

names(merge) <- c("Predicted Species", "Observed Species")

CrossTable(x = iris.testLabels, y = iris_pred, prop.chisq = FALSE)