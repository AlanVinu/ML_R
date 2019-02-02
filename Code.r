library(ggvis)
library(class)


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

