install.packages("jsonlite", repos="https://cran.rstudio.com/")
library("jsonlite")

json_file <- 'https://datahub.io/machine-learning/banknote-authentication/datapackage.json'
json_data <- fromJSON(paste(readLines(json_file), collapse=""))

# get list of all resources:
print(json_data$resources$name)

# print all tabular data(if exists any)
for(i in 1:length(json_data$resources$datahub$type)){
  if(json_data$resources$datahub$type[i]=='derived/csv'){path_to_file = json_data$resources$path[i]
  data <- read.csv(url(path_to_file))
  print(data)}}


my_data <- data("iris")
head(iris,15)
tail(iris,10)

# split data
set.seed(567)
n= nrow(iris)
index_train_data <- sample(1:n, 0.8*n)
training_set <- iris[index_train_data,]
testing_set <- iris[-index_train_data,]


setosa_subset <- iris[iris$Species=="setosa",]

entropy <- function(target) {
  freq <- table(target)/length(target)
  # vectorize
  vec <- as.data.frame(freq)[,2]
  #drop 0 to avoid NaN resulting from log2
  vec<-vec[vec>0]
  #compute entropy
  -sum(vec * log2(vec))
}

entropy(setosa_subset$Species)

#normalized dataset

norm1 <- (training_set$Sepal.Length - min(training_set$Sepal.Length))/(max(training_set$Sepal.Length)-min(training_set$Sepal.Length))


norm_func <- function(x){(x-min(x))/(max(x)-min(x))}
data_norm <- as.data.frame(lapply(iris[,c(1,2,3,4)], norm_func))
summary(data_norm)

#data.frame = used to build a data frame
#as.data.frame = used to force other object into your data frame


iris_train <- data_norm[index_train_data,]
iris_test <- data_norm[-index_train_data,]

iris_target_category <- iris[index_train_data,5]
iris_test_category <- iris[-index_train_data,5]

#KNN classification algorithm

library(class)

Model <- knn(iris_train, iris_test, k=13, cl= iris_target_category)

#cl = factor of true classification of training set

# confusion matrix
tab <- table(Model,iris_test_category)
#accuracy of the model
accuracy1 <- sum(diag(tab))/sum(tab)


#decsision tree

library (rpart)


new_data <- data.frame(iris_train,iris_target_category)

#classifcation tree
fit <- rpart(new_data$iris_target_category ~. , data=new_data, method = "class")

#regression tree
#fit <- rpart(new_data$iris_target_category ~. , data=new_data, method = "anova")
print(fit)
summary(fit)
plot(fit)
text(fit)

#accuracy of the decision tree

new_data_test <- data.frame(iris_test,iris_test_category)

prediction_all <- predict(fit,newdata = new_data_test, type= "class")

#confusion matrix
tab_2 <- table(prediction_all,new_data_test$iris_test_category)
#accuracy
accuracy2 <- sum(diag(tab_2))/ sum(tab_2)