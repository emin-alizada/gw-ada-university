library(tidyverse)
library(readxl)
library(psych)
library(ggplot2)
library(GGally)
library(corrplot)
library(class)
library(gmodels)
library(caret)
library(nnet)

# Reading data
df <- readxl::read_xls("dataset/Concrete_data.xls")

# Renaming columns
df.names <- c(
  "cement", 
  "blastFurnaceSlag", 
  "flyAsh", 
  "water",
  "superplasticizer", 
  "coarseAggregate", 
  "fineAggregate", 
  "age",
  "concreteCompressiveStrength"
  )
names(df) <- df.names

# ==================================

# Part 1 

# Default options for plotting
pairs(df, main="Pairwise relationships in our data model")

# Side package plotting functions
psych::pairs.panels(df, main="Pairwise relationships in our data model and their corellation")
GGally::ggpairs(df, title="Pairwise relationships in our data model and their corellation")


# Plotting corellated attributes and their corellation
# water and superplasticizer
ggplot(df, aes(x=water, y=superplasticizer)) + 
  geom_point(colour='red')+
  geom_smooth(method=lm) +
  ggtitle("Relationship between water and superplasticizer")

# cement and concreteCompressiveStrength
ggplot(df, aes(x=cement, y=concreteCompressiveStrength)) + 
  geom_point(colour='red')+
  geom_smooth(method=lm) +
  ggtitle("Relationship between cement and Concrete Compressive Strength")

# water and fine aggregate
ggplot(df, aes(x=water, y=fineAggregate)) + 
  geom_point(colour='red')+
  geom_smooth(method=lm) +
  ggtitle("Relationship between water and fine aggregate")


# cement and fly ash
ggplot(df, aes(x=cement, y=flyAsh)) + 
  geom_point(colour='red')+
  geom_smooth(method=lm) +
  ggtitle("Relationship between cement and fly ash")




# ==================================

# Part 2

# To get structure of an object in R
str(df)

# To get the statistical summary of our data
summary(df)

# Another statistical summary by psych package
psych::describe(df)



# Normalization techniques
print(df[1:6, ])

# Min-max normalization
minMaxNormalize <- function(x) {((x-min(x))/(max(x)-min(x)))}
df.minMaxNormalized <- as.data.frame(lapply(df, minMaxNormalize))
print(df.minMaxNormalized[1:6, ])

# Z score normalization

zScoreNormalize <- function(x) {(x-mean(x))/sd(x)}
df.zScoreNormalized <- as.data.frame(lapply(df, zScoreNormalize))
print(df.zScoreNormalized[1:6, ])



# Correlation of variables
df.correlation = cor(df)
print(df.correlation)
# Visualization of correlation matrix
corrplot(df.correlation)

# removal of least correlated attributes
df.simplified <- df[c(-2, -3, -8)]
print(df.simplified)

# ==================================

# Part 3

# Min max normalization
df.simplified.norm =  as.data.frame(lapply(df.simplified, minMaxNormalize))
# Z score normalization
df.simplified.scaled = scale(df.simplified)

# K means

# K-means clustering with 2 clusters

# min max normalized
df.simplified.norm.k2 <- kmeans(df.simplified.norm, centers=2, nstart=30)
print(df.simplified.norm.k2)
factoextra::fviz_cluster(
  df.simplified.norm.k2, 
  df.simplified.norm, 
  main = "Concrete dataset divided into 2 clusters"
)

# z score normalized
df.simplified.scaled.k2 <- kmeans(df.simplified.scaled, centers=2, nstart=30)
print(df.simplified.scaled.k2)
factoextra::fviz_cluster(
  df.simplified.scaled.k2, 
  df.simplified.scaled, 
  main = "Concrete dataset divided into 2 clusters"
)


# K-means clustering with 3 clusters

# min max normalized
df.simplified.norm.k3 <- kmeans(df.simplified.norm, centers=3, nstart=30)
print(df.simplified.norm.k3)
factoextra::fviz_cluster(
  df.simplified.norm.k3, 
  df.simplified.norm, 
  main = "Concrete dataset divided into 3 clusters"
)

# z score normalized
df.simplified.scaled.k3 <- kmeans(df.simplified.scaled, centers=3, nstart=30)
print(df.simplified.scaled.k3)
factoextra::fviz_cluster(
  df.simplified.scaled.k3, 
  df.simplified.scaled, 
  main = "Concrete dataset divided into 3 clusters"
)

# K-means clustering with 4 clusters

# min max normalized
df.simplified.norm.k4 <- kmeans(df.simplified.norm, centers=4, nstart=30)
print(df.simplified.norm.k4)
factoextra::fviz_cluster(
  df.simplified.norm.k4, 
  df.simplified.norm, 
  main = "Concrete dataset divided into 4 clusters"
)

# z score normalized
df.simplified.scaled.k4 <- kmeans(df.simplified.scaled, centers=4, nstart=30)
print(df.simplified.scaled.k4)
factoextra::fviz_cluster(
  df.simplified.scaled.k4, 
  df.simplified.scaled, 
  main = "Concrete dataset divided into 4 clusters"
)


# K-means clustering with 5 clusters

# min max normalized
df.simplified.norm.k5 <- kmeans(df.simplified.norm, centers=5, nstart=30)
print(df.simplified.norm.k5)
factoextra::fviz_cluster(
  df.simplified.norm.k5, 
  df.simplified.norm, 
  main = "Concrete dataset divided into 5 clusters"
)

# z score normalized
df.simplified.scaled.k5 <- kmeans(df.simplified.scaled, centers=5, nstart=30)
print(df.simplified.scaled.k5)
factoextra::fviz_cluster(
  df.simplified.scaled.k5, 
  df.simplified.scaled, 
  main = "Concrete dataset divided into 5 clusters"
)

# K-means clustering with 6 clusters

# min max normalized
df.simplified.norm.k6 <- kmeans(df.simplified.norm, centers=6, nstart=30)
print(df.simplified.norm.k6)
factoextra::fviz_cluster(
  df.simplified.norm.k6, 
  df.simplified.norm, 
  main = "Concrete dataset divided into 6 clusters"
)

# z score normalized
df.simplified.scaled.k6 <- kmeans(df.simplified.scaled, centers=6, nstart=30)
print(df.simplified.scaled.k6)
factoextra::fviz_cluster(
  df.simplified.scaled.k6, 
  df.simplified.scaled, 
  main = "Concrete dataset divided into 6 clusters"
)





# Function to visualize within-groups sums of squares against the number of clusters
wssplot <- function(data, nc=15, seed=1234) {
  wss <- (nrow(data)-1)*sum(apply(data,2,var)) 
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss) 
  }
  plot(1:nc, wss, type="b",
       xlab="Number of Clusters", ylab="Within groups sum of squares")
}

# Attempting to find the best number of clusters for 
# both min max normalized, and z scaled normalized
wssplot(df.simplified.norm)
wssplot(df.simplified.scaled)

factoextra::fviz_nbclust(
  df.simplified.norm, 
  FUNcluster=kmeans, 
  k.max = 6, 
  verbose = T,
  print.summary=TRUE,
) + labs(title= "Optimal number of clusters for min-max normalized data")

factoextra::fviz_nbclust(
  df.simplified.scaled,
  FUNcluster=kmeans, 
  k.max = 6,
  verbose = T,
  print.summary=TRUE
) + labs(title= "Optimal number of clusters for z-score normalized data")


# Analysis of centroids of optimal number of clusters

print(df.simplified.norm.k2$centers)
print(df.simplified.scaled.k2$centers)







# KNN

#training and test  70/30 split (setA)
df.simplified.norm.setA.train.index <- sample(nrow(df.simplified.norm), 0.7 * nrow(df.simplified.norm))

df.simplified.norm.setA.train <- df.simplified.norm[df.simplified.norm.setA.train.index, ]
print(df.simplified.norm.setA.train[1:8, ])

df.simplified.norm.setA.test <-df.simplified.norm[-df.simplified.norm.setA.train.index, ]
print(df.simplified.norm.setA.test[1:8, ])

#training and test  60/40 split (setB)
df.simplified.norm.setB.train.index <- sample(nrow(df.simplified.norm), 0.6 * nrow(df.simplified.norm))

df.simplified.norm.setB.train <- df.simplified.norm[df.simplified.norm.setB.train.index, ]
print(df.simplified.norm.setB.train[1:8, ])

df.simplified.norm.setB.test <-df.simplified.norm[-df.simplified.norm.setB.train.index, ]
print(df.simplified.norm.setB.test[1:8, ])

#training and test  50/50 split (setC)
df.simplified.norm.setC.train.index <- sample(nrow(df.simplified.norm), 0.5 * nrow(df.simplified.norm))

df.simplified.norm.setC.train <- df.simplified.norm[df.simplified.norm.setC.train.index, ]
print(df.simplified.norm.setC.train[1:8, ])

df.simplified.norm.setC.test <-df.simplified.norm[-df.simplified.norm.setC.train.index, ]
print(df.simplified.norm.setC.test[1:8, ])



# Evaluation of setA (70/30)

# Getting train set kmeans clusters according to our 
# optimal number of k
df.simplified.norm.setA.train.k2 <- kmeans(df.simplified.norm.setA.train, centers = 2, nstart = 30) 

# applying knn to test set
df.simplified.norm.setA.test.k2 <- knn(
  df.simplified.norm.setA.train, 
  df.simplified.norm.setA.test,
  df.simplified.norm.setA.train.k2$cluster,
  k=2
)

# Getting real kmeans cluster for test set
df.simplified.norm.setA.test.kmeans.k2 <- kmeans(df.simplified.norm.setA.test, centers = 2, nstart = 30)

# Renaming for better understanding in cross table
knnPredictedValues <- df.simplified.norm.setA.test.k2
realKmeansClusterValues <- df.simplified.norm.setA.test.kmeans.k2$cluster

# Creating cross table
df.simplified.norm.setA.ct <- CrossTable(
  realKmeansClusterValues, 
  knnPredictedValues, 
  prop.chisq=FALSE
)

#compute accuracy
df.simplified.norm.setA.comparison <- data.frame(
  row.names = names(df.simplified.norm.setA.test.kmeans.k2$cluster), 
  Actual=df.simplified.norm.setA.test.kmeans.k2$cluster, 
  Predicted=df.simplified.norm.setA.test.k2
)
df.simplified.norm.setA.comparison$Predicted <- as.numeric(df.simplified.norm.setA.comparison$Predicted)
df.simplified.norm.setA.acc <- mean(df.simplified.norm.setA.comparison$Actual==df.simplified.norm.setA.comparison$Predicted)
print(df.simplified.norm.setA.acc)







# Evaluation of setB (60/40)

# Getting train set kmeans clusters according to our 
# optimal number of k
df.simplified.norm.setB.train.k2 <- kmeans(df.simplified.norm.setB.train, centers = 2, nstart = 30) 

# applying knn to test set
df.simplified.norm.setB.test.k2 <- knn(
  df.simplified.norm.setB.train, 
  df.simplified.norm.setB.test,
  df.simplified.norm.setB.train.k2$cluster,
  k=2
)

# Getting real kmeans cluster for test set
df.simplified.norm.setB.test.kmeans.k2 <- kmeans(df.simplified.norm.setB.test, centers = 2, nstart = 30)

# Renaming for better understanding in cross table
knnPredictedValues <- df.simplified.norm.setB.test.k2
realKmeansClusterValues <- df.simplified.norm.setB.test.kmeans.k2$cluster

# Creating cross table
df.simplified.norm.setB.ct <- CrossTable(
  realKmeansClusterValues, 
  knnPredictedValues, 
  prop.chisq=FALSE
)

#compute accuracy
df.simplified.norm.setB.comparison <- data.frame(
  row.names = names(df.simplified.norm.setB.test.kmeans.k2$cluster), 
  Actual=df.simplified.norm.setB.test.kmeans.k2$cluster, 
  Predicted=df.simplified.norm.setB.test.k2
)
df.simplified.norm.setB.comparison$Predicted <- as.numeric(df.simplified.norm.setB.comparison$Predicted)
df.simplified.norm.setB.acc <- mean(df.simplified.norm.setB.comparison$Actual==df.simplified.norm.setB.comparison$Predicted)
print(df.simplified.norm.setB.acc)







# Evaluation of setC (50/50)

# Getting train set kmeans clusters according to our 
# optimal number of k
df.simplified.norm.setC.train.k2 <- kmeans(df.simplified.norm.setC.train, centers = 2, nstart = 30) 

# applying knn to test set
df.simplified.norm.setC.test.k2 <- knn(
  df.simplified.norm.setC.train, 
  df.simplified.norm.setC.test,
  df.simplified.norm.setC.train.k2$cluster,
  k=2
)

# Getting real kmeans cluster for test set
df.simplified.norm.setC.test.kmeans.k2 <- kmeans(df.simplified.norm.setC.test, centers = 2, nstart = 30)

# Renaming for better understanding in cross table
knnPredictedValues <- df.simplified.norm.setC.test.k2
realKmeansClusterValues <- df.simplified.norm.setC.test.kmeans.k2$cluster

# Creating cross table
df.simplified.norm.setC.ct <- CrossTable(
  realKmeansClusterValues, 
  knnPredictedValues, 
  prop.chisq=FALSE
)

#compute accuracy
df.simplified.norm.setC.comparison <- data.frame(
  row.names = names(df.simplified.norm.setC.test.kmeans.k2$cluster), 
  Actual=df.simplified.norm.setC.test.kmeans.k2$cluster, 
  Predicted=df.simplified.norm.setC.test.k2
)
df.simplified.norm.setC.comparison$Predicted <- as.numeric(df.simplified.norm.setC.comparison$Predicted)
df.simplified.norm.setC.acc <- mean(df.simplified.norm.setC.comparison$Actual==df.simplified.norm.setC.comparison$Predicted)
print(df.simplified.norm.setC.acc)

# ==================================

# Part 4


#training and test 75/25 split (setD)
df.simplified.norm.setD.train.index <- sample(nrow(df.simplified.norm), 0.75 * nrow(df.simplified.norm))

df.simplified.norm.setD.train <- df.simplified.norm[df.simplified.norm.setD.train.index, ]
print(df.simplified.norm.setD.train[1:8, ])

df.simplified.norm.setD.test <-df.simplified.norm[-df.simplified.norm.setD.train.index, ]
print(df.simplified.norm.setD.test[1:8, ])

# created a new variable, as we'll often address it
conStr <- df$`concreteCompressiveStrength`

# dependent variables for binomial (A) and multinomial (B) logistic regression
dependentA <- quantile(conStr, probs = seq(0, 1, 0.5))
dependentB <- quantile(conStr, probs = seq(0, 1, 0.333333))

Strength_bi <- Strength_multi <- conStr

# Assigning strength values for binomial
Strength_bi[Strength_bi <= dependentA[2]] <- 0 # low
Strength_bi[Strength_bi > dependentA[2]] <- 1 # high

df.setD.train.bi <- Strength_bi[df.simplified.norm.setD.train.index]
df.setD.test.bi <- Strength_bi[-df.simplified.norm.setD.train.index]

# Assigning strength values for multinomial
Strength_multi[Strength_multi <= dependentB[2]] <- 1 # low
Strength_multi[Strength_multi <= dependentB[3] & Strength_multi > dependentB[2]] <- 2 #mid
Strength_multi[Strength_multi > dependentB[3]] <- 3 # high

df.setD.train.multi <- Strength_multi[df.simplified.norm.setD.train.index]
df.setD.test.multi <- Strength_multi[-df.simplified.norm.setD.train.index]

# binomial logistic regression
df.bi <- df.simplified.norm.setD.train
df.bi$strength <- df.setD.train.bi

# fitting the training data
df.bi.fit <- glm(strength ~
                   cement + water + superplasticizer
                 + coarseAggregate + fineAggregate,
                 family = binomial, data = df.bi)
summary(df.bi.fit)

# prediction on the test data
strength.predict.bi <- predict(df.bi.fit, newdata = df.simplified.norm.setD.test, type = "response")
strength.predict.bi <- ifelse(strength.predict.bi > 0.5, 1, 0) # more than 0.5 is high (1) otherwise is low (0)

# accuracy check (it will also be mentioned in Confusion Matrix)
accuracy <- mean(strength.predict.bi == df.setD.test.bi)

confusionMatrix(data=as.factor(strength.predict.bi), reference = as.factor(df.setD.test.bi))

# multinomial logistic regression
df.multi <- df.simplified.norm.setD.train
df.multi$strength <- df.setD.train.multi

# fitting the training data
df.multi.fit <- multinom(strength ~
                           cement + water + superplasticizer
                         + coarseAggregate + fineAggregate, data = df.multi)
summary(df.multi.fit)

#prediction
strength.predicted.multi <- predict(df.multi.fit, newdata = df.simplified.norm.setD.test, type = "class")
confusionMatrix(as.factor(strength.predicted.multi), as.factor(df.setD.test.multi))

prediction <- strength.predicted.multi
realKmeansClusterValues <- df.simplified.norm.k3$cluster[-df.simplified.norm.setD.train.index]

# comparison / cross-tabulation with k-means (k=3)
CrossTable(
  realKmeansClusterValues, 
  prediction, 
  prop.chisq=FALSE
)

# multionomial logistic regression with different set of attributes:
df.multi.fit2 <- multinom(strength ~
                            cement + water + superplasticizer
                          + fineAggregate, data = df.multi)

summary(df.multi.fit2)

# prediction
strength.predicted.multi2 <- predict(df.multi.fit2, newdata = df.simplified.norm.setD.test, type = "class")
confusionMatrix(as.factor(strength.predicted.multi2), as.factor(df.setD.test.multi))

prediction <- strength.predicted.multi2
realKmeansClusterValues <- df.simplified.norm.k3$cluster[-df.simplified.norm.setD.train.index]

# comparison / cross-tabulation with k-means (k=3)
CrossTable(
  realKmeansClusterValues, 
  prediction, 
  prop.chisq=FALSE
)


