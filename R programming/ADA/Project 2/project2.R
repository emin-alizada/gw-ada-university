library("readxl")
library(psych)
library(ggplot2)
library(data.table)
library(factoextra)
library(class)
library(gmodels)
library(caret)
library(nnet)
df <- read_xls("Concrete_data.xls")
#pair plot using default pairs function
pairs(df)
plot(df$`Blast Furnace Slag (composnent 2)(kg in a m^3 mixture)`, df$`Fly Ash (component 3)(kg in a m^3 mixture)`)
plot(df$`Water  (component 4)(kg in a m^3 mixture)`, df$`Superplasticizer (component 5)(kg in a m^3 mixture)`)
# pair plot using other libraries
pairs.panels(df)
df_copy <- copy(df)
names(df_copy) <- c('cement', 'blast', 'flyash', 'water', 'superplast', 'coarse', 'fine', 'age', 'strength')
ggplot(df_copy, aes(x=cement, y=strength)) + 
       geom_point()+
       geom_smooth(method=lm)
ggplot(df_copy, aes(x=water, y=superplast)) + 
  geom_point()+
  geom_smooth(method=lm)
ggplot(df_copy, aes(x=cement, y=flyash)) + 
  geom_point()+
  geom_smooth(method=lm)

#PART TWO
#get structure
str(df_copy)
#get statistical info
describe(df_copy)
#remove column 9
df_copy <- df_copy[-9]
#normalize data
normalize <- function(x) {((x-min(x))/(max(x)-min(x)))}
df.norm <- as.data.frame(lapply(df_copy, normalize))
#names(df.norm) <- names(df)
df.norm[1:5,]
# z score normalization
zscore <- function(x) {(x-mean(x))/sd(x)}
df.znorm <- as.data.frame(lapply(df_copy, zscore))
#names(df.znorm) <- names(df)
df.znorm[1:5,]
#remove unnecessary for norm
corr <- cor(df.norm)
df.reduced <- df.norm[-3]
corr.reduced <- cor(df.reduced)
df.reduced <- df.reduced[-2]
corr.reduced <- cor(df.reduced)
#reduced for zscores
df.reduced.z <- df.znorm[-2: -3]

#training and test (to be used in knn) 70/30
df.reduced.nrows <- nrow(df.reduced)
df.reduced.sample <- 0.7
df.reduced.train.index <- sample(df.reduced.nrows, df.reduced.sample*df.reduced.nrows)
length(df.reduced.train.index)
df.reduced.train <- df.reduced[df.reduced.train.index,]
df.reduced.train[1:10,]
df.reduced.test <- df.reduced[-df.reduced.train.index,]
df.reduced.test[1:10,]


#60/40 split
df.reduced.sample60 <- 0.6
df.reduced.train60.index <- sample(df.reduced.nrows, df.reduced.sample60*df.reduced.nrows)
length(df.reduced.train60.index)
df.reduced.train60 <- df.reduced[df.reduced.train60.index,]
df.reduced.train60[1:10,]
df.reduced.test60 <- df.reduced[-df.reduced.train60.index,]
df.reduced.test60[1:10,]

#50/50 split
df.reduced.sample50 <- 0.5
df.reduced.train50.index <- sample(df.reduced.nrows, df.reduced.sample50*df.reduced.nrows)
length(df.reduced.train50.index)
df.reduced.train50 <- df.reduced[df.reduced.train50.index,]
df.reduced.train50[1:10,]
df.reduced.test50 <- df.reduced[-df.reduced.train50.index,]
df.reduced.test50[1:10,]

#PART THREE
#kmeans
fviz_nbclust(df.reduced, FUNcluster=kmeans,print.summary=TRUE)
clust.kmeans.k2 <- kmeans(df.reduced, centers=2, nstart=30)
fviz_cluster(clust.kmeans.k2,df.reduced)
clust.kmeans.k3 <- kmeans(df.reduced, centers=3, nstart=30)
fviz_cluster(clust.kmeans.k3,df.reduced)
clust.kmeans.k4 <- kmeans(df.reduced, centers=4, nstart=30)
fviz_cluster(clust.kmeans.k4,df.reduced)
clust.kmeans.k5 <- kmeans(df.reduced, centers=5, nstart=30)
fviz_cluster(clust.kmeans.k5,df.reduced)
clust.kmeans.k6 <- kmeans(df.reduced, centers=6, nstart=30)
fviz_cluster(clust.kmeans.k6,df.reduced)

#analyze centers
clust.kmeans.k5$centers

#kmeans with zscores
fviz_nbclust(df.reduced.z, FUNcluster=kmeans,print.summary=TRUE)
clust.z.kmeans.k8 <- kmeans(df.reduced.z, centers=8, nstart=30)
fviz_cluster(clust.z.kmeans.k8,df.reduced.z)

#analyze centers
clust.z.kmeans.k8$centers

#knn with k=5
#labels
df.reduced.train.k5 <- kmeans(df.reduced.train, centers = 5)
df.reduced.test.k5 <- knn(df.reduced.train, df.reduced.test, df.reduced.train.k5$cluster, k=5)
df.reduced.train.labels <- df.reduced.train.k5$cluster

df.reduced.test.k5 <- kmeans(df.reduced.test, centers =5)
df.reduced.test.labels <- df.reduced.test.k5$cluster

#evaluate
df.reduced.test.pred <- knn(df.reduced.train, df.reduced.test, df.reduced.train.k5$cluster, k=5)
df.reduced.ct <- CrossTable(df.reduced.test.labels, df.reduced.test.pred, prop.chisq=FALSE)
cm.k5 <- data.frame(row.names = names(df.reduced.test.labels), Actual=df.reduced.test.labels, Predicted=df.reduced.test.pred)
cm.k5$Predicted <- as.numeric(cm.k5$Predicted)
#compute accuracy
acc.k5 <- mean(cm.k5$Actual==cm.k5$Predicted)

#knn with k=2
df.reduced.train.k2 <- kmeans(df.reduced.train, centers = 2)
df.reduced.test.k2 <- knn(df.reduced.train, df.reduced.test, df.reduced.train.k2$cluster, k=2)
df.reduced.train.labels <- df.reduced.train.k2$cluster

df.reduced.test.k2 <- kmeans(df.reduced.test, centers =2)
df.reduced.test.labels <- df.reduced.test.k2$cluster

#evaluate
df.reduced.test.pred <- knn(df.reduced.train, df.reduced.test, df.reduced.train.k2$cluster, k=2)
df.reduced.ct <- CrossTable(df.reduced.test.labels, df.reduced.test.pred, prop.chisq=FALSE)
cm.k2 <- data.frame(row.names = names(df.reduced.test.labels), Actual=df.reduced.test.labels, Predicted=df.reduced.test.pred)
cm.k2$Predicted <- as.numeric(cm.k2$Predicted)
#compute accuracy
acc.k2 <- mean(cm.k2$Actual==cm.k2$Predicted)

#knn with k=3
df.reduced.train.k3 <- kmeans(df.reduced.train, centers = 3)
df.reduced.test.k3 <- knn(df.reduced.train, df.reduced.test, df.reduced.train.k3$cluster, k=3)
df.reduced.train.labels <- df.reduced.train.k3$cluster

df.reduced.test.k3 <- kmeans(df.reduced.test, centers =3)
df.reduced.test.labels <- df.reduced.test.k3$cluster

#evaluate
df.reduced.test.pred <- knn(df.reduced.train, df.reduced.test, df.reduced.train.k3$cluster, k=3)
df.reduced.ct <- CrossTable(df.reduced.test.labels, df.reduced.test.pred, prop.chisq=FALSE)
cm.k3 <- data.frame(row.names = names(df.reduced.test.labels), Actual=df.reduced.test.labels, Predicted=df.reduced.test.pred)
cm.k3$Predicted <- as.numeric(cm.k3$Predicted)
#compute accuracy
acc.k3 <- mean(cm.k3$Actual==cm.k3$Predicted)

#knn with k=4
df.reduced.train.k4 <- kmeans(df.reduced.train, centers = 4)
df.reduced.test.k4 <- knn(df.reduced.train, df.reduced.test, df.reduced.train.k4$cluster, k=4)
df.reduced.train.labels <- df.reduced.train.k4$cluster

df.reduced.test.k4 <- kmeans(df.reduced.test, centers =4)
df.reduced.test.labels <- df.reduced.test.k4$cluster

#evaluate
df.reduced.test.pred <- knn(df.reduced.train, df.reduced.test, df.reduced.train.k4$cluster, k=4)
df.reduced.ct <- CrossTable(df.reduced.test.labels, df.reduced.test.pred, prop.chisq=FALSE)
cm.k4 <- data.frame(row.names = names(df.reduced.test.labels), Actual=df.reduced.test.labels, Predicted=df.reduced.test.pred)
cm.k4$Predicted <- as.numeric(cm.k4$Predicted)
#compute accuracy
acc.k4 <- mean(cm.k4$Actual==cm.k4$Predicted)

#knn with k=6
df.reduced.train.k6 <- kmeans(df.reduced.train, centers = 6)
df.reduced.test.k6 <- knn(df.reduced.train, df.reduced.test, df.reduced.train.k6$cluster, k=6)
df.reduced.train.labels <- df.reduced.train.k6$cluster

df.reduced.test.k6 <- kmeans(df.reduced.test, centers = 6)
df.reduced.test.labels <- df.reduced.test.k6$cluster

#evaluate
df.reduced.test.pred <- knn(df.reduced.train, df.reduced.test, df.reduced.train.k6$cluster, k=6)
df.reduced.ct <- CrossTable(df.reduced.test.labels, df.reduced.test.pred, prop.chisq=FALSE)
cm.k6 <- data.frame(row.names = names(df.reduced.test.labels), Actual=df.reduced.test.labels, Predicted=df.reduced.test.pred)
cm.k6$Predicted <- as.numeric(cm.k6$Predicted)
#compute accuracy
acc.k6 <- mean(cm.k6$Actual==cm.k6$Predicted)

#iclust
iclust(df.reduced, nclusters = 2)
iclust(df.reduced, nclusters = 3)
iclust(df.reduced, nclusters = 4)
iclust(df.reduced, nclusters = 5)
iclust(df.reduced, nclusters = 6)

#PART FOUR
#get labels for test and train (binomial)
dependent <- quantile(df$`Concrete compressive strength(MPa, megapascals)`, probs = seq(0, 1, 0.5))
strength_binary <- df$`Concrete compressive strength(MPa, megapascals)`
strength_binary[strength_binary<=dependent[2]] <- 0
strength_binary[strength_binary>dependent[2]] <- 1
df.reduced.train.binarylabels <- strength_binary[df.reduced.train.index]
df.reduced.test.binarylabels <- strength_binary[-df.reduced.train.index]

#get labels for test and train (multinomial)
dependent_multi <- quantile(df$`Concrete compressive strength(MPa, megapascals)`, probs = seq(0, 1, 0.333333))
strength_multi <- df$`Concrete compressive strength(MPa, megapascals)`
strength_multi[strength_multi<=dependent_multi[2]] <- 1
strength_multi[strength_multi>dependent_multi[2] & strength_multi<=dependent_multi[3]] <- 2
strength_multi[strength_multi>dependent_multi[3]] <- 3
df.reduced.train.multilabels <- strength_multi[df.reduced.train.index]
df.reduced.test.multilabels <- strength_multi[-df.reduced.train.index]

#fit binomial 
df.bin <- df.reduced.train
df.bin$strength <- df.reduced.train.binarylabels

glm.fit <- glm(strength ~ cement + 
                 water + superplast + coarse+ fine + age, 
               family = binomial, data = df.bin)
summary(glm.fit)

#evaluate binomial
glm.probs <- predict(glm.fit, df.reduced.test, type = "response")
glm.probs <- ifelse(glm.probs >0.5, 1, 0)
accuracy <- sum(glm.probs==df.reduced.test.binarylabels)/length(glm.probs)

glm.trainprobs <- predict(glm.fit, type = "response")
glm.trainprobs <- ifelse(glm.trainprobs >0.5, 1, 0)
train_accuracy <- sum(glm.trainprobs==df.reduced.train.binarylabels)/length(glm.trainprobs)
confusionMatrix(data=as.factor(glm.probs), reference = as.factor(df.reduced.test.binarylabels))

# fit multinomial
df.mbin <- df.reduced.train
df.mbin$strength <- df.reduced.train.multilabels

glm.mfit <- multinom(strength ~ cement + 
                 water + superplast + coarse+ fine + age, data = df.mbin)
summary(glm.mfit)

#evaluate multinomial
glm.mprobs <- predict(glm.mfit, df.reduced.test, type = "class")
glm.mtrainprobs <- predict(glm.mfit, type = "class")

confusionMatrix(data=as.factor(glm.mprobs), reference = as.factor(df.reduced.test.multilabels))

#compare unsupervised and supervised
comp <- data.frame(Kmeans= as.numeric(df.reduced.test.k3$cluster), Multinomial = as.numeric(glm.mprobs))

#improve multinomial
glm.mfit <- multinom(strength ~ cement + 
                       water  + coarse+ fine + age, data = df.mbin)
glm.mprobs <- predict(glm.mfit, df.reduced.test, type = "class")

confusionMatrix(data=as.factor(glm.mprobs), reference = as.factor(df.reduced.test.multilabels))
comp <- data.frame(Kmeans= as.numeric(df.reduced.test.k3$cluster), Multinomial = as.numeric(glm.mprobs))
