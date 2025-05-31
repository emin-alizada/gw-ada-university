library(psych)
library(factoextra)

df <- USArrests
print(df)

summary(df)

describe(df)

# Removing Missing Data
df <- na.omit(df)

scaledDf <- scale(df)
print(scaledDf)

k2 <- kmeans(scaledDf, centers = 2, nstart = 30)
print(k2)
str(k2)


k3 <- kmeans(scaledDf, centers = 3, nstart = 40)
print(k3)
str(k3)

k4 <- kmeans(scaledDf, centers = 4, nstart = 40)
print(k4)
str(k4)

k5 <- kmeans(scaledDf, centers = 5, nstart = 40)
print(k5)
str(k5)

usaDfViz <- factoextra::fviz_cluster(k4, df)
usaDfViz


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

wssplot(scaledDf)

fviz_nbclust(scaledDf, kmeans)



plot(df)
plot(scaledDf)

iclust(df, 4)
