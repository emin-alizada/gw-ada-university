# Install packages if required
# install.packages("readr")
# install.packages("igraph")
# install.packages("sna")
# install.packages("stringr")
# install.packages("tidyverse")

# Load libraries into memory
library(igraph)
library(readr)
library(sna)
library(stringr)
library(tidyverse)


# Getting all paths from our dataset
# In our case dataset is in the same working directory as the project is
pathToDataset <- paste(getwd(), "maildir", sep = "/")
topFolders <- list.files(path = pathToDataset) #list of folders available

workingDirectoryPath <- c() #that we will traverse later

for(folder in topFolders) {
  pathToFolder = paste(pathToDataset, folder, sep = "/")
  eightSubFolders <- list.files(path = pathToFolder)[0:8] #getting first 8 subfolders
  for (subfolder in eightSubFolders) {
    #adding them to our lis
    workingDirectoryPath <- c(workingDirectoryPath, paste(pathToFolder, subfolder, sep = "/"))
  }
}

# Creating empty data frame 
df1 <- data.frame(From = character(), To = character(), Subject = character())


# Reading context of files

# Function to get the context of file and finf out list of To's or return NA if couldn't found
getAndCleanTo <- function(contextOfFile) {
  toStarts <- str_locate(contextOfFile, "To: ")[2]
  toEnds <- (str_locate(contextOfFile, "Subject: ")[1]) - 1
  if (toEnds > toStarts) {
    toDirty <- str_sub(contextOfFile, start = toStarts, end = toEnds)
    toDirty <- str_replace_all(toDirty, "\n", "")
    toDirty <- str_replace_all(toDirty, "\r", "")
    toDirty <- str_replace_all(toDirty, "\t", "")
    toCleaned <- str_replace_all(toDirty, " ", "")
    
    toList <- str_split(toCleaned, ",", simplify = T)
    return(toList)
  } else {
    return(NA)
  }
}

# Function to check if string is valid mail
isValidEmail <- function(x) {
  grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", as.character(x), ignore.case=TRUE)
}


# Started reading contexts
for (folder in workingDirectoryPath) {
  files <- list.files(path = folder, full.names = T, recursive = T)
  
  for (file in files) {
    # Getting context of individual file
    contextOfFile <- read_file(file)
    subject <- str_match(contextOfFile, "Subject: *(.+)")[2]
    from <- str_match(contextOfFile, "From: ([A-Za-z0-9+._-]+@[A-Za-z0-9+._-]+)")[2]
    # Using function to retreive list of TO's of email 
    toList <- getAndCleanTo(contextOfFile)

    # For each TO in List checking if Value is not NA, and it's valid mail
    for (to in toList) {
      if(!is.na(to)) {
        if (isValidEmail(to)){
          # Adding to Data Frame
          df1[nrow(df1) + 1,] = c(from, to, subject)
        }
      }
    }
  }
}

# Finished reading context of files

# Cleaning data frame

df1 <- na.omit(df1) # Getting rid of rows with NA
df1 <- df1[!duplicated(df1), ] # Removing duplicates

write_csv(df1, "filtered_dataset.csv") #Creating initial csv file

# The following code was used by our team in order not to read the contents of dataset 
# each time we work on the project, because it takes a lot of time. So first time we generated CSV file
# and the next times we were populating data frame from that CSV 
# df1 <- as.data.frame(read.csv("filtered_dataset.csv"))

#Question 3
ig <-  graph_from_data_frame(df1, directed = T)

#Visualization of graph
V(ig)$size = 6
plot.igraph(ig, vertex.label = NA, edge.arrow.size = 0.5)

# Filtering out all mails that are not from the company domain
df1 <- df1[str_ends(df1$From, "enron.com"), ]
df1 <- df1[str_ends(df1$To, "enron.com"), ]
# Removing mail to self
df1 <- subset(df1, df1$From != df1$To)

# Getting weights according to the number of emails from one employee to other
df1 = df1 %>% group_by(From,To) %>% summarise(weight=n())

igs <- graph_from_data_frame(df1 , directed = T)
igs <- igraph::simplify(igs)

#Visualization of graph
V(igs)$size = 6
plot.igraph(igs, vertex.label = NA, edge.arrow.size = 0.5)

# We can see the structure with str() function
str(igs)

# Getiing adjacency Matrix
igs.adj <- igraph::get.adjacency(igs)
print(igs.adj[1:9, 1:9])

# Printing degrees of each vertex
igraph::degree(igs)
# Histogram of Degrees of vertexes
hist(igraph::degree(igs), main = "Degrees of graph vertices")

# List of Vertexes
V(igs)
# List of edges
E(igs)

# Gettiing density of graph
igs.density = sna::gden(igs.adj)
# Getting edge density
igraph::edge_density(igs)

# Central Betweennees
igraph::centr_betw(igs)


# Diameter of graph 
igraph::diameter(igs)

#Question 4

#Func 1
is_directed(igs)

#Func 2
is_igraph(igs)

#Func 3
neighbors(igs, 2, "out") 

#Func 4
gsize(ig)
gsize(igs) 


#Func 5
gorder(ig)
gorder(igs) 


#Func 6
plot.igraph(subgraph.edges(igs, 37:43))

#Func 7
random_walk(igs, 21, 6)

#Func 8
articulation_points(igs)

#Func 9
mean_distance(igs)

#Func 10
igraph::transitivity(igs)

#Question 5
# Central Nodes
igraph::centr_degree(igs)

#longest path
igraph::diameter(igs)
igraph::farthest_vertices(igs)

#largest cliques
igraph::largest_cliques(igs)

#Ego
igraph::ego(igs)

#power centrality
igraph::power_centrality(igs, loops = F, exponent = 0.9)

#community
igraph::walktrap.community(igs)

# Question 6
# To get litte number of vertices we have used weights

df2 <- df1[df1$weight > 34, ]
ig2 <- graph_from_data_frame(df2 , directed = T)

wc <- igraph::walktrap.community(ig2)
colors <- rainbow(max(membership(wc)))

plot.igraph(
  ig2,
  layout = layout.gem,
  # vertex.label = NA,
  vertex.size = 10,
  vertex.color= colors[membership(wc)],
  edge.arrow.size = 0.2,
  edge.label = ig2$weight, 
  edge.label.color = "red",
  edge.width = 2 * ig2$weight,
  edge.length = 40,
)

V(ig2)
igraph::degree(ig2)
