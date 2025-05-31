library(igraph)
library(readtext)
library(stringr)
library(sna)

#read paths
path <- "/Users/eminalizade/Desktop/Programming/University/R programming/ADA/Project 1/maildir"
setwd(path)
folder_list <- list.files(path=path) #list of folders available
subfolder_list <- c()
for (folder in folder_list) {
  vals <- list.files(path=gsub(" ", "", paste(path, '/',folder)))[1:15]
  dirs <- c()
  for (value in vals) { 
    value <- gsub(" ", "", paste(path, '/',folder, '/', value))
    dirs <- c(dirs, value)
  }
  subfolder_list <- c(subfolder_list, dirs)
}
#read files
subjects <- c()
froms <- c()
tos <- c()
for (folder in subfolder_list) {
  for (file in list.files(path=folder, recursive = TRUE)) {
    filename <- gsub(" ", "", paste(folder, '/',file))
    text <- readtext(filename)
    subject <- str_match(text, "Subject: *(.+)")[2]
    subjects <- c(subjects, subject)
    from <- str_match(text, "From: ([A-Za-z0-9+._-]+@[A-Za-z0-9+._-]+)")[2]
    froms <- c(froms, from)
    to <- str_match(text, "To: (\\s{0,}[A-Za-z0-9+._-]+@[A-Za-z0-9+._-]+,?\\s.*)+")[1]
    tos <- c(tos, to)
  }
}
#clean recipients
tos_cleaned <- c()
for (to in tos) {
  tos_cleaned <- c(tos_cleaned, str_split(to, 'Subject:', simplify = T)[1])
}
tos_final <- c()
for (to in tos_cleaned) {
  res <- str_split(to, 'To: ', simplify = T)[2]
  res <- gsub("\n", " ", res)
  res <- gsub("\t", " ", res)
  tos_final <- c(tos_final, res)
}
df <- data.frame(subjects, froms, tos_final)
tos_list <- c()
froms_list <- c()
subjects_list <- c()
i = 1
for (to in tos_final) {
  res <- gsub(" ", "", c(str_split(to, ",", simplify = T)))
  tos_list <- c(tos_list, res)
  j <- length(res)
  while(j>0) {
    froms_list <- c(froms_list, froms[i])
    subjects_list <- c(subjects_list, subjects[i])
    j <- j-1
  }
  i <- i+1
}
#create final dataset
df_final <- data.frame(froms_list, tos_list, subjects_list)
df_final$froms_list = gsub(" ", "", df_final$froms_list)

#question 3
my_graph <- graph_from_data_frame(df_final)
my_graph.adj <- igraph::get.adjacency(my_graph)
my_graph.density <- edge_density(my_graph)

simplified = simplify(my_graph)  
simplified.adj <- igraph::get.adjacency(simplified)
simplified.density <- edge_density(simplified)
hist(igraph::degree(simplified))
centr_info <- igraph::centr_betw(simplified)
centr_clos <- igraph::centr_clo(simplified)
sp <- igraph::shortest.paths(simplified)

#E(simplified)$weight <- rnorm(ecount(simplified))
#V(simplified)$weight <- rnorm(vcount(simplified))
#sg <- induced.subgraph(simplified, which(V(simplified)$weight > 0.8))

#check if graph is connected
conn <- is_connected(simplified)

#question 4
#number of articulation points
art_points <- articulation_points(simplified)
n_art_points <- length(articulation_points(simplified))

#authority scores
authority_scores <- authority.score(simplified)

#mean distance
mean_dist <- mean_distance(simplified)

#diameter of graph
diam <- diameter(simplified)
diam_vertices <- farthest_vertices(simplified)

#multiplicity of edge
#check whether non-simplified contains multiple edges
usual_contains <- any_multiple(my_graph)
mult <- count_multiple(my_graph)
#check whether simplified contains multiple edges
simpl_contains <- any_multiple(simplified)

#plot component distribution
hist(component_distribution(simplified))
max_distribution <- max(component_distribution(simplified))

#calculate distances between edges
dists <- distances(simplified)

#Question 5
#power centrality
simplified.center <- power_centrality(simplified, exponent=0.9)

#longest path
diam <- diameter(simplified)
diam_vertices <- farthest_vertices(simplified)

#largest cliques
lc <- largest_cliques(simplified)

#community
comm <- cluster_walktrap(simplified)









