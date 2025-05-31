library(tidyverse)

# Read email files
emails = list.files("dataset/", full.names = T, recursive = T)

# raw ata frame
inboxesRaw = data.frame(
  from = apply(as.data.frame(emails),1, function(x) {readLines(x,warn = F)[3]}),
  to = emails,
  stringsAsFactors = F
)

# Filter out non ENRON employees
inboxes = inboxesRaw[grepl("@enron.com", inboxesRaw$from),]
inboxes$from = str_sub(inboxes$from,7,nchar(inboxes$from) - 10)
to = str_split(inboxes$to, "/")
inboxes$to = sapply(to,"[",3)



#inboxes = dplyr::distinct(inboxes)


users = data.frame(user = paste0("dataset//", unique(inboxes$to)))

sent = apply(users,1, function(x) {sum(grepl("sent",dir(x)))})
users = subset(users, sent != 0)

users$mailname = NA
for (i in 1:nrow(users)) {
  
  path = paste0(users$user[i],"/",dir(users$user[i])[1])
  file = dir(path)[1]
  name <- readLines(paste0(path,"/",file), warn = F)[3]
  name <- str_sub(name, 7, nchar(name)-10)
  users$mailname[i] <- name
  
}

users$user <- str_sub(users$user, 10)
inboxes <- merge(inboxes, by.x="to", users, by.y="user")
inboxes <- data.frame(from = inboxes$from, to = inboxes$mailname)

#inboxes$from <- as.character(inboxes$from)
#inboxes$to <- as.character(inboxes$to)

inboxes = inboxes %>% group_by(from,to) %>% summarise(weight=n())

# Only e-mails between inbox users
inboxes <- inboxes[inboxes$from %in% inboxes$to,]

# Remove emails to self
inboxes <- subset(inboxes, inboxes$from != inboxes$to)

library(igraph)
set.seed(1)
#g <- graph_from_edgelist(as.matrix(inboxes, directed = T)
g = graph_from_data_frame(inboxes, directed =  T)

# Task 3

## 1. Vertices of Graph
V(g)
## 2. Edges of Graph
E(g)
## 3. Adjacency Matrix of Graph
get.adjacency(g)


# Task 4
##1 Edge Density
## The proportion of present edges from all possible edges in the network.
igraph::edge_density(g)

##2 Transitivity
##  The proportion of reciprocated ties (for a directed network).
igraph::reciprocity(g)

##3 Transitivity
## global - ratio of triangles (direction disregarded) to connected triples.
## local - ratio of triangles to connected triples each vertex is part of.
transitivity(g, type="global")
transitivity(g, type="local")

##4 Diameter
#A network diameter is the longest geodesic distance 
#(length of the shortest path between two nodes)in the network. 
igraph::diameter(g, directed = F)
igraph::diameter(g, directed = T)
igraph::diameter(g, directed = F, weights = NA)

##5 Degree
igraph::degree(g, mode="all")
##6 Degree Distribution
igraph::degree_distribution(g)

##7 Centrality
igraph::centr_degree(g)

##8 Closeness
igraph::closeness(g, mode="all", weights = NA)

##9 Betweenens
igraph::betweenness(g,directed = T, weights = NA)

##10 Hub Score
igraph::hub_score(g, weights = NA)

##11 Cocitation
igraph::cocitation(g)


## Task 5
#a Central Nodes
igraph::centr_degree(g, mode="in", normalized = T)
#b 
igraph::diameter(g, directed = T)
#c
largest.cliques(g)
#d
igraph::ego(g)
#e power centrality
power_centrality(g, loops = F, exponent = 0.9)

#f
# coms <- spinglass.community(g)
# coms <- edge.betweenness.community(g)
coms <- walktrap.community(g)
#coms <- fastgreedy.community(g)

# Plot network
par(mar = c(0,0,2,0))
#comps = decompose(g)
#gr = graph_from_edgelist(as.matrix(comps[2]), directed = T)
plot(g, 
     vertex.label=users$mailname, 
     #layout = layout.fruchterman.reingold,
     layout = layout.circle,
     vertex.size = 16,
     edge.arrow.size = 0.2,
     edge.label = inboxes$weight,
     edge.label.color = "red",
     edge.color = inboxes$weight,
     #edge.width = 2*log2(inboxes$weight),
     edge.width = 0.2 * inboxes$weight,
)

plot(coms,g, 
     vertex.label=NA, 
     #layout = layout.fruchterman.reingold,
     layout = layout.fruchterman.reingold,
     vertex.size = 5,
     edge.arrow.size = 0.2,
     edge.label = inboxes$weight,
     edge.label.color = "red",
     edge.color = inboxes$weight,
     edge.width = 2*log2(inboxes$weight),
)


tkplot(g,  
       #layout = layout.fruchterman.reingold,
       layout = layout.fruchterman.reingold,
       vertex.size = 12,
       vertex.color= "yellow",
       edge.arrow.size = 0.2,
       edge.label = inboxes$weight,
       edge.label.color = "red",
       edge.color = inboxes$weight,
       edge.width = 2*log2(inboxes$weight),
)

