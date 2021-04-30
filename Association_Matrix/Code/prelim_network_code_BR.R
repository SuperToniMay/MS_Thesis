# Read in data ----
# read in most recent census csv file
census <- read.csv("census_withLatLong_20200511.csv", header = T)
dim(census) # returns the dimensions of census
head(census) # print the first six rows

# build unique group label by pasting together the map label and the group number
census$unique_grp <- paste(census$map, "_", census$groupid, sep = "")


# Build social contact network in one year ----
year_in <- 1997 #this should be changed when you want to specify other years
y1997 <- subset(census, year == year_in)
dim(y1997) # check dimensions to be sure this actually got cut down

# build adjacency matrix
# need to get number of times animals co-occur in groups for each dyad
# 1.  extract all animal IDs from 1997
nodes <- levels(factor(y1997$fulltag_clean))[-1]
# 2. Build an empty matrix to store the edge strengths in
y1997_adj <- co_occur <- matrix(NA, nrow = length(nodes), ncol = length(nodes))
# loop over all dyads
for(i in 1:(length(nodes) - 1)){ # loop over all rows (aka individuals)
  for(j in (i + 1): length(nodes)){ # loop over all OTHER animals that haven't yet been in a dyad with i
    # subset down to just the observations of the two animals in the dyad
    animal_1 <- subset(y1997, as.character(fulltag_clean) == as.character(nodes[i]))
    animal_2 <- subset(y1997, as.character(fulltag_clean) == as.character(nodes[j]))
    # figure out how many times animal_1 and animal_2 are in the same group
    co_occur[i,j] <- co_occur[j,i] <- length(which(animal_1$unique_grp %in% animal_2$unique_grp))
    #print(paste(i, "_", j, " cooccur = ", co_occur, sep = ""))
    # take the number of co-occurrences and divide by total obs for animal_1
   if(dim(animal_1)[1] >= 1 & dim(animal_2)[1] >= 1){
     
     y1997_adj[i, j] <- co_occur[i, j] / dim(animal_1)[1]
    # take the number of co-occurrences and divide by total obs of animal_2
    y1997_adj[j, i] <- co_occur[j, i] / dim(animal_2)[1]
   }
  }
}
which(is.na(co_occur), arr.ind = T)
head(y1997_adj)


# build and visualize graph ----
# 1) load igraph package (may need to first run install.packages("igraph"))
# install.packages("igraph")
library(igraph) # same as require(igraph)

# specify that adjacency matrix built above should be converted to graph.
# the graph should be undirected (mode = "undirected"), but with weighted edges (weighted = T)
y1997_graph <- graph.adjacency(adjmatrix = y1997_adj, 
                               mode = "undirected", 
                               weighted = T)
# specify that the name of each node is the corresponding element in the node vector
# (otherwise R will assign each node a number label by default; setting the names gets the
# animal's actual label affixed to the node)
# NOTE: V(1997_graph) gets me to the VERTICES of the y1997_graph object
V(y1997_graph)$name <- as.character(nodes)
# plot out the graph
plot(y1997_graph, 
     vertex.color = "grey80")


# read in BRPOP to get sex, age, etc.----
brpop <- read.csv("BRPOP_v02.csv", header = T)
head(brpop)

# add sex attribute onto nodes:
# 1. add storage space onto V(y1997_graph) for sex field. 
V(y1997_graph)$sex <- rep(NA, length(V(y1997_graph)))
V(y1997_graph)$birth_yr <- rep(NA, length(V(y1997_graph)))
# loop over all vertices in graph
for(i in 1:length(V(y1997_graph))){ 
  # cut brpop down to just the animal with the name of node i
  k <- subset(brpop, name == V(y1997_graph)$name[i])
  V(y1997_graph)$sex[i] <- as.character(k$sex[1])
  V(y1997_graph)$sex[i] <- ifelse(is.na(V(y1997_graph)$sex[i]) == T, 
                                  "missing", as.character(V(y1997_graph)$sex[i]))
  V(y1997_graph)$birth_yr[i] <- k$yearb[1]

}

V(y1997_graph)$current_age <- year_in - V(y1997_graph)$birth_yr
V(y1997_graph)$current_age <- ifelse(is.na(V(y1997_graph)$current_age) == T, 
                                     0, V(y1997_graph)$current_age)

V(y1997_graph)$node_color <- ifelse(V(y1997_graph)$sex == "F", "green", "grey80")
#V(y1997_graph)$node_color[85] <- "grey80"
#V(y1997_graph)$current_age[85] <- 1
# plot out the graph
edg.transparent.color <- rgb(1, 1, 1, alpha = .3)
layout_in <- layout_with_fr(y1997_graph)
#"Need finite xlim values" error
plot(y1997_graph, 
     #layout = layout_in,
     vertex.color = V(y1997_graph)$node_color,
     vertex.size = V(y1997_graph)$current_age * 2,
     edge.color = "grey90")

# sum columns in adj
diag(y1997_adj) <- rep(0, length(nodes))
strengths <- apply(y1997_adj, 1, sum)
dim(y1997_graph)

plot(strengths ~ V(y1997_graph)$current_age,
     col = as.numeric(as.factor(V(y1997_graph)$sex)),
     pch = 16)
