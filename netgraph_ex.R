
### Examples Using Network Modeling and SNA for Clinician/Nurse Texting in R ####

library(ggplot2)
library(tidyverse)
library(dplyr)
#import phase for text file extract from Teradata sample 
library(readr)
library(igraph)
library(networkD3)

# test with String names of the roles
#x <- as.matrix.data.frame(select(New_Text_Extract, MHB_User_Role_Desc))
#y <- as.matrix.data.frame(select(New_Text_Extract, MHB_User_Role_Desc_1))

#group_ed
xff <- tally(group_by(New_Text_Extract, MHB_User_Role_Desc, MHB_User_Role_Desc_1))
xf <- as.matrix.data.frame(select(xff, MHB_User_Role_Desc_1)) #removes added grouping - Now matric of SEnt -> Receiver
xff <- (filter(xff, n > 100))
xv <- as.matrix.data.frame(xff)
xn <- as.numeric(xv[,3])

set.seed(1410)


edges_x <- as.matrix(xv[, 1:2])
eddx <- c(edges_x[,1], edges_x[,2])

### Data frame used for the graph
relations <- data.frame(from = c(edges_x[,1]) ,
                        
                        to = c(edges_x[,2]))
                        
###Make GRaph!
g1 <- graph_from_data_frame(relations, directed = TRUE )  # graphs reads directly from dataframe FROM -> to

#Vertice Modifications
V(g1)$number <- 1:vcount(g1) #changes label to nunber of node/vert occurence
V(g1)$degree <- degree(g1, v = V(g1), mode = c("all"), loops = TRUE, normalized = FALSE) #calc number of edges /vert
V(g1)$betweenness <- betweenness(g1, v = V(g1), directed = TRUE, weights = NULL,
            nobigint = TRUE, normalized = FALSE)
V(g1)$color <- "green"
V(g1)[ number < 4 ]$color <- "red"

#Edge Modifications
E(g1)$width <- (log(xn)) # divide by 1000 to temporarily reduce the scale 
E(g1)$color <-  "grey"
#E(g1)[ width < 0.01 ]$width <- 0.5
#E(g1)$weight <- 0.3

##### Cluster densities
wc <- cluster_walktrap(g1, steps = 5, modularity = TRUE)
members <- membership(wc) 

### Ploting the Graph ###
tkplot(g1, layout = layout_with_lgl,
     #vertex.label=V(g1)$number ,
     edge.width=E(g1)$width ,
     edge.arrow.size = 0.05 ,
     edge.color = E(g1)$color ,
     edge.curved = 0.2 ,
     vertex.color = V(g1)$color 
     )

### make a force graph wiht D3
net <- igraph_to_networkD3(g1, group = members)
forceNetwork(Links = net$links, Nodes = net$nodes,
             Source = 'source',
             Target = 'target', NodeID = 'name', Group = 'group',
             zoom = TRUE, linkDistance = 200)
           

##################################################################
########## example of isolating node and edges by name ###########
inc.edges <- incident(g1,  V(g1)[name =="Administration"], mode="out")
  ecol <- rep("gray80", ecount(g1))

  ecol[inc.edges] <- "orange"

  vcol <- rep("grey40", vcount(g1))

  vcol[V(g1)$name=="Administration"] <- "gold"

tkplot(g1, vertex.color=vcol, 
       edge.color=ecol, 
       vertex.size = 5, 
       edge.arrow.size = 0.05,
       edge.curved = 0.2)
#################################################################

# Test to color all outbound text messeges for multi vertex
inc.edges <- incident_edges(g1,  V(g1)[1:4], mode="in")
  ecol <- rep("gray80", ecount(g1))

  ecol[inc.edges$Administration] <- "orange"
  ecol[inc.edges$`Behavioral Health RN`] <- "green"

  vcol <- rep("grey40", vcount(g1))

  vcol[V(g1)] <- "gold"
  
l = layout.graphopt(g1) #setting layout as variable saves the graph coords!! Very useful for lloking at change over time or other comparisons
ll <- layout_with_fr(g1)
tkplot(g1, layout = ll,
       vertex.color=vcol, 
       edge.color=ecol, 
       vertex.size = 5, 
       edge.arrow.size = 0.05,
       edge.curved = 0.2)

######################################################
### Run mutiple plot configuration inside of a matrix
l = layout.graphopt(g1) #setting layouts to save
ll <- layout_with_fr(g1)

par(mfrow=c(2,2), mar=c(0,0,0,0)) #sets up matrix
plot(g1, layout=layout_with_fr, vertex.label = NA, vertex.size = 5, 
     edge.arrow.size = 0.05,
     edge.curved = 0.2, main = "test")
plot(g1, layout=layout.circle, vertex.label = NA, vertex.size = 5, 
     edge.arrow.size = 0.05,
     edge.curved = 0.2)
plot(g1, layout=l, vertex.label = NA, vertex.size = 5, 
     edge.arrow.size = 0.05,
     edge.curved = 0.2)
plot(g1, layout=ll, vertex.label = NA, vertex.size = 5, 
     edge.arrow.size = 0.05,
     edge.curved = 0.2)

########################################################

layouts <- grep("^layout_", ls("package:igraph"), value=TRUE)[-1] 

# Remove layouts that do not apply to our graph.

layouts <- layouts[!grepl("bipartite|merge|norm|sugiyama|tree", layouts)]

  par(mfrow=c(3,3), mar=c(1,1,1,1))
  
  for (layout in layouts) {
    
    print(layout)
    
    l <- do.call(layout, list(g1)) 
    
    plot(g1, edge.arrow.mode=0, vertex.label = NA, layout=l, main=layout) }

###########################################################

ceb <- cluster_edge_betweenness(g1) 

dendPlot(ceb, mode="dendrogram")
#plot(ceb, g1, vertex.label = NA, vertex.size = 4, edge.arrow.size = 0.05, edge.color = "grey")
 #### ends(g1, es=E(g1), names=T)[,1]
#edge.start <- ends(net, es=E(net), names=F)[,1]

#edge.col <- V(net)$color[edge.start]

#notes
#You can set the "weight" parameter which 
#increases the attraction forces among nodes connected by heavier edges.

plot(net, edge.color=edge.col, edge.curved=.1)  



