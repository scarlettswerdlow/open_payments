###############################################################################
#                                                                             #
#  Big Data                                                                   #
#  Project: Association rules helper functions                                #
#  Coded by Scarlett Swerdlow                                                 #
#  scarlettswerdlow@uchicago.edu                                              #
#  May 26, 2015                                                               #
#                                                                             #
###############################################################################

################################################################################
# Function to create network                                                   #
# Args:                                                                        #
#   - df (data frame): Payments data frame                                     #
#   - node (str): Name of column of network node                               #
#   - edge (str): Name of column of network edge                               #
#   - s (num): Support threshold; default is 0.0001                            #
#   - c (num): Confidence threshold; default is 0.1                            #
#   - ml (num): Maximum length ceiling: default is 2                           #
# Return:                                                                      #
#   - List that includes network object, top six nodes on degrees, and top     #
#     six nodes on betweenness                                                 #
################################################################################

graphNetwork <- function(df, node, edge, s=.0001, c=.1, ml=2) {
  
  # Transform data frame into list with physician ID as key and manu as value
  cat(paste("Creating list with ", edge, " as key and ", node, " as value\n", sep=""))
  glist <- split(x=as.character(df[node][[1]]), f=as.character(df[edge][[1]]))
  glist <- lapply(glist, unique)
  
  # Make rules
  cat("Creating association rules\n")
  gtrans <- as(glist, "transactions")
  grules <- apriori(gtrans, parameter=list(support=s, confidence=c, maxlen=ml))
  
  # Create matrix of connected nodes
  cat("Making pairs for network\n")
  pairs <- labels(grules)
  pairs <- gsub("\\{|\\}","", pairs)
  pairs <- strsplit(pairs," => ")
  pairs <- do.call(rbind, pairs)
  pairs <- pairs[pairs[,1]!="",]
  
  # Create network and calculate network statistics
  cat("Creating network\n")
  network <- graph.edgelist(pairs)
  network <- as.undirected(network)
  d <- tail(sort(degree(network)))
  b <- tail(sort(betweenness(network)))
  
  rv <- list(network=network, degrees=d, betweenness=b)
  return(rv)
}

################################################################################
# Function to graph neighborhood of network                                    #
# Args:                                                                        #
#   - graph (iGraph graph object): Entity network graph                        #
#   - l (num): Distance of neighborhood; l=2 goes one edge out from center     #
#   - node (str): Name of entity that is the center of the neighborhood        #
#   - manu (bool): If true, indicates that center is a manufacturer; if false, #
#                  center is a physician                                       #
# Return:                                                                      #
#   - Plots neighborhood                                                       #
################################################################################

graphNei <- function(graph, l, node, manu) {
  nei <- graph.neighborhood(graph, l, V(graph)[node])[[1]]
  V(nei)$size <- ifelse(V(nei)$name == node, 6, 3)
  V(nei)$color <- ifelse(V(nei)$name == node, "gold", ifelse(manu, "turquoise", "pink"))
  V(nei)$label.color <- "black"
  V(nei)$label.cex <- ifelse(V(nei)$name == node, 1, .01)
  par(mar=c(0,0,0,0)+.01)
  plot(nei, edge.curved=F)
}


