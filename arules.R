###############################################################################
#                                                                             #
#  Big Data                                                                   #
#  Project: Association rules                                                 #
#  Coded by Scarlett Swerdlow                                                 #
#  scarlettswerdlow@uchicago.edu                                              #
#  May 26, 2015                                                               #
#                                                                             #
###############################################################################

###############
#  CONSTANTS  #
###############

WD <- "~/Google Drive/Grad school/Courses/BUS41201 Big Data/project/Big Data Final Project/"
FN <- "data/OPPR_ALL_DTL_GNRL_09302014.csv"

#################
#  SOURCE CODE  #
#################

setwd(WD)
source("code/data.R")
source("code/arules_starter.R")

general_pmts <- loadData(WD, FN)

manu_network <- graphNetwork(general_pmts, "manu", "phys_id")
phys_network <- graphNetwork(general_pmts, "phys_id", "manu", s=.01, c=.9)

# Graph manufacturers network
manug <- manu_network$network
V(manug)$color <- "turquoise"
par(mar=c(0,0,0,0)+.01)
plot(manug, vertex.label=NA, vertex.size=3, edge.curved=F)

# Graph physician network
physg <- phys_network$network
V(physg)$color <- "pink"
par(mar=c(0,0,0,0)+.01)
plot(physg, vertex.label=NA, vertex.size=3, edge.curved=F)

# Graph neighborhood around manufacturer with most degrees and betweenness
graphNei(manug, 2, labels(manu_network$d[6]), T)
graphNei(manug, 2, labels(manu_network$b[6]), T)
