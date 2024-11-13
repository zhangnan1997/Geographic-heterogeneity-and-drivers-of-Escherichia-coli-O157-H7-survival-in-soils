library(Hmisc)
library(igraph)
#input data
genus <- read.csv("inputdata.csv",header = T,row.names = 1)

#filtered data, it depends on the inputdata
b=dim(genus)
genus <- genus[which(rowSums(genus) >= 0.005), ]    
genus1 <- genus
genus1[genus1>0] <- 1
genus <- genus[which(rowSums(genus1) >= round(b[2]*20/100)), ]    
#spearman correlation
genus_corr <- rcorr(t(genus), type = 'spearman')
# r>=0.6
r <- genus_corr$r
r[abs(r) < 0.6] <- 0
# p<0.05
p <- genus_corr$P
p <- p.adjust(p, method = 'BH')    
p[p>=0.05] <- -1
p[p<0.05 & p>=0] <- 1
p[p==-1] <- 0
#Data is retained according to the r and p values filtered above
z <- r * p
diag(z) <- 0    #Converts the value in the diagonal of the correlation matrix (which represents the autocorrelation) to 0
head(z)[1:6,1:6]
##Acquisition network
g <- graph.adjacency(z, weighted = TRUE, mode = 'undirected')
g <- simplify(g)
g <- delete.vertices(g, names(degree(g)[degree(g) == 0]))
E(g)$correlation <- E(g)$weight
E(g)$weight <- abs(E(g)$weight)
write.graph(g, 'network.graphml', format = 'graphml')

#set the size of random network
n=- #number of nodes
e=-  #number of edges

#generate 1000 random networks
for (i in 1:1000) {
  g <- erdos.renyi.game(n, e,'gnm')
  
  # Global toplogical features
  c <- cluster_walktrap(g)
  md <- modularity(g, membership(c), weights = NULL)
  cc <- transitivity(g, vids = NULL,
                     weights = NULL)
  spl <- average.path.length(g, directed=FALSE, unconnected=TRUE)
  gd  <- graph.density(g, loops=FALSE)
  ND<- diameter(g, directed = FALSE, unconnected = TRUE, weights = NULL)
  
  nd <- degree(g, v = V(g), mode="all")
  ad  <- mean(nd)
  
  global.topol <- data.frame(n,e,cc,spl,md,gd,ND,ad)}
write.csv(global.topol,file ="topol_random.network.csv" )

