library(vegan)      
otu <- read.delim('all_8300.txt', row.names = 1, header = T,sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
otu=read.csv("fungal.csv",row.names = 1,header=T)
otu <- t(otu)
S<-specnumber(otu)
tree <- read.tree('tree.nwk')
#????????
alpha_all <- alpha(otu, base = 2)
alpha_all <- alpha(otu, tree, base = 2)

#????
write.csv(alpha_all, 'fungal_alpha.csv', quote = FALSE)
#?????Լ??㺯??
alpha <- function(x, tree = NULL, base = exp(1)) {
  est <- estimateR(x)
  Richness <- est[1, ]
  Chao1 <- est[2, ]
  ACE <- est[4, ]
  Shannon <- diversity(x, index = 'shannon', base = base)
  Simpson <- diversity(x, index = 'simpson')    
  Pielou <- Shannon / log(Richness, base)
  goods_coverage <- 1 - rowSums(x == 1) / rowSums(x)
  evenness<-Shannon/log(S)
  result <- data.frame(Richness, Shannon, Simpson, Pielou, Chao1, ACE, goods_coverage,evenness)
  if (!is.null(tree)) {
    PD_whole_tree <- pd(x, tree, include.root = FALSE)[1]
    names(PD_whole_tree) <- 'PD_whole_tree'
    result <- cbind(result, PD_whole_tree)
  }
  result
}

