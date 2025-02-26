library(vegan)
#input data
data=read.csv("inputdata.csv",row.names = 1,header=T)
data <- t(data)
S<-specnumber(data)
#function
alpha <- function(x, tree = NULL, base = exp(1)) {
  est <- estimateR(x)
  Richness <- est[1, ]
  Shannon <- diversity(x, index = 'shannon', base = base)
  evenness<-Shannon/log(S)
  result <- data.frame(Richness, Shannon, evenness)
  if (!is.null(tree)) {
    PD_whole_tree <- pd(x, tree, include.root = FALSE)[1]
    names(PD_whole_tree) <- 'PD_whole_tree'
    result <- cbind(result, PD_whole_tree)
  }
  result
}
#calculation
alpha_all <- alpha(data, base = 2)

