library("randomForest")
library("A3")
library("rfPermute")
#input_data that contaning survival data, climate factors and physicochemical properties/functional genes
otu <- read.delim('inputdata.txt', row.names = 1)

#Overall model random forest and significance
set.seed(123)
otu_forest.pval <- a3(td~., data = otu, model.fn = randomForest, p.acc = 0.001, model.args = list(importance = TRUE, ntree = 5000))
otu_forest.pval

#The importance score of the factors for survival data
otu_rfP <- rfPermute(td~., data = otu, importance = TRUE, ntree = 5000, nrep = 1000, num.cores = 4)
importance_otu.scale <- data.frame(importance(otu_rfP, scale = TRUE), check.names = FALSE)
importance_otu.scale

#The significance of important factors
importance_otu.scale.pval <- (otu_rfP$pval)[ , , 2]
importance_otu.scale.pval
importance_otu.scale$OTU_name <- rownames(importance_otu.scale)
importance_otu.scale$OTU_name <- factor(importance_otu.scale$OTU_name, levels = importance_otu.scale$OTU_name)

for (OTU in rownames(importance_otu.scale)) {
  importance_otu.scale[OTU,'%IncMSE.pval'] <- importance_otu.scale.pval[OTU,'%IncMSE']
  if (importance_otu.scale[OTU,'%IncMSE.pval'] >= 0.05) importance_otu.scale[OTU,'%IncMSE.sig'] <- ''
  else if (importance_otu.scale[OTU,'%IncMSE.pval'] >= 0.01 & importance_otu.scale[OTU,'%IncMSE.pval'] < 0.05) importance_otu.scale[OTU,'%IncMSE.sig'] <- '*'
  else if (importance_otu.scale[OTU,'%IncMSE.pval'] >= 0.001 & importance_otu.scale[OTU,'%IncMSE.pval'] < 0.01) importance_otu.scale[OTU,'%IncMSE.sig'] <- '**'
  else if (importance_otu.scale[OTU,'%IncMSE.pval'] < 0.001) importance_otu.scale[OTU,'%IncMSE.sig'] <- '***'
}
importance_otu.scale
write.csv(importance_otu.scale, "importance.csv")

