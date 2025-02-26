library(ggplot2) 
library(vegan)
#input data
data = read.csv("inputdata.csv", header = T, row.names = 1)
data <- data.frame(t(data))

#Bray–Curtis or Euclidean distances
distance_bc<- vegdist(data, method = 'bray') 
distance_eu<- vegdist(data, method = 'euclidean') 

#PcoA calculation based on the Bray–Curtis distances
#Using distance_eu for PcoA calculation of Euclidean distance
group = read.csv("group.csv", header = T, row.names = 1)
pcoa <- cmdscale(distance_bc, k = (nrow(data) - 1), eig = TRUE)
pcoa$eig
point <- data.frame(pcoa$point)
species <- wascores(pcoa$points[,1:2], data)
pcoa_eig <- (pcoa$eig)[1:2] / sum(pcoa$eig)
sample_site <- data.frame({pcoa$point})[1:2]
sample_site$names <- rownames(sample_site)
names(sample_site)[1:2] <- c('PCoA1', 'PCoA2')
sample_site <- merge(sample_site, group, by = 'names', all.x = TRUE)
#PcoA plot
ggplot(sample_site, aes(x=PCoA1, y=PCoA2,fill=Group,color=Group))+
  xlab("PCoA 1")+ylab("PCoA 2")+geom_point(size=7,alpha=0.8,shape=21)+
  theme_bw() + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  scale_fill_manual(values = c("#BB3945","#377483"))+
  scale_color_manual(values = c("#BB3945","#377483"))+
  theme(axis.text=element_text(size=14),axis.title.y = element_text(size=14),axis.title.x = element_text(size=14))+theme(legend.text = element_text(size=14))+
  theme(legend.title = element_text(size=14),legend.position= 'none')
#anosim and adonis. Take the Bray–Curtis distances as an example
dis <- as.dist(distance_bc)	
anosim_result_dis <- anosim(dis, group$Group,permutations = 999)
summary(anosim_result_dis)
adonis_result_dis <- adonis2(dis~Group, group, permutations = 999)
adonis_result_dis


