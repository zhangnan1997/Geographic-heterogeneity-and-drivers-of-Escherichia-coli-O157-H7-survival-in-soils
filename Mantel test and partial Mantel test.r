library(vegan)
#Climate factors
climate=read.csv("climate.csv",row.names = 1)
climate<- vegdist(climate, method = 'euclidean')

#Physicochemical properties
physi=read.csv("soil_properties.csv",row.names = 1)
physi<- vegdist(physi, method = 'euclidean')

#survival data
survival=read.csv("survival_data.csv",row.names = 1)
survival<- vegdist(survival, method = 'euclidean')

#manteltest for climate and survival
mantel <-mantel(climate, survival, method="spearman", permutations=999)
mantel
#manteltest for Physicochemical properties and survival
mantel <-mantel(physi, survival, method="spearman", permutations=999)
mantel


#Partial mantel controlling for Physicochemical properties
mantel.partial<-mantel.partial(survival,climate ,physi ,method="spearman", permutations=999)
mantel.partial

#Partial mantel controlling for Climate factors
mantel.partial<-mantel.partial(survival,physi ,climate ,method="spearman", permutations=999)
mantel.partial
