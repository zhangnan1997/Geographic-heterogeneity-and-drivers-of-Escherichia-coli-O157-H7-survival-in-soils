library("ggplot2")
#input data
data=read.delim('inputdata.txt', row.names = 1, sep = '\t')
#R^2,slope, and p
linear_model <- lm(data$survivaltime ~ data$key_factors,data=data)
summary(linear_model)
r_squared <- summary(linear_model)$r.squared
slope <- coef(linear_model)[2] 
P=coef(summary(model))[2,c(4)]

#plot 
key_factors_plot=ggplot(data=data,aes(x=key_factors,y=survivaltime))+
  geom_point(data=data,aes(x=key_factors,y=survivaltime,fill=Group,color=Group),shape=21,size=5,alpha=0.65)+
  geom_smooth(method='lm')+ylab("Survival time")+
  scale_fill_manual(values = c("#B83945","#377483"))+
  scale_color_manual(values = c("#B83945","#377483"))+
  theme_bw() + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  theme(axis.text=element_text(size=14),axis.title.y = element_text(size=14),axis.title.x = element_text(size=14))+
  theme(legend.text = element_text(size=14))
key_factors_plot