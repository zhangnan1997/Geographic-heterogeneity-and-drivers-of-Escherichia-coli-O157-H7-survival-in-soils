require(ggplot2)
require(RColorBrewer)
library(sf)
library(maptools)
library(automap)
library(ggspatial)

#load china map
china_map = readShapePoly("bou2_4p.shp")
china_map1<-fortify(china_map)

#Regions defined for each Polygons
china_map1$long<-round(china_map1$long,2)
china_map1$lat<-round(china_map1$lat,2)
#Resolve the issue of Chinese characters appearing garbled when using R and readShapePoly to read .SHP provincial data
china_map@data$NAME<-iconv(china_map@data$NAME,"GBK") 

#choose of plot scale
NAME<-c("黑龙江省","吉林省","辽宁省","河北省","山东省","上海市","江苏省","安徽省","湖北省","江西省","浙江省","福建省","湖南省","广西壮族自治区","广东省","海南省")
df_map<-china_map[as.vector(china_map@data$NAME)%in%NAME,]
df_grid1<-spsample(df_map, n =20461, "regular")
gridded(df_grid1)<-T

#input data
data<-read.csv("survivaldata.csv",head=T) 
coordinates(data) =~ LONG+LAT
kriging_result = autoKrige(td~1,input_data=data,new_data=df_grid1,data_variogram=data)
prediction_spdf = kriging_result$krige_output
a<-coordinates(prediction_spdf)
dat<-as.data.frame(cbind(a,RS=prediction_spdf$var1.pred))

#output figure_1a
color<-c("#377483","#CFE7C4","#f16644","#B83945")[1:4]
china_shp <- "中国省级地图GS（2019）1719号.geojson"
nine <- "九段线GS（2019）1719号.geojson"
china <- sf::read_sf(china_shp)
nine_line <- sf::read_sf(nine)
ggplot() + 
  geom_sf(data = china,fill="white",size=0.5,color="black") + 
  geom_sf(data = nine_line)  + 
  coord_sf(crs = "+proj=laea +lat_0=40 +lon_0=104")+
  annotation_scale(location = "bl") +
  # 自动添加正确方向的指北针
  annotation_north_arrow(location = "tl", which_north = "false",
                         style = north_arrow_fancy_orienteering)+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  geom_point(data=dat,aes(x1,x2,color=RS),size=2,shape=15)+
  scale_color_gradientn(colours = color) +
  coord_sf()+
  theme_bw()+
  theme(panel.grid=element_blank())


