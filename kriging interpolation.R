require(automap)

#input data
data<-read.csv("survivaldata.csv",head=T) 
coordinates(data) =~ LONG+LAT
kriging_result = autoKrige(td~1,input_data=data,new_data=df_grid1,data_variogram=data)
prediction_spdf = kriging_result$krige_output
a<-coordinates(prediction_spdf)
dat<-as.data.frame(cbind(a,RS=prediction_spdf$var1.pred))

