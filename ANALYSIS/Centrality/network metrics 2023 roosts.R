##load network package
library(sna)

##night time metrics
net_night<-read.csv("~/Library/CloudStorage/OneDrive-AustralianNationalUniversity/People/Lisa Fontana/network_movement_night_2023_all_roosts.csv", header=T, row.names=1)

##subset it to the sites of interest here, if you want (this would change the metrics as it would only include their relationships to the other sites). The below is on all sites altogether

SITES_NIGHT<-rownames(net_night)
data_output_night<-as.data.frame(SITES_NIGHT)
data_output_night$WEIGHTED_DEGREE<-degree(as.matrix(net_night), gmode="graph", cmode="freeman", rescale=FALSE)
data_output_night$UNWEIGHTED_DEGREE<-degree(as.matrix(net_night), gmode="graph", cmode="freeman", rescale=FALSE, ignore.eval=TRUE)
data_output_night$BETWEENESS<-betweenness(as.matrix(net_night), gmode="graph", rescale=FALSE)

write.csv(data_output_night,"~/Library/CloudStorage/OneDrive-AustralianNationalUniversity/People/Lisa Fontana/night_network_metrics.csv")

##day time metrics
net_day<-read.csv("~/Library/CloudStorage/OneDrive-AustralianNationalUniversity/People/Lisa Fontana/network_movement_day_2023_all_roosts.csv", header=T, row.names=1)

##again, subset it to the sites of interest here, if you want (this would change the metrics as it would only include their relationships to the other sites). The below is on all sites altogether

SITES_DAY<-rownames(net_day)
data_output_day<-as.data.frame(SITES_DAY)
data_output_day$WEIGHTED_DEGREE<-degree(as.matrix(net_day), gmode="graph", cmode="freeman", rescale=FALSE)
data_output_day$UNWEIGHTED_DEGREE<-degree(as.matrix(net_day), gmode="graph", cmode="freeman", rescale=FALSE, ignore.eval=TRUE)
data_output_day$BETWEENESS<-betweenness(as.matrix(net_day), gmode="graph", rescale=FALSE)

write.csv(data_output_day,"~/Library/CloudStorage/OneDrive-AustralianNationalUniversity/People/Lisa Fontana/night_network_metrics.csv")



