#######################################################
################## GEO SPATIAL CLUSTER ANALYTICS ######
#######################################################
install.packages("RJSONIO")
setInternet2(TRUE)

#########get longitude
geocodeLongAdddress <- function(address) {
  require(RJSONIO)
  url <- "http://maps.google.com/maps/api/geocode/json?address="
  url <- URLencode(paste(url, address, "&sensor=false", sep = ""))
  x <- fromJSON(url, simplify = FALSE)
  if (x$status == "OK") {
    out <- c(x$results[[1]]$geometry$location$lng)
  } else {
    out <- NA
  }
  Sys.sleep(0.2)  # API only allows 5 requests per second
  out
}
#######get latitude
geocodeLatAdddress <- function(address) {
  require(RJSONIO)
  url <- "http://maps.google.com/maps/api/geocode/json?address="
  url <- URLencode(paste(url, address, "&sensor=false", sep = ""))
  x <- fromJSON(url, simplify = FALSE)
  if (x$status == "OK") {
    out <- c(x$results[[1]]$geometry$location$lat)
  } else {
    out <- NA
  }
  Sys.sleep(0.2)  # API only allows 5 requests per second
  out
}
########## Read CSV File
Inp<-read.csv(file="D:/PersonalDev/R/Sample1.csv",header = TRUE,sep = ",",na.strings=c("","NA"))
Inp$Address

######### Map the Geo Co-ordintes
for (i in 1:nrow(Inp)) {
  Inp$long[i]<-geocodeLongAdddress(Inp$Address[i])
  Inp$lat[i]<-geocodeLatAdddress(Inp$Address[i])
}
Inp
### Exculde the NA records from the data frame from Processing ##################
Inp.Cleaned<-na.omit(Inp)
NROW(Inp.Cleaned)
write.csv(Inp.Cleaned, file = "D:/PersonalDev/R/OutputData.csv")
structure(Inp.Cleaned)
Inp.Cleaned$long

III<-read.csv(file="D:/PersonalDev/R/OutputData.csv",header = TRUE,sep = ",")
III

####### Our Data is Ready Now #################
######Begin Geo Spatial Clustering Analytics based on Plan purchased ############
##### This will be helpful in pitching the plan to the customer calls based on their locale ######
install.packages("ggplot2")
install.packages("ggmap")
library(ggplot2)
library(ggmap)

# getting the map
mapgilbert <- get_map(location = c(lon = mean(Inp.Cleaned$long), lat = mean(Inp.Cleaned$lat)), zoom = 4,
                      maptype = "terrain", scale = 2)
##maptype satellite or terrain

# plotting the map with some points on it
ggmap(mapgilbert) +
  geom_point(data = Inp.Cleaned, aes(colour = factor(Inp.Cleaned$Plan), x =Inp.Cleaned$long, y = Inp.Cleaned$lat
                                     , alpha = 0.8), size = 5, shape = 21) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE)

######Begin Geo Spatial Clustring Analytics based on AdvertisingChannel for the purchases ############
###### This will help in strategising the advertising approach based on locale #######################
# getting the map
mapgilbert <- get_map(location = c(lon = mean(Inp.Cleaned$long), lat = mean(Inp.Cleaned$lat)), zoom = 4,
                      maptype = "terrain", scale = 2)
##maptype satellite or terrain

# plotting the map with some points on it
ggmap(mapgilbert) +
  geom_point(data = Inp.Cleaned, aes(colour = factor(Inp.Cleaned$AdvertisingChannel), x =Inp.Cleaned$long, y = Inp.Cleaned$lat
                                     , alpha = 0.8), size = 5, shape = 21) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE)


######################################## Focus on Melbourne and Suburb calls ###
library(geosphere)
##Melbourne Coordinates 
##Latitude:-37.814251
##Longitude:144.963169
for (i in 1:nrow(Inp.Cleaned)) {
  if (distm (c(144.963169, -37.814251), c(Inp.Cleaned$long[i],Inp.Cleaned$lat[i]), fun = distHaversine)<25000){
    Inp.Cleaned$MelIndicator[i] <-1
  }
  else {
    Inp.Cleaned$MelIndicator[i] <-0
  }
}
Inp.Cleaned$MelIndicator
#Filter Melborune Data
MelData <-subset(Inp.Cleaned, MelIndicator==1)
MelData


##Gives Distance in Meters 
## Study 25Km or 25000 Meters Suburbs of Melbourne


######Begin Melborune Geo Spatial Clustering Analytics based on Plan purchased ############
# getting the map
mapgilbert <- get_map(location = c(lon = 144.963169, lat =-37.814251), zoom = 11,
                      maptype = "terrain", scale = 2)
##maptype satellite or terrain

# plotting the map with some points on it
ggmap(mapgilbert) +
  geom_point(data = MelData, aes(colour = factor(MelData$Plan), x =MelData$long, y = MelData$lat
                                     , alpha = 0.8), size = 5, shape = 20) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE)

######Begin Melborune Geo Spatial Clustring Analytics based on AdvertisingChannel for the purchases ############
# getting the map
mapgilbert <- get_map(location = c(lon = mean(MelData$long), lat = mean(MelData$lat)), zoom = 11,
                      maptype = "terrain", scale = 2)
##maptype satellite or terrain

# plotting the map with some points on it
ggmap(mapgilbert) +
  geom_point(data = MelData, aes(colour = factor(MelData$AdvertisingChannel), x =MelData$long, y = MelData$lat
                                     , alpha = 0.8), size = 5, shape = 20) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE)



#############STUDY ADELAIDE 
##Longitude of Adelaide: 138.5999594
##Latitude of Adelaide: -34.9286212
for (i in 1:nrow(Inp.Cleaned)) {
  if (distm (c(138.5999594, -34.9286212), c(Inp.Cleaned$long[i],Inp.Cleaned$lat[i]), fun = distHaversine)<25000){
    Inp.Cleaned$AdlIndicator[i] <-1
  }
  else {
    Inp.Cleaned$AdlIndicator[i] <-0
  }
}
Inp.Cleaned$AdlIndicator
#Filter Adelaide Data
AdlData <-subset(Inp.Cleaned, AdlIndicator==1)
AdlData

######Begin Adelaide Geo Spatial Clustering Analytics based on Plan purchased ############
# getting the map
mapgilbert <- get_map(location = c(lon = 138.5999594, lat =-34.9286212), zoom = 11,
                      maptype = "terrain", scale = 2)
##maptype satellite or terrain

# plotting the map with some points on it
ggmap(mapgilbert) +
  geom_point(data = AdlData, aes(colour = factor(AdlData$Plan), x =AdlData$long, y = AdlData$lat
                                 , alpha = 0.8), size = 5, shape = 20) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE)

######Begin Melborune Geo Spatial Clustring Analytics based on AdvertisingChannel for the purchases ############
# getting the map
mapgilbert <- get_map(location = c(lon = 138.5999594, lat =-34.9286212), zoom = 11,
                      maptype = "terrain", scale = 2)
##maptype satellite or terrain

# plotting the map with some points on it
ggmap(mapgilbert) +
  geom_point(data = AdlData, aes(colour = factor(AdlData$AdvertisingChannel), x =AdlData$long, y = AdlData$lat
                                 , alpha = 0.8), size = 5, shape = 20) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE)





#######################################################################################################
################ Heat map Clustering based on proximity when there is no singnificant data classifiers.

install.packages("sp")
install.packages("rgdal")
install.packages("geosphere")
library(sp)
library(rgdal)
library(geosphere)

xy <- SpatialPointsDataFrame(
  matrix(c(Inp.Cleaned$long,Inp.Cleaned$lat), ncol=2), data.frame(ID=seq(1:length(Inp.Cleaned$long))),
  proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
# use the distm function to generate a geodesic distance matrix in meters
mdist <- distm(xy)
# cluster all points using a hierarchical clustering approach
hc <- hclust(as.dist(mdist), method="complete")
# define the distance threshold, in this case 20000 m or 20 KMS
d=20000
# define clusters based on a tree "height" cutoff "d" and add them to the SpDataFrame
xy$clust <- cutree(hc, h=d)
##Add the clustering vector back to the dataframe
Inp.Cleaned$Cluster<-xy$clust

##### Spatial Classifiaction based on on Clusters
install.packages("cluster")
install.packages("flexclust")
library(cluster)
library(flexclust)
plot(Inp.Cleaned$long, Inp.Cleaned$lat, col=Inp.Cleaned$Cluster, main="VividWireless Spatial Clustering", cex=0.5, pch=16, xlab="long", ylab="lat")
plot(Algerie,add=TRUE)








mapgilbert <- get_map(location = c(lon = mean(Inp.Cleaned$long), lat = mean(Inp.Cleaned$lat)), zoom = 4,
                      maptype = "terrain", scale = 2)
##maptype satellite or terrain

# plotting the map with some points on it
ggmap(mapgilbert) +
  geom_point(data = Inp.Cleaned, aes(colour = factor(Inp.Cleaned$Plan), x =Inp.Cleaned$long, y = Inp.Cleaned$lat
                                     ), size = 5, shape = 21) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE)