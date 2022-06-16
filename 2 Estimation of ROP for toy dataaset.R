
# Hole Depth should be a numeric variable
data_r$Hole.Depth..feet.<-as.numeric(as.character(data_r$Hole.Depth..feet.))

# 25 minutes ~ 10 feet

data_chunk<-data_r[data_r$Hole.Depth..feet.>352&data_r$Hole.Depth..feet.<363&!is.na(data_r$Hole.Depth..feet.),]

# Weigth on bit is zero at some registers
hist(data_chunk$Weight.on.Bit..klbs.)

# May be better to smooth it with a moving averages
# Moving averages function
filtro<-function(x,d){
  f<-c()
  for(i in (d+1):(length(x)-d)){
    f[i-d]<-mean(x[(i-d:i+d)])
  }
  f<-c(rep(0,d),f,rep(0,d))
  return(f)
}


plot(data_chunk$Weight.on.Bit..klbs.,type="l")
# Doing the moving averages
ma <- filtro(data_chunk$Weight.on.Bit..klbs.,2)
# show the moving average on the same plot
points(ma,col="blue",type="l")

# We use this new variable 

data_chunk$ma<-ma

# For which we have to cut the first 2 and last 2 registers
data_chunk<-data_chunk[-c(1,2,151,152),]

# Plot RPM vs SWOB vs ROP
fig <- plot_ly(data = data_chunk, x = ~ Rotary.RPM..RPM., y = ~ma, color = ~Rate.Of.Penetration..ft_per_hr.)
fig

# RPM vs SWOB vs Torque
fig2 <- plot_ly(data = data_chunk, x = ~ Rotary.RPM..RPM., y = ~ma, color = ~Torque.Motor..Units...fps.units.)
fig2

model<-lm(log(Rate.Of.Penetration..ft_per_hr.)~log(Rotary.RPM..RPM.)+log(ma),data=data_chunk)

summary(model)

#*****Inverse distance weighting for estimation ROP
library(leaflet)
library(sf)


# creation of the grid **************

# make a boundary box using min and max values for RPM and SWOB

summary(data_chunk$Rotary.RPM..RPM.)
summary(data_chunk$ma)

# to include all the points we may have to make the box bigger

x1<-min(data_chunk$Rotary.RPM..RPM.)
x2<-max(data_chunk$Rotary.RPM..RPM.)
y1<-min(data_chunk$ma)
y2<-max(data_chunk$ma)

# creation of the boundary box
boundarybox <- matrix(c(x1,y1,
                        x1,y2,
                        x2,y2,
                        x2,y1,
                        x1,y1), byrow = TRUE, ncol = 2) %>%
  list() %>% 
  st_polygon() #%>% 
  #st_sfc(., crs = 4326)

# generate grid of 30x30 tiles 
grid<- st_make_grid(boundarybox, n = c(30, 30), 
                    #crs = 4326, 
                    what = 'polygons') %>%
  st_sf('geometry' = ., data.frame('ID' = 1:length(.)))

# extract the centroids to associate the estimation 
centroids<-as.data.frame(st_coordinates(st_centroid(grid)))


# set the power of the distance weighting
alpha=1

est<-c()
est2<-c()
for(i in 1:nrow(centroids)){
  c<-0
  w<-0
  x<-0
  for(h in 1:nrow(data_chunk)){
    d_ijh<-sqrt(sum((c(data_chunk$Rotary.RPM..RPM.[h], data_chunk$ma[h])- c(centroids$X[i], centroids$Y[i]))^2))
    # d_ijh is the distance between the centroid of each box in the grid and the observed points
    if(d_ijh<3){
      c<-c+1
      w<-w+d_ijh^alpha
      x<-x+d_ijh^alpha*data_chunk$Rate.Of.Penetration..ft_per_hr.[h]
      y<-x+d_ijh^alpha*data_chunk$Torque.Motor..Units...fps.units.[h]
    }
  }
  if(c>0){
    est[i]<-x/w 
    est2[i]<-y/w
  }
}
# we paste this data to the grid shape
grid$ROP_est<-est
grid$TORque_est<-est2

centroids$ROP_est<-est
centroids$TORque_est<-est2

# Pretty plotting
# color palette for the estimation
# qpal2 <- colorQuantile("RdYlGn", grid$estimation, n = 9)
# map
# leaflet(grid)   %>%  
#   addPolygons(fillColor = ~qpal2(estimation), color = "", smoothFactor = 0.5,
#               opacity = 0.5, fillOpacity = 0.5,label=grid$estimation) %>%
#   addCircleMarkers(data_chunk,lng=data_chunk$Rotary.RPM..RPM.,
#                    lat=data_chunk$ma,label=data_chunk$Rate.Of.Penetration..ft_per_hr.) 
# how to remove the crs from the plot? standarise the domain 'coordinates'?

grid$ID<-NULL

plot(grid)







