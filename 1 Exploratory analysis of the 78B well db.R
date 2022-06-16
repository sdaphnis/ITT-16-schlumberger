# Exploratory analysis of the Well 78B dataset for ROP project

data<-read.csv("78B.32.10.sec.data.27200701.csv",header=T)

names(data)
# Filter one
data2<-data[data$Rate.Of.Penetration..ft_per_hr.<2000&data$Rate.Of.Penetration..ft_per_hr.>0,]
# Filter two
data3<-data2[data2$Torque.Motor..Units...fps.units.>0&data2$AWOB..klbs.>0,]
  
# Depth strata
unique(data3$Hole.Diameter..in.)

par(mfrow=c(2,2))
for(i in sort(unique(data3$Hole.Diameter..in.)[-2],decreasing = T)){
  x<-data3[data3$Hole.Diameter..in.==i,]$Torque.Motor..Units...fps.units.
  y<-data3[data3$Hole.Diameter..in.==i,]$AWOB..klbs.
  plot(x,y,main=paste("hole diameter ~ ",i))
  abline(lm(y~x),col="blue") 
}

# Creating a reduced database with only useful columns

# Selecting columns
data_r<-data[,c("Rate.Of.Penetration..ft_per_hr.","Rotary.RPM..RPM.","Weight.on.Bit..klbs.",
                "HH.MM.SS","YYYY.MM.DD","Hole.Diameter..in.","Hole.Depth..feet.","DAS.MSE..ksi.",
                "Torque.Motor..Units...fps.units.")]
# Filters
data_r<-data_r[data_r$Rate.Of.Penetration..ft_per_hr.<700&data_r$Rate.Of.Penetration..ft_per_hr.>0,]
data_r<-data_r[data_r$Rotary.RPM..RPM.>0,]

# Dimension of the fultered database
dim(data_r)

# Some days are more useful than others
table(data_r$YYYY.MM.DD)

# We can also separate by hole diameter (4 or 5 categories)
hd<-sort(unique(data_r$Hole.Diameter..in),decreasing=T)

# Ploting the relation between WOB and ROP (should be ROP~C*WOB^2 )
  par(mfrow=c(2,3))
  for(i in hd){
  data_hd<-data_r[data_r$Hole.Diameter..in.==i,]
  plot(data_hd$Weight.on.Bit..klbs.,data_hd$Rate.Of.Penetration..ft_per_hr.,
       main=paste("hole diameter",i),xlim=c(0,100),ylim=c(0,600))
}

# Now RPM vs ROP, should be linear
par(mfrow=c(2,3))
for(i in hd){
  data_hd<-data_r[data_r$Hole.Diameter..in.==i,]
  plot(data_hd$Rotary.RPM..RPM.,data_hd$Rate.Of.Penetration..ft_per_hr.,
       main=paste("hole diameter",i),xlim=c(0,100),ylim=c(0,100))
}

# The 3-D Scatter should support the previous statements
library(plotly)

fig <- plot_ly(data_hd, x = ~Rotary.RPM..RPM., y = ~Weight.on.Bit..klbs., z = ~Rate.Of.Penetration..ft_per_hr., colors = c('#BF382A', '#0C4B8E'))
fig <- fig %>% add_markers(size=0.1)
fig <- fig %>% layout(scene = list(xaxis = list(title = 'RPM'),
                                   yaxis = list(title = 'WOB'),
                                   zaxis = list(title = 'ROP')))

fig

# Let's filter one of the busier days for visualisation

data4<-data2[data2$YYYY.MM.DD=="2021/07/06",]

fig <- plot_ly(data4, x = ~Rotary.RPM..RPM., y = ~Weight.on.Bit..klbs., z = ~Rate.Of.Penetration..ft_per_hr., colors = c('#BF382A', '#0C4B8E'))
fig <- fig %>% add_markers(size=0.1)
fig <- fig %>% layout(scene = list(xaxis = list(title = 'RPM'),
                                  yaxis = list(title = 'WOB'),
                                   zaxis = list(title = 'ROP')))

fig
