# Estimation of the MSE and MSE<ß constraint

# MSE Calculation
# We start by doing it on the centroids
centroids$MSE<-pi*unique(data_chunk$Hole.Diameter..in./2)^2*(centroids$Y+120*pi*centroids$TORque_est*centroids$X/centroids$ROP_est)

# Then we assign the value to each square on the grid
grid$MSE<-centroids$MSE

plot(grid)

# From the plot we see: MSE~Torque (problem maybe with the units!) check literature!

# checking if it makes sense by using the original data, not the interpolated one
data_chunk$MSE<-pi*unique(data_chunk$Hole.Diameter..in./2)^2*(data_chunk$Weight.on.Bit..klbs.+120*pi*
                                                               data_chunk$Rotary.RPM..RPM.*data_chunk$Torque.Motor..Units...fps.units./data_chunk$Rate.Of.Penetration..ft_per_hr.)

# We plot the calculated MSE vs the MSE already in the DB
plot(data_chunk$MSE,data_chunk$DAS.MSE..ksi.)

# hahaha wtf
hist(data_chunk$DAS.MSE..ksi.)
