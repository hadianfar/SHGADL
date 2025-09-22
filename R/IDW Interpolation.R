# IDW Interpolation
library(gstat)
station_data=read.csv("station_data.csv")
xy <- station_data[,c(3,4)]
station_datapm <- SpatialPointsDataFrame(coords = xy, data = station_data,
                               proj4string = CRS('+proj=utm +zone=40 +ellps=WGS84 +datum=WGS84 +units=m +no_defs '))

plot(station_datapm)
plot(shape1$geometry,add=TRUE,lwd=2,border = "red")
extent_shape1 <- extent(shape1)
# set the grid cell size in meter
xh <- 500
grid_extent_shape1 <- expand.grid(
  x = seq(
    from = round(extent_shape1@xmin),
    to = round(extent_shape1@xmax),
    by = xh
  ),
  y = seq(
    from = round(extent_shape1@ymin),
    to = round(extent_shape1@ymax),
    by = xh
  )
)

head(grid_extent_shape1, 10)

plot(grid_extent_shape1, asp = 1)

coordinates(grid_extent_shape1) <- ~ x + y
proj4string(grid_extent_shape1) <- proj4string(station_datapm)
new_crs <- "+proj=utm +zone=40 +datum=WGS84 +units=m +no_defs"
grid_extent_shape1_transformed <- spTransform(grid_extent_shape1, CRS(new_crs))
proj4string(grid_extent_shape1) <- CRS(new_crs)
head(grid_extent_shape1)
gridded(grid_extent_shape1) <- TRUE
class(grid_extent_shape1)
plot(grid_extent_shape1,
     col = "grey",
     cex.main = 0.9
)

plot(shape1$geometry, add = TRUE, border = "red")
plot(station_datapm, add = TRUE, pch = 19, cex = 0.5, col = "blue")
######Consequently, we specify our IDW model as follows:
pred.PM <- data.frame(matrix(NA, ncol = 1460, nrow=nrow(grid_extent_shape1@ coords)))
pb <- txtProgressBar(min = 0, max = 1460, char = "=", style = 3)

  for (i in 1:1460) {
    
    station_datapm1=subset(station_datapm,t==i)
    station_datapm1 <- station_datapm1[!is.na(station_datapm1$PM2.5), ]    
  neighbors <- length(station_datapm1)
beta <- 2

idw_temp <- gstat(
  formula = station_datapm1$PM2.5 ~ 1, # intercept-only model
  data = station_datapm1,
  nmax = neighbors,
  set = list(idp = beta)
)
grid_extent_shape1_temp <- predict(
  object = idw_temp,
  newdata = grid_extent_shape1
)
pred.PM[i]=grid_extent_shape1_temp$var1.pred
setTxtProgressBar(pb, i)
  }
close(pb)
pred.PM=cbind(grid_extent_shape1@coords,pred.PM)
##########################################################Figure. 2 
data$Date <- as.Date(data$Date, format = "%m/%d/%Y",origin = "2016-10-24")
test_data_long_tidyr <- pivot_longer(data, cols = c("PM2.5m","CVD"), names_to='variable',values_to="value")
ggplot(data = test_data_long_tidyr,
       aes(x = Date, y = value, colour = variable)) +
  geom_line() +
  scale_x_date(date_labels = "%Y %b ", breaks = "6 months", labels = scales::date_format("%B")) +
  scale_y_continuous("number of CVD hospitalizations", limits = c(0,80),
                     sec.axis = sec_axis(~.*1.2, name = expression("PM"[2.5]*(mu*g/m^3)), 
                                         breaks = c(0,25,50,75,100))) +
  labs(x = "Date", y = "Value") +
  geom_vline(aes(xintercept = as.numeric(data2$Date[1146]), linetype = "COVID-19"), lwd = 1.01) +
  geom_vline(aes(xintercept = as.numeric(data2$Date[685]), linetype = "Screening program"), lwd = 1.01) +
  scale_linetype_manual(name = "Event", values = c("COVID-19" = "dotted", "Screening program" = "dashed")) +
  theme(
    axis.text.x = element_text(size = 12), 
    axis.text.y = element_text(size = 12),  
    axis.title.x = element_text(size = 14),  
    axis.title.y = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14)
  )+ theme(axis.title.y.right = element_text(angle = 90))+
  theme(axis.text=element_text(color="black"))
