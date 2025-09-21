####################################### maps
##### CVD map
shape2 <- read_sf("C:/Users/hadi/Dropbox/air polution and CVD/intership student project/Export_Output_4.shp")
tm_shape(shape2)+tm_polygons("CVD_100k",title = "CVDH per 100,000 population",breaks = c(1000,1200,1400,1800,2200,5200),n=5,palette = c("greenyellow","yellow","orange","red","red3")) +
  tm_layout(aes.palette = list(seq = "RdYlGn"),legend.position = c("left", "bottom"),frame=F,legend.show=TRUE,legend.title.size = 1.4,legend.text.size = 0.9)+
  tm_text("number_zon",col="black", size = 1/1.1)+tmap_options(check.and.fix = TRUE)+
  tm_compass(type = 'arrow',position = c("left", "center"))

###RR
colors <- colorRampPalette(c("green", "red"))
ggplot(data = shape2) +
  geom_sf(aes(fill = RR)) +
  geom_text(aes(x = x, y = y, label = number_zon), angle = 45) +
  scale_fill_gradientn(colors = colors(100), name = "Relative Risk") +
  annotation_north_arrow(location = "tr", pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"))
