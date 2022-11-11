rm(list=ls())

library(raster)
library(tidyverse)
library(paletteer)
library(ragg)
library(extrafont)

#Import DEM data from USGS
image <- raster("C:/Users/cm1cra/Desktop/Aerialod-0.0.1-win64/map/YukonSmaller.tif") %>% 
  rasterToPoints(spatial=TRUE) %>% 
  data.frame()

agg_tiff("Day11_2022_Red.tiff", units="in", width=8, height=7, res=600)
ggplot(image, aes(x=x, y=y, fill=YukonSmaller))+
  geom_raster(show.legend=FALSE)+
  scale_fill_paletteer_c("pals::ocean.amp", direction=-1)+
  theme_void()+
  theme(text=element_text(family="Lora"), plot.title=element_text(size=rel(2.5)))+
  labs(title="Geography is awesome",
       caption="Elevation data from USGS  \nMap by @VictimOfMaths  \n")

dev.off()