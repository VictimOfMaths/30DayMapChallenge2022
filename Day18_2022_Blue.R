rm(list=ls())

library(sf)
library(tidyverse)
library(ragg)
library(extrafont)
library(paletteer)
library(terra)
library(rasterVis)

data <- rast("C:/Users/cm1cra/Desktop/Aerialod-0.0.1-win64/map/SmallBathy.tif")

crs(data) <- "+proj=utm +zone=48 +datum=WGS84"


agg_png("Day18_2022_Blue.png", units="in", width=8, height=6,
        res=600, background="midnightblue")
gplot(data[[1]])+
  geom_tile(aes(fill=value, colour=value), show.legend=FALSE)+
  scale_fill_paletteer_c("pals::ocean.ice", na.value="White")+
  scale_colour_paletteer_c("pals::ocean.ice", na.value="White")+
  theme_void()+
  theme(text=element_text(family="High Alpine", colour="cornsilk"),
        plot.title=element_text(size=rel(4)),
        plot.caption=element_text(family="Lato"),
        plot.background = element_rect(fill="midnightblue", colour="midnightblue"))+
  coord_equal()+
  labs(title="  Under the sea",
       caption="Bathymetry data from Global Fishing Watch  \nMap by @VictimOfMaths  ")

dev.off()

