rm(list=ls())

library(tidyverse)
library(sf)
library(curl)
library(ragg)
library(paletteer)

url <- "https://opendata.arcgis.com/api/v3/datasets/eb05bd0be3b449459b9ad0692a8fc203_0/downloads/data?format=shp&spatialRefId=27700&where=1%3D1"

temp <- tempfile()
temp2 <- tempfile()
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

#The actual shapefile has a different name each time you download it, so need to fish the name out of the unzipped file
name <- list.files(temp2, pattern=".shp")
shapefile <- st_read(file.path(temp2, name))

agg_png("Day4_2022_Green.png", units="in", width=8, height=6.5, res=600, background="cornsilk")
shapefile %>% 
  st_crop(xmin=410000, xmax=450000, ymin=376888, ymax=406888) %>% 
  filter(CATEGORY=="Woodland" & IFT_IOA %in% c("Assumed woodland", "Broadleaved", "Conifer",
                                               "Coppice", "Low density", 
                                               "Mixed mainly broadleaved",
                                               "Mixed mainly conifer", "Shrub",
                                               "Young trees")) %>% 
  ggplot(aes(geometry=geometry, fill=IFT_IOA))+
  geom_sf(colour=NA, show.legend=FALSE)+
  scale_fill_paletteer_d("palettetown::sceptile")+
  theme_void()+
  theme(plot.background=element_rect(colour="cornsilk", fill="cornsilk"),
        text=element_text(colour="ForestGreen", family="High Alpine"),
        plot.title=element_text(size=rel(3.5)),
        plot.caption=element_text(size=rel(1.2)))+
  labs(title="The woods around Sheffield",
       caption="Data from Forestry Commission | Font from @sarahbellmaps | Map by @VictimOfMaths")

dev.off()

