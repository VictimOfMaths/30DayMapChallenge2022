rm(list=ls())

library(curl)
library(tidyverse)
library(sf)
library(ragg)
library(extrafont)
library(rnaturalearth)
library(paletteer)
library(forcats)
library(lwgeom)

font <- "Gadugi"

theme_custom <- function() {
  theme_classic() %+replace%
    theme(plot.title.position="plot", plot.caption.position="plot",
          strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
          plot.title=element_text(face="bold", size=rel(1.5), hjust=0,
                                  margin=margin(0,0,5.5,0)),
          text=element_text(family=font),
          plot.subtitle=element_text(colour="Grey40", hjust=0, vjust=1),
          plot.caption=element_text(colour="Grey40", hjust=1, vjust=1, size=rel(0.8)),
          axis.text=element_text(colour="Grey40"),
          axis.title=element_text(colour="Grey20"),
          legend.text=element_text(colour="Grey40"),
          legend.title=element_text(colour="Grey20"))
}

#Download osm data with all amenities IN THE WORLD (download is massive)
url <- "https://data.osmdata.xyz/amenity_EPSG4326.zip"

temp <- tempfile()
temp2 <- tempfile()
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)
name <- list.files(temp2, pattern=".gpkg")

#Extract all pubs
pubs <- st_read(file.path(temp2, name), layer="amenity_EPSG4326_point",
                query="SELECT * FROM \"amenity_EPSG4326_point\" WHERE amenity IN ('pub')") 

#Extract all bars
bars <- st_read(file.path(temp2, name), layer="amenity_EPSG4326_point",
                query="SELECT * FROM \"amenity_EPSG4326_point\" WHERE amenity IN ('bar')") 

#Extract all nightclubs
clubs <- st_read(file.path(temp2, name), layer="amenity_EPSG4326_point",
                 query="SELECT * FROM \"amenity_EPSG4326_point\" WHERE amenity IN ('nightclub')") 

#Extract all biergartens (suggested by https://wiki.openstreetmap.org/wiki/Tag:amenity%3Dbar)
biergartens <- st_read(file.path(temp2, name), layer="amenity_EPSG4326_point",
                       query="SELECT * FROM \"amenity_EPSG4326_point\" WHERE amenity IN ('biergarten')") 

#Stick it all together
fulldata <- pubs %>% select(osm_id, name, amenity, `_ogr_geometry_`) %>% 
  bind_rows(bars %>% select(osm_id, name, amenity, `_ogr_geometry_`)) %>% 
  bind_rows(clubs %>% select(osm_id, name, amenity, `_ogr_geometry_`)) %>% 
  bind_rows(biergartens %>% select(osm_id, name, amenity, `_ogr_geometry_`))

#Bring in map
map <- ne_countries(scale = "large", returnclass = "sf") %>% 
  select(name, sovereignt, sov_a3, pop_est, iso_a3, continent, geometry)

#Place all outlets within a country
countries <- st_join(fulldata, map, join=st_within)

#Get outlets per capita
density <- countries %>% 
  filter(!is.na(name.y)) %>% 
  group_by(name.y) %>% 
  summarise(n=n(), pop_est=unique(pop_est), .groups="drop") %>% 
  mutate(pop_est=as.numeric(pop_est), density=n*100000/pop_est) %>% 
  st_drop_geometry() %>% 
  set_names("name", "outlets", "pop", "density")

densitymap <- left_join(map, density)

agg_png("Day28_2022_ComfortZone.png", units="in", width=9, height=5.5, res=600)
densitymap %>% 
  mutate(density=if_else(is.na(density), 0, density)) %>% 
  filter(density<100 & continent!="Antarctica") %>% 
  st_transform_proj(crs = "ESRI:54030") %>% 
  ggplot(aes(geometry=geometry, fill=density))+
  geom_sf(colour=NA)+
  scale_fill_paletteer_c("pals::ocean.haline", direction=-1, name="Pubs/bars per 100,000 people")+
  theme_custom()+
  theme(legend.position="top", axis.line=element_blank(), 
        axis.ticks=element_blank(), axis.text=element_blank(),
        plot.title=element_text(size=rel(3)))+
  guides(fill = guide_colorbar(title.position = 'top', title.hjust = .5,
                               barwidth = unit(20, 'lines'), barheight = unit(.5, 'lines')))+
  labs(title="Is it far to the nearest bar?",
       subtitle="Per capita density of locations tagged as pubs/bars/nightclubs/biergarten in OpenStreetMap by country",
       caption="Data from OpenStreetMap | Map by @VictimOfMaths")

dev.off()
