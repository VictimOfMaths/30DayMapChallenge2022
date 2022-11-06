rm(list=ls())

library(sf)
library(ragg)
library(extrafont)
library(tidyverse)
library(osmdata)
library(ggtext)
library(scatterpie)

#Read in data from OpenStreetMap
osmdata <- getbb("Great Britain") %>% 
  opq(timeout=25*100) %>% 
  add_osm_feature("name", "Supertram", value_exact=FALSE) %>% 
  osmdata_sf()

#Filter only the tram stops
Trams_sf <- osmdata$osm_points %>% 
  st_crop(xmin=-1.6, xmax=-1.31, ymin=53.3, ymax=53.46) %>% 
  filter(railway=="tram_stop") 

#Convert to a data frame with x and y coords instead of an sf object
Trams_df <- Trams_sf %>% 
  st_coordinates() %>% 
  as.data.frame() %>% 
  mutate(name=Trams_sf$name,
         name=if_else(substr(name, 1, 8)=="Carbrook", "Carbrook / IKEA", name)) %>% 
  #Remove duplicate stops (for aesthetic reasons)
  group_by(name) %>% 
  slice_head(n=1) %>% 
  #Round x and y coords (also for aesthetic reasons)
  mutate(X=round(X, 3), Y=round(Y, 3)) %>% 
  #Sort out colours
  #First colour
  mutate(Yellow=case_when(
    name %in% c("Middlewood", "Leppings Lane", "Hillsborough Park", 
                "Meadowhall Interchange") ~ 1,
    name %in% c("Hillsborough", "Bamforth Street", "Langsett / Primrose View",
                "Infirmary Road", "Shalesmoor", "Netherthorpe Road", 
                "University of Sheffield", "West Street", "City Hall", "Hyde Park",
                "Cricket Inn Road", "Nunnery Square", "Woodbourn Road", "Attercliffe",
                "Arena / Don Valley Stadium", "Valley Centertainment", "Carbrook / IKEA",
                "Meadowhall South / Tinsley") ~ 0.5,
    name %in% c("Cathedral", "Castle Square", "Fitzalan Square / Ponds Forge") ~ 0.25,
    TRUE ~ 0),
    Blue=case_when(
      name %in% c("Malin Bridge", "White Lane", "Birley Lane", "Birley Moor Road",
                  "Donetsk Way", "Moss Way", "Crystal Peaks", "Beighton / Drake House Lane",
                  "Waterthorpe", "Westfield", "Halfway") ~ 1,
      name %in% c("Hillsborough", "Bamforth Street", "Langsett / Primrose View",
                  "Infirmary Road", "Shalesmoor", "Netherthorpe Road", 
                  "University of Sheffield", "West Street", "City Hall", 
                  "Sheffield Station / Sheffield Hallam University", 
                  "Granville Road / The Sheffield College",
                  "Park Grange Croft", "Park Grange", "Arbourthorne Road", "Spring Lane",
                  "Manor Top / Elm Tree", "Holinsend", "Gleadless Townend") ~ 0.5,
      name %in% c("Cathedral", "Castle Square", "Fitzalan Square / Ponds Forge") ~ 0.25,
      TRUE ~ 0),
    Purple=case_when(
      name %in% c("Herdings / Leighton Road", "Herdings Park") ~ 1,
      name %in% c("Sheffield Station / Sheffield Hallam University", 
                  "Granville Road / The Sheffield College",
                  "Park Grange Croft", "Park Grange", "Arbourthorne Road", "Spring Lane",
                  "Manor Top / Elm Tree", "Holinsend", "Gleadless Townend") ~ 0.5,
      name %in% c("Cathedral", "Castle Square", "Fitzalan Square / Ponds Forge") ~ 0.25,
      TRUE ~ 0),
    Black=case_when(
      name %in% c("Rotherham Central", "Rotherham Parkgate") ~ 1,
      name %in% c("Hyde Park", "Cricket Inn Road", "Nunnery Square", "Woodbourn Road", 
                  "Attercliffe", "Arena / Don Valley Stadium", "Valley Centertainment", 
                  "Carbrook / IKEA", "Meadowhall South / Tinsley") ~ 0.5,
      name %in% c("Cathedral", "Castle Square", "Fitzalan Square / Ponds Forge") ~ 0.25,
      TRUE ~ 0),
    radius=0.0016)

agg_png("Day5_2022_Network.png", units="in", width=10, height=10, res=700, 
        background="#addd8e")
ggplot()+
  geom_scatterpie(data=Trams_df,  aes(x=X, y=Y, r=radius),
                  cols=c("Blue", "Yellow", "Purple", "Black"), colour=NA,
                  show.legend=FALSE)+
  scale_fill_manual(values=c("#00b4ed", "#f7f700", "#ac2c93", "Black"))+
  coord_equal()+
  theme_void()+
  theme(plot.background=element_rect(fill="#addd8e", colour=NA),
        text=element_text(family="Whirly Birdie"),
        plot.title=element_markdown(size=rel(15)),
        plot.caption=element_text(size=rel(2)))+
  labs(title="<span style='color:#00b4ed;'>S<span style='color:#f7f700;'>u<span style='color:#ac2c93;'>p<span style='color:Black;'>e<span style='color:#00b4ed;'>r<span style='color:#f7f700;'>t<span style='color:#ac2c93;'>r<span style='color:Black;'>a<span style='color:#00b4ed;'>m",
       caption="Data from OSM | Map by @VictimOfMaths")

dev.off()

