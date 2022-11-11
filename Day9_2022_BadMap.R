rm(list=ls())

library(curl)
library(readxl)
library(readODS)
library(tidyverse)
library(sf)
library(paletteer)
library(ragg)
library(extrafont)

#Download land use data
temp <- tempfile()
source <- ("https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1113459/Live_Tables_-_Land_Use_Stock_2022.ods")
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

landdata <- read_ods(temp, sheet="P400b", range="A6:R328") %>% 
  select(`ONS code`, `Highways and road transport`) %>% 
  set_names("LADcode", "roadarea")
  
#Download inappropriate population data from NIMS
temp <- tempfile()
source <- ("https://api.coronavirus.data.gov.uk/v2/data?areaType=ltla&metric=VaccineRegisterPopulationByVaccinationDate&format=csv")
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
  
popdata  <- read.csv(temp) %>% 
  filter(date==max(date)) %>% 
  select(-c(date, areaType)) %>% 
  set_names("LADcode", "areaName", "pop")

data <- landdata %>% 
  merge(popdata) %>% 
  mutate(roadspercap=roadarea*10000/pop)

#Download wrong shapefile
temp <- tempfile()
temp2 <- tempfile()
source <- "https://opendata.arcgis.com/api/v3/datasets/4e50abbc857044afa2c8b81d0cff255e_0/downloads/data?format=shp&spatialRefId=27700&where=1%3D1"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

#The actual shapefile has a different name each time you download it, so need to fish the name out of the unzipped file
name <- list.files(temp2, pattern=".shp")
shapefile <- st_read(file.path(temp2, name)) %>% 
  rename("LADcode"="lad11cd")

#Put it in a stupid projection
map.data <- full_join(shapefile, data, by="LADcode") %>% 
  st_transform(crs = st_crs("EPSG:4087"))

#separate out the Isle of Wight
IoW <- map.data %>% 
  filter(areaName=="Isle of Wight") %>% 
  st_geometry()

#Rotate it 180degrees
IoW_c <- st_centroid(IoW)
rot = function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)   
IoW2 <- (IoW-IoW_c)*rot(pi)*1.2+IoW_c 

#Faff about
test <-  map.data %>% 
  filter(areaName=="Isle of Wight") %>% 
  st_set_geometry(NULL) %>% 
  st_set_geometry(IoW2) %>% 
  st_set_crs("EPSG:4087")

#Stick it back in place of the real IoW
map.data2 <- map.data %>% 
  filter(areaName!="Isle of Wight") %>% 
  rbind(test)

agg_png("Day9_2022_BadMap.png", units="in", width=9, height=6, res=700)
ggplot(map.data2, aes(geometry=geometry))+
  geom_sf(aes(fill=roadspercap, colour=roadarea))+
  scale_fill_paletteer_c("grDevices::rainbow", na.value="transparent", trans="log2",
                         name="Road area (m2)\nper capita\n(log scale)")+
  scale_colour_continuous(na.value="Yellow")+
  guides(colour=FALSE)+
  theme_void()+
  theme(text=element_text(family="Times New Roman"),
        plot.title=element_text(size=rel(3)), plot.background=element_rect(fill="Green"))+
  labs(title="A bad map",
       caption="Land use data from Dept for Levelling Up, Housing and Communities\nPopulation data from NIMS\nMap by @VictimOfMaths")

dev.off()



  