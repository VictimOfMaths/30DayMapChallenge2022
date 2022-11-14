rm(list=ls())

library(tidyverse)
library(sf)
library(ragg)
library(showtext)
library(paletteer)
library(rnaturalearth)

rawdata <- read.csv("C:/Data.Projects/Stuff/Data/fleet-daily-csvs-100-v2-2020/2020-01-01.csv")

rawdata <-
  list.files(path="C:/Data.Projects/Stuff/Data/fleet-daily-csvs-100-v2-2020", pattern = "*.csv") %>% 
  map_df(~read_csv(paste0("Data/fleet-daily-csvs-100-v2-2020/", .)))

data <- rawdata %>% 
  group_by(cell_ll_lat, cell_ll_lon) %>% 
  summarise(hours=sum(fishing_hours), .groups="drop")

agg_png("Outputs/Day14_2022_Hexagons.png", units="in", width=8, height=8, res=600, background="Black")
data %>% 
  filter(cell_ll_lon>-17 & cell_ll_lon<8 & cell_ll_lat>45 & cell_ll_lat<67) %>% 
  ggplot(aes(x=cell_ll_lon, y=cell_ll_lat))+
  geom_hex(bins=60, show.legend=FALSE)+
  scale_fill_paletteer_c("viridis::magma", limits=c(0,NA))+
  theme_void()+
  theme(plot.background=element_rect(fill="Black", colour="Black"),
        text=element_text(colour="white", family="Merriweather"),
        plot.title=element_text(size=rel(3)))+
  coord_equal()+
  labs(title="  Ou sont les poissons?", 
       subtitle="     Fishing intensity across 2020 around the UK",
       caption="Fishing data from Globalfishingwatch.org | Map by @VictimOfMaths   ")
dev.off()
