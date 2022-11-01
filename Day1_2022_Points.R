library(tidyverse)
library(sf)
library(googlesheets4)
library(countrycode)
library(rnaturalearth)
library(paletteer)
library(ragg)

rawdata <- read_sheet("https://docs.google.com/spreadsheets/d/1UUXinsHP4iDUwprM_KKEng4DBK2uC7Y1NdbnD1lmkSU/edit#gid=0")

nuldata <- rawdata %>% 
  filter(`Grand Final Points`=="0") %>% 
  group_by(Country) %>% 
  tally() %>% 
  ungroup() %>% 
  mutate(ADM0_A3=countryname(Country, destination="iso3c"))

participants <- rawdata %>% 
  group_by(Country) %>% 
  slice_head(n=1) %>% 
  ungroup() %>% 
  select(Country) %>% 
  mutate(ADM0_A3=countryname(Country, destination="iso3c"))

map <- ne_download(scale=10, type="countries", returnclass="sf") %>% 
  right_join(participants) %>% 
  left_join(nuldata) %>% 
  mutate(n=replace_na(n, 0))
  
agg_png("Day1_2022_Points.png", units="in", width=7, height=8, res=600, background="Grey15")
ggplot(map, aes(geometry=geometry, fill=as.factor(n)))+
  geom_sf(colour="Grey70", size=0.1)+
  scale_fill_paletteer_d("soilpalettes::crait", name="")+
  xlim(c(-10, 50))+
  ylim(c(30, 71))+
  theme_void()+
  theme(text=element_text(family="Lato", colour="Grey90"),
        plot.title=element_text(size=rel(3.8), family="Lobster", colour="#C91105"),
        plot.title.position="plot", plot.caption.position="plot",
        legend.position="top", plot.background=element_rect(fill="Grey15", colour="Grey15"),
        )+
  labs(title="The kings of 'nul points'",
       subtitle="Number of times each nation* has been awarded 0 points in the Eurovision Song Contest\n",
       caption="*Yugoslavia also got nul points in 1946. Australia (not shown) have never scored 0.\n\nData from ESC in context | Map by @VictimOfMaths\n")

dev.off()

