library(tigris)
library(tidyverse)
library(lubridate)
library(sf)
library(dplyr)
library(ggthemes)
library(gridExtra)

##########################################
##########################################
##                                      ##
##     Code for creating figures.       ##
## Run all of "Data.R" before starting  ##
##                                      ##
##########################################
##########################################


##Figure 2

ticks <- read_csv("ticks_COWY2017.csv")

co_counties <- counties(state = "CO", cb = TRUE, class = "sf") %>%
  mutate(county = NAME)

grouped_ticks <- ticks %>%
  filter(state == "CO") %>%
  filter(species == c("Dermacentor andersoni", "Dermacentor variabilis")) %>%
  group_by(county, species) %>%
  dplyr::summarize(freq = dplyr::n()) %>%
  ungroup()

tick_map <- full_join(co_counties, grouped_ticks, by = "county") %>%
  mutate(freq = ifelse(is.na(freq), 0, freq)) %>%
  na.omit(species)

tick_dv <- tick_map %>%
  filter(species == "Dermacentor variabilis")
f <- ggplot() + #ticks CO only
  geom_sf(data = co_counties, color = "lightgrey") +
  geom_sf(data = tick_dv, fill = "lightskyblue3") +
  scale_fill_viridis() +
  ggtitle("NAU", subtitle = "D. variabilis") +
  theme_map() +
  theme(legend.position="right")
f

tick_da <- tick_map %>%
  filter(species == "Dermacentor andersoni")
e<- ggplot() + #ticks CO only
  geom_sf(data = co_counties, color = "lightgrey") +
  geom_sf(data = tick_da, fill = "darkgreen") +
  scale_fill_viridis(discrete = TRUE, option = "E") +
  ggtitle("NAU", subtitle = "D. andersoni") +
  theme_map() +
  theme(legend.position="right")
e

#established distributions from literature 

dvariablis <- tibble(county = c("Boulder", "Conejos"), # lehane 2019
                     present = TRUE)

dvar_map <- full_join(co_counties, dvariablis, by = "county") %>%
  na.omit()

dandersoni <- tibble(county = c("Adams", "Archuleta", "Boulder",  #james 2006
                                "Chaffee", "Clear Creek", "Conejos",
                                "Custer", "Denver", "Douglas", "Eagle", "El Paso",
                                "Fremont", "Garfield", "Gilpin",
                                "Gunnison", "Jackson", "La Plata",
                                "Lake", "Larimer", "Mesa", "Montezuma",
                                "Montrose", "Ouray", "Park", "Pitkin",
                                "Rio Blanco", "Rio Grande", "Routt",
                                "Saguache", "San Miguel", "Summit", "Weld"),
                     present = TRUE) 


dand_map <- full_join(co_counties, dandersoni, by = "county") %>%
  na.omit()

b <- ggplot() + #dermacentor variablis, CO only
  geom_sf(data = co_counties, color = "lightgrey") +
  geom_sf(data = dvar_map, fill= "lightskyblue3") +
  ggtitle("Literature",
          subtitle = "D. variabilis") +
  scale_fill_viridis(discrete = TRUE, option = "E") +
  theme_map() +
  theme(legend.position="right")
b
a <- ggplot() + #dermacentor andersoni, CO only
  geom_sf(data = co_counties, color = "lightgrey") +
  geom_sf(data = dand_map, fill = "darkgreen") + 
  ggtitle("Literature",
          subtitle = "D. andersoni") +
  scale_fill_viridis(discrete = TRUE, option = "E") +
  theme_map() +
  theme(legend.position="right")

#duncan and little 

dl_da <- tibble(county = c("Douglas", "Jefferson"), 
                present = TRUE)

dl_da_map <- full_join(co_counties, dl_da, by = "county") %>%
  filter(present == TRUE)

dl_dv <- tibble(county = c("Yuma", "Broomfield", "Douglas",
                           "Larimer"), present = TRUE)

dl_dv_map  <- full_join(co_counties, dl_dv, by = "county") %>%
  filter(present == TRUE)

g <- ggplot() + 
  geom_sf(data = co_counties, color = "lightgrey") +
  geom_sf(data = dl_da_map, fill = "darkgreen") + 
  ggtitle("Veterinary Surveillance",
          subtitle = "D. andersoni") +
  scale_fill_viridis(discrete = TRUE, option = "E") +
  theme_map() +
  theme(legend.position="right")
g

h <- ggplot() + 
  geom_sf(data = co_counties, color = "lightgrey") +
  geom_sf(data = dl_dv_map, fill = "lightskyblue3") + 
  ggtitle("Veterinary Surveillance",
          subtitle = "D. variabilis") +
  scale_fill_viridis(discrete = TRUE, option = "E") +
  theme_map() +
  theme(legend.position="right")
h

#cdphe maps 

cdphe_da_map <- cdphe_map %>% filter(`Tick species` == "Dermacentor andersoni")
c <- ggplot() + 
  geom_sf(data = co_counties, color = "lightgrey") +
  geom_sf(data = cdphe_da_map, fill = "darkgreen") +
  scale_fill_viridis() +
  ggtitle("CDPHE",
          subtitle = "D. andersoni") +
  theme_map() +
  theme(legend.position="right")

cdphedv_map <- cdphe_map %>% filter(`Tick species` == "Dermacentor variabilis")
d <- ggplot() + 
  geom_sf(data = co_counties, color = "lightgrey") +
  geom_sf(data = cdphedv_map, fill = "lightskyblue3") +
  scale_fill_viridis() +
  ggtitle("CDPHE",
          subtitle = "D. variabilis") +
  theme_map() +
  theme(legend.position="right")

dv_ticks <- ticks %>%
  filter(species == "Dermacentor variabilis")
dv_ticks_map <- left_join(co_counties, dv_ticks, by = "county")
dv_cdphe <- cdphe %>%
  filter(`Tick species` == "Dermacentor variabilis")

dv_one <- full_join(dvariablis, dv_ticks, by = "county")
dv_two <- full_join(dv_one, dv_cdphe, by = "county")
dv_three <- full_join(dv_two, dl_dv, by = "county")

dv_all <- tibble(dv_three, pres = TRUE)

dv_all_map <- full_join(co_counties, dv_all, by = "county")
dv_all_map <- dv_all_map %>% filter(pres == TRUE)
four <- ggplot() +
  geom_sf(data = co_counties, color = "lightgrey") +
  geom_sf(data = dv_all_map, fill = "lightskyblue3") + 
  ggtitle("All Dermacentor variabilis records") +
  scale_fill_viridis(discrete = TRUE, option = "E") +
  theme_map() +
  theme(legend.position="right")
four

da_ticks <- ticks %>%
  filter(species == "Dermacentor andersoni")
da_ticks_map <- left_join(co_counties, da_ticks, by = "county")

da_cdphe <- cdphe %>%
  filter(`Tick species` == "Dermacentor andersoni")

da_one <- full_join(dandersoni, da_ticks, by = "county", "host")

da_two <- full_join(da_one, da_cdphe, by = "county", "host")


da_three <- full_join(da_two, dl_da, by = "county", "host")

da_all <- tibble(da_three, pres = TRUE)

da_all_map <- full_join(co_counties, da_all, by = "county")

da_all_map <- da_all_map %>% filter(pres == TRUE)

five <- ggplot() +
  geom_sf(data = co_counties, color = "lightgrey") +
  geom_sf(data = da_all_map, fill = "darkgreen") +
  scale_fill_viridis() +
  ggtitle("All Dermacentor andersoni records") +
  theme_map() +
  theme(legend.position="right")

five

grid.arrange(a, b, g, h, c, d, e, f, five, four, ncol = 2) #vertical

grid.arrange(a, g, c, e, five, b, h, d, f, four, ncol = 5) #horizantal


##Figure 3

all <- read_csv("combined.csv") %>%
  select(species, host, county, date) %>%
  mutate(date = mdy(date)) %>%
  mutate(host = str_to_lower(host)) %>%
  mutate(species = str_to_sentence(species)) %>%
  mutate(county = str_to_title(county)) %>%
  mutate(year = year(date)) 

all_dv <- all %>% filter(species == "Dermacentor variabilis") %>%
  mutate(year = year(date))

all_da <- all %>% filter(species == "Dermacentor andersoni")

all_da_map <- full_join(co_counties, all_da, by = "county") %>%
  mutate(host = str_to_title(host))
da_hosts <- ggplot() +
  geom_sf(data = co_counties, color = "lightgrey") +
  geom_sf(data = subset(all_da_map, !is.na(host)), fill = "darkgreen") +
  facet_wrap(~host, ncol = 2) +
  ggtitle("All Dermacentor andersoni Records") +
  theme_map() +
  theme(legend.position="right")

all_dv_map <- full_join(co_counties, all_dv, by = "county") %>%
  mutate(host = str_to_title(host))

dv_hosts <- ggplot(data = subset(all_dv_map, !is.na(host))) +
  geom_sf(data = co_counties, color = "lightgrey") +
  geom_sf(data = subset(all_dv_map, !is.na(host)), fill = "lightskyblue3", na.rm = TRUE) +
  facet_wrap(~host, ncol = 2) +
  ggtitle("All Dermacentor variabilis Records") +
  theme_map() +
  theme(legend.position="right")

grid.arrange(da_hosts, dv_hosts, ncol = 2)

##Figure 4

tick_table <- ticks %>% #bins for habitat type
  mutate(habitat = fct_recode(.f = habitat, `natural area` = "wooded trail system", 
                              `natural area` =  "barr lake state wildlife area",
                              `natural area` =  "grassland", 
                              `natural area` =   "yellow stone", 
                              `natural area` =  "medicine bow mountains", 
                              `natural area` =  "arkansas river",
                              `natural area` =  "curt gowdy state park", 
                              `natural area` =  "mount sanitas",
                              `natural area` =  "meadow", 
                              `natural area` =  "desert, brush", 
                              `natural area` =  "canyon",
                              `natural area` =  "washington park", 
                              `natural area` =  "forest", 
                              `natural area` =  "lake area",
                              `natural area` =  "lake shore", 
                              `natural area` =  "ponderosa/ aspen forest",
                              `natural area` =  "woods. ponderosa pine", 
                              `natural area` =  "wooded", 
                              `natural area` =  "wooded area", 
                              `natural area` =  "lodge pole, willows", 
                              `natural area` =  "lodgepole pine forest", 
                              `natural area` =  "pine brush",
                              `natural area` =  "pinyon juniper", 
                              `natural area` =  "pond edge",
                              `natural area` =  "sand dunes", 
                              `natural area` =  "riperian area with cottonwoods",
                              `natural area` =  "sage, grass, ponderosa", 
                              `natural area` =  "creek/riparian",
                              `natural area` =  "cheat grass, sagebrush",
                              `natural area` = "field",
                              `natural area` = "brush", 
                              `natural area` = "outdoors",
                              `natural area` = "grassy",
                              peridomestic = "yard", 
                              peridomestic ="garden", 
                              peridomestic = "long grass in back yard",
                              peridomestic = "scrub oak, garden", 
                              peridomestic = "suburban", 
                              peridomestic ="school",
                              peridomestic = "wooded area around playground",
                              peridomestic = "city",
                              unknown = "unknown",
                              unknown = "",
                              unknown = "NA",
                              unknown = "Missing")) %>%
  mutate(habitat = str_to_title(habitat)) %>%
  mutate(sex = str_to_title(sex)) %>%
  mutate(stage = str_to_title(stage)) %>%
  mutate(bitecrawling = str_to_title(bitecrawling))

label(tick_table$stage) <- "Lifestage"
label(tick_table$sex) <- "Sex"
label(tick_table$habitat) <- "Habitat"
label(tick_table$bitecrawling) <- "Biting or Crawling"

tick_table %>% #ticks by habitat
  filter(species == "Dermacentor andersoni") %>%
  ggplot() +
  geom_bar(aes(x = habitat, fill = habitat)) +
  scale_fill_manual(values= c("mediumpurple3", "mediumpurple3", "mediumpurple3")) +
  labs(x = "Habitat Type", y = "Frequency of Ticks") +
  ggtitle("Tick Frequency by Habitat", subtitle = "Dermacentor andersoni") +
  theme_few() +
  theme(legend.position = 'none')





