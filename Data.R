library(tidyverse)
library(ggthemes)
library(table1)
library(lubridate)
library(readxl)

##############################################################
##############################################################
##                                                         ##
## Read in all data and clean to get matching column names ##
##                                                         ##
##############################################################
##############################################################


#NAU data

nau_da <- read_csv("ticks_COWY2017.csv") %>%
  filter(state == "CO") %>%
  slice(-15) %>%
  mutate(source = "NAU") %>%
  filter(species == "Dermacentor andersoni")

nau_dv <- read_csv("ticks_COWY2017.csv") %>%
  filter(state == "CO") %>%
  slice(-15) %>%
  mutate(source = "NAU") %>%
  filter(species == "Dermacentor variabilis")

nau_dadv <- full_join(nau_da, nau_dv, by = c("species", "county",
                                             "host", "source"))


#CDPHE data
cdphe <- read_excel("cdphe_data.xlsx") %>%
  mutate(county = `County Acquired`) %>%
  mutate(host = `Host found on`) %>%
  mutate(date = `Date Acquired`)  %>%
  mutate(species = `Tick species`) %>%
  mutate(source = "CDPHE")


#Duncan et al. 2020 data 
duncan_da <- read_excel("duncan_etal_data.xlsx") %>%
  slice(1:29) %>%
  mutate(county = County) %>%
  mutate(host = Host)  %>%
  mutate(date = `Date collected`) %>%
  mutate(species = Tick) %>%
  mutate(source = "Duncan et al.") %>%
  filter(species == "Dermacentor andersoni")

duncan_dv <- read_excel("duncan_etal_data.xlsx") %>%
  slice(1:29) %>%
  mutate(county = County) %>%
  mutate(host = Host)  %>%
  mutate(date = `Date collected`) %>%
  mutate(species = Tick) %>%
  mutate(source = "Duncan et al.") %>%
  filter(species == "Dermacentor variabilis")

duncan_dadv <- full_join(duncan_da, duncan_dv, by = c("species", "county",
                                                      "host", "source"))
