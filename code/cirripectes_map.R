library(tidyverse)
library(readxl)
library(ggmap)
library(maps)
library(mapdata)
library(mapproj)
library(raster)
library(rgeos)
library(grid)
library(maptools)
library(mapview)
library(mSpatial)
library(sf)
library(here)

##### gbif citation info ######
# When using this dataset please use the following citation:
#   GBIF.org (25 October 2019) GBIF Occurrence Download https://doi.org/10.15468/dl.ndswit
# 
# Download Information:
#   DOI: https://doi.org/10.15468/dl.ndswit (may take some hours before being active)
# Creation Date: 19:33:33 25 October 2019
# Records included: 1963 records from 4 published datasets
# Compressed data size: 182.4 kB
# Download format: simple tab-separated values (TSV)
# Filter used:
#   InstitutionCode: usnm or bpbm or cas or saiab
# TaxonKey: Cirripectes Swainson, 1839

##### geocode occurrence records from GBIF with no points #####
# occurrence <- read_tsv("occurrence/gbif-occurrence.tab") %>%
#   filter(is.na(decimalLatitude) | is.na(decimalLongitude)) %>%
#   bind_cols(geocode(.$locality)) %>%
#   bind_rows(
#     read_tsv("occurrence/gbif-occurrence.tab") %>%
#       drop_na(decimalLatitude,decimalLongitude)
#   ) %>%
#   mutate(
#     decimalLatitude=ifelse(is.na(decimalLatitude),lat,decimalLatitude),
#     decimalLongitude=ifelse(is.na(decimalLongitude),lon,decimalLongitude)
#   ) %>%
#   select(-lat,-lon)
# write_csv(occurrence,"occurrence/gbif-data-geocoded.csv")
# some hand-editing required for the following localities: Pitcairn Island, Rongelap Atoll, Yugui Island, "Hivaoa"

##### geocode new species occurrence records #####
# gkey <- "<key>"
# register_google(gkey)
# fish <- read_excel("data/cirripectes_specimens.xlsx") %>%
#   mutate(place=gsub(",.*$","",locality)) %>% 
#   mutate(place=gsub("Ducie","Ducie Island",place)) %>%
#   filter(nominal_sp == "redhead" | str_detect(locality,fixed("Johnston",ignore_case = T))) %>%
#   distinct(place)
# newsp_occurrence.j <- geocode(fish$place,output="more") %>%
#   mutate(lon=lon+360) %>%
#   distinct()
#####

# load c. variolosus and vanderbilti occurrence data as exported from gbif
# add in new species occurrence data and convert it to an sf object
occurrence_sf <- read_csv(here("data","variolosus-vanderbilti-geocoded.csv")) %>%
  mutate(
    id=paste(str_to_upper(institutionCode),str_to_upper(catalogNumber)),
    lon=ifelse(decimalLongitude<0,decimalLongitude+360,decimalLongitude)
  ) %>%
  rename(lat=decimalLatitude) %>%
  dplyr::select(id,lat,lon,locality,species) %>%
  # get rid of wonky records
  filter(id != 'USNM RAD121745') %>% 
  filter(id != 'USNM 227978') %>% 
  filter(id != "CAS 228432") %>%
  filter(id != "SAIAB 32459") %>%
  filter(id != "CAS 228409") %>%
  # add in new species records
  bind_rows(
    read_csv(here("data","new_sp_occurrence.csv")) %>%
      filter(!str_detect(address,'johnston')) %>% # drop the johnston "record", though
      mutate(id=paste("RH",seq(1,nrow(.)))) %>%
      mutate(species="Cirripectes matatakaro") %>%
      rename(locality=address) %>%
      dplyr::select(id,lat,lon,locality,species)
  ) %>%
  mutate(lat=jitter(lat,amount=0.1),lon=jitter(lon,amount=0.3)) %>%
  mutate(species=factor(species,levels = c("Cirripectes variolosus","Cirripectes vanderbilti","Cirripectes matatakaro"))) %>%
  st_as_sf(.,coords = c("lon","lat"), crs = 4326)

wrld2 = st_as_sf(map('world2', plot=F, fill=T))
quartz()

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
#####
ggplot() +
  geom_sf(data=wrld2, fill='gray20',color="lightgrey",size=0.07) +
  geom_point(aes(x=190.4664,y=16.7295),color="black",fill="black",shape=25, size=6) + #firebrick4
  geom_sf(aes(shape=species,fill=species),color="black",data=occurrence_sf,size=3) +
  # scale_fill_brewer(palette="Dark2",guide=F) +
  scale_fill_manual(values=brewer.pal(n=8,name="Dark2")[c(1,4,6)],guide=F) +
  scale_shape_manual(values=c(21,24,22),guide=F) +
  coord_sf(xlim=c(100,290), ylim=c(-60,60)) +
  xlab("Longitude") +
  ylab("Latitude")
  
