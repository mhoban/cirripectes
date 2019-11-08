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
# setwd("~/projects/fangblennies/cirripectes/taxonomy/")
# fish <- read_excel("data/cirripectes_specimens.xlsx") %>%
#   mutate(place=gsub(",.*$","",locality)) %>% 
#   mutate(place=gsub("Ducie","Ducie Island",place)) %>%
#   filter(nominal_sp == "redhead" | str_detect(locality,fixed("Johnston",ignore_case = T))) %>%
#   distinct(place)
# newsp_occurrence.j <- geocode(fish$place,output="more") %>%
#   mutate(lon=lon+360) %>%
#   distinct()
#####

rm(list=ls())

# load c. variolosus and vanderbilti occurrence data as exported from gbif
occurrence <- read_csv("../data/variolosus-vanderbilti-geocoded.csv") %>%
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
  filter(id != "CAS 228409")

# load c. matatakaro occurrence data
# it has johnston stuck in there as a hypothetical location
allspp_occurrence <- read_csv("../data/new_sp_occurrence.csv") %>%
  mutate(id=paste("RHJ",seq(1,nrow(.)))) %>%
  mutate(species="Cirripectes matatakaro (Johnston)") %>%
  rename(locality=address) %>%
  dplyr::select(id,lat,lon,locality,species)

# slap all the occurrence data together
allspp_occurrence <- allspp_occurrence %>%
  bind_rows(
    # replicate the new species occurrence data but without Johnston
    allspp_occurrence %>% 
      filter(!str_detect(locality,fixed("Johnston",ignore_case = T))) %>%
      mutate(species="Cirripectes matatakaro") %>%
      mutate(id=paste("RH",seq(1,nrow(.))))
  ) %>%
  bind_rows(occurrence) %>%
  # set up display options
  mutate(
    line=case_when(
      species == "Cirripectes matatakaro (Johnston)" ~ "dashed",
      TRUE ~ "solid"
    ),
    color=case_when(
      species == "Cirripectes variolosus" ~ "grey77",
      species == "Cirripectes vanderbilti" ~ "white",
      TRUE ~ "black"
    ),
    buffer=case_when(
      species == "Cirripectes matatakaro (Johnston)" ~ 3,
      TRUE ~ 2
    ),
    fill=case_when(
      species == "Cirripectes matatakaro" ~ "grey55",
      TRUE ~ as.character(NA)
    ),
    alpha=case_when(
      species == "Cirripectes matatakaro" ~ 0.6,
      TRUE ~ 1
    )
  ) %>%
  mutate(species=factor(species))

occurrence_sf <- st_as_sf(allspp_occurrence,coords = c("lon","lat"), crs = 4326)

# will throw a warning we aren't concerned with
suppressWarnings(
  # collapse all the occurrence records by species and create convex hulls to
  # approximate their areas of occurrence
  occurrence_area <- occurrence_sf %>%
    group_by( species, color, line, buffer, fill, alpha ) %>%
    summarise( geometry = st_combine( geometry ) ) %>%
    st_convex_hull() %>%
    st_buffer(dist=.$buffer)
)

wrld2 = st_as_sf(map('world2', plot=F, fill=T))
quartz()

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
#####
ggplot() +
  geom_sf(data=wrld2, fill='gray20',color="lightgrey",size=0.07) +
  geom_sf(aes(linetype=line,color=species, alpha=alpha, fill=fill),size=0.7,data=occurrence_area) + 
  geom_point(aes(x=190.4664,y=16.7295),color="firebrick4",fill="firebrick4",shape=25, size=4) + 
  geom_sf(data=occurrence_sf %>% filter(species == "Cirripectes matatakaro" | species == "Cirripectes vanderbilti"),size=2, color="black") +
  scale_linetype_identity() + 
  # scale_color_brewer(palette="Set2",guide=F) +
  scale_color_manual(values=cbPalette,guide=F) +
  scale_fill_identity() +
  scale_alpha_identity() +
  coord_sf(xlim=c(100,290), ylim=c(-60,60)) +
  
  xlab("Longitude") +
  ylab("Latitude")
  # geom_polygon(aes(x=long,y=lat,group=group),fill="grey",color="black",alpha=0.6,data=newsp.dist) +
  # geom_polygon(aes(x=long,y=lat,group=group),fill=NA,color="grey77",data=variolosus.dist) +
  #geom_polygon(aes(x=long,y=lat,group=group),fill="grey",color="black",linetype="dashed",alpha=0.6,data=vanderbilti.dist) +
  # geom_polygon(aes(x=long,y=lat,group=group),fill=NA,color="black",linetype="dashed",data=newsp.dist.j) +
  # geom_point(aes(x=lon,y=lat),color="black",data=newsp_occurrence) +
  # geom_point(aes(x=lon,y=lat),color="firebrick4",fill="firebrick4",data=johnston,shape=25) +
  
