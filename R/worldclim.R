### worldclim
### Extraction de donnees climatiques mensuelles
### A partir https://www.worldclim.org
### Mohamed Hilal, CESAER, INRAE/Institut Agro
### Mars 2024

rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
clc <- function() cat(rep("\n", 50))
clc()
install.packages("easypackages")
library(easypackages)

listpack <- c("raster","sf","exactextractr",
              "geodata",
              "data.table","tidyverse")

needpack <- setdiff(listpack, rownames(installed.packages()))
if(length(needpack)) {
  install.packages(needpack)
}

libraries(listpack)

clc()

# Pull departement boundaries for FRANCE
ddep <- st_as_sf(gadm(country='FRA', level=2, path=tempdir())) %>%
  mutate(DEP = CC_2,
         NOM_DEP = NAME_2) %>%
  dplyr::select(DEP) %>%
  arrange(DEP)

#Variable	Description	Unit
#elev altitude m
#tmin	minimum temperature	°C
#tmax	maximum temperature	°C
#tavg	average temperature	°C
#prec	total precipitation	mm
#srad	incident solar radiation	kJ m-2 day-1
#wind	wind speed (2 m above the ground)	m s-1
varclim<-c('elev','tmin','tmax','tavg','prec','srad','wind')

# Calculate data frame of min, max, mean, median, mode for all month
statclim <- c('min','max','mean','median','mode','sum')

dataclim <- ddep %>%
  dplyr::select(DEP) %>%
  sf::st_drop_geometry()

for (v in varclim) {
  print(v)
  rclim <- worldclim_country(country = 'FRA', var=v, res=0.5, path=tempdir())
  dclim <- exact_extract(rclim, ddep, statclim)
  colClean <- gsub(".FRA_wc2.1_30s_", "_", colnames(dclim))
  colnames(dclim) <- colClean
  dataclim <- cbind(dataclim,dclim)
}

write.csv(dataclim,"dataclim.csv")
