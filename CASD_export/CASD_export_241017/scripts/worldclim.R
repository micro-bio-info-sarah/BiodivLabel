### worldclim
### Extraction de donnees climatiques mensuelles
### A partir https://www.worldclim.org
### Sarah HUET & Mohamed Hilal, CESAER, INRAE/Institut Agro
### Juillet 2024

#install.packages("easypackages")
library(easypackages)

tmp_listpack <- c("raster","sf","exactextractr",
              "geodata",
              "data.table","tidyverse")

tmp_needpack <- setdiff(tmp_listpack, rownames(installed.packages()))
if(length(tmp_needpack)) {
  install.packages(tmp_needpack)
}

libraries(tmp_listpack)

# extract climate data ----

tmp_NUTS_clim <- tibble()

for (tmp_country in tmp_country_codes$country_code) {

# Pull departement boundaries for country
tmp_ddep <- gadm(country=tmp_country, level=2, path=tempdir())

if (!is.null(tmp_ddep)) {

tmp_ddep <- st_as_sf(tmp_ddep) %>%
  mutate(DEP = CC_2,
         NOM_DEP = NAME_2) %>%
  #dplyr::select(DEP) %>%
  arrange(DEP)

#Variable	Description	Unit
#elev altitude m
#tmin	minimum temperature	°C
#tmax	maximum temperature	°C
#tavg	average temperature	°C
#prec	total precipitation	mm
#srad	incident solar radiation	kJ m-2 day-1
#wind	wind speed (2 m above the ground)	m s-1


tmp_rclim <- worldclim_country(country = tmp_country, var='tavg', res=0.5, path=tempdir())

# Calculate data frame of min, max, mean, median, mode for all month
tmp_dclim <- exact_extract(tmp_rclim, tmp_ddep, 'mean')
tmp_colClean <- gsub(paste0(".",tmp_country,"_wc2.1_30s_"), "_", colnames(tmp_dclim))
colnames(tmp_dclim) <- tmp_colClean

tmp_dataclim <- tmp_ddep %>%
  #dplyr::select(DEP,NOM_DEP) %>%
  dplyr::mutate(COUNTRY = tmp_country) %>%
  sf::st_drop_geometry() %>%
  cbind(.,tmp_dclim) %>%
  mutate()

tmp_NUTS_clim <- tmp_NUTS_clim %>%
  rbind(.,tmp_dataclim)

}

}

tmp_NUTS_clim <- tmp_NUTS_clim %>%
  rowwise() %>%
  mutate(annual_mean = mean(c_across(starts_with("mean_tavg"))))




write.csv(dataclim,"dataclim.csv")
