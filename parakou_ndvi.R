########################################################
# By Stanislas Mahussi GANDAHO - stangandaho@gmail.com #
########################################################

# load necessaries libraries
library(terra)
library(sf)
library(sf)
library(dplyr)

# define funtion to import and crop raster
rast_handle <- function(.path, .crs = ""){
  rst <- rast(.path) %>%
    crop(y = para_shp %>% st_transform(crs = st_crs(.crs)),
         mask = TRUE, snap = "in")
  return(rst)
}

# define function to compute ndvi
ndvi <- function(.nir, .red){
  ndvi <- (.nir - .red)/(.nir + .red)
  return(ndvi)
}

# Parakou
## 2003
## In Landsat 4-7, NDVI = (Band 4 – Band 3) / (Band 4 + Band 3)

para_shp <- read_sf("./shp/benin/ben_admbnda_adm2_1m_salb_20190816.shp") %>%
  dplyr::filter(adm2_name == "Parakou")

para_b3_2003 <- rast_handle(.path = "./parakou/2003/LE07_L1TP_192053_20030104_20200916_02_T1_B3.TIF",
                            .crs = "EPSG:32631")
para_b4_2003 <- rast_handle(.path = "./parakou/2003/LE07_L1TP_192053_20030104_20200916_02_T1_B4.TIF",
                            .crs = "EPSG:32631")
para_ndvi_2003 <- ndvi(.nir = para_b4_2003, .red = para_b3_2003)
para_ndvi_2003[para_ndvi_2003 < 0.05] <- 0
para_ndvi_2003[para_ndvi_2003 >= 0.05] <- terra::minmax(para_ndvi_2003)[2,]
## 2003
## In Landsat 8-9, NDVI = (Band 5 – Band 4) / (Band 5 + Band 4)
para_b4_2023 <- rast_handle(.path = "./parakou/2023/LC08_L1TP_192054_20230103_20230110_02_T1_B4.TIF",
                            .crs = "EPSG:32631")
para_b5_2023 <- rast_handle(.path = "./parakou/2023/LC08_L1TP_192054_20230103_20230110_02_T1_B5.TIF",
                            .crs = "EPSG:32631")
para_ndvi_2023 <- ndvi(.nir = para_b5_2023, .red = para_b4_2023)
para_ndvi_2023[para_ndvi_2003 < 0.05] <- 0
para_ndvi_2023[para_ndvi_2003 >= 0.05] <- terra::minmax(para_ndvi_2003)[2,]

plot(para_ndvi_2003)
plot(para_ndvi_2023)
