########################################################
# By Stanislas Mahussi GANDAHO - stangandaho@gmail.com #
########################################################

# load necessaries libraries
library(terra)
library(sf)
library(ggplot2)
library(cowplot)
library(showtext)
library(ggtext)
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

# customize font
## FONTS
monts_bold <- font_add("monts_bold",
                       "./Montserrat/Montserrat Bold 700.ttf")
monts_reg <- font_add("monts_reg",
                      "./Montserrat/Montserrat Medium 500.ttf")
monts_it <- font_add("monts_it",
                     "./Montserrat/Montserrat Light 300.ttf")
showtext_auto()
showtext_opts(dpi = 300)
# Parakou
## 2003
## In Landsat 4-7, NDVI = (Band 4 – Band 3) / (Band 4 + Band 3)

### importe benin shapefile filtering parakou boundary
para_shp <- read_sf("./shp/benin/ben_admbnda_adm2_1m_salb_20190816.shp") %>%
  dplyr::filter(adm2_name == "Parakou")
### import raster band and compute NDVI
para_b3_2003 <- rast_handle(.path = "./parakou/2003/LE07_L1TP_192053_20030104_20200916_02_T1_B3.TIF",
                            .crs = "EPSG:32631")
para_b4_2003 <- rast_handle(.path = "./parakou/2003/LE07_L1TP_192053_20030104_20200916_02_T1_B4.TIF",
                            .crs = "EPSG:32631")
para_ndvi_2003 <- ndvi(.nir = para_b4_2003, .red = para_b3_2003)
### convert ndvi into dataframe adding a classification column
para_ndvi_2003_df <- para_ndvi_2003 %>% terra::as.data.frame(x = ., xy = TRUE) %>%
  tidyr::drop_na() %>% rename("value" = 3) %>%
  mutate(Class = case_when(value < 0.1 ~ "Bare soil",
                           value >= 0.1 & value < 0.3 ~ "Moderate vegetation",
                           value >= 0.3 ~ "Dense vegetation"))
para_ndvi_2003_df$Class <- factor(para_ndvi_2003_df$Class,
                                  levels = c("Bare soil", "Moderate vegetation", "Dense vegetation"))

## rate per class
rate_para_2003 <- para_ndvi_2003_df %>%
  group_by(Class) %>%
  summarise(Area = n() * (30*30) /1e+06) %>%
  mutate(Class = stringr::str_wrap(paste0(Class, " (", round(Area, 2), "km²)"), width = 20))
rate_para_2003$Class <- factor(rate_para_2003$Class, levels = unique(rate_para_2003$Class))

ggplot(data = rate_para_2003)+
  geom_col(aes(x = Class, y = Area, fill = Class), show.legend = F)+
  scale_y_continuous(expand = c(0,0))+
  coord_flip()+
  scale_fill_manual(values = c("#d9d9d9","#2aaa00", "#007809"))+
  theme_void()+
  theme(
    axis.text.y = element_text(size = 13, hjust = 1, family = "monts_reg",# lineheight = 2,
                               margin = margin(r = 1, unit = "lines")),
    axis.line.y = element_line(linewidth = 0.2)
  )
ggsave("rate_para_2003.png")
## ggplot map
ggplot(data = para_ndvi_2003_df)+
  geom_raster(aes(x = x, y = y, fill = Class))+
  labs(title = paste0("<p>Parakou<i style = 'color:#747474'>'s</i>  <i style = 'color:#007809'>NDVI</i><br>Jan 2003</p>"))+
  scale_fill_manual(values = c("#d9d9d9", "#2aaa00", "#007809"))+
  guides(fill = guide_legend(title = "", direction = "horizontal",
                             label.position = "top", keywidth = unit(1, "lines"),
                             keyheight = unit(0.4, "lines"),
                             label.theme = element_text(size = 16, vjust = 0, family = "monts_reg")))+
  theme_void()+
  coord_sf()+
  theme(
    plot.title = ggtext::element_markdown(size = 25, halign = 1,
                                          margin = margin(t = 1,l=1.5, unit = "lines"),
                                          family = "monts_bold"),
    #legend
    legend.position = c(0.3, 0.02),
    legend.background = element_blank(),
    legend.text = element_text(family = "monts_reg")
  )
ggsave("para_2003.jpeg", width = 29, height = 22, dpi = 300, units = "cm")


## 2003
## In Landsat 8-9, NDVI = (Band 5 – Band 4) / (Band 5 + Band 4)
para_b4_2023 <- rast_handle(.path = "./parakou/2023/LC08_L1TP_192054_20230103_20230110_02_T1_B4.TIF",
                            .crs = "EPSG:32631")
para_b5_2023 <- rast_handle(.path = "./parakou/2023/LC08_L1TP_192054_20230103_20230110_02_T1_B5.TIF",
                            .crs = "EPSG:32631")
para_ndvi_2023 <- ndvi(.nir = para_b5_2023, .red = para_b4_2023)

para_ndvi_2023_df <- para_ndvi_2023 %>% terra::as.data.frame(x = ., xy = TRUE) %>%
  tidyr::drop_na() %>% rename("value" = 3) %>%
  mutate(Class = case_when(value < 0.1 ~ "Bare soil",
                           value >= 0.1 & value < 0.3 ~ "Moderate vegetation",
                           value >= 0.3 ~ "Dense vegetation"))
para_ndvi_2023_df$Class <- factor(para_ndvi_2023_df$Class,
                                  levels = c("Bare soil", "Moderate vegetation", "Dense vegetation"))

## rate per class
rate_para_2023 <- para_ndvi_2023_df %>%
  group_by(Class) %>%
  summarise(Area = n() * (30*30) /1e+06) %>%
  mutate(Class = stringr::str_wrap(paste0(Class, " (", round(Area, 2), "km²)"), width = 20))
rate_para_2023$Class <- factor(rate_para_2023$Class, levels = unique(rate_para_2023$Class))

ggplot(data = rate_para_2023)+
  geom_col(aes(x = Class, y = Area, fill = Class), show.legend = F)+
  scale_y_continuous(expand = c(0,0))+
  coord_flip()+
  scale_fill_manual(values = c("#d9d9d9","#2aaa00", "#007809"))+
  theme_void()+
  theme(
    axis.text.y = element_text(size = 13, hjust = 1, family = "monts_reg",# lineheight = 2,
                               margin = margin(r = 1, unit = "lines")),
    axis.line.y = element_line(linewidth = 0.2)
  )
ggsave("rate_para_2023.png")
## ggplot map
ggplot(data = para_ndvi_2023_df)+
  geom_raster(aes(x = x, y = y, fill = Class))+
  labs(title = paste0("<p>Parakou<i style = 'color:#747474'>'s</i>  <i style = 'color:#007809'>NDVI</i><br>Jan 2023</p>"))+
  scale_fill_manual(values = c("#d9d9d9", "#2aaa00", "#007809"))+
  guides(fill = guide_legend(title = "", direction = "horizontal",
                             label.position = "top", keywidth = unit(1, "lines"),
                             keyheight = unit(0.4, "lines"),
                             label.theme = element_text(size = 16, vjust = 0, family = "monts_reg")))+
  theme_void()+
  coord_sf()+
  theme(
    plot.title = ggtext::element_markdown(size = 25, halign = 1,
                                          margin = margin(t = 1,l=1.5, unit = "lines"),
                                          family = "monts_bold"),
    #legend
    legend.position = c(0.3, 0.02),
    legend.background = element_blank(),
    legend.text = element_text(family = "monts_reg")
  )
ggsave("para_2023.jpeg", width = 29, height = 22, dpi = 300, units = "cm")

