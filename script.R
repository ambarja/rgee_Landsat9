library(rgee)
library(rgeeExtra)
library(tidyverse)
library(cptcity)
ee_Initialize()

# 1. Study area -----------------------------------------------------------

ica <- ee$Geometry$Rectangle(
  c(-75.9080,-14.3925,-75.4355,-13.8784),
  geodesic = FALSE,
  proj = "EPSG:4326"
  )

# 2. Dataset --------------------------------------------------------------
# Scaling factors
scaling_img <- function(image) {
  opticalBands = image$select('SR_B.')$multiply(0.0000275)$add(-0.2)
  thermalBands = image$select('ST_B.*')$multiply(0.00341802)$add(149.0)
  image$addBands(opticalBands, NULL, TRUE)$
    addBands(thermalBands, NULL, TRUE)
}
# Landsat 9 scaled
l9 <- ee$Image('LANDSAT/LC09/C02/T1_L2/LC09_006070_20220202')$
  clip(ica) %>% 
  scaling_img()
  
# 3. Visualization of color false and ndvi --------------------------------
viz_ndvi <- list(
  min = -0.5,
  max = 0.5,
  palette = cpt("grass_ndvi")
)

viz <- list(
  min = 0.07,
  max = 0.37,
  bands = c("SR_B5","SR_B4","SR_B3")
)

Map$centerObject(ica)

m1 <- Map$addLayer(
  (l9[[5]] - l9[[4]])/(l9[[5]] + l9[[4]]),
  visParams = viz_ndvi) + 
  Map$addLegend(
    visParams = viz_ndvi
    )

m2 <- Map$addLayer(
  l9,
  visParams = viz
  )

m2 | m1 
