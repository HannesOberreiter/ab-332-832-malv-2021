# Creating Sample Map with Oceanographic Images
library(ggOceanMaps)
library(tidyverse)
library(sf)

samples <- tibble(
    side = c("BAB", "IsA", "IsK", "IsG"),
    lat = c(78.658333, 78.255333, 78.3207, 78.128883),
    lon = c(16.675, 15.534, 15.1626, 14.002767)
)
samples <- ggOceanMaps::transform_coord(samples, bind = TRUE)
# samples <- st_as_sf(samples, coords = c("lat", "lon"), crs = 4326)
# samples <- st_transform(samples, crs = 32633)

p <- basemap(
    limits = c(13.0, 17.5, 78.0, 78.8),
    lon.interval = 0.5,
    lat.interval = 0.1,
    legend = FALSE,
    legend.position = "bottom",
    bathymetry = TRUE,
    glaciers = TRUE,
    shapefiles = "Svalbard",
) +
    geom_spatial_label_repel(
        data = samples,
        aes(x = lon, y = lat, label = side),
        color = "#E69F00",
        size = 10
    ) +
    geom_spatial_point(
        data = samples,
        aes(x = lon, y = lat),
        color = "#E69F00",
        size = 5
    )
fSaveImages(p, "SampleMap", w = 7.5, h = 7.5)
