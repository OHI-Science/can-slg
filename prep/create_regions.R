require(ggplot2)
require(tidyverse)
require(raster)
require(sf)

south <- 45
north <- 53
east <- -55
west <- -72

bb <- matrix(c(west,east,east,west,west,
               north,north,south,south,north),
             nrow=5) %>%
  list %>%
  st_polygon() %>%
  st_sfc(crs=4326)

Canada <- st_as_sf(raster::getData(country="CAN",level=1)) %>%
  rbind(st_as_sf(raster::getData(country="USA",level=1))) %>%
  filter(HASC_1=='CA.NB'|HASC_1=='CA.NS'|HASC_1=='CA.PE'|HASC_1=='CA.QC'|HASC_1=='CA.NF'|HASC_1=='US.ME') %>%
  # st_simplify(10000000) %>%
  st_intersection(st_buffer(bb,1))


# This file is from Peter Galbraith along with the limits to BdC and NS below
points <- read.table('prep/Spatial/AE_BoxesOutline.dat') %>%
  filter(V1!=-99) %>%
  st_as_sf(coords=c('V2','V3'), crs = 4326, agr='identity') %>%
  mutate(row=row.names(.))

boxes <- list(st_polygon(list(st_coordinates(points[c(1:12,1),]))),
              st_polygon(list(st_coordinates(points[c(13:25,13),]))),
              st_polygon(list(st_coordinates(points[c(26:38,72,39:40,26),]))),
              st_polygon(list(st_coordinates(points[c(41:56,41),]))),
              # st_polygon(list(st_coordinates(points[c(57:74,57),]))), # has rounded coordinates, leaves gaps between this and northeast gulf, can't use this
              st_polygon(list(st_coordinates(points[c(63,33:38,72:63),])))) %>%
  st_sfc(crs=4326) %>%
  st_sf()

# Create Northumberland

# # Commented out code to create the Northumberland box
# # south <- 45
# north <- 47.05
# east <- -62
# # west <- -72
# Northumberland <- matrix(c(west,east,east,west,west,
#                north,north,south,south,north),
#              nrow=5) %>%
#   list %>%
#   st_polygon() %>%
#   st_sfc(crs=4326) %>%
#   st_intersection(boxes)
#
#
#
# library(mapview)
# library(mapedit)
#
# new_Northumberland <- mapview(Northumberland) %>%
#   editMap("Northumberland")
# test <- st_difference(Northumberland,st_buffer(new_Northumberland$drawn,dist=1e-12)) %>%
#   st_cast("POLYGON") %>%
#   st_sf()
#
# x=st_coordinates(test[2,])[,1:2]

Northumberland <- matrix(c(-64.83417, 47.05000,
                           -63.99971, 47.05000,
                           -64.03450, 46.97359,
                           -64.35750, 46.68241,
                           -64.35750, 46.68241,
                           -64.01370, 46.66437,
                           -64.03196, 46.44452,
                           -63.61249, 46.41472,
                           -63.15257, 46.28963,
                           -62.81419, 46.39695,
                           -62.74313, 46.33986,
                           -62.41110, 46.40877,
                           -62.00000, 46.45119,
                           -62.00000, 45.35590,
                           -64.60400, 46.01400,
                           -65.48429, 46.68975,
                           -64.83417, 47.05000),
                         ncol=2,byrow = TRUE) %>%
  list %>%
  st_polygon() %>%
  st_sfc(crs=4326)





boxes_NS <- st_difference(boxes, Northumberland) %>%
  rbind(Northumberland)

# Create Baie des Chaleurs

# # Commented out code to create the Baie des Chaleurs box
# # south <- 45
# # north <- 53
# east <- -64-(31.6/60)
# # west <- -72
# BdC <- matrix(c(west,east,east,west,west,
#                            north,north,south,south,north),
#                          nrow=5) %>%
#   list %>%
#   st_polygon() %>%
#   st_sfc(crs=4326) %>%
#   st_intersection(boxes)
#
#
#
# library(mapview)
# library(mapedit)
#
# new_BdC <- mapview(BdC) %>%
#   editMap("BdC")
# test <- st_difference(BdC[2],st_buffer(new_BdC$drawn,dist=1e-12)) %>%
#   st_cast("POLYGON") %>%
#   st_sf()
#
#
# x=st_coordinates(test[1,])[,1:2]
# write.csv(round(x,10),'clipboard',row.names = FALSE)




BdC <- matrix(c(-64.5266666667,48.9724855442,
                -64.5266666667,47.983223015,
                -64.533173,47.965953,
                -64.532727,47.961793,
                -64.537042,47.960274,
                -64.539802,47.957299,
                -64.538136,47.954276,
                -64.571576,47.925465,
                -64.579826,47.897468,
                -64.580049,47.89626,
                -64.579757,47.8955,
                -64.578417,47.893049,
                -64.578031,47.891816,
                -64.577651,47.880875,
                -64.581086,47.875349,
                -64.605082,47.841653,
                -64.598915,47.795729,
                -64.662472,47.766408,
                -64.688095,47.751366,
                -64.697179,47.744017,
                -64.70185,47.742979,
                -64.73879,47.721729,
                -64.751239,47.704625,
                -64.7689,47.69262,
                -64.840664,47.698168,
                -64.855708,47.722929,
                -65.10714,47.694718,
                -66.1191054836,47.1770726895,
                -67.3800000002,48.144999997,
                -67.3800000041,48.7499999995,
                -65.7000000015,48.7499999996,
                -65.3691436812,48.6070167369,
                -64.5266666667,48.9724855442),
                         ncol=2,byrow = TRUE) %>%
  list %>%
  st_polygon() %>%
  st_sfc(crs=4326)


plot(BdC)


# standardized projection
# Everything below here must be transformed
proj <- "+proj=lcc +lat_1=46 +lat_2=60 +lat_0=44 +lon_0=-68.5 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
land <- Canada %>% st_transform(proj)
coastline <- st_read("prep/Spatial/egsl/egsl.shp") %>% st_transform(proj)

# rgn_id (unique numeric region identifier)
# rgn_name (unique named region identifier)
# area_km2 or area_hectare (area in km2 or hectares)

# place names
rgn_name <- c("Northwest Gulf",
              "Northeast Gulf",
              "Central Gulf",
              "Mecatina",
              "Madeleine Plateau",
              "Northumberland Strait",
              "Chaleur Bay")
rgn_nom <- c("Nord-west Golfe",
             "Nord-est du Golfe",
             "Centre du Golfe",
             "Mécatina",
             "Plateau Madelinien",
             "Détroit de Northumberland",
             "Baie des Chaleurs")

# put it all together
region_boxes <- st_difference(boxes_NS[c(1:3,5,4,6),], BdC) %>%
  rbind(BdC) %>%
  mutate(rgn_id = 1:7,
         rgn_name = ordered(rgn_name,levels = rgn_name),
         rgn_nom = ordered(rgn_nom,levels = rgn_nom)
  ) %>%
  st_transform(proj)

north <- as.numeric(st_bbox(region_boxes)[4])
south <- as.numeric(st_bbox(region_boxes)[2])
east <- as.numeric(st_bbox(region_boxes)[3])
west <- as.numeric(st_bbox(region_boxes)[1])

ggplot(region_boxes) +
  geom_sf(aes(fill=rgn_name))


ggplot(region_boxes) +
  geom_sf(data=land,col="grey80",fill="grey90") +
  geom_sf(aes(fill=rgn_name),col="transparent",alpha=0.8) +
  theme_bw() +
  coord_sf(ylim = c(north,south),
           xlim = c(west,east))

region_coastline <- st_intersection(region_boxes,st_union(coastline)) %>%
  mutate(area_km2 = units::set_units(st_area(.), km^2))

ggplot(region_coastline) +
  geom_sf(data=land,col="grey80",fill="grey90") +
  geom_sf(aes(fill=as.numeric(area_km2)),col="transparent",alpha=0.8) +
  theme_bw() +
  coord_sf(ylim = c(north,south),
           xlim = c(west,east))


# save shapefiles

st_write(region_boxes,"prep/Spatial/region_boxes.shp")
st_write(region_coastline,"prep/Spatial/region_coastline.shp",delete_layer = TRUE)
