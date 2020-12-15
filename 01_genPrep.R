
######################### STATES ###################################################

proj.crs <- "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"

## Load states 
NAmer <- st_read(dsn = "D:/Shared/BackedUp/Caitlin/boundaries/NorthAmer_StatesProvinces.shp") %>%
  st_buffer(dist = 0) # fix invalid geometries (warning re: lat/long vs. dd)
NAmer <- NAmer[!NAmer$NAME == "Guam",]
NAmer <- NAmer[!NAmer$NAME == "Hawaii",]
NAmer <- NAmer[!NAmer$COUNTRY == "MEX",]
NAmer <- NAmer %>% st_transform(crs = proj.crs) %>% st_buffer(dist = 0)

plot(st_geometry(NAmer))
zoom(st_geometry(NAmer))

# all.sts <- NAmer[NAmer$NAME == "Connecticut"|NAmer$NAME == "Maine"|NAmer$NAME == "Massachusetts"|
#             NAmer$NAME == "New Hampshire"|NAmer$NAME == "New Jersey"| NAmer$NAME == "New York"|
#             NAmer$NAME == "Pennsylvania"|NAmer$NAME == "Rhode Island"|NAmer$NAME == "Vermont" |
#               NAmer$NAME == "Ohio" |NAmer$NAME == "Indiana" |NAmer$NAME == "Illinois" |
#               NAmer$NAME == "Iowa" |NAmer$NAME == "Michigan" |
#               NAmer$NAME == "North Dakota" |NAmer$NAME == "South Dakota" |NAmer$NAME == "Manitoba" |
#             NAmer$NAME == "Wisconsin" |NAmer$NAME == "Minnesota" |NAmer$NAME == "Delaware" |
#               NAmer$NAME == "Maryland" |NAmer$NAME == "West Virginia" |NAmer$NAME == "Virginia"|
#               NAmer$NAME == "Nebraska" |NAmer$NAME == "Kansas"|
#             NAmer$NAME == "Ontario" |NAmer$NAME == "Quebec / Québec" |NAmer$NAME == "New Brunswick / Nouveau-Brunswick" |
#               NAmer$NAME == "Nova Scotia / Nouvelle-Écosse" | NAmer$NAME == "Newfoundland and Labrador / Terre-Neuve-et-Labrador"|
#               NAmer$NAME == "Missouri" |NAmer$NAME == "Kentucky" ,]

sel.sts <- NAmer[NAmer$NAME == "Connecticut"|NAmer$NAME == "Maine"|NAmer$NAME == "Massachusetts"|
                      NAmer$NAME == "New Hampshire"|NAmer$NAME == "New Jersey"| NAmer$NAME == "New York"|
                      NAmer$NAME == "Pennsylvania"|NAmer$NAME == "Rhode Island"|NAmer$NAME == "Vermont" |
                      NAmer$NAME == "Michigan" |NAmer$NAME == "Wisconsin" |NAmer$NAME == "Minnesota" ,] 

# plot(st_geometry(all.sts))
plot(st_geometry(sel.sts))


# Also create boundary, only for select states
bbox <- as(extent(sel.sts), "SpatialPolygons")
proj4string(bbox) <- paste0(proj.crs)
bbox <- st_as_sf(bbox)
plot(st_geometry(bbox))


# rm(NAmer)


# Load H2O
lakes <- st_read("D:/Shared/BackedUp/Caitlin/Water/NAmer_lakes.shp") ; crs(lakes)
lakes <- lakes %>% st_transform(crs = proj.crs) ; crs(lakes) # not sure why I can't use eco.ne
ne.lakes <- lakes %>% st_crop(bbox) # not sure why I can't use eco.ne
# plot(ne.lakes)



######################### FOREST TYPE ###################################################

## Load in forest cover and lu tbls. Grps are bigger; types are too specific.
# src: https://data.fs.usda.gov/geodata/rastergateway/forest_type/; https://data.fs.usda.gov/geodata/rastergateway/forest_type/conus_forest_type_metadata.php
# fortype <- raster(paste0(data.dir, "/for_cover_type/conus_foresttype.img"))
# plot(fortype)
# zoom(fortype)
forgrp <- raster(paste0(data.dir, "for_cover_grp/conus_forestgroup.img"))
# plot(forgrp)
# zoom(forgrp)

# # Load look-up table
# lu.for <- read.csv(paste0(data.dir, "for_cover_grp/for_grp_code.csv"),
#                    head = FALSE, col.names = c("code", "for.grp"))
# 

# Clip to study area (use it's own crs in mask so can avoid projecting raster)
sel.sts.mask <- sel.sts %>%
  st_transform(crs = paste0(crs(forgrp))) %>%
  st_buffer(dist = 0)
plot(sel.sts.mask)
forest <- forgrp %>% crop(sel.sts.mask) #%>% mask(sel.sts.mask) 
plot(forest)
# zoom(for.ne)


# Set all zeros and non-relevant values to NA
forest[forest == 0] <- NA
# Create template raster (all for pixels = 1)
forest1 <- forest
forest1 <- forest*0+1
plot(forest1)



# Get raster data ready for plotting
plot.data <- gplot_data(forest1)

g <- ggplot() + 
  geom_raster(data = plot.data, aes(x = x, y = y, fill = value)) +
  # geom_sf(data = temp) + 
  # scale_fill_manual(values = palette, na.value = NA) +
  theme_bw(base_size = 12) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),# blend lat/long into background
        panel.border = element_rect(fill = NA, color = "black", size = 0.5),
        # panel.background = element_rect(fill = "),
        axis.title = element_blank(),
        legend.background = element_rect(fill = "white", color = "black", size = 0,5))
g





for.type.MA <- for.ne



## FIXME: ADD SCALE BAR & N-ARROW
# For scale bar and N-arrow
# install.packages('ggsn')
# library(ggsn)

bbox

g <- ggplot() + 
  geom_sf(data = NAmer, color = "dark grey", fill = "#f0f0f0", size = 0.5) +
  geom_raster(data = plot.data[!is.na(plot.data$value),], # exclude NA from being plotted
              aes(x = x, y = y, fill = value)) + # factor b/c it's continuous
  geom_sf(data = lakes, color = "#80d0ff", fill = "#e5f6ff", size = 0.5) + 
  # geom_sf(data = eco.ne.dslv, color = "dark grey", fill = NA, size = 0.5) + #"#fcfcfc") +
  # scale_fill_manual("Strata by forest type & ownership",
  #                   values = palette,
  #                   na.value = NA,
  #                   labels = labels) +
  guides(fill=guide_legend(ncol=2)) +
  coord_sf(xlim = c(-91836.6, 2255743),
           ylim = c(1962636, 3012405),
           # crs = st_crs(102003), # Albers Equal area # on means ne.reg won't plot??
           expand = FALSE) +
  theme_bw(base_size = 12) + 
  theme(panel.grid.major = element_line(color = "#fcfcfc"),
        panel.grid.minor = element_blank(),# blend lat/long into background
        panel.border = element_rect(fill = NA, color = "black", size = 0.5),
        panel.background = element_rect(fill = "#e5f6ff"),
        axis.title = element_blank(),
        legend.background = element_rect(fill = "white", color = "black", size = 0,5),
        legend.justification=c("left", "top"), # which side oflegend position coords refer to
        legend.position=c(0,1), 
        legend.box.margin=ggplot2::margin(c(rep(5, 4))), # ggplot2 else draws from other package
        legend.text=element_text(size=10),
        legend.title = element_text(size=12)) #+
# scalebar(location = "bottomright", dist = 100, dist_unit = "km",
#          x.min = 1324033, x.max = 2257533,
#          y.min = 2137911, y.max = 3012911)

g

# png("map_MAs_ownXfor_12.png", width = 480, height = 480, units = "px", bg = "white")
# g
# dev.off()

# tiff("map_MAs_ownXfor_12.tiff", width = 480, height = 480, bg = "white")
# g
# dev.off()

pdf("map_MAs_ownXfor_12.pdf", width = 6, height = 6, bg = "white")
g
dev.off()
