# source('./Code/Quebec-Levis.R')
library(osmdata)
library(tidyverse)
library(sf)
library(graphicsutils)
bbx <- getbb("Lisbon, Portugal")
# bbx <- matrix(c(-71.4,-70.9,46.72,46.89), nrow = 2, byrow = TRUE,
#               dimnames = list(c('x','y'),c('min','max')))

library(tmap)
data(World)
uid <- World$name == 'Portugal'
plot(st_geometry(World[uid, ]))
axis(1);axis(2)

xmin <- bbx[1,1]
xmax <- bbx[1,2]
ymin <- bbx[2,1]
ymax <- bbx[2,2]

dot <- icon::fontawesome("map-pin")

# Graphical parameters
col1 <- col2 <- '#6B737C'
col1 <- col2 <- lighten(col1, 25)
# col1 <- col2 <- '#9ba6b3'
# col1 <- col2 <- '#414141'
col3 <- '#000000'
col4 <- '#000000'
col3 <- col4 <- '#05394a'
# col3 <- '#4bf9d9'
# col4 <- '#339e8a'
land <- '#ffffff'
land <- '#e6e6e6'
# land <- '#414141'
colR <- '#951200'
colR <- '#C9A7A5'
colR <- darken('#C9A7A5', 25)
# colR <- '#967D7B'
colR2 <- '#eb1c00'
colR2 <- '#951200'

# Title
library(showtext)
library(sysfonts)
font_add_google("Cormorant Garamond", "newFont")
showtext_auto()



# Points focaux
# camp <- c(46.76570790680491, -70.926210635683)
# julie <- c(46.77967331127396, -71.17635422232038)
# fd <- c(46.78032265154715, -71.19091384298153)
# cath <- c(46.798338643600445, -71.24816570029948)
# val <- c(46.780281203997504, -71.18938817267288)
# bn <- c(46.812140095003215, -71.20189127357153)
# bs <- c(46.81106804483042, -71.18777212507243)
#
# xy <- data.frame(Longitude = c(camp[2],julie[2],fd[2],cath[2],val[2]),
#                  Latitude = c(camp[1],julie[1],fd[1],cath[1],val[1])) %>%
#       st_as_sf(coords = c('Longitude','Latitude'), crs = 4326, remove = FALSE)
#
# bat <- data.frame(Longitude = c(bn[2], bs[2]),
#                  Latitude = c(bn[1], bs[1])) %>%
#       st_as_sf(coords = c('Longitude','Latitude'), crs = 4326, remove = FALSE)
a <- available_tags("highway")

# Réseau routier
highways <- bbx %>%
            opq()%>%
            add_osm_feature(key = "highway",
                            # value=c("motorway", "trunk",
                            #         "primary","secondary",
                            #         "tertiary","motorway_link",
                            #         "trunk_link","primary_link",
                            #         "secondary_link",
                            #         "tertiary_link")) %>%
                            value=c(a) %>%
            osmdata_sf()

streets <- bbx %>%
           opq()%>%
           add_osm_feature(key = "highway",
                           value = c("residential", "living_street",
                                     "service","unclassified",
                                     "pedestrian", "footway",
                                     "track","path")) %>%
           osmdata_sf()

# # Lakes
# hydro <- st_read('/users/davidbeauchesne/desktop/data/lhy_000c16a_e/hydro.shp') %>%
#          st_transform(crs = 4326)
#
# # Shoreline classification
# stl <- st_read('/users/davidbeauchesne/desktop/data/diss2016shp/quebec.shp') %>%
#        st_transform(crs = 4326)


# # Routes maisons
# library(osrm)
# l <- expand.grid(1:5,1:5)
# route <- list()
# for(i in 1:nrow(l)) {
#   route[[i]] <- osrmRoute(src = xy[l[i,1],], dst = xy[l[i,2],], overview = "full", returnclass = "sf")
# }

# # Routes bateau nord - cath
# route2 <- list()
# route2[[1]] <- osrmRoute(src = xy[4,], dst = bat[1,], overview = "full", returnclass = "sf")
# route2[[2]] <- osrmRoute(src = bat[1,], dst = xy[4,], overview = "full", returnclass = "sf")
#
# # Routes bateau sud - -cath
# route3 <- list()
# route3[[1]] <- osrmRoute(src = xy[1,], dst = bat[2,], overview = "full", returnclass = "sf")
# route3[[2]] <- osrmRoute(src = xy[2,], dst = bat[2,], overview = "full", returnclass = "sf")
# route3[[3]] <- osrmRoute(src = xy[3,], dst = bat[2,], overview = "full", returnclass = "sf")
# route3[[4]] <- osrmRoute(src = xy[5,], dst = bat[2,], overview = "full", returnclass = "sf")
# route3[[5]] <- osrmRoute(src = bat[2,], dst = xy[1,], overview = "full", returnclass = "sf")
# route3[[6]] <- osrmRoute(src = bat[2,], dst = xy[2,], overview = "full", returnclass = "sf")
# route3[[7]] <- osrmRoute(src = bat[2,], dst = xy[3,], overview = "full", returnclass = "sf")
# route3[[8]] <- osrmRoute(src = bat[2,], dst = xy[5,], overview = "full", returnclass = "sf")
#
# # Routes
# route <- c(route,route2,route3)
#
# # Camp vers chemin
# cmrt <- osrmRoute(src = xy[1,], dst = xy[5,], overview = "full", returnclass = "sf") %>%
#         st_coordinates()


mat <- matrix(c(1,2), nrow = 2)
# Figure
png('./Figures/portugal.png', res = 200, width = 175, height = 300, units = "mm")
# par(mar = c(0,0,0,0), bg = col3)
layout(mat, heights = c(.945,.065))
par(mar = c(0,0,0,0), bg = col3)
plot0(x = bbx[1,], y = bbx[2,])
# plot(st_geometry(stl), col = land, border = land, add = TRUE)
# plot(st_geometry(hydro), col = col3, border = col3, add = TRUE)
plot(st_geometry(highways$osm_lines), col = col1, lwd = 1.1, add = TRUE)
plot(st_geometry(streets$osm_lines), col = col2, lwd = .7, add = TRUE)
for(i in 1:length(route)) plot(st_geometry(route[[i]]), lwd = 2.5, col = colR, add = TRUE)
lines(x = bat[,1, drop=TRUE], y = bat[,2, drop=TRUE], lwd = 2.5, col = colR)
lines(x = c(cmrt[1,1], camp[2]), y = c(cmrt[1,2], camp[1]), lwd = 2.5, col = colR)
plot(st_geometry(xy), cex = 1.75, col = colR2, pch = 20, add = TRUE)

# Add frame
x <- par('usr')
xR <- x[2]-x[1]
yR <- x[4]-x[3]
p <- .025
p2 <- .035
rect(x[1], x[3], x[2], x[3]+yR*p2, border = land, col = land)
rect(x[1], x[3], x[1]+xR*p, x[4], border = land, col = land)
rect(x[1], x[4], x[2], x[4]-yR*p2, border = land, col = land)
rect(x[2], x[3], x[2]-xR*p, x[4], border = land, col = land)

# Title
par(bg = land, mar = c(0,0,0,0))
plot0()
x <- par("usr")
rect(x[1],x[3],x[2],x[4], border = land, col = land)
text(x = 0, y = .4, adj = c(.5,.5), labels = 'Q   U   É   B   E   C       -       L   É   V   I   S', family = "newFont", cex = 1.8, col = col3, font = 4)

dev.off()
