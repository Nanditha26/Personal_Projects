#+eval=FALSE

#Loading Libraries
library(readr)
library(dplyr)
library(sf)
library(ggplot2)
library(ggspatial)
library(ggmap)
library(purrr)
library(leaflet)
library(raster)
library(janitor)
library(sfhotspot)

#Reading in crime data

crimes <- read.csv("C:\\Users\\NandithaPlakazhi\\OneDrive - Trilateral Research Ltd\\Documents\\GitHub\\Personal_Projects\\Dissertation\\2019-06-greater-manchester-street.csv")
crimes <- clean_names(crimes)

#Subsetting crime center data
city_centre_lsoa <- c("E01005128","E01033677",
                      "E01033681", "E01033682", "E01033683", 
                      "E01033653", "E01033654", "E01033656") #lsoa codes for Manchester city center

crimes_center <- crimes %>% filter(lsoa_code %in% city_centre_lsoa) #subsetting dataframe to include only locations in Manchester city center

crimes_center$lsoa_code <- as.factor(crimes_center$lsoa_code) #using as.factor to check levels

levels(crimes_center$lsoa_code) #double checking that subsetting was right

#make sf
crimes_center_sf <- st_as_sf(crimes_center,
                             coords = c("longitude", "latitude"),
                             crs = 4326)

#Creating synthetic jittered data
set.seed(260)
crimes_center_sf_jitter <- st_jitter(crimes_center_sf, 0.001)



#Adding back longitude and latitude columns to sf objects for leaflet
crimes_center_sf_jitter_separated <- crimes_center_sf_jitter %>%
  mutate(long = unlist(map(crimes_center_sf_jitter$geometry,1)),
         lat = unlist(map(crimes_center_sf_jitter$geometry,2))) #jittered separated data



#Getting House icon
houseicon_anim <- makeIcon(
  iconUrl = "C:\\Users\\NandithaPlakazhi\\OneDrive - Trilateral Research Ltd\\Documents\\GitHub\\Personal_Projects\\Dissertation\\house_icon_anim.png",
  iconWidth = 38, iconHeight = 38)

#Getting Letter icons
letter_icon_a <- makeIcon(
  iconUrl = "C:\\Users\\NandithaPlakazhi\\OneDrive - Trilateral Research Ltd\\Documents\\GitHub\\Personal_Projects\\Dissertation\\letter-a.png",
  iconWidth = 30, iconHeight = 30
)

letter_icon_b <- makeIcon(
  iconUrl = "C:\\Users\\NandithaPlakazhi\\OneDrive - Trilateral Research Ltd\\Documents\\GitHub\\Personal_Projects\\Dissertation\\letter-b.png",
  iconWidth = 30, iconHeight = 30
)

#Creating dataframe for house locations

house <- c("1", "2")
lat <- c(53.480180, 53.474461)
long <- c(-2.253732, -2.240334)

house_locations <- data.frame(house, lat, long)


#Creating dataframe for letter locations
letter_a <- c("a")
lat_a <- c(53.479672)
long_a <- c(-2.252582)

letter_location_a <- data.frame(letter_a, lat_a, long_a)

letter_b <- c("b")
lat_b <- c(53.473977)
long_b <- c(-2.239087)

letter_location_b <- data.frame(letter_b, lat_b, long_b)


#1 - Point Map on Leaflet
point_map <- leaflet(options = leafletOptions(zoomControl = FALSE,
                                              dragging = FALSE)) %>%
  addProviderTiles(providers$CartoDB.PositronNoLabels)%>% 
  addCircleMarkers(data = crimes_center_sf_jitter_separated,
                   ~long, ~lat,
                   color = "#4439c4",
                   stroke = FALSE,
                   fillOpacity = 0.8,
                   radius = 4) %>%
  addMarkers(data = house_locations,
             ~long, ~lat,
             icon = houseicon_anim) %>%
  addMarkers(data = letter_location_a,
             ~long_a, ~lat_a,
             icon = letter_icon_a) %>%
  addMarkers(data = letter_location_b,
             ~long_b, ~lat_b,
             icon = letter_icon_b)

point_map


#2 - Hexbinning on Leaflet

crimes_center_sf_jitter_separated_BNG <- st_transform(crimes_center_sf_jitter_separated, 27700)

hex_grid <- st_make_grid(crimes_center_sf_jitter_separated_BNG, square = FALSE,
                         cellsize = 80) %>%
  st_as_sf() %>% tibble::rownames_to_column()

crimes_per_hex <- crimes_center_sf_jitter_separated_BNG %>%
  st_join(hex_grid, ., left = FALSE) %>%
  count(rowname)

crimes_per_hex <- st_transform(crimes_per_hex, 4326)

bins_3 <- c(seq(from = 0, to = 80, by = 10))


pal_3 <- colorBin("plasma",
                  domain = crimes_per_hex$n, 
                  bins = bins_3)

hexbin_map <- leaflet(options = leafletOptions(zoomControl = FALSE,
                                               dragging = FALSE),
                      crimes_per_hex) %>%
  addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
  addPolygons(fillColor = ~pal_3(n), fillOpacity = 0.8, weight = 0) %>%
  addLegend("bottomright", pal = pal_3, values = ~n,
            title = "Number of Crimes",
            opacity = 1)%>%
  addMarkers(data = house_locations,
             ~long, ~lat,
             icon = houseicon_anim) %>%
  addMarkers(data = letter_location_a,
             ~long_a, ~lat_a,
             icon = letter_icon_a) %>%
  addMarkers(data = letter_location_b,
             ~long_b, ~lat_b,
             icon = letter_icon_b)

hexbin_map
  

#3 - Rectangular Binning on Leaflet

square_grid <- st_make_grid(crimes_center_sf_jitter_separated_BNG, square = TRUE,
                            cellsize = 80) %>%
  st_as_sf() %>% tibble::rownames_to_column()

crimes_per_square <- crimes_center_sf_jitter_separated_BNG %>%
  st_join(square_grid, ., left = FALSE) %>%
  count(rowname)

crimes_per_square <- st_transform(crimes_per_square, 4326)

bins_4 <- c(seq(from = 0, to = 60, by = 10))

pal_4 <- colorBin("plasma",
                  domain = crimes_per_square$n, 
                  bins = bins_4)

rectangularbin_map <- leaflet(options = leafletOptions(zoomControl = FALSE,
                                                       dragging = FALSE),
                              crimes_per_square) %>%
  addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
  addPolygons(fillColor = ~pal_4(n), fillOpacity = 0.8, weight = 0) %>%
  addLegend("bottomright", pal = pal_4, values = ~n,
            title = "Number of Crimes",
            opacity = 1)%>%
  addMarkers(data = house_locations,
             ~long, ~lat,
             icon = houseicon_anim) %>%
  addMarkers(data = letter_location_a,
             ~long_a, ~lat_a,
             icon = letter_icon_a) %>%
  addMarkers(data = letter_location_b,
             ~long_b, ~lat_b,
             icon = letter_icon_b)


rectangularbin_map

#4 - KDE Map on Leaflet

crimes_center_bng <- st_transform(crimes_center_sf_jitter, 27700) #british national grid projection

crimes_center_hotspots_kde <- hotspot_kde(crimes_center_bng, cell_size = 50)

crimes_center_hotspots_kde_wgs84 <- st_transform(crimes_center_hotspots_kde, 4326)

bins <- c(seq(from = 0, to = 350, by = 50))

pal <- colorBin("plasma", 
                domain = crimes_center_hotspots_kde_wgs84$kde, 
                bins = bins)



kde_map <- leaflet(options = leafletOptions(zoomControl = FALSE,
                                            dragging = FALSE),
                   crimes_center_hotspots_kde_wgs84 %>%
          filter(kde > 50)) %>% 
  addProviderTiles(providers$CartoDB.PositronNoLabels) %>% 
  addPolygons(fillColor = ~pal(kde), fillOpacity = 0.8, weight = 0) %>%
  addLegend("bottomright",
            colors =c("#0D0887", "#5402A3", "#8b0AA5", "#B93289", "#DB5C68",
                      "#F48849", "#FEBC2A", "#F0F921"),
            labels = c("Less", "","","","","","", "More"),
            title = "Density of Crime",
            opacity = 1)%>%
  addMarkers(data = house_locations,
             ~long, ~lat,
             icon = houseicon_anim) %>%
  addMarkers(data = letter_location_a,
             ~long_a, ~lat_a,
             icon = letter_icon_a) %>%
  addMarkers(data = letter_location_b,
             ~long_b, ~lat_b,
             icon = letter_icon_b)

kde_map

#5 - GISTAR Map on Leaflet

crimes_center_hotspots_gistar <- hotspot_gistar(crimes_center_bng, cell_size = 50)


crimes_center_hotspots_gistar_wgs84 <- st_transform(crimes_center_hotspots_gistar, 4326)

min(crimes_center_hotspots_gistar$gistar)
max(crimes_center_hotspots_gistar$gistar)

bin_2 <- c(seq(from = -2, to = 23, by = 5)) 

pal_2 <- colorBin("plasma", 
                domain = crimes_center_hotspots_gistar_wgs84$gistar, 
                bins = bin_2)

gistar_map <- leaflet(options = leafletOptions(zoomControl = FALSE,
                                               dragging = FALSE),
                      crimes_center_hotspots_gistar_wgs84 %>%
          filter(gistar > 0, pvalue <0.05)) %>% 
  addProviderTiles(providers$CartoDB.PositronNoLabels) %>% 
  addPolygons(fillColor = ~pal_2(gistar), fillOpacity = 0.8, weight = 0) %>%
  addLegend("bottomright",
            colors =c("#39359B", "#650286", "#D66B93", "#F5A563", "#F3FA4D"),                  title = "Density of Crime",
            labels = c("Less", "","","", "More"),
            opacity = 1)%>%
  addMarkers(data = house_locations,
             ~long, ~lat,
             icon = houseicon_anim) %>%
  addMarkers(data = letter_location_a,
             ~long_a, ~lat_a,
             icon = letter_icon_a) %>%
  addMarkers(data = letter_location_b,
             ~long_b, ~lat_b,
             icon = letter_icon_b)

gistar_map

#6 - Clustered Points Map on Leaflet

cluster_map <- leaflet(options = leafletOptions(zoomControl = FALSE,
                                                dragging = FALSE)) %>%
  addProviderTiles(providers$CartoDB.PositronNoLabels) %>% 
  addCircleMarkers(data = crimes_center_sf_jitter_separated,
                   ~long, ~lat,
                   color = "red",
                   stroke = FALSE,
                   fillOpacity = 0.5,
                   radius = 6,
                   clusterOptions = markerClusterOptions())%>%
  addMarkers(data = house_locations,
             ~long, ~lat,
             icon = houseicon_anim) %>%
  addMarkers(data = letter_location_a,
             ~long_a, ~lat_a,
             icon = letter_icon_a) %>%
  addMarkers(data = letter_location_b,
             ~long_b, ~lat_b,
             icon = letter_icon_b)

cluster_map  

#Printing Maps
point_map
hexbin_map
rectangularbin_map
kde_map
gistar_map
cluster_map


#Along networks
data("chicago")
plot(chicago)
d60 <- density.lpp(unmark(chicago), 60)
plot(d60)
plot(d60, style="width", adjust=2.5)
