#+eval=FALSE

library(readr)
library(janitor)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(tmap)
library(ggspatial)
library(readxl)
library(spatstat)
library(sf)
library(purrr)
library(rjson)
library(RColorBrewer)
library(leaflet)
# install.packages("osmdata")
library(osmdata)
library(maptools)
library(raster)
library(spdep)
library(sp)
library(spatialreg)
library(sjPlot)


#Month-wise crime data from 2018-07 to 2019-06
data <- list.files("data", full.names = TRUE)
data2 <- lapply(data, read.csv, header = TRUE, 
                stringsAsFactors = FALSE) 
#first two lines call data as a list

data <- bind_rows(data2) %>% clean_names() #transforms the data in the list to a dataframe
rm(data2) #removes unnecessary objects from your environment

#exploring crime_types
count <- table(data$crime_type)

ordered_count <- sort(count)

ordered_count

#filtering thefts from vehicle
vehicle <- data %>% filter(crime_type == "Vehicle crime")

#plotting the vehicle crimes by month
vehicle_per_month <- vehicle %>%
  dplyr :: select(month) %>%
  group_by(month) %>%
  summarise(count = n())

mean(vehicle_per_month$count)

figure_1 <- ggplot(data = vehicle_per_month, mapping = aes(x = month, y = count, group = 1))+
  geom_line(size = 1, color = "red")+
  geom_point()+
  theme_solarized()+
  labs(title = "Vehicle Crimes in Greater Manchester",
       subtitle = "From July, 2018 to June, 2019",
       x = "Month",
       y = "Number of incidents reported")

figure_1

#Making sf
vehicle_sf <- st_as_sf(vehicle,
                       crs = 4326,
                       coords = c("longitude", "latitude"),
                       agr = "constant")

#Reading in shapefile of England LSOAs
shp_name <- "Datasets/BoundaryData/england_lsoa_2011.shp"
manchester_lsoa <- st_read(shp_name)

manchester_lsoa_WGS <- st_transform(manchester_lsoa, 4326)

vehicle_mc <- st_intersects(manchester_lsoa_WGS, vehicle_sf)

vehicle_mc <- vehicle_sf[unlist(vehicle_mc),]

vehicle_per_lsoa <- vehicle_mc %>%
  st_join(manchester_lsoa_WGS, ., left = FALSE) %>%
  count(code)

vehicle_per_lsoa <- vehicle_per_lsoa %>% rename(vehicle = n)

# thematic map
tmap_mode("view")

figure_2 <- tm_shape(vehicle_per_lsoa)+
  tm_polygons("vehicle", id = "code", 
              title = "Number of Vehicle 
    Crimes")

tmap_mode("plot")

figure_2

#Mapping rates instead of counts using census data

#reading in census data
census_lsoa_m <- read_csv("Datasets/Census_Data/Data_UNIT_URESPOP.csv")

#We don't need the first two rows
census_lsoa_m <- slice(census_lsoa_m, 3:284)

#extracting only the columns we need
census_lsoa_m <- dplyr :: select(census_lsoa_m, GEO_CODE, F996:F323339)

#turning columns 2 to 9 into numeric variables
census_lsoa_m[2:9] <- lapply(census_lsoa_m[2:9], as.numeric)  

#renaming the variable columns
census_lsoa_m <- rename(census_lsoa_m, 
                        tothouse = F996, 
                        notdepr = F997,
                        depriv1 = F998,
                        depriv2 = F999,
                        depriv3 = F1000,
                        depriv = F1001,
                        respop = F2384,
                        wkdpop = F323339)

#joinging vehicle_per_lsoa and census data

vehicle_lsoa_census <- left_join(vehicle_per_lsoa, census_lsoa_m,
                                 by = c("code" = "GEO_CODE"))

#calculating crime rates
vehicle_lsoa_census <- mutate(vehicle_lsoa_census, 
                              crimr1 = (vehicle/respop)*100000,
                              crimr2 = (vehicle/wkdpop)*100000)

tmap_mode("plot")

vehicle_count_map <- tm_shape(vehicle_lsoa_census)+
  tm_polygons("vehicle", palette = "Blues", title = "Vehicle Crime Count")+
  tm_layout(panel.labels = "Vehicle Crime Count", legend.position = c("right", "bottom"),
            legend.title.size = 0.8, legend.text.size = 0.5,
            panel.label.height = 3)

wkdpop_pop_map <- tm_shape(vehicle_lsoa_census) + 
  tm_polygons("wkdpop", palette= "Greens", title = "Residential 
population") +
  tm_layout(panel.labels = "Workday population", legend.position = c("right", "bottom"),
            legend.title.size = 0.8, legend.text.size = 0.5,
            panel.label.height = 3)

vehicle_rate_map <- tm_shape(vehicle_lsoa_census) + 
  tm_polygons("crimr2", palette= "Reds", title = "Vehicle Crime Rate") +
  tm_layout(panel.labels = "Vehicle Crime 
            per 100,00 population", legend.position = c("right", "bottom"),
            legend.title.size = 0.8, legend.text.size = 0.5,
            panel.label.height = 3)

figure_3 <- tmap_arrange(vehicle_count_map, wkdpop_pop_map, vehicle_rate_map)

figure_3

#Now we have decided that MCC is our primary area of interest

#Read in geojson file with manchester wards
manchester_ward <- st_read("Datasets/manchester_ward.geojson")

#filtering out only city center area

df1 <- manchester_ward %>%
  filter(wd16nm == "City Centre")

cc <- st_transform(df1, 4326)

st_crs(vehicle_per_lsoa) == st_crs(cc) #TRUE

cc_intersects <- st_intersects(cc, vehicle_per_lsoa)

cc_vehicle <- vehicle_per_lsoa[unlist(cc_intersects),]

#Plotting again, only MCC
tmap_mode("view")

figure_4 <- tm_shape(cc_vehicle)+
  tm_polygons("vehicle", style = "jenks", id = "code", palette = "Reds", title = "Vehicle Crime Counts", alpha = 1)+
  tm_layout(main.title = "Vehicle Crime counts", main.title.size = 1.3 ,
            legend.position = c("right", "top"), legend.title.size = 0.8)+
  tm_view(leaflet.options = popupOptions(autoClose = FALSE, closeOnClick = FALSE))

figure_4

#Spatial Randomness, here remember we are using original sf data, not grouped by lsoa as we want to see each incident as a point.

#Reading in shapefile of MCC lsoas
cc_lsoas <- st_read("Datasets/cc_lsoas.geojson")

vehicle_cc <- st_intersects(cc_lsoas, vehicle_sf)

vehicle_cc <- vehicle_sf[unlist(vehicle_cc),]

ggplot() + 
  geom_sf(data = cc_lsoas) + 
  geom_sf(data = vehicle_cc) + 
  theme(legend.position = "none",
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank())

#Testing for spatial randomness
cc_proj <- st_transform(cc_lsoas, 27700)

window <- as.owin(cc_proj)

vehicle_cc_BNG <- st_transform(vehicle_cc, 27700)

sf_vehicle_cc_coords <- matrix(unlist(vehicle_cc_BNG$geometry), ncol = 2, byrow = T)

vehicle_ppp <- ppp(x = sf_vehicle_cc_coords[,1], y = sf_vehicle_cc_coords[,2],
                   window = window, check = T)

plot(vehicle_ppp)

any(duplicated(vehicle_ppp)) #TRUE

sum(multiplicity(vehicle_ppp) > 1) #2697 duplicated points

#Jittering points to work against multiplicity
jitter_vehicle <- rjitter(vehicle_ppp, retry = TRUE, nsim = 1, drop = TRUE)

plot(jitter_vehicle, main = "Jittered Points of Incidences of Vehicle Crime")

quadrat.test(jitter_vehicle, nx = 3, ny = 2) #p-value well below 0.05 so we can reject null hypothesis that the shoplifting is randomly distributed in the city center

#Kernel Density

#Uniform Function
unif_f <- function(x,y){dnorm(x)*dnorm(y)}

dmap1 <- density.ppp(jitter_vehicle, sigma = bw.diggle(jitter_vehicle),edge=T,
                     kernel = unif_f, scalekernel = TRUE)

r1 <- raster(dmap1)

# r1[r1 < 0.0001 ] <- NA


#make sure we have right CRS, which in this case is British National Grid
epsg27700 <- "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +towgs84=446.448,-125.157,542.06,0.15,0.247,0.842,-20.489 +units=m +no_defs"

crs(r1) <- sp::CRS(epsg27700)

pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(r1),
                    na.color = "transparent")


#and then make map!
leaflet() %>% 
  addTiles() %>%
  addRasterImage(r1, colors = pal, opacity = 0.7) %>%
  addLegend(pal = pal, values = values(r1),
            title = "Vehicle Crimes map")

#Spatial Autocorrelation
vehicle_m <- as(vehicle_per_lsoa, "Spatial")
#Generate list of neighbours using the Queen criteria
w <- poly2nb(vehicle_m, row.names=vehicle_m$lsoa_code)
#Generate list with weights using row standardisation
ww <-  nb2listw(w, style='W')
summary(ww)

#Looking at Moran's scatteplot
moran.plot(vehicle_m$vehicle, ww) #clear positive autocorrelation

#checking global Moran's I
moran.mc(vehicle_m$vehicle, ww, nsim = 99999) #Moran's I value  = 0.53 and p-value lower than 0.05

#Trying to decompose global measure using local Moran

locm_bm <- localmoran(vehicle_m$vehicle, ww)
summary(locm_bm)

#scaling variable of interest
vehicle_m$s_vehicle <- scale(vehicle_m$vehicle) %>% as.vector()

#create a spatial lag variable and save it to a new column
vehicle_m$lag_s_vehicle <- lag.listw(ww, vehicle_m$s_vehicle)

summary(vehicle_m$s_vehicle)
summary(vehicle_m$lag_s_vehicle)

vehicle_m <- st_as_sf(vehicle_m) %>% 
  mutate(quad_sig = ifelse(vehicle_m$s_vehicle > 0 & 
                             vehicle_m$lag_s_vehicle > 0 & 
                             locm_bm[,5] <= 0.05, 
                           "high-high",
                           ifelse(vehicle_m$s_vehicle <= 0 & 
                                    vehicle_m$lag_s_vehicle <= 0 & 
                                    locm_bm[,5] <= 0.05, 
                                  "low-low", 
                                  ifelse(vehicle_m$s_vehicle > 0 & 
                                           vehicle_m$lag_s_vehicle <= 0 & 
                                           locm_bm[,5] <= 0.05, 
                                         "high-low",
                                         ifelse(vehicle_m$s_vehicle <= 0 & 
                                                  vehicle_m$lag_s_vehicle > 0 & 
                                                  locm_bm[,5] <= 0.05,
                                                "low-high", 
                                                "non-significant")))))

#Plot this
qtm(vehicle_m, fill = "quad_sig", fill.title = "LISA")

#Point in Polygon

#reading in data from the web
lic_prem <- read_csv("https://www.manchester.gov.uk/open/download/downloads/id/169/licensed_premises.csv")

#filtering using pattern matching using grepl function
city_center_prems <- lic_prem %>%
  filter(grepl("M1 ", POSTCODE)) #We are using "M1 " instead of "M1" because we dont want any M11, M13, M12 etc.

#functions built by Reka to get latitude and longitude 

geocode_addys_getlng <- function(x){
  
  geocode_result <- fromJSON(readLines(paste0("http://api.getthedata.com/postcode/",gsub(" ", "", x))))
  return(ifelse(!is.null(geocode_result$data$longitude), geocode_result$data$longitude, NA))
}

geocode_addys_getlat <- function(x){
  
  geocode_result <- fromJSON(readLines(paste0("http://api.getthedata.com/postcode/",gsub(" ", "", x))))
  return(ifelse(!is.null(geocode_result$data$latitude), geocode_result$data$latitude, NA))
}

#extracting the longitude and latitude using the above function and adding it to the dataframe

city_center_prems <- city_center_prems %>%
  mutate(longitude = map_chr(POSTCODE, geocode_addys_getlng),
         latitude = map_chr(POSTCODE, geocode_addys_getlat)) #map_chr allows the function we created to input each observation of the column specified, here POSTCODE


city_center_prems$longitude <- as.numeric(city_center_prems$longitude)

city_center_prems$latitude <- as.numeric(city_center_prems$latitude)


#4.3 Spatial Operations 

#making the dataframe an sf object
cc_spatial_prem <- st_as_sf(city_center_prems,
                            coords = c("longitude", "latitude"),
                            crs = 4326,
                            agr = "constant", na.fail = FALSE)

#check whether ward outline and premises both have same crs
st_crs(cc_lsoas) == st_crs(cc_spatial_prem) #TRUE

#Performing spatial operations to remove all licensed premises locations that lie outside the city center polygon

#intersection
cc_intersects_2 <- st_intersects(cc_lsoas, cc_spatial_prem)

#subsetting only those that intersect
cc_intersects_2 <- cc_spatial_prem[unlist(cc_intersects_2),] #pay attention to the comma!!

#plotting this data 
plot(st_geometry(cc_lsoas), border = "grey")
plot(st_geometry(cc_intersects_2), col = "red", add = T)

#Adding buffers
prem_BNG <- st_transform(cc_intersects_2, 27700)
prem_buffer_50 <- st_buffer(prem_BNG, 50)
prem_buffer_100 <- st_buffer(prem_BNG, 100)

#try plotting
plot(st_geometry(prem_buffer_50))
plot(st_geometry(prem_BNG), add = T)

#convert buffer layer back to crs = 4326
buffer_WGS84 <- st_transform(prem_buffer_50, 4326)

#visualize differently by counting the number of points inside the buffer polygon

vehicle_per_prem <- vehicle_sf %>% 
  st_join(buffer_WGS84, ., left = FALSE) %>% 
  count(PREMISESNAME)

pal <- colorBin("RdPu", domain = vehicle_per_prem$n, bins = 5, pretty = TRUE)

?colorBin

leaflet(vehicle_per_prem) %>% 
  addTiles() %>% 
  addPolygons(fillColor = ~pal(n), fillOpacity = 0.8,
              weight = 1, opacity = 1, color = "black",
              label = ~as.character(PREMISESNAME),
              popup = ~as.character(PREMISESNAME),
              popupOptions = popupOptions(autoClose = FALSE, closeOnClick = FALSE)) %>% 
  addLegend(pal = pal, values = ~n, opacity = 0.7, 
            title = 'Vehicle crimes', position = "bottomleft")

#Point in Polygon - Parking Spaces
mcc_parking <- opq("Manchester, UK") %>%
  add_osm_feature(key = "amenity",
                  value = "parking") %>%
  osmdata_sf()

mcc_parking_spaces <- mcc_parking$osm_polygons

st_crs(cc_lsoas) == st_crs(mcc_parking_spaces)

parking_intersection <- st_intersects(cc_lsoas, mcc_parking_spaces)

parking_intersection <- mcc_parking_spaces[unlist(parking_intersection),]

plot(st_geometry(cc_lsoas))
plot(st_geometry(parking_intersection), col = "red", add = T)

#Adding buffers
parking_BNG <- st_transform(parking_intersection, 27700)
parking_buffer_50 <- st_buffer(parking_BNG, 50)
parking_buffer_100 <- st_buffer(parking_BNG, 100)

#try plotting
plot(st_geometry(parking_buffer_50))
plot(st_geometry(parking_BNG), add = T)

buffer_wgs84_2 <- st_transform(parking_buffer_50, 4326)


vehicle_per_parking <- vehicle_sf %>% 
  st_join(buffer_wgs84_2, ., left = FALSE) %>% 
  count(osm_id)

pal <- colorBin("RdPu", domain = vehicle_per_parking$n, bins = 5, pretty = TRUE)

?colorBin

leaflet(vehicle_per_parking) %>% 
  addTiles() %>% 
  addPolygons(fillColor = ~pal(n), fillOpacity = 0.8,
              weight = 1, opacity = 1, color = "black",
              label = ~as.character(osm_id),
              popup = ~as.character(osm_id),
              popupOptions = popupOptions(autoClose = FALSE, closeOnClick = FALSE)) %>% 
  addLegend(pal = pal, values = ~n, opacity = 0.7, 
            title = 'Vehicle crimes', position = "bottomleft")



#Regression
census_2 <- read_csv("Datasets/census_2.csv")

census_2 <- census_2 %>% slice(2:283)

census_2 <- census_2 %>% dplyr::select(GEO_CODE, F998:F359336)

#renaming the variable columns
census_2 <- rename(census_2,
                        depriv1 = F998,
                        depriv2 = F999,
                        depriv3 = F1000,
                        depriv4 = F1001,
                        male_inactive_16_24 = F359325,
                        male_inactive_25_34 = F359336,
                        population = F2384)

census_2[2:8] <- lapply(census_2[2:8], as.numeric)

vehicle_lsoa_census_2 <- left_join(vehicle_per_lsoa, census_2,
                                 by = c("code" = "GEO_CODE"))

#calculating crime rates
vehicle_lsoa_census_2 <- mutate(vehicle_lsoa_census_2, 
                              crimr1 = (vehicle/population)*100000,
                              male_unemployment_16_24_rate = (male_inactive_16_24/population)*100,
                              male_unemployment_25_34_rate = (male_inactive_25_34/population)*100,
                              depriv_rate = (depriv1/population)*100)

#Run OLS Regression
fit_1 <- lm(vehicle ~ male_inactive_16_24 + male_inactive_25_34+ depriv1, data =  vehicle_lsoa_census_2)

summary(fit_1)

vehicle_lsoa_census_2$res_fit_1 <- residuals(fit_1)
vehicle_lsoa_census_2$fitted_fit_1 <- fitted(fit_1)
vehicle_lsoa_census_2$sd_breaks <- scale(vehicle_lsoa_census_2$res_fit_1)[,1]

summary(vehicle_lsoa_census_2$sd_breaks)

my_breaks <- c(-8, -4, -2, 0, 2, 4, 8)

tm_shape(vehicle_lsoa_census_2) + 
  tm_fill("sd_breaks", title = "Residuals", style = "fixed", breaks = my_breaks, palette = "-RdBu") +
  tm_borders(alpha = 0.1) +
  tm_layout(main.title = "Residuals", main.title.size = 0.7 ,
            legend.position = c("right", "bottom"), legend.title.size = 0.8)

vehicle_lsoa_census_2_sp <- as(vehicle_lsoa_census_2, "Spatial")

w_2 <- poly2nb(vehicle_lsoa_census_2_sp, row.names = vehicle_lsoa_census_2_sp$FIPSNO)
summary(w_2)

wm_2 <- nb2mat(w_2, style = 'B')

rwm_2 <- mat2listw(wm_2, style='W')

lm.morantest(fit_1, rwm_2, alternative="two.sided")

lm.LMtests(fit_1, rwm_2, test = c("LMerr","LMlag","RLMerr","RLMlag","SARMA"))

#Spatial Lag model
fit_1_lag <- lagsarlm(vehicle ~ male_inactive_16_24 + male_inactive_25_34+ depriv1, data =  vehicle_lsoa_census_2, rwm_2)

summary(fit_1_lag)
tab_model(fit_1_lag)

W <- as(rwm_2, "CsparseMatrix")
trMC <- trW(W, type="MC")

im<-impacts(fit_1_lag, tr = trMC, R=100)

sums<-summary(im,  zstats=T)

data.frame(sums$res)

#p-values
data.frame(sums$pzmat)
