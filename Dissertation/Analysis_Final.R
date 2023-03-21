#+eval=FALSE

#Section 1 - Map Making

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

crimes <- read.csv("Datasets/2019-06-greater-manchester-street.csv")
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
  iconUrl = "Visuals/house_icon_anim.png",
  iconWidth = 38, iconHeight = 38)

#Getting Letter icons
letter_icon_a <- makeIcon(
  iconUrl = "Visuals/letter-a.png",
  iconWidth = 30, iconHeight = 30
)

letter_icon_b <- makeIcon(
  iconUrl = "Visuals/letter-b.png",
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
gistar_map
kde_map
cluster_map

#Section 2 - Analysis

library(readr)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(ggpubr)
library(tidyr)
library(gmodels)
library(vcdExtra)
library(pwr)
library(Rcmdr)
library(sjPlot)
library(janitor)
library(vcd)
library(summarytools)
library(qualvar)
library(gt)
library(gtExtras)


#reading in data - version 3, 122 responses

data <- read_csv("Datasets/Version_3_123 resp.csv") %>% clean_names()

#sub-setting data with entries
data_new <- data %>% select(20:37) %>% slice(3:125)

data_complete <- data_new[complete.cases(data_new [ , c("q1_1", "q2_1", "q3_1", "q4_1", "q5_1", "q6_1")]), ]


questions <- c("q1_1", "q2_1", "q3_1", "q4_1", "q5_1", "q6_1")

data_new_1 <- data_complete %>% pivot_longer(all_of(questions), names_to = "Question", values_to = "Answer") %>% select(Question, Answer)

#Modal Responses, VR, and IQV

#Point Map
n_total_1 <- data_new_1 %>% filter(Question == "q1_1") %>%
  summarize(n=n())

n_mode_1 <- data_new_1 %>% filter(Question == "q1_1") %>% 
  filter(Answer == "Option B") %>%
  summarize (n=n())

proportion_mode_1 <- n_mode_1/n_total_1

vr_1 <- 1-proportion_mode_1

vr_1

data_point_map <- data_new_1 %>% filter(Question == "q1_1")

IQV_1 <- as.vector(table(data_point_map$Answer))

DM(IQV_1)

#Rectangular Binning
n_total_2 <- data_new_1 %>% filter(Question == "q2_1") %>%
  summarize(n=n())

n_mode_2 <- data_new_1 %>% filter(Question == "q2_1") %>% 
  filter(Answer == "Option B") %>%
  summarize (n=n())

proportion_mode_2 <- n_mode_2/n_total_2

vr_2 <- 1-proportion_mode_2

vr_2

data_rect_map <- data_new_1 %>% filter(Question == "q2_1")

IQV_2 <- as.vector(table(data_rect_map$Answer))

DM(IQV_2)

#Hexagonal binning
n_total_3 <- data_new_1 %>% filter(Question == "q3_1") %>%
  summarize(n=n())

n_mode_3 <- data_new_1 %>% filter(Question == "q3_1") %>% 
  filter(Answer == "Option A") %>%
  summarize (n=n())

proportion_mode_3 <- n_mode_3/n_total_3

vr_3 <- 1-proportion_mode_3

vr_3

data_hex_map <- data_new_1 %>% filter(Question == "q3_1")

IQV_3 <- as.vector(table(data_hex_map$Answer))

DM(IQV_3)

#KDE
n_total_4 <- data_new_1 %>% filter(Question == "q4_1") %>%
  summarize(n=n())

n_mode_4 <- data_new_1 %>% filter(Question == "q4_1") %>% 
  filter(Answer == "Option A") %>%
  summarize (n=n())

proportion_mode_4 <- n_mode_4/n_total_4

vr_4 <- 1-proportion_mode_4

vr_4

data_kde_map <- data_new_1 %>% filter(Question == "q4_1")

IQV_4 <- as.vector(table(data_kde_map$Answer))

DM(IQV_4)

#GISTAR
n_total_5 <- data_new_1 %>% filter(Question == "q5_1") %>%
  summarize(n=n())

n_mode_5 <- data_new_1 %>% filter(Question == "q5_1") %>% 
  filter(Answer == "Option A") %>%
  summarize (n=n())

proportion_mode_5 <- n_mode_5/n_total_5

vr_5 <- 1-proportion_mode_5

vr_5

data_gistar_map <- data_new_1 %>% filter(Question == "q5_1")

IQV_5 <- as.vector(table(data_gistar_map$Answer))

DM(IQV_5)

#Cluster Map
n_total_6 <- data_new_1 %>% filter(Question == "q6_1") %>%
  summarize(n=n())

n_mode_6 <- data_new_1 %>% filter(Question == "q6_1") %>% 
  filter(Answer == "Option B") %>%
  summarize (n=n())

proportion_mode_6 <- n_mode_6/n_total_6

vr_6 <- 1-proportion_mode_6

vr_6

data_cluster_map <- data_new_1 %>% filter(Question == "q6_1")

IQV_6 <- as.vector(table(data_cluster_map$Answer))

DM(IQV_6)

#Descriptive stats - age

table(data_complete$q8_1)
class(data_complete$q8_1)

data_complete$q8_1_ordered <- factor(data_complete$q8_1, levels = c("Under 18", "18 - 24", "25 - 34", "35 - 44", "45 - 54","55 - 64", "65 - 74"))

levels(data_complete$q8_1_ordered)
table(data_complete$q8_1_ordered)

age_histogram <- ggplot(data = data_complete, mapping = aes(x = q8_1_ordered))+
  geom_histogram(stat = "count", fill = "#0096C7")+
  labs(x = "Age of Respondent",
       y = "Number of Respondents",
       title = "Age Distribution of Respondents")+
  theme_solarized()

age_histogram

is.na(data_complete$q8_2)

sex_histogram <- ggplot(data = data_complete, mapping = aes(x = q8_2))+
  geom_histogram(stat = "count", fill = "#0096C7")+
  labs(x = "Sex of Respondent",
       y = "Number of Respondents",
       title = "Sex Distribution of Respondents")+
  theme_solarized()

sex_histogram

table(data_complete$q8_3)
class(data_complete$q8_3)

data_complete$q8_3_ordered <- factor(data_complete$q8_3,
                                     levels = c("Extremely competent",
                                                "Somewhat competent",
                                                "Neither competent nor incompetent",
                                                "Somewhat incompetent", 
                                                "Extremely incompetent"))

tech_histogram <- ggplot(data = data_complete, mapping = aes(x = q8_3_ordered))+
  geom_histogram(stat = "count", fill = "#0096C7")+
  labs(x = "Self-Described Level of Technological Competency of Respondent",
       y = "Number of Respondents",
       title = "Self-Described Level of Technological Competency of Respondent")+
  theme_solarized()+
  theme(axis.text.x = element_text(angle = 35, vjust = 1, hjust = 1))


tech_histogram

ggarrange(age_histogram, sex_histogram, tech_histogram,
          labels = c("A", "B", "C"),
          ncol = 2, nrow = 2)

#Trying to find change in responses
data_new_2 <- data_complete %>% select(q1_1, q2_1, q3_1, q4_1, q5_1, q6_1)

data_new_3 <- data_new_2 %>% mutate(
  change_2 = case_when(q1_1 == q2_1 ~ "no_change",
                       TRUE ~ "change"),
  change_3 = case_when(q1_1 == q3_1 ~ "no_change",
                       TRUE ~ "change"),
  change_4 = case_when(q1_1 == q4_1 ~ "no_change",
                       TRUE ~ "change"),
  change_5 = case_when(q1_1 == q5_1 ~ "no_change",
                       TRUE ~ "change"),
  change_6 = case_when(q1_1 == q6_1 ~ "no_change",
                       TRUE ~ "change")) %>%
  select(change_2, change_3, change_4, change_5, change_6)

table(data_new_2$change_2) 
table(data_new_2$change_3) 
table(data_new_2$change_4) 
table(data_new_2$change_5) 
table(data_new_2$change_6) 

data_new_4 <- data_new_2 %>% mutate(
  change_hex = case_when(q2_1 == q3_1 ~ "no_change",
                         TRUE ~ "change"),
  change_kde = case_when(q3_1 == q4_1 ~ "no change",
                         TRUE ~ "change"),
  change_gi = case_when(q4_1 == q5_1 ~ "no change",
                        TRUE ~ "change"),
  change_cluster = case_when(q5_1 == q6_1 ~ "no change",
                             TRUE ~ "change")) %>%
  select(change_hex, change_kde, change_gi, change_cluster)

table(data_new_4$change_hex)
table(data_new_4$change_kde)
table(data_new_4$change_gi)
table(data_new_4$change_cluster)



group_1 <- data_new_1 %>%
  group_by(Question) %>%
  summarize("Option_A" = sum(na.rm = T, Answer == "Option A"),
            "Option_B" = sum(na.rm = T, Answer == "Option B"),
            "No_Preference" = sum(na.rm = T, Answer == "I don't have a preference"))

group_1 <- group_1 %>% rename(Map_Type = "Question")

group_1$Question <- as.factor(group_1$Map_Type)

group_1$Question <- recode_factor(group_1$Map_Type, `q1_1` = "Point Map", 
                                  `q2_1` = "Rectangular Binning",
                                  `q3_1` = "Hexagonal Binning",
                                  `q4_1` = "Kernel Density Estimate",
                                  `q5_1` = "GISTAR",
                                  `q6_1` = "Cluster Map")



group_1 %>% gt() %>%
  tab_header(
    title = "Location Preference (between House A and House B) by Map Type"
  ) %>%
  gt_theme_espn()



options <- c("Option_A", "Option_B", "No_Preference")

group_2 <- group_1 %>% pivot_longer(all_of(options), names_to = "Option", values_to = "Number")

group_2$Option <- factor(group_2$Option, levels = c("Option_A", "Option_B", "No_Preference"))

bar_plot <- ggplot(data = group_2, aes(fill = Option, x = Question, y = Number))+
  geom_bar(position = "stack", stat = "identity")

bar_plot


#contingency table for chi-square test
with(data_new_1, CrossTable(as.factor(Question),
                            as.factor(Answer), prop.chisq = FALSE, format = c("SPSS")))

#to get only row percent 
with(data_new_1, CrossTable(as.factor(Question),
                            as.factor(Answer), prop.chisq = FALSE, prop.c = FALSE, prop.t = FALSE, format = c("SPSS")))

#chi-square test CrossTab
with(data_new_1, CrossTable(as.factor(Question),
                            as.factor(Answer), 
                            expected = TRUE, prop.c = FALSE, prop.t = FALSE, 
                            format = c("SPSS"))) %>%
  
  #Only chi-square values
  chisq.test(data_new_1$Question, data_new_1$Answer)

#Adjusted Standard Residuals
with(data_new_1, CrossTable(as.factor(Question),
                            as.factor(Answer), 
                            expected = FALSE, prop.c = FALSE, prop.t = FALSE, asresid = TRUE,prop.r = FALSE,
                            format = c("SPSS")))


#plot for chi-square
mosaic(~Question + Answer,
       direction = c("v", "h"),
       data = data_new_1,
       shade = TRUE)


#Power Analysis
prop_tab <- prop.table(mytable)
ES.w2(prop_tab)
dim(data_new)

pwr.chisq.test(w = ES.w2(prop_tab), N = 91, df = 10, sig.level = 0.05)

                                    
#Logistic Regression
data_new_5 <- data_new_1 %>% filter(!(Answer == "I don't have a preference"))

data_new_5$Answer <- droplevels(data_new_5$Answer)
table(data_new_5$Answer)
data_new_5$Answer <- factor(data_new_5$Answer, 
                            levels = c("Option B", "Option A"))

data_new_5$Question <- recode_factor(data_new_5$Question, `q1_1` = "Point Map", 
                                     `q2_1` = "Rectangular Binning",
                                     `q3_1` = "Hexagonal Binning",
                                     `q4_1` = "Kernel Density Estimate",
                                     `q5_1` = "GISTAR",
                                     `q6_1` = "Cluster Map")

fit_1 <- glm(Answer ~ Question, data = data_new_5, family = "binomial")
summary(fit_1)
tab_model(fit_1)
plot_model(fit_1)

#model chi-squared
with(fit_1, null.deviance - deviance)
with(fit_1, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE)) #The p-value is smaller than  α= 0.05, so we conclude that our model is a better fit than a model with no IVs (or predictors).

#pseudo likelihood ratio r squared
with(fit_1, (null.deviance - deviance)/null.deviance)

par(mfrow = c(2, 2))
plot(fit_1)

#Map preference
table(data_complete$q7_1)

data_complete$q7_1 <- recode_factor(data_complete$q7_1, `Map 1` = "Point Map", 
                                    `Map 2` = "Rectangular Binning",
                                    `Map 3` = "Hexagonal Binning",
                                    `Map 4` = "Kernel Density Estimate",
                                    `Map 5` = "GISTAR",
                                    `Map 6` = "Cluster Map")

preference_histogram <- ggplot(data = data_complete, mapping = aes(x = q7_1))+
  geom_histogram(stat = "count", fill = "#0096C7")+
  labs(x = "Map Type",
       y = "Number of Respondents",
       title = "Type of Map Preferred by Respondents")+
  theme_solarized()+
  theme(axis.text.x = element_text(angle = 35, vjust = 1, hjust = 1))

preference_histogram


#Point in Polygon
manchester_ward <- st_read("Datasets/manchester_ward.geojson") #manchester ward

#filtering out only city center area

df1 <- manchester_ward %>%
  filter(wd16nm == "City Centre")

cc <- st_transform(df1, 4326)

st_crs(cc) == st_crs(houses_sf)#TRUE

plot(st_geometry(cc), border = "grey")
plot(st_geometry(houses_sf), col = "red", add = T)

houses_BNG <- st_transform(houses_sf, 27700) 

buffer_400 <- st_buffer(houses_BNG, 400) 

#try plotting
plot(st_geometry(buffer_400))
plot(st_geometry(houses_BNG), add = T) 

#convert buffer layer back to crs = 4326
buffer_WGS84 <- st_transform(buffer_400, 4326) 

crime_per_house <- crimes_center_sf_jitter %>% 
  st_join(buffer_WGS84, ., left = FALSE) %>% 
  count(house) 


leaflet(crime_per_house) %>% 
  addProviderTiles(providers$CartoDB.PositronNoLabels)%>% 
  addPolygons(label = ~as.character(n),
              popup = ~as.character(n),
              popupOptions = popupOptions(autoClose = FALSE, closeOnClick = FALSE))%>%
  addMarkers(data = house_locations,
             ~long, ~lat,
             icon = houseicon_anim) %>%
  addMarkers(data = letter_location_a,
             ~long_a, ~lat_a,
             icon = letter_icon_a) %>%
  addMarkers(data = letter_location_b,
             ~long_b, ~lat_b,
             icon = letter_icon_b)
