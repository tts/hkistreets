library(tidyverse)
library(sp)
library(rgdal)
library(gdalUtils)
library(leaflet)
library(RCurl)
library(rvest)
library(data.table)
library(geosphere)

# Get Helsinki street names
names_url <- "http://www.puhdistussuunnitelmat.fi/helsinki/kadut.htm"

names <- read_html(names_url) %>% 
  html_nodes(xpath = "//li") %>% 
  html_text() %>% 
  as.data.frame()

names(names) <- "katunimi"

# Get Helsinki addresses with coordinates
url <- "kartta.hel.fi/ws/geoserver/avoindata/wfs?version=1.1.0&request=GetFeature&typeName=avoindata:Helsinki_osoiteluettelo&outputFormat=csv"
filename <- "data.csv"
download.file(url = url, destfile = filename, method='curl')
res <- read.csv(filename, encoding = "UTF-8")

# https://raw.githubusercontent.com/rOpenGov/gisfin/master/R/helsinki_spatial.R
# https://gis.stackexchange.com/a/45266
coord.cols <- match(c("e", "n"), names(res))
p4s <- "+init=epsg:3879 +proj=tmerc +lat_0=0 +lon_0=25 +k=1 +x_0=25500000 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
sp <- sp::SpatialPointsDataFrame(coords=res[, coord.cols], proj4string=CRS(p4s), data=res[, -coord.cols])
sp_wgs84 <- spTransform(sp, CRS("+proj=longlat +datum=WGS84"))
street_df <- cbind(sp_wgs84@data[c(3,4,5)], sp_wgs84@coords)

# Join with street names to get addresses that really are streets
# and not parks and other places
streets_names <- left_join(names, street_df)

# Calculate distances.
# First, group by street name. Then, in every group, add two new columns from values of the next row.
# Lastly, fill in missing values at the end of the group, and if there was only one member in the group.
streets_names_gr <- streets_names %>%
  group_by(katunimi) %>%
  arrange(osoitenumero) %>%
  arrange(katunimi, osoitenumero)

streets_names_gr_coord <- streets_names_gr %>%
  group_by(katunimi) %>%
  mutate(e_next = lead(e),
         n_next = lead(n)) %>%
  do(fill(., c(e_next, n_next))) %>%
  mutate(e_next = ifelse(is.na(e_next), e, e_next),
         n_next = ifelse(is.na(n_next), n, n_next))

# https://stackoverflow.com/a/37747560
setDT(streets_names_gr_coord)[, dist := distGeo(matrix(c(e, n), ncol = 2),
                                                matrix(c(e_next, n_next), ncol = 2))]

# Select 1st and last members of each group of streets.
# The idea is to render only two points on the map per street.
first_last <- function(x) {
  bind_rows(slice(x, 1), slice(x, n()))
}

str_d <- streets_names_gr_coord %>%
  group_by(katunimi) %>%
  arrange(osoitenumero) %>%
  do(first_last(.)) %>%
  ungroup() %>%
  select(katunimi, gatan, osoitenumero, e, n)

# Remove duplicates, i.e. when there was only one member in a group
str_d <- str_d[!duplicated(str_d),]
# A few streets without coords so deleting them
str_d <- str_d[!is.na(str_d$e),]

# Calculate total length of the streets (based on addresses). 
# Streets with only one address are just 0 m.
street_length <- streets_names_gr_coord %>%
  group_by(katunimi, gatan) %>%
  summarise(m = ceiling(sum(dist) * 100 / 1000)) %>%
  mutate(range = ifelse(m == 0, "Zero",
                        ifelse(m > 0 & m <= 500, "Under 500 m",
                               ifelse(m > 500 & m <= 1000, "500-1000 m",
                                      ifelse(m > 1000 & m <= 3000, "1-3 km",
                                             "Over 3 km")))))

street_length <- street_length[!is.na(street_length$m),]

# Save objects for the web app
saveRDS(str_d, "coord.RDS")
saveRDS(street_length, "length.RDS")
