library(tidyverse)
library(sp)
#library(rgdal)
#library(gdalUtils)
library(leaflet)
#library(rgeos)
library(RCurl)
library(rvest)

# Street names
names_url <- "http://www.puhdistussuunnitelmat.fi/helsinki/kadut.htm"

names <- read_html(names_url) %>% 
  html_nodes(xpath = "//li") %>% 
  html_text() %>% 
  as.data.frame()

names(names) <- "katunimi"

# Addresses
url <- "kartta.hel.fi/ws/geoserver/avoindata/wfs?version=1.1.0&request=GetFeature&typeName=avoindata:Helsinki_osoiteluettelo&outputFormat=csv"
filename <- "data.csv"
download.file(url = url, destfile = filename, method='curl')

# Keskiviiva-aineistossa ei ole väylien nimiä
# https://kartta.hel.fi/paikkatietohakemisto?id=86

res <- read.csv(filename, encoding = "UTF-8")

# https://raw.githubusercontent.com/rOpenGov/gisfin/master/R/helsinki_spatial.R
# https://gis.stackexchange.com/a/45266
coord.cols <- match(c("e", "n"), names(res))
p4s <- "+init=epsg:3879 +proj=tmerc +lat_0=0 +lon_0=25 +k=1 +x_0=25500000 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
sp <- sp::SpatialPointsDataFrame(coords=res[, coord.cols], proj4string=CRS(p4s), data=res[, -coord.cols])

sp_wgs84 <- spTransform(sp, CRS("+proj=longlat +datum=WGS84"))

street_df <- cbind(sp_wgs84@data[c(3,4,5)], sp_wgs84@coords)

street_df <- readRDS("street_df.RDS")
street_df$katunimi <- iconv(street_df$katunimi, from = "ISO-8859-1", to = "UTF-8")
street_df$gatan <- iconv(street_df$gatan, from = "ISO-8859-1", to = "UTF-8")

# Join with street names to get addresses that really are streets
# and not parks etc.
streets_names <- left_join(names, street_df)

streets_names_gr <- streets_names %>%
  group_by(katunimi) %>%
  arrange(osoitenumero) %>%
  arrange(katunimi, osoitenumero)

# In every group, add two new columns from values of the next row
# Fill in missing values at the end of the group, and if there is only one member in the group
streets_names_gr_coord <- streets_names_gr %>%
  group_by(katunimi) %>%
  mutate(e_next = lead(e),
         n_next = lead(n)) %>%
  do(fill(., c(e_next, n_next))) %>%
  mutate(e_next = ifelse(is.na(e_next), e, e_next),
         n_next = ifelse(is.na(n_next), n, n_next))

# Calculate distance between consecutive points along the same street
library(data.table)
library(geosphere)

setDT(streets_names_gr_coord)[, dist := distGeo(matrix(c(e, n), ncol = 2),
                                                matrix(c(e_next, n_next), ncol = 2))]

# Calculate total length of the streets. Short streets (and those w/o addresses) are just 0 m.
street_length <- streets_names_gr_coord %>%
  group_by(katunimi, gatan) %>%
  summarise(m = ceiling(sum(dist) * 100 / 1000)) %>%
  mutate(range = ifelse(m == 0, "Zero",
                        ifelse(m > 0 & m <= 500, "Under 500 m",
                               ifelse(m > 500 & m <= 1000, "500-1000 m",
                                      ifelse(m > 1000 & m <= 3000, "1-3 km",
                                             "Over 3 km")))))

# A few streets without coords so deleting them
street_length <- street_length[!is.na(street_length$m),]

first_last <- function(x) {
  bind_rows(slice(x, 1), slice(x, n()))
}

# Select 1st and last members of each group of streets
str_d <- street_df_coord %>%
  group_by(katunimi) %>%
  arrange(osoitenumero) %>%
  do(first_last(.)) %>%
  ungroup %>%
  select(katunimi, gatan, osoitenumero, e, n)

# Remove duplicates, i.e. when there was only 1 member in a group
str_d <- str_d[!duplicated(str_d),]
str_d$katunimi <- iconv(str_d$katunimi, from = "ISO-8859-1", to = "UTF-8")
str_d$gatan <- iconv(str_d$gatan, from = "ISO-8859-1", to = "UTF-8")

saveRDS(str_d, "coord.RDS")
saveRDS(street_length, "length.RDS")
