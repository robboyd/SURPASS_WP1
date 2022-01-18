##### clean GBIF data #####

library(countrycode)
library(CoordinateCleaner)
library(dplyr)
library(ggplot2)
library(rgbif)
library(sp)
library(rnaturalearthdata)
library(rgdal)
library(rgeos)

## load GBIF data

#dat <- read.csv("F:/GBIF_downloads_08.11.2021/bats_raw.csv",
#                na.strings = c("", "NA"))

#files <- list.files("F:/GBIF_downloads_08.11.2021/", pattern = "hummingbirds_", full.names = T)

#dat <- lapply(files, read.csv)

#dat <- do.call("rbind", dat)

load("F:/humm_redownload/humm_all.rdata")

dat <- out 

dat$countryCode <- countrycode(dat$countryCode, origin =  'iso2c', destination = 'iso3c')

#nrow(dat[dat$countryCode == "CHL" & dat$datasetKey != "3bccb697-4ccc-4d46-848a-79cb06946e5c" 
#         & dat$datasetKey != "ec4e6fa9-9bae-4e5b-af77-17cf0a1a6725", ])

## drop records without coords

dat <- dat[!is.na(dat$decimalLatitude), ]

dat <- dat[!is.na(dat$decimalLongitude), ]

#dat <- dat[dat$family == "Syrphidae", ]

## add identifier field for occAssess

dat$identifier <- "Trochilidae"

#dat$identifier <- dat$countryCode

#dat$identifier <- ifelse(dat$family == "Phyllostomidae", "Phyllostomidae", "Syrphidae")

## select columns needed for occAssess

dat <- dat[, c("species", "decimalLongitude", "decimalLatitude", "year", "coordinateUncertaintyInMeters", "identifier", "countryCode")]

## drop data not in South and Central America 

shp <- raster::shapefile("C:/Users/Rob.Lenovo-PC/Documents/surpass/Data/South America country boundaries/ne_110m_admin_0_countries/ne_110m_admin_0_countries.shp")

shp <- shp[shp$CONTINENT %in% c("South America", "North America"), ]

shp <- shp[!shp$NAME %in% c("United States of America", "Canada", "Greenland"), ]

coordinates(dat) <- ~decimalLongitude+decimalLatitude

proj4string(dat) <- CRS(proj4string(shp))

dat <- dat[!is.na(over(dat, shp))[,1], ]

dat <- data.frame(dat)

plot(shp)

points(dat[,2:3])

## clean the data

dat <- clean_coordinates(x = dat,
                              lon = "decimalLongitude",
                              lat = "decimalLatitude",
                              value = "clean",
                              species = "species",
                              countries = "countryCode",
                              tests = c("capitals", "centroids", "equal","gbif", "institutions",
                                        "zeros", "countries"))

nrow(dat)

length(unique(dat$species))

dat <- dat[, -7]

colnames(dat) <- c("species", "x", "y", "year", "spatialUncertainty", "identifier")

write.csv(cleanDat,
          "F:/GBIF_downloads_08.11.2021/bees_clean.csv",
          row.names = F)

save(dat, file = "F:/GBIF_downloads_08.11.2021/hummingbirds_clean_18_01_2022.rdata")


