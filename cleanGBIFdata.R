##### clean GBIF data #####

library(countrycode)
library(CoordinateCleaner)
library(dplyr)
library(ggplot2)
library(rgbif)
library(sp)
library(rnaturalearthdata)

## load GBIF data

dat <- read.csv("C:/Users/Rob.Lenovo-PC/Documents/surpass/Data/GBIF/07.04.21/bees.csv",
                na.strings = c("", "NA"))
unique(dat$family)
dat$countryCode <-  countrycode(dat$countryCode, origin =  'iso2c', destination = 'iso3c')


dat <- dat[dat$family == "Apidae", ]

nrow(dat[dat$countryCode == "CHL", ])

nrow(dat[dat$countryCode == "CHL" & dat$datasetKey != "3bccb697-4ccc-4d46-848a-79cb06946e5c" 
         & dat$datasetKey != "ec4e6fa9-9bae-4e5b-af77-17cf0a1a6725", ])

## drop records without coords

dat <- dat[!is.na(dat$decimalLatitude), ]

dat <- dat[!is.na(dat$decimalLongitude), ]

#dat <- dat[dat$family == "Syrphidae", ]

## add identifier field for occAssess

dat$identifier <- "Anthophila"

#dat$identifier <- dat$countryCode

#dat$identifier <- ifelse(dat$family == "Phyllostomidae", "Phyllostomidae", "Syrphidae")

## select columns needed for occAssess

dat <- dat[, c("species", "decimalLongitude", "decimalLatitude", "year", "coordinateUncertaintyInMeters", "identifier", "countryCode")]

## clean the data

cleanDat <- clean_coordinates(x = dat,
                              lon = "decimalLongitude",
                              lat = "decimalLatitude",
                              value = "clean",
                              species = "species",
                              countries = "countryCode",
                              tests = c("capitals", "centroids", "equal","gbif", "institutions",
                                        "zeros", "countries"))

nrow(cleanDat[cleanDat$countryCode == "CHL", ])

length(unique(cleanDat$species))

cleanDat <- cleanDat[, -7]

colnames(cleanDat) <- c("species", "x", "y", "year", "spatialUncertainty", "identifier")

write.csv(cleanDat,
          "C:/Users/Rob.Lenovo-PC/Documents/surpass/Data/GBIF/07.04.21/cleanedDataAnthophila.csv",
          row.names = F)


