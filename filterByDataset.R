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

## drop records outside Chile

dat$countryCode <-  countrycode(dat$countryCode, origin =  'iso2c', destination = 'iso3c')

dat <- dat[dat$countryCode == "CHL", ]

#dat <- dat[dat$family == "Apidae", ]

## check total number of records before and after digitization

nrow(dat)

nrow(dat[dat$datasetKey != "3bccb697-4ccc-4d46-848a-79cb06946e5c" 
         & dat$datasetKey != "ec4e6fa9-9bae-4e5b-af77-17cf0a1a6725", ])

## create new dataset without newly-digitized records

datPre <- dat[dat$datasetKey != "3bccb697-4ccc-4d46-848a-79cb06946e5c" 
              & dat$datasetKey != "ec4e6fa9-9bae-4e5b-af77-17cf0a1a6725", ]

## drop records without coords

dat <- dat[!is.na(dat$decimalLatitude), ]

dat <- dat[!is.na(dat$decimalLongitude), ]

datPre <- datPre[!is.na(datPre$decimalLatitude), ]

datPre <- datPre[!is.na(datPre$decimalLongitude), ]

#dat <- dat[dat$family == "Syrphidae", ]

## add identifier field for occAssess

## distinguish pre and post-digitization datasets using identifiers 

dat$identifier <- "post-digitization"

datPre$identifier <- "pre-digitization"

#dat$identifier <- dat$countryCode

#dat$identifier <- ifelse(dat$family == "Phyllostomidae", "Phyllostomidae", "Syrphidae")

## select columns needed for occAssess

## clean data for other spatial issues 

dat <- dat[, c("species", "decimalLongitude", "decimalLatitude", "year", "coordinateUncertaintyInMeters", "identifier", "countryCode")]

datPre <- datPre[, c("species", "decimalLongitude", "decimalLatitude", "year", "coordinateUncertaintyInMeters", "identifier", "countryCode")]


## clean the data

cleanDat <- clean_coordinates(x = dat,
                              lon = "decimalLongitude",
                              lat = "decimalLatitude",
                              value = "clean",
                              species = "species",
                              countries = "countryCode",
                              tests = c("capitals", "centroids", "equal","gbif", "institutions",
                                        "zeros", "countries"))

cleanDatPre <- clean_coordinates(x = datPre,
                              lon = "decimalLongitude",
                              lat = "decimalLatitude",
                              value = "clean",
                              species = "species",
                              countries = "countryCode",
                              tests = c("capitals", "centroids", "equal","gbif", "institutions",
                                        "zeros", "countries"))

## number of records with no spatial issues

nrow(cleanDat)

nrow(cleanDatPre)

## number of records with no spatial issues and identified to species level 

nrow(cleanDat[!is.na(cleanDat$species), ])

nrow(cleanDatPre[!is.na(cleanDatPre$species), ])

## number of species 

length(unique(cleanDat$species))

length(unique(cleanDatPre$species))

## create a combined dataset 

cleanDat <- cleanDat[, -7]

cleanDatPre <- cleanDatPre[, -7]

colnames(cleanDat) <- c("species", "x", "y", "year", "spatialUncertainty", "identifier")

colnames(cleanDatPre) <- c("species", "x", "y", "year", "spatialUncertainty", "identifier")

out <- rbind(cleanDat, cleanDatPre)

write.csv(out,
          "C:/Users/Rob.Lenovo-PC/Documents/surpass/Data/GBIF/07.04.21/preAndPostDigBeesChile.csv",
          row.names = F)


