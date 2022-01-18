setwd("C:/Users/Rob.Lenovo-PC/Documents/surpass/")

library(occAssess)
library(devtools)
library(raster)
library(gridExtra)
library(ggplot2)
library(colorRamps)

x <- read.csv("F:/GBIF_downloads_08.11.2021/bees_clean.csv")

y <- read.csv("F:/GBIF_downloads_08.11.2021/bats_clean.csv")

z <- read.csv("F:/GBIF_downloads_08.11.2021/hoverflies_clean.csv")

load("F:/GBIF_downloads_08.11.2021/hummingbirds_clean.rdata")

spDat <- rbind(x[,1:6], y[,1:6], z[,1:6], dat[,1:6])

periods <- list(1950:1959, 1960:1969, 1970:1979, 1980:1989, 1990:1999, 2000:2009, 2010:2019)

countries = c("Brazil", "Argentina", "Chile", "Colombia", "Ecuador", "Bolivia", "Uruguay", 
              "Venezuela",  "Paraguay", "Peru", "Suriname", "Mexico", "Panama", "Costa Rica", "Honduras",
              "Nicaragua", "Belize", "El Salvador", "Guatemala", "Cuba", "Dominican Republic") 

map <- assessSpatialCov2(dat = spDat,
                 res = 1,
                 logCount = T,
                 periods = periods,
                 countries = countries,
                 output = "density",
                 minPeriods = 5,
                 shp=NULL)

map$Anthophila

png("Phyllostomidae.png", width = 16, height = 12, units = "in", res = 600)
print(map$Phyllostomidae +
        ggtitle("Phyllostomidae")
)
dev.off()
citation("sparta")


