##### occAssess GBIF bee data in Chile #####

library(occAssess)
library(raster)
library(sp)
library(ggplot2)
library(cowplot)
library(gridExtra)
library(ggplot2)

## load occurrence data 

spDat <- read.csv("C:/Users/Rob.Lenovo-PC/Documents/surpass/Data/GBIF/07.04.21/preAndPostDigBeesChile.csv")

nrow(spDat[spDat$identifier == "post-digitization" & !is.na(spDat$species), ])

length(unique(spDat$species[spDat$identifier == "pre digitization" & !is.na(spDat$species)]))

## choose periods 

periods <- list(1951:1960, 1961:1970, 1971:1980, 1981:1990, 1991:2000, 2001:2010, 2011:2019)

## check temporal variation in number of records 

nRec <- assessRecordNumber(dat = spDat,
                           periods = periods)

png("nRec.png", width = 8, height = 5, units = "in", res = 500)
nRec$plot 
dev.off()

## check number of species recorded 

nSpec <- assessSpeciesNumber(dat = spDat,
                             periods = periods)

png("nSpec.png", width = 8, height = 5, units = "in", res = 500)
nSpec$plot 
dev.off()

## check proportion identified to species level 

propID <- assessSpeciesID(dat = spDat,
                          periods = periods,
                          type = "proportion")

png("propID.png", width = 8, height = 5, units = "in", res = 500)
propID$plot + 
  ylim(c(0, 1))
dev.off()

## check for rarity bias 

taxBias <- assessRarityBias(dat = spDat,
                            periods = periods,
                            res = 1,
                            prevPerPeriod = TRUE)

png("rarityBias.png", width = 8, height = 5, units = "in", res = 500)
taxBias$plot +
  ylim(c(0,1))
dev.off()

## check spatial coverage 

png("spatialCov.png", width = 8, height = 5, units = "in", res = 500)
cov <- assessSpatialCov2(dat = spDat,
                 periods = periods,
                 res = 1,
                 logCount = TRUE,
                 countries = c("Chile"),
                 output = "nPeriods")
dev.off()

## check spatial uncertainty 

png("spatUncert.png", width = 8, height = 3, units = "in", res = 500)
spatUncert <- assessSpatialUncertainty(dat = spDat,
                                       periods = periods)

print(spatUncert$plot) + ylim(c(0,5000))
dev.off()

## check for temporal variation in spatial uncertainty 

assessSpatialUncertainty(dat = spDat,
                         periods = periods)

spatBias$plot

## check for spatial bias 

mask <- raster::raster("C:/Users/Rob.Lenovo-PC/Documents/surpass/Data/occAssessData/climSA.asc")

shp <- raster::shapefile("C:/Users/Rob.Lenovo-PC/Documents/surpass/Data/South America country boundaries/South America country boundaries/data/commondata/data0/southamerica_adm0.shp")

shp <- shp[shp$COUNTRY == "CHILE", ]

shp <- spTransform(shp, crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

mask <- raster::mask(mask, shp)

mask <- disaggregate(mask, fact =2)

spatBias <- assessSpatialBias(dat = spDat,
                              periods = periods,
                              mask = mask,
                              nSamps = 30,
                              degrade = TRUE)

png("spatBias.png", width = 8, height = 5, units = "in", res = 500)
spatBias$plot +
  ylim(c(0,1))
dev.off()

## check for environmental bias 

clim <- raster::stack(list.files("C:/Users/Rob.Lenovo-PC/Documents/surpass/Data/bio/bio/",
                                 full.names = T))

spDat <- cbind(spDat, raster::extract(clim, spDat[, c("x", "y")]))

envBias <- assessEnvBias(dat = spDat,
                         periods = periods,
                         nEnvVar = 19,
                         frame = TRUE,
                         frame.type = "norm")

png("envBias.png", width = 8, height = 5, units = "in", res = 500)
envBias$plot
dev.off()

cov$`pre-digitization`

png("digComp.png", width = 13, height = 7.4, units = "in", res = 500)
  grid.arrange(
  nRec$plot + xlab("") + ggtitle("A") + scale_color_manual(values=matlab.like2(2)),
  nSpec$plot + xlab("") + ggtitle("B") + scale_color_manual(values=matlab.like2(2)), 
  taxBias$plot + ggtitle("C") + scale_color_manual(values=matlab.like2(2)), 
  spatBias$plot + ggtitle("D") + scale_color_manual(values=matlab.like2(2)) + scale_fill_manual(values = matlab.like2(2)),
  cov[[2]] + xlim(c(-77.5, -65)) + ggtitle("E) pre digitization"),
  cov[[1]] + xlim(c(-77.5, -65)) + ggtitle("F) post digitization"),
  widths = c(1, 1.2, 1.2),
  layout_matrix = rbind(c(1, 5, 6),
                        c(2, 5, 6),
                        c(3, 5, 6),
                        c(4, 5, 6))
)
dev.off()

?grid.arrange
