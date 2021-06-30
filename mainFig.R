
library(occAssess)
library(devtools)
library(raster)
library(gridExtra)
library(ggplot2)
library(colorRamps)

x <- read.csv("C:/Users/Rob.Lenovo-PC/Documents/surpass/Data/GBIF/07.04.21/cleanedData.csv")

y <- read.csv("C:/Users/Rob.Lenovo-PC/Documents/surpass/Data/GBIF/07.04.21/cleanedDataAnthophila.csv")

z <- read.csv("C:/Users/Rob.Lenovo-PC/Documents/surpass/Data/GBIF/07.04.21/cleanedDataHummingBirds.csv")

spDat <- rbind(x, y, z)

periods <- list(1950:1959, 1960:1969, 1970:1979, 1980:1989, 1990:1999, 2000:2009, 2010:2019)

countries = c("Brazil", "Argentina", "Chile", "Colombia", "Ecuador", "Bolivia", "Uruguay", 
              "Venezuela", "Guayana", "Paraguay", "Peru", "Suriname", "Mexico")

#### Periods

clim <- raster::stack(list.files("C:/Users/Rob.Lenovo-PC/Documents/surpass/Data/bio/bio/",
                                 full.names = T)) # my local version (19 raster layers)

#shp <- raster::shapefile("C:/Users/Rob.Lenovo-PC/Documents/surpass/Data/South America country boundaries/South America country boundaries/data/commondata/data0/southamerica_adm0.shp")

#shp <- sp::spTransform(shp, raster::crs(clim))

shp <- raster::shapefile("C:/Users/Rob.Lenovo-PC/Documents/surpass/Data/South America country boundaries/ne_110m_admin_0_countries/ne_110m_admin_0_countries.shp")

shp <- shp[shp$CONTINENT %in% c("South America", "North America"), ]

shp <- shp[!shp$NAME %in% c("United States of America", "Canada", "Greenland"), ]

clim <- raster::crop(clim, raster::extent(shp))

clim <- raster::mask(clim, shp)

## exract climate data at coordinates of occurrence data 

envDat <- raster::extract(clim, spDat[, c("x", "y")])

## extract background environmental data 

backgroundEnvDat <- raster::sampleRandom(clim, size = 10000,
                                         xy = F)

### Functions

length(unique(spDat$species[spDat$identifier == "Trochilidae" & !is.na(spDat$species)]))

rarityBias <- assessRarityBias(dat = spDat,
                           periods = periods,
                           res = 1,
                          prevPerPeriod = F)

pRarity <- rarityBias$plot + 
  ylim(c(0, 1))

pRarity

nRec <- assessRecordNumber(dat = spDat,
                           periods = periods)

pNRec <- nRec$plot 

pNRec

nSpec <- assessSpeciesNumber(dat = spDat,
                           periods = periods)

pNSpec <- nSpec$plot 

pNSpec

propID <- assessSpeciesID(dat = spDat,
                           periods = periods,
                           type = "proportion")

pPropID<- propID$plot + 
  ylim(c(0,1)) +
  ggtitle("C)")

pPropID

map <- assessSpatialCov2(dat = spDat,
                 res = 1,
                 logCount = T,
                 periods = periods,
                 countries = countries,
                 output = "nPeriods",
                 minPeriods = 5,
                 shp=NULL)
map$Anthophila

countries = c("Brazil", "Argentina", "Chile", "Colombia", "Ecuador", "Bolivia", "Uruguay", 
              "Venezuela",  "Paraguay", "Peru", "Suriname", "Mexico", "Panama", "Costa Rica", "Honduras",
              "Nicaragua", "Belize", "El Salvador", "Guatemala", "Cuba", "Dominican Republic")

pMap1 <- map$Phyllostomidae +
  xlim(c(-90, -30)) + 
  ggtitle("Ei)")
assessSpatialCov
pMap2 <- map$Syrphidae +
  xlim(c(-90, -30)) +
  ggtitle("Eii)")

pMap3 <- map$Apoidae +
  xlim(c(-90, -30)) +
  ggtitle("Eiii)")

pMap4 <- map$Trochilidae +
  xlim(c(-90, -30)) +
  ggtitle("Eiv)")

#mask <- raster::raster("C:/Users/Rob.Lenovo-PC/Documents/surpass/Data/occAssessData/climSA.asc")

mask <- clim[[1]]

spatBias <- assessSpatialBias(dat = spDat,
                              periods = periods,
                              mask = mask,
                              nSamps = 30,
                              degrade = TRUE)

pSpatBias <- spatBias$plot + 
  ylim(c(0, 1)) 

pSpatBias
envBias <- assessEnvBias(dat = spDat,
                         envDat = envDat,
                         backgroundEnvDat = backgroundEnvDat,
                         periods = periods,
                         xPC = 1,
                         yPC = 2)


pEnvDat <- envBias 

png("allPlots.png", width = 16, height = 12, units = "in", res = 600)
grid.arrange(pNRec, pNSpec, pPropID, pRarity, pMap1, pMap2, pSpatBias, pEnvDat, ncol = 3)
dev.off()

png("rarity.png", width = 7, height = 4, units = "in", res = 600)
print(pRar)
dev.off()

png("mapAnthophila.png", width = 10, height = 10, units = "in", res = 600)
print(map$Anthophila)
dev.off()

png("mainFig.png", width = 13, height = 7.4, units = "in", res = 500)
grid.arrange(
  nRec$plot + xlab("") + ggtitle("A") + scale_color_manual(values=matlab.like2(4)),
  nSpec$plot + xlab("") + ggtitle("B") + scale_color_manual(values=matlab.like2(4)), 
  rarityBias$plot + ggtitle("C") + scale_color_manual(values=matlab.like2(4)), 
  spatBias$plot + ggtitle("D") + scale_color_manual(values=matlab.like2(4)) + scale_fill_manual(values = matlab.like2(4)),
  map[[3]] +  ggtitle("E) Anthophila"),
  map[[2]] +  ggtitle("F) Syrphidae"),
  map[[1]] +  ggtitle("G) Phyllostomidae"),
  map[[4]] +  ggtitle("H) Trochilidae"),
  widths = c(1, 1.2, 1.2),
  layout_matrix = rbind(c(1, 5, 7),
                        c(2, 5, 7),
                        c(3, 6, 8),
                        c(4, 6, 8))
)
dev.off()


x <- assessTaxonBias(dat = spDat,
                periods = periods, 
                minRecs = 1)


