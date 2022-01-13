##### occAssess GBIF bee data in Chile #####

library(occAssess)
library(raster)
library(sp)
library(ggplot2)
library(cowplot)
library(gridExtra)
library(ggplot2)
library(colorRamps)

## load occurrence data 

spDat <- read.csv("C:/Users/Rob.Lenovo-PC/Documents/surpass/Data/GBIF/07.04.21/preAndPostDigBeesChile.csv")

nrow(spDat[spDat$identifier == "post-digitization", ])

nrow(spDat[spDat$identifier == "post-digitization" & !is.na(spDat$species), ])

nrow(spDat[spDat$identifier == "pre-digitization", ])

nrow(spDat[spDat$identifier == "pre-digitization" & !is.na(spDat$species), ])

## choose periods 

periods <- list(1951:1960, 1961:1970, 1971:1980, 1981:1990, 1991:2000, 2001:2010, 2011:2019)

## check temporal variation in number of records 

nRec <- assessRecordNumber(dat = spDat,
                           periods = periods,
                           species = "species",
                           identifier = "identifier",
                           x = "x", 
                           y = "y",
                           year = "year",
                           spatialUncertainty = "spatialUncertainty")


## check number of species recorded 

nSpec <- assessSpeciesNumber(dat = spDat,
                             periods = periods,
                             species = "species",
                             identifier = "identifier",
                             x = "x", 
                             y = "y",
                             year = "year",
                             spatialUncertainty = "spatialUncertainty")


## check for rarity bias 

taxBias <- assessRarityBias(dat = spDat,
                            periods = periods,
                            res = 1,
                            prevPerPeriod = TRUE,
                            species = "species",
                            identifier = "identifier",
                            x = "x", 
                            y = "y",
                            year = "year",
                            spatialUncertainty = "spatialUncertainty")


## check spatial coverage 

cov <- assessSpatialCov2(dat = spDat,
                         periods = periods,
                         res = 1,
                         logCount = TRUE,
                         countries = c("Chile"),
                         output = "nPeriods")


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
                              nSamps = 15,
                              degrade = TRUE,
                              species = "species",
                              identifier = "identifier",
                              x = "x", 
                              y = "y",
                              year = "year",
                              spatialUncertainty = "spatialUncertainty")


## combine plots

myCol <- rgb(255,255,255, max = 255, alpha = 0, names = "blue50")

png("digCompv2.png", width = 12, height = 7.4, units = "in", res = 500)
grid.arrange(
  nRec$plot + xlab("") + ggtitle("A") + scale_color_manual(values=c("purple", "blue")) + theme(legend.position = "none"),
  nSpec$plot + xlab("") + ggtitle("B") + scale_color_manual(values=c("purple", "blue")) + theme(legend.position = "none") + aes(y = val/464) + ylab("Prop. species recorded"), 
  taxBias$plot + ggtitle("C") + scale_color_manual(values=c("purple", "blue")) + theme(legend.position = "none") + xlab(""), 
  spatBias$plot + ggtitle("D") + scale_color_manual(values=c("purple", "blue")) + scale_fill_manual(values = c("purple", "blue")) + xlab("Decade") + theme(legend.position = "none"),
  cov[[2]] + xlim(c(-77.5, -65)) + ggtitle("E) pre digitization") + xlab("") + ylab("") + 
    ggplot2::scale_fill_manual(values = brewer.pal(7, "BuPu"), na.value = myCol, name = "", 
                               na.translate = FALSE) +
    theme(legend.position = "none"),
  cov[[1]] + xlim(c(-77.5, -65)) + ggtitle("F) post digitization") +xlab("") + ylab("") + 
    ggplot2::scale_fill_manual(values = brewer.pal(7, "BuPu"), na.value = myCol, name = "", 
                               na.translate = FALSE) +
    theme(legend.position = c(0.8, 0.3), 
          legend.background = element_rect(colour ="black")),
  widths = c(1.4, 1, 1),
  layout_matrix = rbind(c(1, 5, 6),
                        c(2, 5, 6),
                        c(3, 5, 6),
                        c(4, 5, 6))
)
dev.off()

