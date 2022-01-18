
library(occAssess)
library(devtools)
library(raster)
library(gridExtra)
library(ggplot2)
library(colorRamps)
library(RColorBrewer)

x <- read.csv("F:/GBIF_downloads_08.11.2021/bees_clean.csv")

y <- read.csv("F:/GBIF_downloads_08.11.2021/bats_clean.csv")

z <- read.csv("F:/GBIF_downloads_08.11.2021/hoverflies_clean.csv")

load("F:/GBIF_downloads_08.11.2021/hummingbirds_clean_18_01_2022.rdata")

spDat <- rbind(x[,1:6], y[,1:6], z[,1:6], dat[,1:6])

periods <- list(1950:1959, 1960:1969, 1970:1979, 1980:1989, 1990:1999, 2000:2009, 2010:2019)

countries = c("Brazil", "Argentina", "Chile", "Colombia", "Ecuador", "Bolivia", "Uruguay", 
              "Venezuela", "Guayana", "Paraguay", "Peru", "Suriname", "Mexico")


clim <- raster::stack(list.files("C:/Users/Rob.Lenovo-PC/Documents/surpass/Data/bio/bio/",
                                 full.names = T)) # my local version (19 raster layers)

#shp <- raster::shapefile("C:/Users/Rob.Lenovo-PC/Documents/surpass/Data/South America country boundaries/South America country boundaries/data/commondata/data0/southamerica_adm0.shp")

#shp <- sp::spTransform(shp, raster::crs(clim))

shp <- raster::shapefile("C:/Users/Rob.Lenovo-PC/Documents/surpass/Data/South America country boundaries/ne_110m_admin_0_countries/ne_110m_admin_0_countries.shp")

shp <- shp[shp$CONTINENT %in% c("South America", "North America"), ]

shp <- shp[!shp$NAME %in% c("United States of America", "Canada", "Greenland"), ]

clim <- raster::crop(clim, raster::extent(shp))

clim <- raster::mask(clim, shp)

### Functions

rarityBias <- assessRarityBias(dat = spDat,
                           periods = periods,
                           res = 1,
                          prevPerPeriod = T,
                          species = "species",
                          year = "year",
                          x = "x",
                          y = "y",
                          spatialUncertainty = "spatialUncertainty",
                          identifier = "identifier")



nRec <- assessRecordNumber(dat = spDat,
                           periods = periods,
                           species = "species",
                           year = "year",
                           x = "x",
                           y = "y",
                           spatialUncertainty = "spatialUncertainty",
                           identifier = "identifier",
                           normalize = T)


nSpec <- assessSpeciesNumber(dat = spDat,
                           periods = periods,
                           species = "species",
                           year = "year",
                           x = "x",
                           y = "y",
                           spatialUncertainty = "spatialUncertainty",
                           identifier = "identifier")


map <- assessSpatialCov2(dat = spDat,
                 res = 1,
                 logCount = T,
                 periods = periods,
                 countries = countries,
                 output = "nPeriods",
                 minPeriods = 5,
                 shp=NULL)


countries = c("Brazil", "Argentina", "Chile", "Colombia", "Ecuador", "Bolivia", "Uruguay", 
              "Venezuela",  "Paraguay", "Peru", "Suriname", "Mexico", "Panama", "Costa Rica", "Honduras",
              "Nicaragua", "Belize", "El Salvador", "Guatemala", "Cuba", "Dominican Republic")

#mask <- raster::raster("C:/Users/Rob.Lenovo-PC/Documents/surpass/Data/occAssessData/climSA.asc")

mask <- clim[[1]]

mask <- disaggregate(mask, fact = 5)

spatBias <- assessSpatialBias(dat = spDat,
                              periods = periods,
                              mask = mask,
                              nSamps = 15,
                              degrade = TRUE,
                              species = "species",
                              year = "year",
                              x = "x",
                              y = "y",
                              spatialUncertainty = "spatialUncertainty",
                              identifier = "identifier")


myCol <- rgb(255,255,255, max = 255, alpha = 0, names = "blue50")

png("mainFig.png", width = 12, height = 7.4, units = "in", res = 500)
grid.arrange(
  nRec$plot + xlab("") + ggtitle("A") + scale_color_manual(values=brewer.pal(4, "Dark2")) + theme(legend.position = "none") +
    ylab("Number of records 
    (normalized)"),
  nSpec$plot + xlab("") + ggtitle("B") + scale_color_manual(values=brewer.pal(4, "Dark2")) + theme(legend.position = "none") + 
    aes(y = val/c(rep(5000, 7), rep(160, 7), rep(2000, 7), rep(361, 7))) + ylab("Prop. species"), 
  rarityBias$plot + ggtitle("C") + scale_color_manual(values=brewer.pal(4, "Dark2")) + 
    theme(legend.position = "none") + 
    xlab(""), 
  spatBias$plot + ggtitle("D") + scale_color_manual(values=brewer.pal(4, "Dark2")) + 
    scale_fill_manual(values = brewer.pal(4, "Dark2")) + 
    theme(legend.position = "none") +
    xlab("Decade"),
  map[[1]] +  ggtitle("E) Anthophila") + 
    ggplot2::scale_fill_manual(values = brewer.pal(7, "BuPu"), na.value = myCol, name = "", 
                               na.translate = FALSE) +
    theme(legend.position = "none") + xlab("") + xlab("") + ylab(""),
  map[[3]] +  ggtitle("F) Syrphidae") + 
    ggplot2::scale_fill_manual(values = brewer.pal(7, "BuPu"), na.value = myCol, name = "", 
                               na.translate = FALSE) +
    theme(legend.position = "none") + xlab("") + xlab("") + ylab(""),
  map[[2]] +  ggtitle("G) Phyllostomidae") + 
    ggplot2::scale_fill_manual(values = brewer.pal(7, "BuPu"), na.value = myCol, name = "", 
                               na.translate = FALSE) +
    theme(legend.position = "none") + xlab("") + ylab(""),
  map[[4]] +  ggtitle("H) Trochilidae") + 
    ggplot2::scale_fill_manual(values = brewer.pal(7, "BuPu"), na.value = myCol, name = "", 
                               na.translate = FALSE) +
    theme(legend.position = c(0.2, 0.4),
          legend.background = element_rect(colour ="black")) + xlab("") + ylab(""),
  widths = c(1, 1.2, 1.2),
  layout_matrix = rbind(c(1, 5, 7),
                        c(2, 5, 7),
                        c(3, 6, 8),
                        c(4, 6, 8))
)
dev.off()




