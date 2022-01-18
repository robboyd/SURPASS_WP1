#### fit models to estimate temporal trends in species' distributions ####

library(occAssess)
library(raster)
library(reshape2)
library(plyr)
library(dplyr)
library(ggplot2)
library(gridExtra)

# Load sparta

library(sparta)

## setup model grid

shp <- raster::shapefile("C:/Users/Rob.Lenovo-PC/Documents/surpass/Data/South America country boundaries/South America country boundaries/data/commondata/data0/southamerica_adm0.shp")

shp <- shp[shp$COUNTRY == "CHILE", ]

shp <- spTransform(shp, crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

grid <- raster("C:/Users/Rob.Lenovo-PC/Documents/surpass/Data/maskLayers/mask_CHL.asc")

grid <- crop(grid, shp)

grid <- aggregate(grid, fact = 6)

## load species data 

dat <- read.csv("C:/Users/Rob.Lenovo-PC/Documents/surpass/Data/GBIF/07.04.21/preAndPostDigBeesChile.csv",
                na.strings = c("", NA))

dat <- dat[-which(is.na(dat$species)), ]

pre <- dat[dat$identifier == "pre-digitization", ]

post <- dat[dat$identifier == "post-digitization", ]

## format species data for use with sparta models

formatDat <- function(data, x) { 
  cell <- extract(grid, data.frame(x = data$x[x],
                                   y = data$y[x]),
                  cellnumbers = TRUE)
  
  cell <- cell[1]
  
  data.frame(species = data$species[x],
             cell = cell,
             year = data$year[x])
  }

fDatPre <- lapply(1:nrow(pre),
                  formatDat,
                  data = pre)               

fDatPre <- do.call("rbind", fDatPre)

fDatPost <- lapply(1:nrow(post),
                  formatDat,
                  data = post)               

fDatPost <- do.call("rbind", fDatPost)

## drop data from Chilean island outside of domain

fDatPost <- fDatPost[-which(is.na(fDatPost$cell)), ]

periods <- list(1950:1959, 1960:1969, 1970:1979, 1980:1989, 1990:1999,2000:2010, 2011:2019)

fDatPre$Period <- NA

fDatPost$Period <- NA

for (i in 1: length(periods)) {
  
  fDatPre$Period <- ifelse(fDatPre$year %in% periods[[i]], i, fDatPre$Period)
  
  fDatPost$Period <- ifelse(fDatPost$year %in% periods[[i]], i, fDatPost$Period)
  
}


#fDatPre <- fDatPre[-which(is.na(fDatPre$cell)), ]

#fDatPost <- fDatPost[-which(is.na(fDatPost$cell)), ]

## fit models 
# first the reporting rate model with list length as a covariate and a random site intercept 

rrPre <- reportingRateModel(taxa = fDatPre$species,
                                 site = fDatPre$cell,
                                 time_period = as.numeric(fDatPre$Period),
                                 list_length = TRUE,
                                 site_effect = TRUE)

rrPost <- reportingRateModel(taxa = fDatPost$species,
                            site = fDatPost$cell,
                            time_period = as.numeric(fDatPost$Period),
                            list_length = TRUE,
                            site_effect = TRUE)

## check for models that didn't converge 

nrow(rrPre[!is.nan(rrPre$error_message), ])

nrow(rrPost[!is.nan(rrPost$error_message), ])

## then a simpler model without the random site intercept 

rr2Pre <- reportingRateModel(taxa = fDatPre$species,
                              site = fDatPre$cell,
                              time_period = as.numeric(fDatPre$Period),
                              list_length = TRUE,
                              site_effect = FALSE)

rr2Post <- reportingRateModel(taxa = fDatPost$species,
                              site = fDatPost$cell,
                              time_period = as.numeric(fDatPost$Period),
                              list_length = TRUE,
                              site_effect = FALSE)

## compare models 

rrMods <- merge(rrPre, rrPost, by = "species_name", all.y = T)

rr2Mods <- merge(rr2Pre, rr2Post, by = "species_name", all.y = T)

## check species converged in BOTH models

nrow(rrMods[is.na(rrMods$error_message.x) & is.na(rrMods$error_message.y), ])

nrow(rrMods)

## convert coefficients to the probability scale

logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

rrMods$year.prob.x <- sapply(X = rrMods$year.estimate.x,
              FUN = logit2prob)

rrMods$year.prob.y <- sapply(X = rrMods$year.estimate.y,
               FUN = logit2prob)

rr2Mods$year.prob.x <- sapply(X = rr2Mods$year.estimate.x,
                             FUN = logit2prob)

rr2Mods$year.prob.y <- sapply(X = rr2Mods$year.estimate.y,
                             FUN = logit2prob)

## plot predictions from post-digitization data against those from pre digitization data

plot(rrMods$year.estimate.y ~ rrMods$year.estimate.x)

plot(rr2Mods$year.estimate.y ~ rr2Mods$year.estimate.x)

cor.test(rrMods$year.estimate.y, rrMods$year.estimate.x)

cor.test(rr2Mods$year.estimate.y, rr2Mods$year.estimate.x)

## now format the data for the Telfer model 

fDatTelfPre <- fDatPre[fDatPre$Period %in% c(1,2,3, 5, 6, 7), ] # p1 = decades 1, 2 and 3; and p2 = decades 5, 6 and 7

fDatTelfPre$Period <- ifelse(fDatTelfPre$Period %in% c(1,2, 3), 1, 2)

fDatTelfPost <- fDatPost[fDatPost$Period %in% c(1,2,3,5,6,7), ]

fDatTelfPost$Period <- ifelse(fDatTelfPost$Period %in% c(1,2,3), 1, 2)

## fit Telfer model 

telferPre <- sparta::telfer(taxa = fDatTelfPre$species,
                             site = fDatTelfPre$cell,
                             time_period = as.numeric(fDatTelfPre$Period),
                             minSite = 2)

telferPost <- sparta::telfer(taxa = fDatTelfPost$species,
                         site = fDatTelfPost$cell,
                         time_period = as.numeric(fDatTelfPost$Period),
                         minSite = 2)

colnames(telferPre)[1] <- "species_name"

colnames(telferPost)[1] <- "species_name"

## compare Telfer models 

telferMods <- merge(telferPre, telferPost, by = "species_name", all.y = T)

## establish number of species which could be fitted using both pre and post-digitization data 

nrow(telferMods[!is.na(telferMods$Telfer_1_2.x) & !is.na(telferMods$Telfer_1_2.y), ])

## create new column to denote bombus terrestris is in the RR models 

rrMods$bomb <- ifelse(rrMods$species_name == "Bombus terrestris", 1, 0)

rr2Mods$bomb <- ifelse(rr2Mods$species_name == "Bombus terrestris", 1, 0)

rrMods$bomb[rrMods$species_name == "Bombus dahlbomii"] <- 2

rr2Mods$bomb[rr2Mods$species_name == "Bombus dahlbomii"] <- 2

telferMods$bomb <- ifelse(telferMods$species_name == "Bombus dahlbomii", 1, 0)

## plot model predictions from post-digitization data on predictions from pre-digitization data 

pTelfer <- ggplot(data = telferMods, aes(x = Telfer_1_2.x, y = Telfer_1_2.y, colour = as.factor(bomb))) +
  geom_point(aes(size = as.factor(bomb)), alpha = 0.5) +
  theme_linedraw() +
  xlab("pre digitization index") +
  ylab("post digitization index") + 
  geom_abline(slope = 1,
              intercept = 0) +
  ggtitle("A) Telfer") + 
  ylim(-1.5, 3.5) +
  xlim(-1.5, 3.5) +
  scale_colour_manual(values = c("black", "red")) +
  theme(legend.position = "none")

pRR <- ggplot(data = rrMods, aes(x = year.estimate.x, y = year.estimate.y, colour = as.factor(bomb))) +
  geom_point(aes(size = as.factor(bomb)), alpha = 0.5) +
  theme_linedraw() +
  xlab("pre digitization index") +
  ylab("post digitization index") + 
  geom_abline(slope = 1,
              intercept = 0) +
  ggtitle("C) RR + site") +
  ylim(-650, 200) +
  xlim(-650, 200) +
  scale_colour_manual(values = c("black", "blue", "red")) +
  theme(legend.position = "none")


pRR <- ggplot(data = rrMods, aes(x = year.prob.x, y = year.prob.y, colour = as.factor(bomb))) +
  geom_point(aes(size = as.factor(bomb))) +
  theme_linedraw() +
  xlab("pre digitization index") +
  ylab("post digitization index") + 
  geom_abline(slope = 1,
              intercept = 0) +
  ggtitle("C) RR + site") +
  ylim(0, 1) +
  xlim(0, 1) +
  scale_colour_manual(values = c("black", "blue", "red")) +
  theme(legend.position = "none")

pRR2 <- ggplot(data = rr2Mods, aes(x = year.estimate.x, y = year.estimate.y, colour = as.factor(bomb))) +
  geom_point(aes(size = as.factor(bomb)), alpha = 0.5) +
  theme_linedraw() +
  xlab("pre digitization index") +
  ylab("post digitization index") + 
  geom_abline(slope = 1,
              intercept = 0) +
  ggtitle("B) RR") +
  ylim(-25, 35) +
  xlim(-25, 35) +
  scale_colour_manual(values = c("black", "blue", "red")) +
  theme(legend.position = "none")

pRR2 <- ggplot(data = rr2Mods, aes(x = year.prob.x, y = year.prob.y, colour = as.factor(bomb))) +
  geom_point(aes(size = as.factor(bomb))) +
  theme_linedraw() +
  xlab("pre digitization index") +
  ylab("post digitization index") + 
  geom_abline(slope = 1,
              intercept = 0) +
  ggtitle("C) RR + site") +
  ylim(0, 1) +
  xlim(0, 1) +
  scale_colour_manual(values = c("black", "blue", "red")) +
  theme(legend.position = "none")

library(cowplot)

png("preVsPostMods.png", width = 9, height = 3, units = "in", res = 500)
plot_grid(pTelfer, 
          pRR2,
          pRR,
          ncol = 3,
          align = "v")
dev.off()

write.csv(rrMods, "C:/Users/Rob.Lenovo-PC/Documents/surpass/Data/preVsPostDig/RRPlusSiteEffectModelOutputs.csv",
          row.names = F)

write.csv(rr2Mods, "C:/Users/Rob.Lenovo-PC/Documents/surpass/Data/preVsPostDig/RRModelOutputs.csv",
          row.names = F)

write.csv(telferMods, "C:/Users/Rob.Lenovo-PC/Documents/surpass/Data/preVsPostDig/telferModelOutputs.csv",
          row.names = F)

## compare B terrestris outputs for the RR models 

rrMods[rrMods$species_name == "Bombus terrestris", ]

rr2Mods[rr2Mods$species_name == "Bombus terrestris", ]

## compare B dhalbomii 

rrMods[rrMods$species_name == "Bombus dahlbomii", ]

rr2Mods[rr2Mods$species_name == "Bombus dahlbomii", ]

## convert log odds to probabilities 


plot(post~pre)
cbind(rrMods$species_name, rrMods$year.estimate.x, rrMods$year.estimate.y)
rrMods[which.max(rrMods$year.estimate.y),]
