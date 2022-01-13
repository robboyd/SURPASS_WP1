assessSpatialCov2 <- function (dat, res, logCount = FALSE, countries = NULL, shp = NULL, 
                               periods, maxSpatUncertainty = NULL, output = "density", 
                               minPeriods = NULL) 
{
  if (!output %in% c("density", "overlap", "nPeriods")) 
    stop("Output must be one of density, overlap or nPeriods")
  if (is.list(countries) | is.list(shp)) 
    print("You have specified shp or countries as a list object; this should only be the case if your identifier field denotes spatial subsets of your data.")
  if (!is.null(shp) & !is.null(countries)) 
    stop("Only one of countries and shp may be specified; they do the same thing. If you are working on WGS84 it is recommended that you use countries; otherwise, you cannot use countries and should use shp.")
  if (any(!(c("species", "x", "y", "year", 
              "spatialUncertainty", "identifier") %in% 
            colnames(dat)))) 
    stop("Data must includes columns for species, x, y, year, spatialUncertainty and identifier")
  if (any(is.na(dat$identifier))) 
    stop("One or more NAs in the identifier field. NAs are not permitted.")
  if (!is.null(maxSpatUncertainty)) 
    dat <- dat[!is.na(dat$spatialUncertainty) & dat$spatialUncertainty <= 
                 maxSpatUncertainty, ]
  if (nrow(dat) == 0) 
    stop("No records with with spatialUncertainty < maxSpatUncertainty")
  if (any(is.na(dat$year))) {
    warning("Removing data without a specified year")
    dat <- dat[-which(is.na(dat$year)), ]
  }
  if (any(!dat$year %in% unlist(periods))) {
    drop <- which(!dat$year %in% unlist(periods))
    dat <- dat[-drop, ]
  }
  dat <- dat[order(dat$year), ]
  dat$Period <- NA
  for (i in 1:length(periods)) {
    dat$Period <- ifelse(dat$year %in% periods[[i]], paste0("p", 
                                                            i), dat$Period)
  }
  xmin <- min(dat$x, na.rm = T)
  xmax <- max(dat$x, na.rm = T)
  ymin <- min(dat$y, na.rm = T)
  ymax <- max(dat$y, na.rm = T)
  rast <- raster::raster(ncol = length(seq(xmin, xmax, res)), 
                         nrow = length(seq(ymin, ymax, res)), xmn = xmin, xmx = xmax, 
                         ymn = ymin, ymx = ymax)
  for (i in unique(dat$identifier)) {
    rasts <- lapply(X = unique(dat$Period), function(x) {
      data <- dat[dat$identifier == i & dat$Period == x, 
                  c("x", "y")]
      if (nrow(data) >= 1) {
        raster::rasterize(data, rast)
      }
      else {
        r <- raster::setValues(rast, NA)
        r
      }
    })
    names(rasts) <- unique(dat$Period)
    rasts <- raster::stack(rasts)
    if (logCount == TRUE) 
      rasts <- log10(rasts)
    if (output == "density") {
      assign(paste0("rasts", i), rasts)
    }
    else if (output == "nPeriods") {
      rasts <- sum(as.logical(rasts), na.rm = T)
      rasts[rasts == 0] <- NA
      assign(paste0("rasts", i), rasts)
    }
    else {
      if (is.null(minPeriods)) 
        minPeriods <- length(unique(dat$Period))
      rasts <- sum(as.logical(rasts), na.rm = T)
      rasts[rasts < minPeriods] <- NA
      assign(paste0("rasts", i), as.logical(rasts))
    }
  }
  if (output == "overlap") {
    leg <- "sampled in minPeriods\n    periods"
  }
  else if (output == "density") {
    leg <- ifelse(logCount == TRUE, "log10(n records)", 
                  "n records")
  }
  else {
    leg <- "Number of 
    periods 
    sampled"
  }
  myCol <- rgb(255, 255, 255, max = 255, alpha = 0, names = "blue50")
  out <- lapply(as.character(unique(dat$identifier)), function(x) {
    if (is.list(countries) & !any(!countries %in% unique(ggplot2::map_data("world")$region)) | 
        !is.list(countries) & !is.null(countries) & !any(!countries %in% 
                                                         unique(ggplot2::map_data("world")$region)) | 
        !is.null(shp)) {
      if (is.null(shp)) {
        if (is.list(countries)) {
          map <- ggplot2::map_data("world", regions = countries[[x]])
        }
        else {
          map <- ggplot2::map_data("world", regions = countries)
        }
      }
      else {
        if (is.list(shp)) {
          map <- ggplot2::fortify(shp[[x]])
        }
        else {
          map <- ggplot2::fortify(shp)
        }
      }
    }
    else {
      warning("Some or all country names provided are not in unique(ggplot2::map_data(world)$region)")
      map <- NULL
    }
    p <- rasterVis::gplot(get(paste0("rasts", x))) + 
      ggplot2::theme_linedraw()
    if (output == "density") {
      p <- p + ggplot2::geom_tile(ggplot2::aes(fill = value)) + 
        ggplot2::facet_wrap(~variable) + ggplot2::scale_fill_gradient2(low = "red", 
                                                                       high = "blue", na.value = myCol, name = leg)
    }
    else {
      n <- length(unique(raster::getValues(get(paste0("rasts", 
                                                      x)))))
      p <- p + ggplot2::geom_tile(ggplot2::aes(fill = factor(value))) + 
        ggplot2::scale_fill_manual(values = matlab.like2(n), na.value = myCol, name = leg, 
                                   na.translate = FALSE) + ggplot2::ggtitle(x)
      if (output == "overlap") 
        p <- p + ggplot2::theme(legend.position = "none")
    }
    if (!is.null(map)) 
      p <- p + ggplot2::geom_polygon(data = map, ggplot2::aes(x = long, 
                                                              y = lat, group = group), colour = "black", 
                                     fill = myCol, inherit.aes = F)
    return(p)
  })
  names(out) <- unique(dat$identifier)
  return(out)
}
tail(cleanDat[!is.na(cleanDat$spatialUncertainty), ])
write.csv(tail(cleanDat[!is.na(cleanDat$spatialUncertainty), ]), "hello.csv")
