file <- "G:/humm_redownload/humm1950.2019.csv"

dat <- readr::read_tsv(file, 
                quote = "")

head(data.frame(dat))

out <- dat[, c("family", "species", "countryCode", "decimalLatitude", "decimalLongitude", "coordinateUncertaintyInMeters", "day", "month", "year", "basisOfRecord")]

out <- data.frame(out)

save(out, file = "G:/humm_redownload/humm_all.rdata")
