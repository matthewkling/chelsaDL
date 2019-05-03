
#' Build a list of files to download
#'
#' This function constructs a set of download URLs for the requested
#' combinations of metadata variables. It currently supports download of
#' historic climatologies and future CMIP5 model predictions for both basic
#' monthly variables and bioclimatic variables. CHELSA provides historic data in
#' integer*10 format and floating point format, but only the former is available
#' using this function, for consistency with CMIP5 futures which are available
#' only in integer*10 format.
#'
#' @param variables character vector, options include "tmin", "tmax", "temp",
#'   "prec", and "bio".
#' @param layers integer vector, options include 1:12 for base variables
#'   (representing months) and 1:19 for bio (representing biovariable number).
#' @param models character vector, specify only for future data.
#' @param scenarios character vector, specify only for future data.
#' @param timeframes character vector, options include "1979-2013", "2014-2060",
#'   and "2061-2080".
#' @return a data frame of metadata for all factorial combinations of the
#'   requested variables
ch_queries <- function(variables, layers, models=NA, scenarios=NA, timeframes){
      require(dplyr)
      expand.grid(model=models,
                  scenario=scenarios,
                  timeframe=timeframes,
                  variable=variables,
                  layer=layers) %>%
            mutate(variable2=case_when(variable=="tmin" ~ "tasmin",
                                       variable=="tmax" ~ "tasmax",
                                       variable=="temp" ~ "tas",
                                       variable=="prec" ~ "pr",
                                       variable=="bio" ~ "bio"),
                   addendum=case_when(variable=="prec" ~ "",
                                      TRUE ~ "_V1.2"),
                   histdir=case_when(variable=="bio" ~ "bioclim/integer/",
                                     variable=="prec" ~ "climatologies/prec/",
                                     TRUE ~ paste0("climatologies/temp/integer/", variable, "/")),
                   file=case_when(timeframe=="1979-2013" &
                                        variable != "bio" ~ paste0(histdir, "CHELSA_",
                                                                   variable, "10_",
                                                                   str_pad(layer, 2, "left", "0"), "_land.7z"),
                                  timeframe=="1979-2013" &
                                        variable == "bio" ~ paste0(histdir, "CHELSA_",
                                                                   variable, "10_",
                                                                   str_pad(layer, 2, "left", "0"), ".tif"),
                                  TRUE ~ paste0("cmip5/", timeframe, "/", variable,
                                                "/CHELSA_", variable2, "_mon_", model,
                                                "_", scenario, "_r*i1p1_g025.nc_", layer, "_",
                                                timeframe, addendum, ".tif")),
                   url=paste0("https://www.wsl.ch/lud/chelsa/data/", file))
}

#' Download CHELSA data
#'
#' Many standard palette generators use only a slice of color space, which can
#' cause a lack of differentiability in palettes used to visualize categorical
#' factors with many levels. This function attempts to overcome this by
#' generating colors using nearest-neighbor distance maximization in 3D RGB
#' space.
#'
#' @param md Variables to download (a data frame created by ch_queries).
#' @param dest Path to file folder where downloaded files should be stored
#'   (character).
#' @param skip_existing Should files that are already present in the destination
#'   be igored (logical).
#' @param method Download method, passed to downloa.file (character).
#' @param crop Spatial bounding box to crop each downloaded raster to (an extent
#'   object, or any spatial object with an extent).
ch_dl <- function(md, dest=NULL, skip_existing=TRUE, method="curl", crop=NULL){

      if(is.null(dest)) dest <- getwd()

      for(i in 1:nrow(md)){
            message(paste("File", i, "of", nrow(md), "..."))
            md$status[i] <- "incomplete"
            md$path[i] <- paste0(dest, "/", basename(md$file[i]))

            runs <- c("1", "2", "12")

            if(skip_existing){
                  # previously-failed downloads have small file size
                  paths <- sapply(runs, function(x) sub("\\*", x, md$path[i]))
                  size <- file.size(paths)
                  if(any(!is.na(size) & log(size)>10)){
                        md$path[i] <- paths[!is.na(size) & log(size)>10]
                        md$status[i] <- "already done"
                        next()
                  }
            }

            # run numbers vary by model. try all options.
            for(run in runs){
                  url <- sub("\\*", run, md$url[i])
                  path <- sub("\\*", run, md$path[i])
                  r <- try(download.file(url, path, method=method, quiet=T))
                  size <- file.size(path)
                  if(!is.na(size) & log(size)>10){
                        md$url[i] <- url
                        md$path[i] <- path
                        break()
                  }
                  file.remove(path)
            }

            if(class(r)=="try-error"){
                  md$status[i] <- as.character(r)
                  next()
            }
            if(file.exists(md$path[i])) md$status[i] <- "download completed"

            if(!is.null(crop) & file.exists(md$path[i])){
                  require(raster)
                  r <- raster(md$path[i]) %>%
                        crop(crop) %>%
                        writeRaster(md$path[i], overwrite=T)
                  md$status[i] <- "raster cropped"
            }
      }
      return(md)
}


# generate a metadata table given a set of file paths
ch_parse <- function(paths){

}

# list available data
ch_datasets <- function(){

}
