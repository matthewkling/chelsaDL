
#' Build a list of files to download
#'
#' This function constructs a set of download URLs for the requested
#' combinations of metadata variables
#'
#' @param variables character vector.
#' @param months character vector.
#' @param models character vector.
#' @param scenarios character vector.
#' @param timeframes character vector.
#' @return a data frame of metadata for all factorial combinations of the
#'   requested variables
ch_queries <- function(variables, months, models, scenarios, timeframes){
      require(dplyr)
      expand.grid(model=models,
                  scenario=scenarios,
                  timeframe=timeframes,
                  variable=variables,
                  month=months) %>%
            mutate(variable2=case_when(variable=="tmin" ~ "tasmin",
                                       variable=="tmax" ~ "tasmax",
                                       variable=="temp" ~ "tas",
                                       variable=="prec" ~ "pr",),
                   addendum=case_when(variable=="prec" ~ "",
                                      TRUE ~ "_V1.2"),
                   file=paste0(timeframe, "/", variable, "/CHELSA_", variable2, "_mon_", model,
                               "_", scenario, "_r*i1p1_g025.nc_", month, "_", timeframe, addendum, ".tif"),
                   url=paste0("https://www.wsl.ch/lud/chelsa/data/cmip5/", file))
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
ch_dl <- function(md, dest, skip_existing=TRUE, method="curl", crop=NULL){
      for(i in 1:nrow(md)){
            message(paste("File", i, "of", nrow(md), "..."))

            md$path[i] <- paste0(dest, "/", basename(md$file[i]))

            if(skip_existing){
                  # previously-failed downloads have small file size
                  size <- file.size(md$path[i])
                  if(!is.na(size) & log(size)>10){
                        md$status[i] <- "already done"
                        next()
                  }
            }

            # run numbers vary by model. try all options.
            runs <- c("1", "2", "12")
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
            if(file.exists(md$path)) md$status[i] <- "download completed"

            if(!is.null(crop)){
                  require(raster)
                  r <- raster(md$path[i]) %>%
                        crop(crop) %>%
                        writeRaster(md$path[i], overwrite=T)
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
