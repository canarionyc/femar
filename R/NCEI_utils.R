
#' Get hurricane location history
#' https://www.ncei.noaa.gov/data/international-best-track-archive-for-climate-stewardship-ibtracs/v04r00/access/csv/ibtracs.ALL.list.v04r00.csv
#' as POINT geometry
#' @return a data.table with units
#' @import units
#' @import utils
#' @import fst
#' @import fstutils
#' @import magrittr
#' @importFrom dplyr filter select
#' @import sf
#' @importFrom purrr map2_dfc
#' @export

get_best_tracks_dt <- function(){
  # utils::browseURL()
  # browser()
  best_tracks_rds <- file.path(.noaa_workdir, "best.rds"); print(file.info(best_tracks_rds))
  if(file.exists(best_tracks_rds)) {
    best_tracks <- readRDS(best_tracks_rds)
    print(nrow(best_tracks))
    # best[, c('SEASON', 'USA_SSHS'):=lapply(.SD, drop_units), .SDcols =  c('SEASON', 'USA_SSHS')]
    # str(best[, .SD,  .SDcols =  c('SEASON', 'USA_SSHS')])
    # saveRDS(best, file = best_tracks_rds)
  }  else {
    url <- "https://www.ncei.noaa.gov/data/international-best-track-archive-for-climate-stewardship-ibtracs/v04r00/access/csv/ibtracs.ALL.list.v04r00.csv"
    # print(basename(url))
    # browseURL( fs::path_dir(url))

    best_tracks_csv <- file.path(.noaa_datadir,"NCEI",  basename(url)); print(file.info(best_tracks_csv))
    # cat(R.utils::countLines(best_tracks_csv), "ines read from", best_tracks_csv)
    print(readLines(best_tracks_csv, n=3L))

    best_units_df <- fread(best_tracks_csv, nrows =1L,data.table=FALSE, na.strings = c(""))
    str(best_units_df)
    best_units_df$SEASON <- NA
    best_units_df$USA_SSHS <- NA
    best_units_valid <-   which(!is.na(best_units_df))
    print(best_units_df[best_units_valid])
    # length(best_units_valid)

    # ?fread
    best_tracks.df <- fread(best_tracks_csv, skip = 2L, col.names = names(best_units_df),data.table=FALSE
                            , na.strings = c("", "-999.9"))
    str(best_tracks.df)
    # best.df$HKO_CAT
    # ?set_units
    # ?mapply
    length(best_tracks.df)
    length( as.character(best_units_df))
    # debugonce(`units<-`)
    # debugonce(mapply)
    # mapply( `units<-`, best, as.character(data_units))
    #   (cols_with_units <- which(sapply(best, is.character)==FALSE))
    # length(cols_with_units)
    #   best.df <- as.data.frame(best)
    best_tracks.df[,best_units_valid] <- best_tracks.df[,best_units_valid] %>%
      purrr::map2_dfc(as.character(best_units_df)[best_units_valid],  ~set_units(.x, .y, mode = "standard"))
    str(best_tracks.df)

    best_tracks.df <- best_tracks.df%>% fstutils::remove_na_cols()
    str(best_tracks.df)
    best_tracks <- setDT(best_tracks.df)
    # best_tracks.df <- within(best_tracks.df, SEASON <- units::drop_units(SEASON))
    best_tracks[, c('SEASON', 'USA_SSHS'):=lapply(.SD, drop_units), .SDcols =  c('SEASON', 'USA_SSHS')]


    # save result -------------------------------------------------------------


    saveRDS(best_tracks,  file = best_tracks_rds);   print(file.info(best_tracks_rds))
  }# ; str(best_tracks)

  # print(fsummary(best_tracks))
  return(invisible(best_tracks))
}

lcc <- "+proj=lcc +lat_1=60 +lat_2=30 +lon_0=-60"

#' Get hurricane location history
#' https://www.ncei.noaa.gov/data/international-best-track-archive-for-climate-stewardship-ibtracs/v04r00/access/best_points/IBTrACS.ALL.list.v04r00.points.zip
#' as POINT geometry
#' @return a simple feature object
#' @export
#'
get_best_points_sf <- function(){
  best_points_url <- "https://www.ncei.noaa.gov/data/international-best-track-archive-for-climate-stewardship-ibtracs/v04r00/access/best_points/IBTrACS.ALL.list.v04r00.points.zip"
  best_points_rds <- file.path(.noaa_workdir, basename(best_points_url) %>% fs::path_ext_set("Rds")); print(file.info(best_points_rds))
  if(file.exists(best_points_rds)) {
    IBTrACS.ALL.points_sf <- readRDS(best_points_rds)
  } else {
    # browseURL( file.path(.noaa_datadir,"NCEI"))
    best_points_zip <- file.path(.noaa_datadir,"NCEI", basename(best_points_url)); print(file.info(best_points_zip))
    # download.file(best_points_url, destfile = best_points_zip)
    (best_points_dsn <- tools::file_path_sans_ext(best_points_zip))
    # unzip(best_points_zip, exdir = tools::file_path_sans_ext(best_points_zip))
    # list.files(best_points_dsn)
    # browseURL( file.path(.noaa_datadir,"NCEI"))
    st_layers(best_points_dsn)
    IBTrACS.ALL.points_sf <- st_read(best_points_dsn
                                     , query = "select * from \"IBTrACS.ALL.list.v04r00.points\" where BASIN='NA' and WMO_AGENCY='hurdat_atl' and  USA_WIND>34 and USA_SSHS>0 and USA_RMW is not null"
    ) # %>% st_set_crs(4269) %>% st_transform(st_crs(lcc))
    print(st_crs(IBTrACS.ALL.points_sf))
    saveRDS(IBTrACS.ALL.points_sf , best_points_rds); print(file.info(best_points_rds))

  }; print(IBTrACS.ALL.points_sf)
  IBTrACS.ALL.points_sf
}

#' Get hurricane location history
#' https://www.ncei.noaa.gov/data/international-best-track-archive-for-climate-stewardship-ibtracs/v04r00/access/best_points/IBTrACS.ALL.list.v04r00.lines.zip
#' as LINES geometry (one feature per hurricane and 6 hours interval)
#' @return a simple feature object
#' @export
#'
get_best_tracks_sf <- function(){
  best_tracks_url <- "https://www.ncei.noaa.gov/data/international-best-track-archive-for-climate-stewardship-ibtracs/v04r00/access/shapefile/IBTrACS.ALL.list.v04r00.lines.zip"

  best_tracks_rds <- file.path(.noaa_workdir,basename(best_tracks_url)%>% fs::path_ext_set("Rds")); print(file.info(best_tracks_rds))
  if(file.exists(  best_tracks_rds)) {
    best_tracks_sf <- readRDS(best_tracks_rds)
  } else {
    # library(shapefiles)

    best_tracks_zip <- file.path(.noaa_datadir,"NCEI", basename(best_tracks_url)); print(file.info(best_tracks_zip))
    # download.file(best_tracks_url, destfile = best_tracks_zip)
    (best_tracks_dsn <- tools::file_path_sans_ext(best_tracks_zip))
    print(st_layers(best_tracks_dsn))
    # unzip(best_tracks_zip, exdir = tools::file_path_sans_ext(best_tracks_zip))
    print(list.files(best_tracks_dsn))
    # browseURL( file.path(.noaa_datadir,"NCEI"))

    best_tracks_sf <- st_read(best_tracks_dsn
                              , query = "select * from \"IBTrACS.ALL.list.v04r00.lines\" where BASIN='NA' and WMO_AGENCY='hurdat_atl' and  USA_WIND>34 and USA_SSHS>0 and USA_RMW is not null"
    )# %>% st_set_crs(4269) %>% st_transform(st_crs(lcc))

    print(st_crs(best_tracks_sf))

    best_tracks_sf$ISO_TIME <- as.POSIXct(best_tracks_sf$ISO_TIME )
    #  best_tracks_sf_rds <- best_tracks_dsn %>% fs::path_ext_set("Rds"); print(file.info(best_tracks_sf_rds))

    saveRDS(best_tracks_sf, best_tracks_rds); print(file.info(best_tracks_rds))
  }; str(best_tracks_sf)
  return(best_tracks_sf)
}

#' Get best points with county info
#'
#' @return simple feature
#' @import tigris
#' @importFrom dplyr left_join
#' @export
get_best_points_cnty_sf <- function() {
  best_points_cnty_dsn <- file.path(.noaa_workdir, "best_points_cnty.gpkg"); print(file.info(best_points_cnty_dsn))
  if(file.exists(best_points_cnty_dsn)) {
    best_points_cnty_sf <- st_read(best_points_cnty_dsn)
  } else {
    # ?st_intersection.sf
    # ?left_join.sf
    # ?join_by
    best_points_sf <- get_best_points_sf()
    counties_sf <- tigris::counties(cb=FALSE)
    best_points_cnty_sf <- dplyr::left_join(best_points_sf, counties_sf)
    (best_points_cnty_sf <- st_intersection(best_points_sf, counties_sf['COUNTYFP']))

    # ?st_write
    st_write(best_points_cnty_sf,best_points_cnty_dsn)
    # browseURL(.noaa_workdir)
  }; print(best_points_cnty_sf)
  print(sum(table(best_points_cnty_sf$COUNTYFP)))
  return(best_points_cnty_sf)
}


