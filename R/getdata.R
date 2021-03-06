#' Get species occurence data
#'
#' @param grid grid polygons
#' @param g index of the grid polygon
#' @param latlong Latitude Longitude coordinate reference system
#'
#' @return
#' @export
#'
#' @examples
#' occ <- getdata(grid, g,latlong)
getdata <- function(grid, g,latlong){
  # browser()
  cell <- grid[g,]

  n <- spocc::occ(from=c("inat"),
                  geometry= sf::st_as_text(sf::st_geometry(sf::st_transform(cell,latlong))),
                  limit=1)$inat$meta$found

  subgrid <- sf::st_make_grid(cell,n=(n%/%10000+1)*2-1)

  occ <- data.frame(geometry=sf::st_sfc(crs=latlong)) %>%
    sf::st_as_sf()
  for(s in 1:length(subgrid)){

    llcell <- sf::st_transform(subgrid[s],latlong)


    # get OBIS data
    print(paste("Querying OBIS for subgrid:",s))

    obis <- robis::occurrence(geometry = sf::st_as_text(llcell))




    if(nrow(obis)==0){
      obis <- data.frame(scientificName=character(),
                         decimalLongitude=double(),
                         decimalLatitude=double(),
                         eventDate=character(),
                         dataset_id=character(),
                         stringsAsFactors = FALSE)
    }
    if(!'eventDate' %in% names(obis)) obis$eventDate=NA

    # get GBIF data
    print(paste("Querying GBIF for subgrid:",s))

    gbif <- data.frame()
    anygbif <- rgbif::occ_search(geometry = sf::st_as_text(llcell),
                                 basisOfRecord="OBSERVATION")$meta$count
    while(nrow(gbif)<anygbif){
      gbif <- dplyr::bind_rows(gbif,
                               rgbif::occ_data(geometry = sf::st_as_text(llcell),
                                                 basisOfRecord="OBSERVATION",
                                                 start=nrow(gbif),
                                                 limit=ifelse(anygbif-nrow(gbif)>500,500,anygbif-nrow(gbif)))$data %>%
                                 dplyr::select_if(names(.) %in% c("scientificName",
                                                                  "decimalLongitude",
                                                                  "decimalLatitude",
                                                                  "eventDate",
                                                                  "gbifID")))
      # print(nrow(gbif))
    }

    if(nrow(gbif)==0){
      gbif <- data.frame(scientificName=character(),
                         decimalLongitude=double(),
                         decimalLatitude=double(),
                         eventDate=character(),
                         gbifID=character(),
                         stringsAsFactors = FALSE)
    }
    if(!'eventDate' %in% names(gbif)) gbif$eventDate=NA


    # get inat data
    print(paste("Querying iNaturalist for subgrid:",s))

    inat <- data.frame()
    while(nrow(inat)%%30==0){
      new <- spocc::occ(from=c("inat"),
                        geometry= sf::st_as_text(llcell),
                        page=nrow(inat)/30+1)$inat$data[[1]]

      if(nrow(new)==0){
        break
      }else{
        inat <- dplyr::bind_rows(inat,
                          new)
      }
      # print(nrow(inat))
    }

    if(nrow(inat)==0){
      inat <- data.frame(name=character(),
                         longitude=double(),
                         latitude=double(),
                         observed_on_details.date=character(),
                         id=character(),
                         quality_grade=character(),
                         stringsAsFactors = FALSE)
    }
    if(!'name' %in% names(inat)) inat$name=NA
    if(!'observed_on_details.date' %in% names(inat)) inat$observed_on_details.date=NA


    # combine the datasets and make into sf objects
    occ <- dplyr::bind_rows(occ,
                     rbind(obis %>%
                             sf::st_as_sf(coords=c("decimalLongitude","decimalLatitude"),crs=latlong) %>%
                             dplyr::mutate(link=paste0('https://obis.org/dataset/',dataset_id)) %>%
                             dplyr::select(scientificName,link,eventDate),
                           gbif %>%
                             sf::st_as_sf(coords=c("decimalLongitude","decimalLatitude"),crs=latlong) %>%
                             dplyr::mutate(link=paste0('https://www.gbif.org/occurrence/',gbifID)) %>%
                             dplyr::select(scientificName,link,eventDate),
                           inat %>%
                             dplyr::filter(!is.na(longitude),
                                           !is.na(latitude),
                                           !is.na(name),
                                           quality_grade=='research') %>%
                             st_as_sf(coords=c("longitude","latitude"),crs=latlong) %>%
                             dplyr::mutate(incell=lengths(st_intersects(.,llcell))>0,
                                           link=paste0('https://www.inaturalist.org/observations/',id),
                                           scientificName=name,
                                           eventDate=observed_on_details.date) %>%
                             dplyr::filter(incell,!is.na(scientificName)) %>%
                             dplyr::select(scientificName,link,eventDate)))
    print(nrow(occ))
  }
  return(unique(occ) %>%
           dplyr::mutate(geometry=sf::st_sfc(geometry,crs=latlong)))
}
