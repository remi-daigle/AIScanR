#' Creates 'wet' polygons
#'
#' @param grid grid polygons
#' @param g index of the grid polygon
#' @param EEZ polygon for EEZ
#' @param latlong Latitude Longitude coordinate reference system
#' @param proj Projected coordinate reference system
#' @param cachedir Directory for canvec data caching
#'
#' @return
#' @export
#'
#' @examples
#' wet <- iswet(grid,g,EEZ,latlong,proj)
iswet <- function(grid,g,EEZ,latlong,proj,Canada,cachedir=NULL,datadir="data/wet/"){
  require(sf)
  if(file.exists(paste0(datadir,"wet_",sprintf("%05d",g),".shp"))){
    wet <- st_read(paste0(datadir,"wet_",sprintf("%05d",g),".shp"))
  }else{
    cell <- grid[g,] %>% st_transform(st_crs(Canada))
    if(lengths(sf::st_intersects(cell,Canada))>0){
      NTSs <- rcanvec::nts(bbox=sp::bbox(as(sf::st_transform(cell,latlong),"Spatial")))

      rcanvec::canvec.download(NTSs,cachedir = cachedir)

      freshwater <- st_sf(geometry=st_sfc(crs=proj))
      for(n in NTSs){
        print(n)
        canvecs <- try(c(rcanvec::canvec.load(n,"waterbody",cachedir = cachedir),
                     rcanvec::canvec.load(n,"river",cachedir = cachedir),
                     rcanvec::canvec.load(n,"string_bog",cachedir = cachedir),
                     rcanvec::canvec.load(n,"wetland",cachedir = cachedir),
                     rcanvec::canvec.load(n,"palsa_bog",cachedir = cachedir),
                     rcanvec::canvec.load(n,"tundra_pond",cachedir = cachedir)) %>%
          lapply(sf::st_as_sf) %>%
          lapply(function(x) as.data.frame(x) %>% dplyr::select(geometry)) %>%
          dplyr::bind_rows() %>%
          sf::st_as_sf(crs="+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs") %>%
          sf::st_combine() %>%
          sf::st_union() %>%
          sf::st_transform(proj) %>%
          st_buffer(5) %>%
          sf::st_intersection(cell))
        if(length(canvecs)>0 & !class(canvecs)=="try-error"){
          freshwater <- st_union(freshwater,canvecs)
        }

      }

      # fw <- c(rcanvec::canvec.load(n,"waterbody"),
      #         rcanvec::canvec.load(n,"river"),
      #         rcanvec::canvec.load(n,"string_bog"),
      #         rcanvec::canvec.load(n,"wetland"),
      #         rcanvec::canvec.load(n,"palsa_bog"),
      #         rcanvec::canvec.load(n,"tundra_pond")) %>%
      #   lapply(sf::st_as_sf) %>%
      #   lapply(function(x) as.data.frame(x) %>% dplyr::select(geometry)) %>%
      #   dplyr::bind_rows() %>%
      #   sf::st_as_sf(crs="+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs") %>%
      #   sf::st_combine() %>%
      #   sf::st_union() %>%
      #   sf::st_transform(proj) %>%
      #   st_buffer(1) %>%
      #   sf::st_intersection(cell)

      #
      #
      #   fw <- canvecs %>%
      #     sf_st_buffer(1) %>%
      #     sf::st_intersection(cell)
      #
        # ggplot(cell)+
        #   geom_sf(fill='red')+
        #   geom_sf(data=wet,fill='blue',colour='transparent')+
        #   geom_sf(data=fw,fill='yellow')
      #
      #   %>%
      #                       sf::st_transform(proj) %>%
      #                       sf::st_buffer(1) %>%
      #                       sf::st_transform(latlong))
      #
      #
      #   d=0
      #   while(class(fw)=="try-error"){
      #     d <- d+1
      #     message(paste("Warning: Canvec layers are too complex, trying to simplify with dTolerance =",d))
      #     fw <- try(canvecs %>%
      #                         sf::st_simplify(preserveTopology = TRUE, dTolerance=d) %>%
      #                         sf::st_buffer(1) %>%
      #                         sf::st_crop(cell) %>%
      #                         sf::st_transform(latlong))
      #   }
      # }

      wet <- EEZ %>%
        sf::st_crop(cell) %>%
        sf::st_transform(latlong) %>%
        sf::st_union(.,st_transform(freshwater,latlong))
    } else {
      wet <- EEZ %>%
        sf::st_crop(cell) %>%
        sf::st_transform(latlong)
    }

    sf::st_write(wet,paste0(datadir,"wet_",sprintf("%05d",g),".shp"))
  }

  return(wet)
}
