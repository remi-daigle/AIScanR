#' Are species occurences aquatic?
#'
#' @param occ Species occurence dataframe
#' @param wet 'wet' polygons
#'
#' @return
#' @export
#'
#' @examples
#' isaquatic(occ,wet)
isaquatic <- function(occ,wet){
  if(nrow(occ)!=0){
    if(file.exists("data/aquatic_species.csv")){
      aquasp <- read.csv("data/aquatic_species.csv",stringsAsFactors = FALSE)
    } else {
      aquasp <-  occ  %>%
        dplyr::select(scientificName) %>%
        unique() %>%
        dplyr::group_by(scientificName) %>%
        dplyr::summarize(geometry=st_combine(geometry)) %>%
        dplyr::mutate(aquatic=lengths(st_intersects(.,wet))>0) %>%
        as.data.frame() %>%
        dplyr::select(scientificName,aquatic)
    }

    new <- occ  %>%
      dplyr::filter(!scientificName %in% aquasp$scientificName) %>%
      dplyr::select(scientificName) %>%
      unique() %>%
      dplyr::group_by(scientificName) %>%
      dplyr::summarize(geometry=st_combine(geometry)) %>%
      dplyr::mutate(aquatic=lengths(st_intersects(.,wet))>0) %>%
      as.data.frame() %>%
      dplyr::select(scientificName,aquatic)

    aquasp <- dplyr::full_join(new,aquasp,by="scientificName") %>%
      dplyr::rowwise() %>%
      dplyr::mutate(aquatic=any(aquatic.x,aquatic.y)) %>%
      dplyr::select(scientificName,aquatic) %>%
      dplyr::mutate(aquatic=replace_na(aquatic,replace=FALSE))

    write.csv(aquasp,"data/aquatic_species.csv",row.names = FALSE)
  } else {
    aquasp <- data.frame()
  }
  return(aquasp)
}
