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
        select(scientificName) %>%
        unique() %>%
        group_by(scientificName) %>%
        summarize(geometry=st_combine(geometry)) %>%
        mutate(aquatic=lengths(st_intersects(.,wet))>0) %>%
        as.data.frame() %>%
        select(scientificName,aquatic)
    }

    new <- occ  %>%
      filter(!scientificName %in% aquasp$scientificName) %>%
      select(scientificName) %>%
      unique() %>%
      group_by(scientificName) %>%
      summarize(geometry=st_combine(geometry)) %>%
      mutate(aquatic=lengths(st_intersects(.,wet))>0) %>%
      as.data.frame() %>%
      select(scientificName,aquatic)

    aquasp <- full_join(new,aquasp,by="scientificName") %>%
      rowwise() %>%
      mutate(aquatic=any(aquatic.x,aquatic.y)) %>%
      select(scientificName,aquatic) %>%
      mutate(aquatic=replace_na(aquatic,replace=FALSE))

    write.csv(aquasp,"data/aquatic_species.csv",row.names = FALSE)
  } else {
    aquasp <- data.frame()
  }
  return(aquasp)
}
