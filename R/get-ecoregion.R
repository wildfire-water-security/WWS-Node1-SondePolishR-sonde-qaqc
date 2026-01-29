#' Get the US EPA Ecoregion at a Site
#'
#' Identities the US EPA Ecoregion at a provided latitude and longitude.
#'
#' @param site a character vector in the form lat, long or sf object of length 1
#' @param geometry logical, should geometry be returned?
#' @param lvl a number (1,2, or 3) indicating the EPA ecoregion level to return (only used if geometry = FALSE)
#'
#' @returns
#' if geometry is TRUE
#'  - a sf object of the ecoregion the site is in
#' if geometry is FALSE
#'  - a character vector returing the name of the ecoregion
#' @export
#'
#' @examples
#' get_ecoregion(c("44.20",	"-122.2"))
get_ecoregion <- function(site=NULL, geometry = TRUE, lvl=3){
  #make site a sf object
  if(!inherits(site, "sf")){
    site <- sf::st_as_sf(data.frame(x=site[1], y=site[2]), coords = c("y", "x"), crs = "epsg:4326")
  }

  #project to match ecoregions
  site_pj <- sf::st_transform(site, terra::crs(SondePolishR::ecoregions))

  #get overlap
  eco <- SondePolishR::ecoregions[as.numeric(sf::st_within(site_pj, SondePolishR::ecoregions)[[1]]),]

  #return ecoregion
  if(geometry){
    return(eco)
  }else{
    name <- dplyr::case_when(
      lvl == 3 ~ eco$NA_L3NAME,
      lvl == 2 ~ eco$NA_L2NAME,
      lvl == 1 ~ eco$NA_L1NAME,
    )
    return(name)
  }
}
