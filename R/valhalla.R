


#' Point-to-Point Routing with Valhalla
#'
#' Note: This is a highly experimental function and will almost certainly change.
#' Right now from and to are one-row tibbles. That's probably not correct.
#'
#' It does no input validation, no error handling, nothing. Use at own risk.
#'
#' @param from
#' @param to
#' @param costing
#' @param unit
#'
#' @return
#' @export
valhalla_route <- function(from = NA, to = NA, costing = "auto", unit = "kilometers", min_road_class = "residential"){
  post_data <- list()

  post_data$locations <- from %>%
    dplyr::select(lat, lon = lng) %>%
    dplyr::bind_rows({
      to %>%
        dplyr::select(lat, lon = lng)}
    ) %>% bind_cols(tibble(search_filter = rep(list(list("min_road_class" = min_road_class)), 2)))

  post_data$costing = costing
  post_data$directions_options$units = unit



  post_json <- post_data %>%
    jsonlite::toJSON(auto_unbox = TRUE)

  #post_data
  #curl http://localhost:8002/route --data '{"locations":[{"lat":45.380521,"lon": -75.665359},{"lat":45.384269,"lon":-75.671515}],"costing":"auto","directions_options":{"units":"miles"}}'

  resp <- httr::POST(url = "http://localhost:8002/route", body = post_json)

  resp_data <- resp %>%
    httr::content(type = "text") %>%
    jsonlite::fromJSON()

  resp_data %>%
    purrr::pluck(1)

  #{"sources":[{"lat":40.744014,"lon":-73.990508},{"lat":40.739735,"lon":-73.979713},{"lat":40.752522,"lon":-73.985015},{"lat":40.750117,"lon":-73.983704},{"lat":40.750552,"lon":-73.993519}],"targets":[{"lat":40.744014,"lon":-73.990508},{"lat":40.739735,"lon":-73.979713},{"lat":40.752522,"lon":-73.985015},{"lat":40.750117,"lon":-73.983704},{"lat":40.750552,"lon":-73.993519}],"costing":"pedestrian"}&id=ManyToMany_NYC_work_dinner


}
#
#
# # ## TESTING BAD FORM
# library(tidyverse)
# to = tibble::tibble(lat=45.5, lng=-75)
# from = tibble::tibble(lng = -75.84322, lat = 45.10085)
# trip <- valhalla_route(from, to, min_road_class="residential")
#
# print_trip(trip, all_details = TRUE)
#


#' Decode Valhalla Route Shape
#'
#' @param encoded
#'
#' @return
#' @export
decode <- function(encoded) {
  # got algorithm from here https://valhalla.readthedocs.io/en/latest/decoding/
  chars <- stringr::str_split(encoded, "")[[1]]

  lats <- vector(mode = "integer", length = 1)
  lons <- vector(mode = "integer", length = 1)
  i <- 0

  while (i < length(chars)){
    shift <- 0
    result <- 0
    byte <- 0x20L

    while (byte >= 0x20) {
      i <- i + 1
      byte <- chars[[i]] %>% utf8ToInt() - 63
      result <- bitwOr(result, bitwAnd(byte, 0x1f) %>% bitwShiftL(shift))
      shift <- shift + 5
      if (byte < 0x20) break
    }

    if (bitwAnd(result, 1)) {
      result <- result %>% bitwShiftR(1) %>% bitwNot()
    } else {
      result <- result %>% bitwShiftR(1)
    }

    lats <- c(lats, (lats[[length(lats)]] + result))

    shift <- 0
    result <- 0
    byte <- 10000L

    while (byte >= 0x20) {
      i <- i + 1
      byte <- chars[[i]] %>% utf8ToInt() - 63
      result <- bitwOr(result, bitwAnd(byte, 0x1f) %>% bitwShiftL(shift))
      shift <- shift + 5
      if (byte < 0x20) break
    }

    if (bitwAnd(result, 1)) {
      result <- result %>% bitwShiftR(1) %>% bitwNot()
    } else {
      result <- result %>% bitwShiftR(1)
    }

    lons <- c(lons, (lons[[length(lons)]] + result))
  }

  decoded <- tibble::tibble(lat = lats[2:length(lats)]/1000000,
                            lng = lons[2:length(lons)]/1000000)

  return (decoded)
}








#' Source-to-Targets Origin/Destination Matrices with Valhalla
#'
#' HIGHLY EXPERIMENTAL AND UNDER DEVELOPMENT. NOT EVEN CLOSE TO DONE.
#'
#' @param froms
#' @param tos
#' @param chunk_size
#'
#' @return
#' @export
#'
#' @examples
sources_to_targets <- function(froms, tos, costing = "auto", chunk_size = 1){


  test_make <- list()

  test_make$sources = froms
  test_make$targets = tos
  test_make$costing = costing#"pedestrian"


  ## HYOPTHESIS: the qgis plugin works for pedestrians by running one-to-many over and over and over

  #test_make %>% jsonlite::toJSON() %>% nchar()
  #jsontext <-   '{"sources":[{"lat":45.409,"lon":-75.7099},{"lat":45.4414,"lon":-76.353},{"lat":45.4414,"lon":-76.353},{"lat":45.4414,"lon":-76.353},{"lat":45.2291,"lon":-76.1875}], "targets":[{"lat":45.4344,"lon":-76.3532},{"lat":45.4414,"lon":-76.353},{"lat":45.4414,"lon":-76.353},{"lat":45.4344,"lon":-76.3532},{"lat":45.4414,"lon":-76.353}],"costing":"pedestrian"}'
  #resp <- httr::POST(url = "http://localhost:8002/sources_to_targets", body = jsontext)


  resp2 <- httr::POST(url = "http://localhost:8002/sources_to_targets", body = test_make %>% jsonlite::toJSON(auto_unbox = TRUE))

  # resp %>%
  #   httr::content(type = "text") %>%
  #   jsonlite::fromJSON()

  #{"sources":[{"lat":40.744014,"lon":-73.990508},{"lat":40.739735,"lon":-73.979713},{"lat":40.752522,"lon":-73.985015},{"lat":40.750117,"lon":-73.983704},{"lat":40.750552,"lon":-73.993519}],"targets":[{"lat":40.744014,"lon":-73.990508},{"lat":40.739735,"lon":-73.979713},{"lat":40.752522,"lon":-73.985015},{"lat":40.750117,"lon":-73.983704},{"lat":40.750552,"lon":-73.993519}],"costing":"pedestrian"}&id=ManyToMany_NYC_work_dinner

  matrix <- resp2 %>% httr::content(type = "text") %>%
    jsonlite::fromJSON()

  mat_tibble <- matrix$sources_to_targets %>%
    tibble::enframe() %>%
    dplyr::select(-name) %>%
    tidyr::unnest(cols = value)

}



#' Title
#'
#' @param trip
#' @param all_details
#'
#' @return
#' @export
print_trip <- function(trip, all_details = FALSE) {
  cat (paste0("From lat/lng: ", trip$locations$lat[[1]], ", ", trip$locations$lon[[1]]))
  cat (paste0("\nTo   lat/lng: ", trip$locations$lat[[2]], ", ", trip$locations$lon[[2]]))
  cat (paste0("\nTime: ", round(trip$summary$time/60, digits = 1), " minutes"))
  cat (paste0("\nDist: ", trip$summary$length, " km\n"))

  if (all_details){
    maneuvers <- trip$legs$maneuvers[[1]]
    for (i in 1:nrow(maneuvers))  {
      sprintf("Step %d: %s\n", i, maneuvers[i,]$instruction) %>% cat()
      sprintf("   Dist: %3.2f km\n", maneuvers[i,]$length ) %>% cat()
      sprintf("   Time: %3.2f minutes\n", maneuvers[i,]$time/60) %>% cat()
    }

  }
}



## TESTING BAD FORM
#
# library(tidyverse)
#
#
# from = onsr::geocode_ottawa(tibble::tibble(address = "1243 Willowdale Ave"), address) %>% dplyr::select(-address) %>% dplyr::rename(lon = 2)
# tos = tibble::tibble(lat = runif(n = 10, min = 45.3, max = 45.4),
#                      lon = runif(n = 10, min = -75.8, max = -75.6))
#
# test <- sources_to_targets(from, tos)
#
# test
#
# valhalla_route(from %>% rename(lng = 2), tos[1,] %>% rename(lng = 2))
