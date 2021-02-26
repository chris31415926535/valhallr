

#' Get Lat/Lon Coordinates for Testing
#'
#' This function gives quick access to lat/lon coordinates for a few points
#' around Ontario for testing and benchmarking purposes.
#'
#' @return A one-row tibble with a location's name, latitude, and longitude.
#' @export
test_data <- function(dataset){
  result <- tibble::tibble()
  dataset <- tolower(dataset)

  datasets <- tibble::tribble(~name, ~lat, ~lon,
                              "myhouse", 45.380738, -75.665578,
                              "uottawa", 45.423382, -75.683170,
                              "parliament", 45.424774, -75.699473,
                              "cntower", 43.642748, -79.386602,
                              "portagestore", 45.534769, -78.707470,
                              "cdntirecentre", 45.297533, -75.927875,
                              "zwicksisland", 44.153853, -77.387684,
                              "bignickel", 46.473435, -81.033971,
                              "kenora", 49.765876, -94.487444,
                              "killarney", 46.012289, -81.401437)

  result <- dplyr::filter(datasets, name == dataset)

  if (nrow(result) == 0){
    stop(paste0("Please specify a test dataset. Possible values are: ",
                stringr::str_flatten(datasets$name, collapse = ", ")))
  }

  return(result)

}


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
valhalla_route <- function(from = NA, to = NA, costing = "auto", unit = "kilometers", min_road_class = "residential", minimum_reachability = 50){
  # see API reference here
  #https://valhalla.readthedocs.io/en/latest/api/turn-by-turn/api-reference/

  post_data <- list()

  post_data$locations <- from %>%
    dplyr::select(lat, lon) %>% # = lng) %>%
    dplyr::bind_rows({
      to %>%
        dplyr::select(lat, lon)} # = lng)}
    ) %>%
    dplyr::bind_cols(tibble::tibble(search_filter = rep(list(list("min_road_class" = min_road_class)), 2))) %>%
    dplyr::bind_cols(tibble::tibble(minimum_reachability = rep(minimum_reachability, 2) ))


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
#
# trip <- valhalla_route(from, to, min_road_class="residential")
# print_trip(trip, all_details = TRUE)
#
# trip2 <- valhalla_route(from, to, min_road_class="motorway")
# print_trip(trip2, all_details = TRUE)
#
# #
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
sources_to_targets <- function(froms, tos, costing = "auto", chunk_size = 1, min_road_class = "residential", minimum_reachability = 50){


  test_make <- list()

  test_make$sources = froms %>% bind_cols(tibble(search_filter = rep(list(list("min_road_class" = min_road_class)), nrow(froms)))) %>%
    bind_cols(tibble(minimum_reachability = rep(minimum_reachability, nrow(froms)) ))

  test_make$targets = tos %>% bind_cols(tibble(search_filter = rep(list(list("min_road_class" = min_road_class)), nrow(tos)))) %>%
    bind_cols(tibble(minimum_reachability = rep(minimum_reachability, nrow(tos)) ))

  test_make$costing = costing

  resp2 <- httr::POST(url = "http://localhost:8002/sources_to_targets", body = test_make %>% jsonlite::toJSON(auto_unbox = TRUE))

  matrix <- resp2 %>% httr::content(type = "text") %>%
    jsonlite::fromJSON()

  mat_tibble <- matrix$sources_to_targets %>%
    tibble::enframe() %>%
    dplyr::select(-name) %>%
    tidyr::unnest(cols = value)

}



#' Generate Tidy Origin-Destination Data using Valhalla
#'
#' @description This function creates a tidy (i.e. long) tibble of
#'   origin-destination trip data using the Valhalla routing engine. For a set
#'   of o origins and d destinations, it returns a tibble with (o x d) rows with
#'   the travel distance and time between each pair. It can handle several
#'   different travel modes and routing options.
#'
#'   This function calls `valhalla::sources_to_targets()`, which interacts with
#'   the Valhalla API directly, but offers several user-friendly features.
#'
#'   * You can specify human-readable indices with `from_id_col` and
#'   `to_id_col`. (Valhalla's API only returns zero-indexed integer
#'   identifiers.)
#'   * You can specify a `batch_size` to break computation into
#'   several smaller API calls, to prevent your Valhalla instance from running
#'   out of memory. This seems especially important for pedestrian routing,
#'   where I've sometimes needed to use a batch size as small as 5.
#'
#'
#' @param froms A tibble containing origin locations in columns named `lat` and
#'   `lon`, and an optional column with human-readable names.
#' @param from_id_col The name of the column in `froms` that contains
#'   human-readable names.
#' @param tos A tibble containing destination locations in columns named `lat`
#'   and `lon`, and an optional column with human-readable names.
#' @param to_id_col The name of the column in `tos` that contains human-readable
#'   names.
#' @param costing The travel costing method: at present "auto" and "pedestrian"
#'   are supported.
#' @param batch_size The number of origin points to process per API call.
#' @param minimum_reachability The minimum number of nodes a candidate network
#'   needs to have before it is included. Try increasing this value (e.g. to
#'   500) if Valhalla is getting stuck in small disconnected road networks.
#'
#' @return
#' @export
od_matrix <- function(froms, from_id_col, tos, to_id_col, costing, batch_size, minimum_reachability){
  # FIXME TODO: do input validation!!

  # get the human-readable names of the from- and to-data
  from_names <- froms %>%
    select(from_id_col) %>%
    rowid_to_column(var = "from_index")

  to_names <- tos %>%
    select(to_id_col) %>%
    rowid_to_column(var = "to_index")

  # if human-readable column names are identical, append "_from" and "_to" so they differ
  if (from_id_col == to_id_col) {
    new_from <- paste0(from_id_col,"_from")
    from_names <- rename(from_names, !!(new_from) := from_id_col)
    from_id_col <- new_from

    new_to <- paste0(to_id_col, "_to")
    to_names <- rename(to_names, !!(new_to) := to_id_col)
    to_id_col <- new_to
  }

  # set up our batching
  n_iters <- nrow(froms) %/% batch_size + 1
  results <- list(rep(NA, n_iters))

  # do each batch
  for (i in 1:n_iters){
    message(paste0(i,"/",n_iters))
    start_index <- (i-1)*batch_size + 1
    end_index <- min( (i*batch_size), nrow(froms))

    froms_iter = froms[start_index:end_index, ] %>%
      drop_na()
    od <- valhallr::sources_to_targets(froms= froms_iter, tos = tos, costing = costing, minimum_reachability = minimum_reachability)

    # FIXME TODO: confirm that sources_to_targets gave us meaningful data!

    # make start_index match the original DB row number and doc row number
    od <- od %>%
      mutate(from_index = from_index + start_index,
             to_index = to_index + 1) %>%
      left_join(from_names, by = "from_index") %>%
      left_join(to_names, by = "to_index") %>%
      select(-to_index, -from_index)

    # add results to our pre-built list
    results[[i]] <- od

  }

  # get results back into a tibble
  output <- results %>%
    enframe() %>%
    unnest(value) %>%
    select(from_id_col, to_id_col, distance, time)

  return(output)
  #
#   output  %>%
#     select(from_id_col, to_id_col, distance, time) %>%
#     write_csv(paste0("data/valhalla_matrix_",costing,"_target.csv"))
}

#
# # # ## TESTING BAD FORM
# library(tidyverse)
# library(valhallr)
# library(sf)
#
# tos = tibble::tibble(lat=45.5, lon=-75)
# froms = tibble::tibble(lon = -75.84322, lat = 45.10085)
#
# od <- sources_to_targets(froms, tos, min_road_class="residential")
# od
#
# od2 <- sources_to_targets(froms, tos, min_road_class="service_other")
# od2
#
# trip_res <- valhalla_route(froms, tos)
# print_trip(trip_res)
#
# trip_svc <- valhalla_route(froms, tos, min_road_class = "service_other")
# print_trip(trip_svc)
#
# trip_mw <- valhalla_route(froms, tos, min_road_class="motorway")
# print_trip(trip2, all_details = TRUE)
#
# map_trip(trip_svc)
# map_trip(trip_res)
#
# # # #
#





#' Print Trip Summary and Turn-By-Turn Directions
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




#' Make a Leaflet Map from a Trip
#'
#' @param trip
#' @param method Which mapping service to use. Defaults to leaflet; also can use ggplot.
#'
#' @return
#' @export
map_trip <- function(trip, method = "leaflet"){

  ## decode and turn into a sf line
  trip_shp <- valhallr::decode(trip$legs$shape) %>%
    sf::st_as_sf(coords = c("lng", "lat"), crs = "WGS84")  %>%
    dplyr::summarise(do_union = FALSE) %>%
    sf::st_cast("LINESTRING")

  # then plot with leaflet
  if (method == "leaflet"){
    trip_shp %>%
      leaflet::leaflet() %>%
      leaflet::addTiles() %>%
      leaflet::addPolylines()
  }

  if (method == "ggplot"){
    trip_shp %>%
      ggplot2::ggplot() +
      ggplot2::geom_sf()

  }
}


#' Generate isochrones
#'
#' @param from
#' @param costing
#' @param contours_time
#' @param min_road_class
#' @param minimum_reachability
#'
#' @return
#' @export
#'
#' @examples
isochrone <- function(from, costing = "pedestrian", contours_time = c(5, 10, 15), min_road_class = "residential", minimum_reachability = 500){
  # see API reference here
  #https://valhalla.readthedocs.io/en/latest/api/turn-by-turn/api-reference/

  post_data <- list()

  post_data$locations <- from %>%
    dplyr::select(lat, lon) #%>%
    #dplyr::bind_cols(tibble::tibble(search_filter =list("min_road_class" = min_road_class))) %>%
    #dplyr::bind_cols(tibble::tibble(minimum_reachability = minimum_reachability ))


  post_data$costing <- costing

  # FIXME handle colours better and multiple contours!!
  post_data$contours <-  tibble::tibble(time = contours_time)

  post_data$polygons <- TRUE


  post_json <- post_data %>%
    jsonlite::toJSON(auto_unbox = TRUE)

  #post_json <- '{"locations":[{"lat":45.380738,"lon":-75.665578}],"costing":"pedestrian","contours":[{"time":15,"color":"ff0000"}]}'

  resp <- httr::POST(url = "http://localhost:8002/isochrone", body = post_json)

  resp_data <- resp %>%
    httr::content(type = "text") %>%
    geojsonio::geojson_sf() %>%
    tibble::as_tibble() %>%
    sf::st_as_sf()

  return (resp_data)

}



#' Generate maps of isochrones
#'
#' @param isochrome
#' @param method
#'
#' @return
#' @export
#'
#' @examples
map_isochrone <- function(isochrone, method = "leaflet") {

  metric_name <- "ERROR: METRIC NOT DETECTED"
  output <- "ERROR: Please supply method leaflet or ggplot."
  if (isochrone$metric[[1]] == "time") metric_name <- "Minutes"
  if (isochrone$metric[[1]] == "distance") metric_name <- "Kilometres"

  if (method == "leaflet"){

    iso_labels <- paste0(isochrone$contour, " ", metric_name) %>%
      purrr::map(htmltools::HTML)

    output <- isochrone %>%
      leaflet::leaflet() %>%
      leaflet::addTiles() %>%
      leaflet::addPolygons(fillColor = ~ color,
                           label = iso_labels)
  }

  if (method == "ggplot"){
    output <- isochrone %>%
      tibble::as_tibble() %>%
      sf::st_as_sf() %>%
      ggplot2::ggplot() +
      ggspatial::annotation_map_tile() +
      ggplot2::geom_sf(ggplot2::aes(fill = contour),
                       alpha = 0.3) +
      ggplot2::labs(fill = metric_name)
  }

  return(output)
}
