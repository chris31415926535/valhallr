

#' Get Lat/Lon Coordinates for Testing
#'
#' This function gives quick access to lat/lon coordinates for a few points
#' around Ontario for testing and benchmarking purposes.
#'
#' @param dataset The name of a test dataset. By default, and if an unknown input
#' is given, it returns all values.
#'
#' @importFrom magrittr %>%
#' @return A tibble with one or more location names, latitudes, and longitudes.
#' @export
test_data <- function(dataset = NA){
  name <- NULL
  datasets <- tibble::tribble(~name, ~lat, ~lon,
                              "uottawa", 45.423382, -75.683170,
                              "parliament", 45.424774, -75.699473,
                              "cntower", 43.642748, -79.386602,
                              "portagestore", 45.534769, -78.707470,
                              "cdntirecentre", 45.297533, -75.927875,
                              "zwicksisland", 44.153853, -77.387684,
                              "bignickel", 46.473435, -81.033971,
                              "kenora", 49.765876, -94.487444,
                              "killarney", 46.012289, -81.401437)

  result <- tibble::tibble()

  result <- dplyr::filter(datasets, name == dataset)
  if (nrow(result) == 0) result <- datasets

  return(result)

}


#' Point-to-Point Routing with Valhalla
#'
#' This function calls Valhalla's `route` API to return turn-by-turn directions from one
#' origin to one destination. Several costing methods are supported, and there are
#' parameters that let you give custom options to Valhalla. **Please note that this
#'   function requires access to a running instance of Valhalla.**
#'
#' For more details, please check the Valhalla API documentation here:
#'
#' * [https://valhalla.readthedocs.io/en/latest/api/turn-by-turn/api-reference/](https://valhalla.readthedocs.io/en/latest/api/turn-by-turn/api-reference/)
#'
#' @param from A tibble containing one origin location in columns named `lat` and
#'   `lon`.
#' @param to A tibble containing one destination location in columns named `lat` and
#'   `lon`.
#' @param costing The travel costing method. Values "auto", "bicycle", and "pedestrian"
#'   all work.
#' @param unit Distance measurement units. Defaults to "kilometres".
#' @param minimum_reachability The minimum number of nodes a candidate network
#'   needs to have before it is included. Try increasing this value (e.g. to
#'   500) if Valhalla is getting stuck in small disconnected road networks.
#' @param from_search_filter A named list of options provided to Valhalla API. Defaults set a
#'   maximum road class ("motorway", the highest) and minimum road class ("residential",
#'   which is one above the lowest, "service_other"). See API documentation for details.
#' @param to_search_filter A named list of options provided to Valhalla API. Defaults set a
#'   maximum road class ("motorway", the highest) and minimum road class ("residential",
#'   which is one above the lowest, "service_other"). See API documentation for details.
#' @param costing_options A named list of options provided to the Valhalla API that affect route costing,
#'   e.g. willingness to travel on highways or through alleys. See API documentation for details.
#' @param hostname Hostname or IP address of your Valhalla instance. Defaults to "localhost".
#' @param port The port your Valhalla instance is monitoring. Defaults to 8002.
#' @return A trip object.
#'
#' @examples
#' \dontrun{
#'   library(valhallr)
#'   # set up origin and destination data
#'   from <- test_data("uottawa")
#'   to <- test_data("cdntirecentre")
#'
#'   # calculate the trip
#'   trip <- route(from = from, to = to)
#'
#'   # show overall trip information
#'   print_trip(trip, all_details = FALSE)
#'
#'   # make an interactive map of the trip using the leaflet package
#'   map_trip(trip, method = "leaflet")
#'}
#' @export
route <- function(from = NA, to = NA, costing = "auto", unit = "kilometers", from_search_filter = list(max_road_class = "motorway", min_road_class = "residential"), to_search_filter = list(max_road_class = "motorway", min_road_class = "residential"),minimum_reachability = 50, costing_options = list(), exclude_polygons = NA, hostname = "localhost", port = 8002){
  # see API reference here
  #https://valhalla.readthedocs.io/en/latest/api/turn-by-turn/api-reference/

  if ((nrow(from) > 1 ) | (nrow(to) > 1)) stop("Either `from` or `to` has more than one row. Please supply one-row tibbles with `lat` and `lon` columns.")

  post_data <- list()

  post_data$locations <- from %>%
    dplyr::select("lat", "lon") %>%
    dplyr::bind_rows({
      to %>%
        dplyr::select("lat", "lon")}
    ) %>%
    dplyr::bind_cols(tibble::tibble(search_filter = list(from_search_filter, to_search_filter))) %>%
    dplyr::bind_cols(tibble::tibble(minimum_reachability = rep(minimum_reachability, 2) ))

  post_data$costing = costing
  if (costing == "auto") post_data$costing_options$auto = costing_options
  if (costing == "pedestrian") post_data$costing_options$pedestrian = costing_options
  if (costing == "bicycle") post_data$costing_options$bicycle = costing_options
  if (costing == "truck") post_data$costing_options$truck = costing_options

  post_data$directions_options$units = unit

  # add polygons to exclude, if we are given any.
  if (any(!is.na(exclude_polygons))){
    # if we get any NA inputs throw an error
    if (any(is.na(exclude_polygons))) warning("NAs supplied to exclude_polygons. Please supply either one tibble or a list of tibbles with columns `lat` and `lon`.")

    # must be in a list, so if it's just one tibble then put it in a list
    if(tibble::is_tibble(exclude_polygons)) exclude_polygons <- list(exclude_polygons)

    # each polygon must be a tibble with lat and lon columns, make sure that's true
    names_good <- purrr::map_lgl(exclude_polygons,
                                 function(x)( "lat" %in% names(x) ) & ("lon" %in% names(x)))
    if (!all(names_good)) stop ("`exclude_polygons` must be a tibble or list of tibbles with columns named `lat` and `lon`.")

    # set the input data using exclude_polygons. from the API docs, it must be
    # one or more exterior rings of polygons in the form of nested JSON arrays,
    # e.g. [[[lon1, lat1], [lon2,lat2]],[[lon1,lat1],[lon2,lat2]]]

    post_data$exclude_polygons  <- exclude_polygons %>%
      purrr::map(function(x) {
        select(x, lon, lat) %>%
          unlist() %>%
          matrix(ncol = 2)
      })
  } # end exclude_polygons processing

  post_json <- jsonlite::toJSON(post_data, auto_unbox = TRUE)

  url <- paste0("http://",hostname,":",port,"/route")
  resp <- httr::POST(url = url,
                     body = post_json,
                     httr::user_agent("https://github.com/chris31415926535/valhallr"))

  if (httr::http_type(resp) != "application/json") stop ("API did not return json.", call. = FALSE)
  if (httr::http_error(resp)){
    message("Error: API call returned error. Returning API response for debugging.")
    return(resp)
  }

  resp_data <- jsonlite::fromJSON(httr::content(resp, type = "text", encoding = "UTF-8"))

  return(resp_data[[1]])
}



#' Decode Valhalla Route Shape
#'
#' For point-to-point routing, Valhalla's API provides a route shapefile in a
#' special ASCII-encoded format. This function takes an encoded string, decodes
#' it, and returns the lat/lon coordinates as a tibble.
#'
#' To map the results, see also `valhallr::map_trip()`.
#'
#' @param encoded An encoded shapefile in ASCII format from Valhalla's API.
#'
#' @return A tibble containing point locations in `lat` and `lon` columns.
#' @export
decode <- function(encoded) {
  # got algorithm from here (but I wrote the R version) https://valhalla.readthedocs.io/en/latest/decoding/
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
#' @description This function creates a tidy (i.e. long) table of
#'   origin-destination trip data using the Valhalla routing engine. For a set
#'   of o origins and d destinations, it returns a tibble with (o x d) rows with
#'   the travel distance and time between each pair. It can handle several
#'   different travel modes and routing options. **Please note that this
#'   function requires access to a running instance of Valhalla.**
#'
#'   This function provides fine-grained control over Valhalla's API options.
#'
#'   * For a user-friendly function, see the function `valhallr::od_table()`.
#'   * For details about the API, see Valhalla's documentation here: [https://valhalla.readthedocs.io/en/latest/api/matrix/api-reference/](https://valhalla.readthedocs.io/en/latest/api/matrix/api-reference/)
#'
#'
#' @param froms A tibble containing origin locations in columns named `lat` and
#'   `lon`.
#' @param tos A tibble containing destination locations in columns named `lat` and
#'   `lon`.
#' @param costing The travel costing method: at present "auto", "bicycle", and "pedestrian"
#'   are supported.
#' @param minimum_reachability The minimum number of nodes a candidate network
#'   needs to have before it is included. Try increasing this value (e.g. to
#'   500) if Valhalla is getting stuck in small disconnected road networks.
#' @param from_search_filter A named list of options provided to Valhalla API. Defaults set a
#'   maximum road class ("motorway", the highest) and minimum road class ("residential",
#'   which is one above the lowest, "service_other"). See API documentation for details.
#' @param to_search_filter A named list of options provided to Valhalla API. Defaults set a
#'   maximum road class ("motorway", the highest) and minimum road class ("residential",
#'   which is one above the lowest, "service_other"). See API documentation for details.
#' @param costing_options A named list of options provided to the Valhalla API that affect route costing,
#'   e.g. willingness to travel on highways or through alleys. See API documentation for details.
#' @param hostname Hostname or IP address of your Valhalla instance. Defaults to "localhost".
#' @param port The port your Valhalla instance is monitoring. Defaults to 8002.
#' @return A tibble showing the trip distances and times from each origin to each destination.
#'
#' @examples
#' \dontrun{
#' # NOTE: Assumes an instance of Valhalla is running on localhost:8002.
#' library(dplyr)
#' library(valhallr)
#' froms <- bind_rows(test_data("parliament"), test_data("uottawa"))
#' tos <- bind_rows(test_data("cdntirecentre"), test_data("parliament"))
#' st <- sources_to_targets(froms, tos)
#' }
#' @export
sources_to_targets <- function(froms, tos, costing = "auto",from_search_filter = list(max_road_class = "motorway", min_road_class = "residential"), to_search_filter = list(max_road_class = "motorway", min_road_class = "residential"), minimum_reachability = 50, costing_options = list(), exclude_polygons = NA, hostname = "localhost", port = 8002){

  post_data <- list()

  post_data$sources = froms %>%
    dplyr::bind_cols(tibble::tibble(search_filter = rep(list(from_search_filter)), nrow(froms)) ) %>%
    #dplyr::bind_cols(tibble::tibble(search_filter = rep(list(list("min_road_class" = min_road_class)), nrow(froms)))) %>%
    dplyr::bind_cols(tibble::tibble(minimum_reachability = rep(minimum_reachability, nrow(froms)) ))

  post_data$targets = tos %>%
    dplyr::bind_cols(tibble::tibble(search_filter = rep(list(to_search_filter)), nrow(tos)) ) %>%
    #dplyr::bind_cols(tibble::tibble(search_filter = rep(list(list("min_road_class" = min_road_class)), nrow(tos)))) %>%
    dplyr::bind_cols(tibble::tibble(minimum_reachability = rep(minimum_reachability, nrow(tos)) ))

  post_data$costing = costing
  if (costing == "auto") post_data$costing_options$auto = costing_options
  if (costing == "pedestrian") post_data$costing_options$pedestrian = costing_options
  if (costing == "bicycle") post_data$costing_options$bicycle = costing_options
  if (costing == "truck") post_data$costing_options$truck = costing_options


  # add polygons to exclude, if we are given any.
  if (any(!is.na(exclude_polygons))){
    # if we get any NA inputs throw an error
    if (any(is.na(exclude_polygons))) warning("NAs supplied to exclude_polygons. Please supply either one tibble or a list of tibbles with columns `lat` and `lon`.")

    # must be in a list, so if it's just one tibble then put it in a list
    if(tibble::is_tibble(exclude_polygons)) exclude_polygons <- list(exclude_polygons)

    # each polygon must be a tibble with lat and lon columns, make sure that's true
    names_good <- purrr::map_lgl(exclude_polygons,
                                 function(x)( "lat" %in% names(x) ) & ("lon" %in% names(x)))
    if (!all(names_good)) stop ("`exclude_polygons` must be a tibble or list of tibbles with columns named `lat` and `lon`.")

    # set the input data using exclude_polygons. from the API docs, it must be
    # one or more exterior rings of polygons in the form of nested JSON arrays,
    # e.g. [[[lon1, lat1], [lon2,lat2]],[[lon1,lat1],[lon2,lat2]]]

    post_data$exclude_polygons  <- exclude_polygons %>%
      purrr::map(function(x) {
        select(x, lon, lat) %>%
          unlist() %>%
          matrix(ncol = 2)
      })
  } # end exclude_polygons processing


  post_json <- jsonlite::toJSON(post_data, auto_unbox = TRUE)

  url <- paste0("http://",hostname,":",port,"/sources_to_targets")
  resp <- httr::POST(url = url,
                     body = post_json,
                     httr::user_agent("https://github.com/chris31415926535/valhallr"))

  if (httr::http_type(resp) != "application/json") stop ("API did not return json.", call. = FALSE)
  if (httr::http_error(resp)){
    message("Error: API call returned error. Returning API response for debugging.")
    return(resp)
  }

  matrix <- jsonlite::fromJSON(httr::content(resp, type = "text", encoding = "UTF-8"))

  mat_tibble <- matrix$sources_to_targets %>%
    tibble::enframe() %>%
    dplyr::select(-"name") %>%
    tidyr::unnest(cols = "value")

}



#' Generate Tidy Origin-Destination Data using Valhalla
#'
#' @description This function creates a tidy (i.e. long) table of
#'   origin-destination trip data using the Valhalla routing engine. For a set
#'   of o origins and d destinations, it returns a tibble with (o x d) rows with
#'   the travel distance and time between each pair. It can handle several
#'   different travel modes and routing options.
#'
#'   This function is a user-friendly wrapper around`valhalla::sources_to_targets()`,
#'   which calls the Valhalla API directly. `sources_to_targets()` offers finer-
#'   grained control over API options, and so this latter function may be more
#'   useful for advanced users.
#'
#'   Notable features of `od_matrix()`:
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
#' @param costing The travel costing method: at present "auto", "bicycle", and
#' "pedestrian" are supported.
#' @param batch_size The number of origin points to process per API call.
#' @param minimum_reachability The minimum number of nodes a candidate network
#'   needs to have before it is included. Try increasing this value (e.g. to
#'   500) if Valhalla is getting stuck in small disconnected road networks.
#' @param verbose Boolean. Defaults to FALSE. If TRUE, it will provide updates on
#'   on the batching process (if applicable).
#' @param hostname Hostname or IP address of your Valhalla instance. Defaults to "localhost".
#' @param port The port your Valhalla instance is monitoring. Defaults to 8002.
#'
#' @return A tibble showing the trip distances and times from each origin to each named destination.
#' @importFrom rlang :=
#' @examples
#' \dontrun{
#' library(dplyr)
#' library(valhallr)
#' # set up our inputs
#' origins <- bind_rows(test_data("parliament"), test_data("uottawa"), test_data("cntower"))
#' destinations <- bind_rows(test_data("cdntirecentre"), test_data("parliament"))
#'
#' # generate a tidy origin-destination table
#' od <- od_table (froms = origins,
#'                 from_id_col = "name",
#'                 tos = destinations,
#'                 to_id_col = "name",
#'                 costing = "auto",
#'                 batch_size = 100,
#'                 minimum_reachability = 500)
#' }
#' @export
od_table <- function(froms, from_id_col, tos, to_id_col, costing = "auto", batch_size = 100, minimum_reachability = 500, verbose = FALSE, exclude_polygons = NA, hostname = "localhost", port = 8002){
  # note: got importFrom rlang trick here: https://stackoverflow.com/questions/58026637/no-visible-global-function-definition-for
  from_index <- to_index <- NULL

  # Basic input validation
  if ((!"lat" %in% colnames(froms)) | (!"lon" %in% colnames(tos))) stop("Input data `froms` and `tos` must both have columns named `lat` and `lon`. ")

  # temporarily rename the data columns so they're easier to work with
  # we'll put the names back right at the end
  froms <- rename(froms, .from_id_col = {{from_id_col}})
  tos   <- rename(tos, .to_id_col = {{to_id_col}})

  # get the human-readable names of the from- and to-data
  from_names <- froms %>%
    dplyr::select(.from_id_col) %>%
    tibble::rowid_to_column(var = "from_index")

  to_names <- tos %>%
    dplyr::select(.to_id_col) %>%
    tibble::rowid_to_column(var = "to_index")

  # if human-readable column names are identical, append "_from" and "_to" so they differ
  if (from_id_col == to_id_col) {
    #new_from <- paste0(from_id_col,"_from")
    #from_names <- dplyr::rename(from_names, !!(new_from) := from_id_col)
    from_id_col <- paste0(from_id_col,"_from")

    #new_to <- paste0(to_id_col, "_to")
    #to_names <- dplyr::rename(to_names, !!(new_to) := to_id_col)
    to_id_col <- paste0(to_id_col, "_to")
  }

  # set up our batching
  n_iters <- ceiling(nrow(froms) / batch_size)
  results <- list(rep(NA, n_iters))

  # do each batch
  for (i in 1:n_iters){
    if (verbose) message(paste0(i,"/",n_iters))
    start_index <- (i-1)*batch_size + 1
    end_index <- min( (i*batch_size), nrow(froms))

    froms_iter = froms[start_index:end_index, ] %>%
      tidyr::drop_na()

    od <- valhallr::sources_to_targets(froms = froms_iter,
                                       tos = tos,
                                       costing = costing,
                                       minimum_reachability = minimum_reachability,
                                       exclude_polygons = exclude_polygons,
                                       hostname = hostname,
                                       port = port)

    # POTENTIAL TODO: validate the API response from sources_to_targets()

    # make start_index match the original DB row number and doc row number
    od <- od %>%
      dplyr::mutate(from_index = from_index + start_index,
                    to_index = to_index + 1) %>%
      dplyr::left_join(from_names, by = "from_index") %>%
      dplyr::left_join(to_names, by = "to_index") %>%
      dplyr::select(-to_index, -from_index)

    # add results to our pre-built list
    results[[i]] <- od
  }

  # get results from a list of tibbles back into a single tibble
  # this has been rewritten speed and memory optimization
  # originally it was much shorter and used tidyr::unnest() but it was a major
  # bottleneck for large datasets (and would even fail)

  # get info about our results
  n_results <- length(results)
  n_rows <- purrr::map_dbl(results, nrow)
  total_rows <- sum(n_rows)

  # set up our final output tibble
  output <- tibble::tibble(.from_id_col = rep(NA, total_rows),
                           .to_id_col = rep(NA, total_rows),
                           distance = rep(NA_real_, total_rows),
                           time = rep(NA_real_, total_rows))

  # loop through each batch of results, move the results from the batch to the
  # final output tibble. a loop is much faster than using tidyr::unnest() here
  # because it modifies in place and isn't binding rows.
  for (i in 1:n_results) {

    # set our start and end indices
    index_start <- 1
    index_end <- n_rows[[1]]
    if (i != 1){
      index_start <- sum(n_rows[1:(i-1)]) + 1
      index_end <- sum(n_rows[1:i])
    }

    # overwrite the relevant bits of the vectors
    output$distance[index_start:index_end] <- results[[i]]$distance
    output$time[index_start:index_end] <- results[[i]]$time
    output$.from_id_col[index_start:index_end] <- results[[i]]$.from_id_col
    output$.to_id_col[index_start:index_end] <- results[[i]]$.to_id_col
  }

  # set the names back
  output <- dplyr::rename(output,
                          {{from_id_col}} := .from_id_col,
                          {{to_id_col}} := .to_id_col)

  # voila
  return(output)
}




#' Print Trip Summary and Turn-By-Turn Directions
#'
#' @param trip A trip response from `valhallr::route()`.
#' @param all_details Boolean. Should we print each turn-by-turn instruction
#'   along with an overall summary?
#'
#' @return The input `trip` object, invisibly.
#' @inherit route examples
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

  invisible(trip)
}





#' Make a Map from a Trip
#'
#' @param trip A trip response from `valhallr::route()`.
#' @param method Which mapping service to use. Defaults to leaflet; also can use ggplot.
#'
#' @return A map object, either leaflet or ggplot.
#' @inherit route examples
#' @export
map_trip <- function(trip, method = "leaflet"){

  ## decode and turn into a sf line
  trip_shp <- valhallr::decode(trip$legs$shape) %>%
    sf::st_as_sf(coords = c("lng", "lat"), crs = "WGS84")  %>%
    dplyr::summarise(do_union = FALSE) %>%
    sf::st_cast("LINESTRING")

  # then plot with leaflet
  if (method == "leaflet"){
    trip_plot <- trip_shp %>%
      leaflet::leaflet() %>%
      leaflet::addTiles() %>%
      leaflet::addPolylines()
  }

  if (method == "ggplot"){
    trip_plot <- trip_shp %>%
      ggplot2::ggplot() +
      ggspatial::annotation_map_tile(progress = "none",
                                     zoomin = -1,
                                     cachedir = tempdir()) +
      ggplot2::geom_sf(colour = "blue", size = 2)

  }

  trip_plot
}


#' Generate Isochrones
#'
#' An isochrone, also known as a service area, is a polygon that shows the
#' area reachable from a starting point by traveling along a road network
#' for a certain distance or time. This function provides an interface to
#' the Valhalla routing engine's isochrone API. It lets you provide a starting
#' point's latitude and longitude, a distance or time metric, and a vector
#' of distances/times, and if it's successful it returns an sf-class tibble of
#' polygons.
#'
#' More more information, please see Valhalla's API documentation:
#'
#' * [https://valhalla.readthedocs.io/en/latest/api/isochrone/api-reference/](https://valhalla.readthedocs.io/en/latest/api/isochrone/api-reference/)
#'
#' @param from A tibble containing one origin location in columns named `lat` and
#'   `lon`.
#' @param costing The travel costing method: at present "auto", "bicycle", and "pedestrian"
#'   are supported.
#' @param contours A numeric vector of values at which to produce the isochrones.
#' @param metric Distance or time. Accepts parameters "min" and "km".
#' @param min_road_class The minimum road classification Valhalla will consider. Defaults to `residential`.
#' @param minimum_reachability The minimum number of nodes a candidate network
#'   needs to have before it is included.
#' @param hostname Hostname or IP address of your Valhalla instance. Defaults to "localhost".
#' @param port The port your Valhalla instance is monitoring. Defaults to 8002.
#'
#' @return An sf/tibble object containing isochrone polygons.
#' @examples
#' \dontrun{
#' library(valhallr)
#' # set up our departure point: the University of Ottawa
#' from <- test_data("uottawa")
#'
#' # generate a set of isochrones for travel by bicycle
#' i <- valhallr::isochrone(from, costing = "bicycle")
#'
#' # map the isochrones
#' map_isochrone(i)
#' }
#' @export
isochrone <- function(from, costing = "pedestrian", contours = c(5, 10, 15), metric = "min", min_road_class = "residential", minimum_reachability = 500, hostname = "localhost", port = 8002){
  # see API reference here
  # https://valhalla.readthedocs.io/en/latest/api/isochrone/api-reference/

  # validating input
  if (nrow(from) > 1) stop ("More than one location supplied. Please supply a one-row input tibble with `lat` and `lon` columns.")
  if (! (("lat" %in% names(from)) & ("lon" %in% names(from))) ) stop ("From tibble must inclide one column named `lat` and one named `lon`.")
  if (!metric %in% c("min", "km")) stop ("Invalid metric. Please use `min` for time in minutes or `km` for distance in kilometres.")

  post_data <- list()

  post_data$locations <- dplyr::select(from, "lat", "lon")
  post_data$costing <- costing
  if (metric == "min") post_data$contours <-  tibble::tibble(time = contours)
  if (metric == "km")  post_data$contours <-  tibble::tibble(distance = contours)

  post_data$polygons <- TRUE

  post_json <- jsonlite::toJSON(post_data, auto_unbox = TRUE)

  url <- paste0("http://",hostname,":",port,"/isochrone")
  resp <- httr::POST(url = url,
                     body = post_json,
                     httr::user_agent("https://github.com/chris31415926535/valhallr"))

  if (httr::http_type(resp) != "application/json") stop ("API did not return json.", call. = FALSE)
  if (httr::http_error(resp)){
    message("Error: API call returned error. Returning API response for debugging.")
    return(resp)
  }

  resp_data <- httr::content(resp, type = "text", encoding = "UTF-8") %>%
    geojsonio::geojson_sf() %>%
    tibble::as_tibble() %>%
    sf::st_as_sf()

  resp_data$costing <- costing

  return (resp_data)

}



#' Generate maps of isochrones
#'
#' This is a convenience function that takes the output of `valhallr::isochrone()`
#' and generates either a static or interactive map.
#'
#' @param isochrone An isochrone sf object generated by `valhallr::isochrone()`.
#' @param method The method used to map it. Two methods are supported:
#'  * "leaflet" produces an interactive HTML map using the Leaflet package.
#'  * "ggplot" produces a static map.
#'
#' @return A plot of the isochrones, either a a leaflet object or a ggplot object.
#' @inherit isochrone examples
#' @export
map_isochrone <- function(isochrone, method = "leaflet") {
  contour <- NULL
  if (!method %in% c("leaflet", "ggplot")) stop ("Invalid map method. Please specify `leaflet` or `ggplot`.")

  metric_name <- "ERROR: METRIC NOT DETECTED"
  costing_name <- "ERROR: COSTING NOT DETECTED"


  if (isochrone$metric[[1]] == "time")     metric_name <- "Minutes"
  if (isochrone$metric[[1]] == "distance") metric_name <- "Kilometres"
  if (isochrone$costing[[1]] == "auto")    costing_name <- "Driving"
  if (isochrone$costing[[1]] == "pedestrian")    costing_name <- "Walking"
  if (isochrone$costing[[1]] == "bicycle")    costing_name <- "Cycling"

  if (method == "leaflet"){

    iso_labels <- paste0(isochrone$contour, " ", metric_name, " ", costing_name) %>%
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
      ggspatial::annotation_map_tile(progress = "none",
                                     zoomin = -1,
                                     cachedir = tempdir()) +
      ggplot2::geom_sf(ggplot2::aes(fill = contour),
                       alpha = 0.3) +
      ggplot2::labs(fill = metric_name)
  }

  return(output)
}




#' Convert `sf` objects to **valhallr**-friendly tibbles
#'
#' @description This function converts simple feature (sf) POINT objects to tidy
#' tibbles that you can feed to **valhallr** functions. This is handy if you're
#' working with a lots of geospatial data and would like to run a set of
#' locations through, for example, `valhallr::od_table()`.
#'
#' @param data A simple feature collection with geometry class POINT.
#' @param output_crs The desired output coordinate reference system (CRS).
#' Defaults to WGS84.
#'
#' @return A non-sf tibble with new columns `lat` and `lon` containing latitudes
#'  and longitudes respectively that can be fed into other **valhallr**
#'  functions like `valhallr::od_table()`.
#' @export
sf_to_latlon <- function(data, output_crs = "WGS84"){
  # input validation: make sure it's an sf object
  if (!"sf" %in% class(data)) stop ("Input data is not a simple feature object with class sf.")
  # make sure it doesn't already have lat/lon columns
  if (("lat" %in% colnames(data)) | "lon" %in% colnames(data)) stop ("Input data already has columns named `lat` and/or `lon`.")
  # make sure it's all point data
  if (!(unique(sf::st_geometry_type(data)) == "POINT")) stop ("Input data is not exclusively point data.")

  .coords <- data %>%
    sf::st_transform(crs = output_crs) %>%
    sf::st_coordinates() %>%
    tibble::as_tibble() %>%
    dplyr::rename(lon = X, lat = Y)

  data %>%
    sf::st_set_geometry(NULL) %>%
    dplyr::bind_cols(.coords)

}
