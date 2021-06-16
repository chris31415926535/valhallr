#' Title
#'
#' @param route A route object from route.
#' @param hostname Hostname or IP address of your Valhalla instance. Defaults to "localhost".
#' @param port The port your Valhalla instance is monitoring. Defaults to 8002.

#'
#' @return a sf data.frame with cumulative distance, elevation and point geometry.
#' @export
#'
#' @examples
#' \dontrun{
#'library(valhallr)
#'
#' # set up origin and destination data
#' from = test_data("leeds_university")
#' to   = test_data("leeds_chemic_pub")
#'
#' # calculate the trip
#' trip = route(from, to, costing = "pedestrian")
#'
#' # get the elevation & cumulative distance
#' ele = route_elevation(trip)
#' }
#'

route_elevation = function(route, hostname = "localhost", port = 8002){
  # Collect arguments for the request
  shape = route$legs$shape
  post_json = list(
    range=TRUE,
    "encoded_polyline"=shape
  ) %>%
    jsonlite::toJSON(auto_unbox = TRUE)

  #Request the data
  url = paste0("http://",hostname,":",port,"/height")
  resp = httr::POST(url = url,
                     body = post_json,
                     httr::user_agent("https://github.com/chris31415926535/valhallr"))

  #Handle errors
  if (httr::http_type(resp) != "application/json") stop ("API did not return json.", call. = FALSE)
  if (httr::http_error(resp)){
    message("Error: API call returned error. Returning API response for debugging.")
    return(resp)
  }

  # Decode the json
  resp_data = jsonlite::fromJSON(httr::content(resp, type = "text", encoding = "UTF-8"))

  # API gave us a matrix, we would like a tibble:
  elevation = resp_data[[2]] %>%
    tibble::as_tibble(.name_repair = function(x) c("distance", "elevation"))

  # Collect results as a sf:
  decode(shape) %>%
    dplyr::bind_cols(elevation) %>% #Join the route and the request
    sf::st_as_sf(coords = c("lng", "lat"), crs = "WGS84")
}
