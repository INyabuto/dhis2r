#'Modify a DHIS2 endpoint
#'
#'Short hand to modify a `DHIS2 like` resource endpoint or an API link from R.
#'This is useful when defining or modifying a DHIS2 API link.
#'
#'\code{modify_endpoint} defines or modifies an API link from an endpoint or API
#'params. It parses the API parameters first into a list before building the API
#'link.
#'
#'
#'@name modify_endpoint
#'
#'@param endpoint A DHIS2 resource or on an API endpoint link, for example
#'  `dataElements` or  `api/dataElements`.
#'@param id The unique identifier of an object in the endpoint, a character
#'  string of size 11.
#'@param ref An endpoint reference, a character string.
#'@param ref_id The unique identifier of an object in the referenced endpoint, a
#'  character string of size 11.
#'@param version A version of the web API.
#'@param ... Additional parameters passed to the \code{\link{api_query}}.
#'@return A URL, a DHIS2 web API link.
#'@export
#' @examples
#' modify_endpoint() # default api link
#' modify_endpoint(endpoint = "dataElements") # point to dataElements endpoint
#' # refer to a specific dataElement object
#' modify_endpoint(endpoint = "dataElements", id = "ID")
#' # modify an object ID of an API link, `api/dataElements/ID`
#' modify_endpoint(endpoint = "api/dataElements/ID", id = "ID2")
#' # modify the version of an API link, `api/dataElements/ID`
#' version <- 29L
#' modify_endpoint(endpoint = "api/dataElements/ID", version = version)
#' # select data elements name, id and shortNames
#' modify_endpoint(endpoint = "dataElements", fields = c("name", "id", "shortNames"))
#' # arrange the data elements name in ascending order
#' modify_endpoint(
#'   endpoint = "dataElements", fields = c("name", "id", "shortNames"),
#'   order = api_order("name", by = "asc")
#' )
#'@export

modify_endpoint <- function(endpoint = NULL, id = NULL, ref = NULL, ref_id = NULL,
                                version = NULL, ...) {
  params <- parse_endpoint(endpoint = endpoint, ..., id = id, ref = ref, ref_id = ref_id, version = version)

  build_endpoint(params)
}


#' Parse DHIS2 API endpoint parameters
#'
#' This function parse DHIS2 API endpoint parameters into a list object.
#'
#' @inheritParams modify_endpoint
#'
#' @return An S3 object of class `api_params`.
#' @export
#' @examples
#' # default endpoint endpoint
#' endpoint_params <- parse_endpoint()
#' endpoint_params
#' # endpoint params with a DHIS2 query
#' endpoint_params <- parse_endpoint(endpoint = "dataElements", field = c("name", "code", "id"))
#' endpoint_params
parse_endpoint <- function(endpoint = NULL, id = NULL, ref = NULL, ref_id = NULL,
                             version = NULL, ...) {
  # parse the parameters to an S3 type object
  structure(
    list(
      endpoint = endpoint,
      id = id,
      ref = ref,
      ref_id = ref_id,
      version = version,
      query = api_query(...)
    ),
    class = "endpoint_params"
  )
}




#' Build an API link from a list
#'
#' @param params A list, the parsed endpoint parameters.
#' @importFrom stringr str_remove_all
#' @importFrom utils URLencode
#' @noRd
build_endpoint <- function(params) {

  api <- "api"
  version <- 0

  # parse api endpoint
  if (!is.null(params$endpoint)) {
    endpoint <- params$endpoint
    endpoint_parts <- strsplit(endpoint, "/")
    endpoint_parts <- unlist(endpoint_parts)

    # look for the API version in the endpoints and map the endpoint params by
    # size of the endpoint_parts.
    if (grepl("/\\d+/", endpoint)){

      if (length(endpoint_parts) == 3){
        api <- endpoint_parts[1]
        version <- endpoint_parts[2]
        endpoint <- endpoint_parts[3]
      }

      if (length(endpoint_parts) == 4){
        api <- endpoint_parts[1]
        version <- endpoint_parts[2]
        endpoint <- endpoint_parts[3]
        id = endpoint_parts[4]
      }

      if (length(endpoint_parts) == 5){
        api <- endpoint_parts[1]
        version <- endpoint_parts[2]
        endpoint <- endpoint_parts[3]
        id = endpoint_parts[4]
        ref = endpoint_parts[5]
      }

      if (length(endpoint_parts) == 6){
        api <- endpoint_parts[1]
        version <- endpoint_parts[2]
        endpoint <- endpoint_parts[3]
        id = endpoint_parts[4]
        ref = endpoint_parts[5]
        ref_id = endpoint_parts[6]
      }

    } else { # the endpoint is missing version

      # reuse the endpoint parts
      if (length(endpoint_parts) == 2){
        api <- endpoint_parts[1]
        endpoint <- endpoint_parts[2]
      }

      if (length(endpoint_parts) == 3){
        api <- endpoint_parts[1]
        endpoint <- endpoint_parts[2]
        id = endpoint_parts[3]
      }

      if (length(endpoint_parts) == 4){
        api <- endpoint_parts[1]
        endpoint <- endpoint_parts[2]
        id <- endpoint_parts[3]
        ref <- endpoint_parts[4]
      }

      if (length(endpoint_parts) == 5){
        api <- endpoint_parts[1]
        endpoint <- endpoint_parts[2]
        id <- endpoint_parts[3]
        ref <- endpoint_parts[4]
        ref_id <- endpoint_parts[5]
      }
    }

  } else {
    endpoint <- "resources"
  }

  # parse endpoint id
  if (!is.null(params$id)) {
    id <- params$id
  } else {
    id <- 0
  }

  # parse api sub endpoint
  if (!is.null(params$ref)) {
    ref <- params$ref
  } else {
    ref <- 0
  }

  # parse api sub endpoint id
  if (!is.null(params$ref_id)) {
    ref_id <- params$ref_id
  } else {
    ref_id <- 0
  }

  # parse the api version
  if (!is.null(params$version)) {
    version <- params$version
  }

  # else {
  #   version <- 0
  # }

  # parse the api query
  if (!is.null(params$query)) {
    query <- params$query
  } else {
    query <- 0
  }

  # build link

  paste(
      paste(api, version, endpoint, id, ref, ref_id, sep = "/"),
      query,
      sep = "/"
    ) -> link



  # clean and return encoded url
  link <- str_remove_all(link, "/0")

  return(URLencode(link))
}

