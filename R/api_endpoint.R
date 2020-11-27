#' Modify a DHIS2 API endpoint link
#'
#' Short hand to modify a `DHIS2 like` API endpoint link from R. This is useful
#' if you need to define or parse API parameters quickly.
#'
#'
#' @name modify_api_endpoint
#'
#' @param endpoint A DHIS2 endpoint, example `dataElements`, `dataElementGroups`, e.tc.
#' @param ... Query parameters parsed to \code{\link{api_query}}.
#' @param id The unique identifier for an object in the endpoint.
#' @param ref A reference within the endpoint.
#' @param ref_id The unique identifier of a referenced object in the endpoint.
#' @param version An integer, the version of DHIS2 web API. The default version
#'   is `current supported version`, you can override this using the
#'   \code{\link{api_version}} or by specifying the value explicitly.
#' @return A string, the DHIS2 web API link.
#' @export
#' @examples
#' # default api link
#' modify_api_endpoint()
#' # navigate to dataElements endpoint
#' modify_api_endpoint(endpoint = "dataElements")
#' # select data elements name, id and shortNames
#' modify_api_endpoint(endpoint = "dataElements", fields = c("name", "id", "shortNames"))
#' # arrange the data elements name in ascending order
#' modify_api_endpoint(
#'   endpoint = "dataElements", fields = c("name", "id", "shortNames"),
#'   order = api_order("name", by = "asc")
#' )
modify_api_endpoint <- function(endpoint, ..., id = NULL, ref = NULL, ref_id = NULL,
                                version = NULL) {
  params <- parse_api_params(endpoint, ..., id = id, ref = ref, ref_id = ref_id, version = version)

  build_api_endpoint(params)
}


#' Parse DHIS2 API endpoint parameters
#'
#' This function parse DHIS2 API endpoint parameters into a list object.
#'
#' @inheritParams modify_api_endpoint
#'
#' @return An S3 object of class `api_params`.
#' @export
#' @examples
#' # default endpoint endpoint
#' api_params <- parse_api_params()
#' # api params with query object
#' api_params <- parse_api_params(endpoint = "dataElements", field = c("name", "code", "id"))
parse_api_params <- function(endpoint, ..., id = NULL, ref = NULL, ref_id = NULL,
                             version = NULL) {
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
    class = "api_params"
  )
}




#' Build an API link from a list
#'
#' @param params A list, the parsed api parameters.
#' @importFrom stringr str_remove_all
#' @importFrom utils URLencode
#' @noRd
build_api_endpoint <- function(params) {

  # parse api endpoint
  if (!is.null(params$endpoint)) {
    endpoint <- params$endpoint
  } else {
    endpoint <- "endpoints"
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
  } else {
    version <- api_version()
  }

  # parse the api query
  if (!is.null(params$query)) {
    query <- params$query
  } else {
    query <- 0
  }

  paste(
    paste("api", version, endpoint, id, ref, ref_id, sep = "/"),
    query,
    sep = "/"
  ) -> link

  # clean and return encoded url
  cleaned_link <- str_remove_all(link, "/0")

  return(URLencode(cleaned_link))
}
