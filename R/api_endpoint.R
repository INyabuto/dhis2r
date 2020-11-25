#' Generate a DHIS2 API endpoint link
#'
#' Short hand to generate a DHIS2 like API endpoint link from R. This is useful
#' if you need to define or parse API parameters quickly. By default, the
#' resources endpoint is returned.
#'
#'
#' @name generate_api_endpoint
#'
#' @param ... Additional parameters parsed to \code{\link{api_query}}.
#' @param resource A character string, the DHIS2 resource name, example
#'   `dataElements`, `dataElementGroups`, e.tc.
#' @param id A character string, the unique identifier for a specific object in
#'   the resource.
#' @param path A character string, specifies a sub directory in the resource.
#' @param version An integer, the version of DHIS2 web API. The default version
#'   is `current supported version`, you can override this using the
#'   \code{\link{api_version}} or by specifying the value explicitly.
#' @return A string, the DHIS2 web API link.
#' @export
#' @examples
#' # default api link
#' generate_api_endpoint()
#' # navigate to dataElements endpoint
#' generate_api_endpoint(resource = "dataElements")
#' # select data elements name, id and shortNames
#' generate_api_endpoint(resource = "dataElements", fields = c("name", "id", "shortNames"))
#' # arrange the data elements name in ascending order
#' generate_api_endpoint(
#'   resource = "dataElements", fields = c("name", "id", "shortNames"),
#'   order = api_order("name", by = "asc")
#' )
generate_api_endpoint <- function(..., resource = "resources", id = NULL, path = NULL, version = NULL) {
  params <- parse_api_params(..., resource = resource, id = id, path = path, version = version)

  build_api_endpoint(params)
}


#' Parse DHIS2 API endpoint parameters
#'
#' This function parse DHIS2 API endpoint parameters into a list object.
#'
#' @inheritParams generate_api_endpoint
#'
#' @return An S3 object of class `api_params`.
#' @export
#' @examples
#' # default resource endpoint
#' api_params <- parse_api_params()
#' # api params with query object
#' api_params <- parse_api_params(resource = "dataElements", field = c("name", "code", "id"))
parse_api_params <- function(..., resource = NULL, id = NULL, path = NULL, version = NULL) {
  # parse the parameters to an S3 type object
  structure(
    list(
      endpoint = resource,
      id = id,
      path = path,
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
    endpoint <- "resources"
  }

  # parse resource id
  if (!is.null(params$id)) {
    id <- params$id
  } else {
    id <- 0
  }

  # parse api path
  if (!is.null(params$path)) {
    path <- params$path
  } else {
    path <- 0
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
    paste("api", version, endpoint, id, path, sep = "/"),
    query,
    sep = "/"
  ) -> link

  # clean and return encoded url
  cleaned_link <- str_remove_all(link, "/0")

  return(URLencode(cleaned_link))
}
