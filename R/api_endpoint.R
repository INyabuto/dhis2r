#' Generate DHIS2 API endpoint
#'
#' This function returns a DHIS2 API endpoint, used for browsing DHIS2 web
#' resources from R. Useful for constructing, defining, or parsing API
#' parameters quickly.
#' @name api_endpoint
#'
#' @param ... Additional parameters parsed to \code{\link{api_query}}.
#' @param resource A character string, the DHIS2 resource name, example `dataElements`
#' @param id A character string, the Unique Identifier for this Object.
#' @param path A character string, specifies a sub directory of the resource.
#' @param version An integer, the version of DHIS2 web API. The default version is
#'   current supported version, you can override this using the \code{\link{api_version}} or by
#'   setting the value explicitly.
#' @return A string. DHIS2 web API link
#' @export
#' @examples
#' # default api link - the resource endpoint
#' api_endpoint()
#' # navigate to dataElements endpoint
#' api_endpoint(resource = "dataElements")
#' # select data elements name, id and shortNames
#' api_endpoint(resource = "dataElements", fields = c("name","id","shortNames"))
#' # arrange the data elements name in ascending order
#' api_endpoint(resource = "dataElements", fields = c("name","id","shortNames"),
#'    order = api_order("name", by = "asc"))
api_endpoint <- function(..., resource = "resources", id = NULL, path = NULL, version = NULL){

  api_params <- parse_api_params(resource = resource, id = id, path = path, version = version, ...)

  build_api_endpoint(api_params)

}


#' Parse DHIS2 API endpoint parameters
#'
#' This function parse DHIS2 API endpoint parameters into a list object.
#'
#' @rdname api_endpoint
#' @name parse_api_params
#'
#' @return An S3 object of class `dhis2_api_params`
#' @export
#' @examples
#' # default resource endpoint
#' api_params <- parse_api_params()
#' # api params with query object
#' api_params <- parse_api_params(resource = "dataElements", field = c("name","code","id"))
parse_api_params <- function(..., resource = NULL, id = NULL, path = NULL, version = NULL){
  # parse the parameters to an S3 type object
  structure(
    list(endpoint = resource,
         id = id,
         path = path,
         version = version,
         query = api_query(...)
         ),
    class = "dhis2_api_params"
    )
}




#' @name build_api_endpoint
#' Build an API link from a list
#'
#' @param api_params A list, the parsed api parameters.
#' @importFrom stringr str_remove_all
#' @importFrom utils URLencode
#' @rdname api_endpoint
#' @export
build_api_endpoint <- function(api_params){

  # parse api endpoint
  if (!is.null(api_params$endpoint))
    endpoint <- api_params$endpoint
  else
    endpoint <- "resources"

  # parse resource id
  if (!is.null(api_params$id))
    id <- api_params$id
  else
    id <- 0

  # parse api path
  if (!is.null(api_params$path))
    path <- api_params$path
  else
    path <- 0

  # parse the api version
  if (!is.null(api_params$version))
    version <- api_params$version
  else
    version <- api_version()

  # parse the api query
  if (!is.null(api_params$query))
    query <- api_params$query
  else
    query <- 0

  paste(
    paste("api", version, endpoint, id, path, sep = "/"),
    query, sep = "/"
  ) -> link

  # clean and return encoded url
  cleaned_link <- str_remove_all(link, "/0")

  return(URLencode(cleaned_link))


}



