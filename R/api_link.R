#' Generate a DHIS2 API link/endpoint
#'
#' This function returns an API link/endpoint for browsing DHIS2 web
#' resources from R. Useful for constructing, defining, or parsing API
#' parameters quickly.
#'
#' @param resource A character string, the endpoint to browse from, should be a
#'   valid DHIS2 resource for example; `dataElements`.
#' @param id A character string, an identifier of the DHIS2 resource.
#' @param path A character string, specifies a sub directory of the resource.
#' @param version An integer, the version of the web API. The default version is
#'   `v29`, you can override this using the \code{\link{api_version}} or by
#'   setting the value explicitly.
#' @param ... Addional parameters parsed to the query option.
#' @return A string. DHIS2 web API link
#' @export
#' @examples
#' # default api link - the resource endpoint
#' api_link()
#' # navigate to dataElements endpoint
#' api_link("dataElements")
#' # select data elements name, id and shortNames
#' api_link("dataElements", fields = c("name","id","shortNames"))
#' # arrange the data elements name in ascending order
#' api_link("dataElements", fields = c("name","id","shortNames"), order = api_order("name", order_by = "asc"))
api_link <- function(resources = "resources", id = NULL, path = NULL, version = NULL, ...){

  api_params <- parse_api_params(resources = resources, id = id, path = path, version = version, ...)

  build_api_link2(api_params)

}


#' Parse DHIS2 API link/endpoint params in a list
#'
#' This function returns an API link/endpoint for browsing DHIS2 web
#' resources from R. Useful for constructing, defining, or parsing API
#' parameters quickly.
#'
#' @param resource A character string, the endpoint to browse from, should be a
#'   valid DHIS2 resource for example; `dataElements`.
#' @param id A character string, an identifier of the DHIS2 resource.
#' @param path A character string, specifies a sub directory of the resource.
#' @param version An integer, the version of the web API. The default version is
#'   `v29`, you can override this using the \code{\link{api_version}} or by
#'   setting the value explicitly.
#' @param ... Addional parameters parsed to \code{\link{api_query}}
#'
#' @return An S3 object of class `dhis2_api_params`
#' @export
#' @examples
#' # default resource endpoint
#' api_params <- parse_api_params()
#' # api params with query object
#' api_params <- parse_api_params("dataElements", field = c("name","code","id"))
parse_api_params <- function(resources = NULL, id = NULL, path = NULL, version = NULL, ...){
  # parse the parameters to an S3 type object
  structure(
    list(endpoint = resources,
         id = id,
         path = path,
         version = version,
         query = api_query(...)
         ),
    class = "dhis2_api_params"
    )
}




#' Build an API link from a list
#'
#' @param api_param A list, the parsed api parameters.
#' @rdname api_link
#' @export
build_api_link <- function(api_params){

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



