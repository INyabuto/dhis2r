#' Construct an API link/endpoint
#'
#' This function returns an API link/endpoint for browsing the DHIS2 web
#' resources from R. Useful for constructing, defining, or parsing API
#' parameters quickly.
#'
#' By default, \code{api_link}, returns the `api/29/resources` endpoint with
#' paging set to `TRUE` and displays the first page with the default size of
#' `fifty`. You can override or just specify these options directly.
#'
#' @param endpoint A character string, the endpoint to browse from, should be a
#'   valid DHIS2 resource for example; `dataElements`.
#' @param id A character string, an identifier of the DHIS2 resource.
#' @param ... Additional parameters to reference the endpoint.
#' @param fields A character string, the resource fields to extract for example;
#'   you can select `name`, `id` and the `shortName` from a `dataElement`
#'   resource. Use \code{\link{api_fields}} to parse the resource fields more
#'   consistently.
#' @param filter A character string, defines the resource filter parameters. Use
#'   \code{\link{api_filter}} to parse the resource filters consistently.
#' @param version An integer, the version of the web API. The default version is
#'   `v29`, you can override this using the \code{\link{api_version}} or by
#'   setting the value explicitly.
#' @param skip_paging Logical, indicates whether to return lists of elements in
#'   pages. The default option is FALSE.
#' @param page An integer, defines which page number to return. The default
#'   option is page 1.
#' @param page_size An integer, defines the number of elements to return for
#'   each page. The default size in 50.
#' @param order A character string, a resource field to arrange/sort.
#' @param order_by A character string, the specified order, only properties that
#'   are both persisted and simple (no collections, idObjects etc) are
#'   supported. iasc and idesc are case insensitive sorting.
#'
#' @return A string. DHIS2 web API link
#'
#' @note \code{\link{api_fields}} returns a character string of the API fields.
#' @note \code{\link{api_filter}} returns a character string of the API filters.
#' @note \code{\link{api_version}} returns an integer, the version of the web
#'   API. By default, this is sets this to `v29`.
#' @note \code{\link{api_order}} returns a character string, the order params of
#'   the API.
#' @export
#' @seealso \code{\link{api_fields}}, \code{\link{api_version}},
#'   \code{\link{api_filter}}, \code{\link{api_order}}
#' @examples
#' # the resources endpoint
#' create_api_link()
#' # navigate to dataElements endpoint
#' create_api_link("dataElements")
#' # select data elements name, id and shortNames
#' create_api_link("dataElements", fields = api_fields("name","id","shortNames"))
#' # arrange the data elements name in ascending order
#' create_api_link("dataElements", fields = api_fields("name","id","shortNames"), order = "name", order_by = "asc")
api_link <- function(endpoint = "resources", id = NULL, ..., fields = NULL,
                     filter = NULL, version = api_version(), skip_paging = TRUE, page = 1L, page_size = 50L,
                     order = NULL, order_by = c("asc", "iasc", "desc", "idesc")){


  api_args <- list(resources = endpoint, id = id, extra = list(...), version = version,
                       fields = fields, filter = filter, skip_paging = skip_paging, page = page, page_size = page_size,
                       order = order, order_by = match.arg(order_by))



  build_api_link(api_args)

}

#' Parse DHIS2 API link/endpoint params in a list
#'
#' This function parse the API link/endpoint params in a list.
#'
#'
#' @details By default, \code{parse_api_args}, returns an S3 object from the
#'   `api/29/resources` endpoint with paging set to `TRUE` and displays the
#'   first page with the default size of `fifty`. You can override or just
#'   specify these options directly.
#'
#' @param endpoint A character string, the endpoint to browse from, should be a
#'   valid DHIS2 resource for example; `dataElements`.
#' @param id A character string, an identifier of the DHIS2 resource.
#' @param ... Additional parameters to reference the endpoint.
#' @param fields A character string, the resource fields to extract for example;
#'   you can select `name`, `id` and the `shortName` from a `dataElement`
#'   resource. Use \code{\link{api_fields}} to parse the resource fields more
#'   consistently.
#' @param filter A character string, defines the resource filter parameters. Use
#'   \code{\link{api_filter}} to parse the resource filters consistently.
#' @param version An integer, the version of the web API. The default version is
#'   `v29`, you can override this using the \code{\link{api_version}} or by
#'   setting the value explicitly.
#' @param skip_paging Logical, indicates whether to return lists of elements in
#'   pages. The default option is FALSE.
#' @param page An integer, defines which page number to return. The default
#'   option is page 1.
#' @param page_size An integer, defines the number of elements to return for
#'   each page. The default size in 50.
#' @param order A character string, a resource field to arrange/sort.
#' @param order_by A character string, the specified order, only properties that
#'   are both persisted and simple (no collections, idObjects etc) are
#'   supported. iasc and idesc are case insensitive sorting.
#' @return An S3 object of class dhis2_api_args.
#'
#' @note \code{\link{api_fields}} returns a character string of the API fields.
#' @note \code{\link{api_filter}} returns a character string of the API filters.
#' @note \code{\link{api_version}} returns an integer, the version of the web
#'   API. By default, this is sets this to `v29`.
#' @note \code{\link{api_order}} returns a character string, the order params of
#'   the API.
#'
#' @export
parse_api_args <- function(endpoint = "resources", id = NULL, ..., version = api_version(), fields = NULL, filter = NU,
                        skip_paging = "FALSE", page = 1L, page_size = 50L, order = NULL, order_by = c("asc", "iasc", "desc", "idesc")){

  api_args <- list(resources = endpoint, id = id, extra = list(...), version = version,
       fields = fields, filter = filter, skip_paging = skip_paging, page = page, page_size = page_size,
       order = order, order_by = match.arg(order_by))

  structure(api_args, class = "dhis2_api_args")

}


#' @rdname parse_api_args
#' @importFrom stringr str_remove_all
#' @param api_args A list. DHIS2 web API parameters
#' @examples
#' api_args <- parse_api_args()
#' build_api_link(api_args)
#' @export
build_api_link <- function(api_args){

  if (is.list(api_args$extra)){
    extra <- paste0(api_args$extra)
    if (length(extra) > 0){
      extra <- str_split(extra, " ")
      extra <- paste0(unlist(extra), collapse = "/")
    }else{
    extra <- NULL
    }
  }


  if (!is.null(api_args$fields))
    fields <- paste0("fields=", api_args$fields)
  else
    fields <- NULL

  if (!is.null(api_args$filter))
    filter <- paste0("filter=", api_args$filter)
  else
    filter <- NULL


  if (is.logical(api_args$skip_paging) && isFALSE(api_args$skip_paging))
    paging <- paste0("paging=", api_args$skip_paging)
  else
    paging <- paste0("paging=", TRUE)

  if (is.numeric(api_args$page))
    page <- paste0("page=", api_args$page)
  else
    page <- 1L

  if (is.numeric(api_args$page_size))
    page_size <- paste0("pageSize=", api_args$page_size)
  else
    page_size <- paste0("pageSize=", 50L)

  if (!is.null(api_args$order))
    order <- paste0("order=", api_args$order, ":", api_args$order_by)
  else
    order <- NULL


  # construct endpoint
  paste(
    paste("api", api_args$version, api_args$resources,
          if (!is.null(api_args$id)) api_args$id else "0",
          if (!is.null(extra)) extra else "0", sep = "/"),
    paste(if (!is.null(fields)) fields else "0",
          if (!is.null(filter)) filter else "0",
          if (!is.null(order)) order else "0",
          if (isFALSE(api_args$skip_paging)) paging else paste(paging, page, page_size, sep = "&") , sep = "&"),
    sep = "?"
  ) -> endpoint


  # clean endpoints
  str_remove_all(endpoint, "&0") %>%
    str_remove_all(., "0&") %>%
    str_remove_all(., "/0") %>%
    URLencode(.)

}



#' Parse DHIS2 API fields
#'
#' This function parse API fields params in standard DHIS2 format.
#'
#' @export
#' @examples
#' # select all the fields
#' api_fields("*")
#' # select only name, id, shortName and code
#' api_fields("name","id","shortName","code")
#' # select fields in collection
#' api_fields("name", organisationUnits = c("name","id","code"))
api_fields <- function(...){

  args <- list(...)

  arg_names <- names(args)

  missing <-
    if (is.null(arg_names)){
      rep(TRUE, length(args))
    } else {
      !nzchar(arg_names)
    }

  collections <- args[!missing]

  collection_fields <- purrr::map(collections, commas)

  collection_names <- names(collection_fields)

  collection_transformed <- vector("list", length(collection_fields))

  for (i in seq_along(collection_names)){
    collection_transformed[[i]] <- paste0(collection_names[i], "[", collection_fields[[i]], "]")
  }

  paste(
    paste0(args[missing], collapse = ","),
    paste0(collection_transformed, collapse = ","),
    sep = ","
  )

}

#'@export
api_filter <- function(x, filter, property) paste0(x, ":", filter, ":", property)





