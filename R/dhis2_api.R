#' Create a DHIS2 web API link
#'
#' @description This function creates an API link for browsing the DHIS2 web
#' resources from R. Use these utilities to construct, define, and parse
#' API parameters quickly.
#'
#' \itemize{
#'  \item `api_fields` returns a string of the API fields
#'  \item `api_filter` returns a string of the API filters
#'  \item `api_version` returns a number, a version of the web API. By default, this is sets this to version `29`
#'  \item `api_order` returns a string of the API order params.
#' }

#'
#' @details
#' \code{create_api_link} provides an interface to construct, define or parse
#' DHIS2 web API parameters easily. By default, it returns the endpoint
#' `api/29/resources`, with paging set to `TRUE` and displays the first page
#' with the default size of `fifty`.
#'
#'
#' @param resources A character. The name of the resources to browse, such as
#'   dataElements.
#' @param id A character. An identifier of the resource or the identifiable object.
#' @param ... Additional params to parsed before the query option.
#' @param version An integer. The web API version.
#' @param skip_paging Logical. Indicates whether to return lists of elements in
#'   pages. The default option is FALSE.
#' @param page An integer. Defines which page number to return. The default
#'   option is 1.
#' @param page_size An integer. Defines the number of elements to return for
#'   each page. The default size in 50.
#' @param order A character. Order the output using a specified order, only
#'   properties that are both persisted and simple (no collections, idObjects
#'   etc) are supported. iasc and idesc are case insensitive sorting.
#'
#' @return A string. DHIS2 web API link
#' @export
#' @examples
#' # the resources endpoint
#' create_api_link()
#' # navigate to dataElements endpoint
#' create_api_link("dataElements")
#' # select data elements name, id and shortNames
#' create_api_link("dataElements", fields = api_fields("name","id","shortNames"))
#' # arrange the data elements name in ascending order
#' create_api_link("dataElements", fields = api_fields("name","id","shortNames"), order = "name", order_by = "asc")
create_api_link <- function(resources = "resources", id = NULL, ..., fields = api_fields(...),
                     filter = NULL, version = api_version(), skip_paging = TRUE, page = 1L, page_size = 50L,
                     order = NULL, order_by = c("asc", "iasc", "desc", "idesc")){


  api_args <- list(resources = resources, id = id, extra = list(...), version = version,
                       fields = fields, filter = filter, skip_paging = skip_paging, page = page, page_size = page_size,
                       order = order, order_by = match.arg(order_by))



  build_api_link(api_args)

}

#' @title
#' Parse DHIS2 API params in a list
#'
#' @description
#' This function parse the API params in a list.
#'
#' @rdname create_api_link
#' @return An S3 object of class dhis2_api_args
#' @export
parse_api_args <- function(resources = "resources", id = NULL, ..., version = api_version(), fields = NULL, filter = NU,
                        skip_paging = "FALSE", page = 1L, page_size = 50L, order = NULL, order_by = c("asc", "iasc", "desc", "idesc")){

  api_args <- list(resources = resources, id = id, extra = list(...), version = version,
       fields = fields, filter = filter, skip_paging = skip_paging, page = page, page_size = page_size,
       order = order, order_by = match.arg(order_by))

  structure(api_args, class = "dhis2_api_args")

}


#' @importFrom stringr str_remove_all
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
#' @rdname api_link
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


api_filter <- function(x, filter, property) paste0(x, ":", filter, ":", property)





