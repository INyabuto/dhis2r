#' Construct a DHIS2 API link
#'
#' This function constructs an API link for querying resources on the DHIS2 Web
#' API.
#'
#' @param resources A character. The name of resources to query from, such as
#'   dataElements.
#' @param id A character. An identifier of a resource or an identifiable object.
#' @param section A character. The section of a resource or an identifiable object.
#' @param version An integer. The web API version.
#' @param skip_paging Logical. Indicates whether to return lists of elements in
#'   pages. The default option is TRUE.
#' @param page An integer. Defines which page number to return. The default option
#'   is 1.
#' @param page_size An integer. Defines the number of elements to return for each
#'   page. The default size in 50.
#' @param order A character. Order the output using a specified order, only
#'   properties that are both persisted and simple (no collections, idObjects
#'   etc) are supported. iasc and idesc are case insensitive sorting.
#'
#' @return A DHIS2 API link
#' @export
api_link <- function(resources = "resources", id = NULL, ...,
                     version = api_version(), skip_paging = TRUE, page = 1L, page_size = 50L,
                     order = c("asc", "iasc", "desc", "idesc")){

  entry_point <- paste("api", version, resources, sep = "/")

  if (!is.null(id)) URLencode(paste(entry_point, id, ...,  sep = "/")) else entry_point

  if (skip_paging)
    URLencode(paste(paste(entry_point, ..., sep = "/"), "paging=false", sep = "?"))
  else
    URLencode(paste(paste(entry_point, ..., sep = "/"), sprintf("page=%d & pageSize=%d", page, page_size), sep = "?"))



}


