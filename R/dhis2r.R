#' #' Define HTTP request
#' #'
#' #' Define an http request.
#' #'
#' #' @name dhis2r_query
#' #'
#' #' @param url URL.
#' #' @param ... Additional
#' #' @importFrom httr GET POST DELETE content stop_for_status
#'
#' dhis2r_query <- function(http_method, url, ..., params = NULL) {
#'   if (method == "GET") {
#'     res <- httr::GET(url, ...)
#'   }
#'
#'   if (method == "POST") {
#'     res <- httr::POST(url, ..., body = params)
#'   }
#'
#'   if (method == "DELETE") {
#'     res <- httr::DELETE(url, ...)
#'   }
#'
#'   stop_for_status(res)
#'
#'   content(res)
#' }
#'
#' dhis2r_create <- function(url, ..., params = NULL) {
#'   dhis2r_query("POST", url, ..., params = params)
#' }
