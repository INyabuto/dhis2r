#' API version
#'
#' Sets an API version. Last three versions of DHIS2 are supported.
#'
#' @param version An integer. The API version.
#' @return Integer
#' @keywords internal
api_version <- function(version = 29) {
  if (as.integer(version) %in% 30:28)
    version
  else
    stop("unsupported API version", call. = F)
}




#' %>%
#'
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL


# # empty list
# is.empty <- function(...){
#   args <- list(...)
#   identical(length(args), 0L)
# }
#
# # named list
# has_names <- function(...){
#   args <- list(...)
#   names <- names(args)
#   !is.null(names)
# }


commas <- function(...) paste0(..., collapse = ",")


