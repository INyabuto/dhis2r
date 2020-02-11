api_version <- function(version = 29) if (version %in% 30:28) version else stop("Unsupported API version", call. = F)

api_order <- function(x, order_by = c("asc", "iasc", "desc", "idesc")){
  paste0("order=", x, ":", match.arg(order_by))
}


#' %>%
#'
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL


#' check for empty list()
is.empty <- function(...){
  args <- list(...)
  identical(length(args), 0L)
}

has_names <- function(...){
  args <- list(...)
  names <- names(args)
  !is.null(names)
}

commas <- function(...) paste0(..., collapse = ",")
