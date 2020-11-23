api_version <- function(version = 29) if (version %in% 30:28) version else stop("unsupported API version", call. = F)

#' @export
api_order <- function(x, order_by = c("asc", "iasc", "desc", "idesc")){
  paste0(x, ":", match.arg(order_by))
}


#' %>%
#'
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL


# empty list
is.empty <- function(...){
  args <- list(...)
  identical(length(args), 0L)
}

# named list
has_names <- function(...){
  args <- list(...)
  names <- names(args)
  !is.null(names)
}

commas <- function(...) paste0(..., collapse = ",")

#'@export
api_filter <- function(x, filter, property) paste0(x, ":", filter, ":", property)
