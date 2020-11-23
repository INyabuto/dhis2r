#' Parse DHIS2 filter
#'
#' Short hand for specifying an filter parameter in an \code{\link{api_query}}.
#'
#' @param x An endpoint field.
#' @param filter A DHIS2 filter object.
#' @param obj Objects to filter.
#' @export
#' @examples
#' api_filter("name", "eq", "TZ FP A360 - Conversion rate < 20")
#' api_filter("name", "ilike", "TZ FP A360 - Conversion rate < 20")
#' @name api_filter
api_filter <- function(x, filter, obj){
  paste0(x, ":", filter, ":", obj)
}

