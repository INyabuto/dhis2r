#' Parse DHIS2 order
#'
#' Short hand for specifying an order parameter in an \code{\link{api_query}}.
#'
#' @param x An endpoint field.
#' @param by Type of ordering. Use `asc` to order in ascending order, `desc` in
#'   descending order. The variants `iasc` and `idesc` are case insensitive.
#' @export
#' @examples
#' # order name in ascending order
#' api_order("name", by = "asc")
#' # order name in descending order
#' api_order("name", by = "desc")
#' # example in a query to order by ascending order
#' api_query(order = api_order("name", "asc"))
#' # order data elements in descending order
#' api_endpoint(resource = "dataElements", order = api_order("name", "desc"))
api_order <- function(x, by = c("asc", "iasc", "desc", "idesc")){
  paste0(x, ":", match.arg(by))
}
