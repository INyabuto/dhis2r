#' Parse DHIS2 endpoint fields
#'
#' This function parses DHIS2 endpoint fields into a character string.
#'
#' @param ... DHIS2 query parameters.
#' @return A character string.
#'
#' @export
#' @examples
#' # select all the fields
#' api_fields("*")
#' # select only name, id, shortName and code
#' api_fields("name", "id", "shortName", "code")
#' # select fields in a collection
#' api_fields("name", organisationUnits = c("name", "id", "code"))
api_fields <- function(...) {
  fields <- list(...)

  named_fields <- fields[is_named(fields)]

  unnamed_fields <- fields[!is_named(fields)]

  parsed_named_fields <- purrr::map(named_fields, commas)

  parsed_named_fields <- purrr::imap(parsed_named_fields, function(.x, .y){
    paste0(.y, "[", .x, "]")
  })




  if (length(parsed_named_fields) > 0 && length(unnamed_fields) > 0) {
    paste(
      paste0(fields[!is_named(fields)], collapse = ","),
      paste0(parsed_named_fields, collapse = ","),
      sep = ","
    ) -> api_field
  } else if (length(parsed_named_fields) > 0 && length(unnamed_fields) == 0) {
    paste0(parsed_named_fields, collapse = ",") -> api_field
  }
  else {
    paste0(fields[!is_named(fields)], collapse = ",") -> api_field
  }

  parse_presets(api_field)
}



parse_presets <- function(field = NULL) {
  if (!is.null(field)) {
    to_parse <- field

    to_parse <- gsub("all", ":all", to_parse, fixed = T)

    to_parse <- gsub("nameable", ":nameable", to_parse, fixed = T)

    to_parse <- gsub("identifiable", ":identifiable", to_parse, fixed = T)

    to_parse <- gsub("persisted", ":persisted", to_parse, fixed = T)

    to_parse <- gsub("owner", ":owner", to_parse, fixed = T)

    to_parse
  }
}


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
api_filter <- function(x, filter, obj) {
  paste0(x, ":", filter, ":", obj)
}


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
#' modify_endpoint(endpoint = "dataElements", order = api_order("name", "desc"))
api_order <- function(x, by = c("asc", "iasc", "desc", "idesc")) {
  paste0(x, ":", match.arg(by))
}




#' Parse DHIS2 endpoint query
#'
#' This function parses DHIS2 endpoint queries into a character string.
#'
#' @param ... DHIS2 query parameters.
#' @return A character string.
#' @name api_query
#' @examples
#' api_query(fields = "name", totalPages = TRUE)
#' api_query(fields = api_fields("name", "id", organisationUnit = c("name")))
#' @export
api_query <- function(...) {
  args <- list(...)

  check_params(args)

  query <- args[is_named(args)]

  # parse the query with multiple options as comma separated
  query_fields <- purrr::map(query, commas)

  query <- purrr::imap(query_fields, ~ paste0(.y, "=", .x))


  if (!missing(...)) {
    query <- paste0("?", paste0(query, collapse = "&"))
  } else {
    query <- NULL
  }

  query
}


check_params <- function(params) {
  if (any(!is_named(params))) {
    stop("All elements of `...` must be named")
  }
}


