#' @export
api_query <- function(...){

  # parse the query options to a list
  args <- list(...)

  # get the arg names
  arg_names <- names(args)

  # identify the missing ones
  missing <-
    if (is.null(arg_names)){
      rep(TRUE, length(arg_names))
    } else{
      !nzchar(arg_names)
    }

  # filter out the missing elements from the list
  query <- args[!missing]

  # treat the remaining elements (without names) as fields
  fields <- args[missing]

  if (length(fields) > 0){
    stop(
      sprintf("the following [%s] are not valid DHIS2 object filters, \n did you mean parse them as fields? if so, please parse them using api_fields()", paste0(fields, collapse = ", ")),
      call. = FALSE
    )
  }

  # parse the query options fields
  query_fields <- purrr::map(query, commas)

  query_names <- names(query_fields)

  # transforms the query elements
  for (i in seq_along(query_names)){
    query[[i]] <- paste0(query_names[i], "=", query_fields[[i]])
  }

  if (!missing(...))
    query <- paste0("?", paste0(query, collapse = "&"))
  else
    query <- NULL


  query


}
