#' Parse DHIS2 API fields
#'
#' This function parse API fields params in standard DHIS2 format.
#'
#' @export
#' @examples
#' # select all the fields
#' api_fields("*")
#' # select only name, id, shortName and code
#' api_fields("name","id","shortName","code")
#' # select fields in a collection
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

  collection <- args[!missing]

  collection_fields <- purrr::map(collection, commas)

  collection_names <- names(collection_fields)

  collection_transformed <- vector("list", length(collection_fields))

  for (i in seq_along(collection_names)){
    collection_transformed[[i]] <- paste0(collection_names[i], "[", collection_fields[[i]], "]")
  }

  if (length(collection_transformed) > 0){
    paste(
      paste0(args[missing], collapse = ","),
      paste0(collection_transformed, collapse = ","),
      sep = ","
    )
  } else {
    paste0(args[missing], collapse = ",")
  }


}


