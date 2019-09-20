#' Evaluate a server response
#'
#' \code{status_check} evaluates the status  code of an HTTP request and returns an appropriate message.
#' @param req An HTTP request
#' @return an error message or an invisible object.
#'@export
status_check <- function(req){
  if (req$status_code < 400){
    return(invisible())
  }

  if (req$status_code == 502){
    stop("HTTP failure: 502, bad gateway. This error code is often returned when the server is down. Try again later")
  }

  if (req$status_code == 504){
    stop("HTTP failure: 504, timed out. This error code is often returned when the server takes so long to respond. Try again later")
  }

  message <- httr::content(req, as = "text", encoding = "UTF-8")
  stop("HTTP failure: ", req$status_code, "\n", message, call. = FALSE)
}
