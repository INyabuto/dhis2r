#'Login to a DHIS2 instance
#'
#'@param host A server domain
#'@return Login details
#'@examples
#'base <- "clone.psi-mis.org"
#'login_dhis2(base)
login_dhis2 <- function(host){

  usr <- get_kc_account(host, type = "internet")
  pwd <- decrypt_kc_pw(host, type = "internet")

  path = "api/me"
  url <- modify_url("https://api.github.com", path = path, hostname = host)

  resp <- GET(url, authenticate(usr, pwd), timeout(60))
  if (http_type(resp) != "application/json"){
    stop("API did not return json", call. = FALSE)
  }

  if (resp$status_code == 200){
    msg <- "success!"
  } else{
    msg <- "failed!"
  }


  structure(
    list(content = msg,
         path = host,
         response = resp
         ),
    class = "login_dhis2"
  )

}


print.login_dhis2 <- function(x, ...){
  cat("[DHIS2: ", x$path,"]\n", sep = " ")
  cat("Status: ", x$content, sep = "")
  invisible(x)
}

