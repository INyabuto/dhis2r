#'Login to a DHIS2 instance
#'
#'@param host A string. Server domain name
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


#'Retrieve secret from windows
#'
#'\code{win_secret} returns an invisible decrypted password stored with
#'Micrososft data protection API.
#'
#'Invisible means that the password won't be displayed in the console, but can
#'be assigned to a variable or used inline.'Under the hood, \code{win_secret} is
#'using \code{\link[keyringr]decrypt_dpapi_pw} to help retrieve and unlock
#'passwords stored with the Microsoft data protection API. It Requires
#'Powershell to be installed and execution policy set to RemoteSigned.
#'@seealso \code{\link[keyringr]{decrypt_dpapi_pw}}
#'@param cred_label A string. Usually the name of your secret file.
#'@return An invisible string.
#'
win_secret <- function(cred_label){

  credential_path <- paste(Sys.getenv("USERPROFILE"),
                           '\\DPAPI\\passwords\\',
                           Sys.info()["nodename"],
                           '\\', credential_label, '.txt', sep="")
  decrypt_dpapi_pw(credential_path)

}
