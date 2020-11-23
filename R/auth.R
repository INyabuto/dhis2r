#' #'Login to a DHIS2 instance
#' #'
#' #'\code{login_dhis2} authenticates the user to a DHIS2 server.
#' #'
#' #'It uses basic authentication mechanisism to identify the user with an account stored in
#' #'the specified DHIS2 server. Basic authentication is a technique for clients to
#' #'send login credentials over HTTP to a web server. Technically speaking, the
#' #'username is appended with a colon and the password, Base64-encoded, prefixed
#' #'Basic and supplied as the value of the Authorization HTTP header
#' #'
#' #'@param host A string, server domain name
#' #'@param credential_lable A string, usually the name of your secret file.
#' #'@return the server domain if a successful authentication is passed.
#' #'@examples
#' #'base <- "clone.psi-mis.org"
#' #'login_dhis2(base)
#' #'@export
#' login_dhis2 <- function(host,credential_label = NULL, naked = FALSE,...){
#'
#'   args <- list(...)
#'
#'   if (naked){
#'     if (!("usr" %in% names(args) & "pwd" %in% names(args))){
#'       stop("DHIS2 credentials is missing, perhaps dhis2r found no hits? Try specifying the credential label or saving the secrect file", call. = FALSE)
#'     }
#'     else{
#'       usr <- args$usr
#'       pwd <- args$pwd
#'
#'     }
#'
#'   }
#'   else{
#'     usr <- secrets(host,credential_label)[1]
#'     pwd <- secrets(host, credential_label)[2]
#'   }
#'
#'
#'   path = "api/29/me"
#'   url <- httr::modify_url("https://api.github.com", path = path, hostname = host)
#'
#'   resp <- httr::GET(url, authenticate(usr, pwd), timeout(60))
#'   if (http_type(resp) != "application/json"){
#'     stop("API did not return json", call. = FALSE)
#'   }
#'
#'   status_check(resp)
#'
#'   structure(
#'     list(path = host,
#'          response = resp
#'          ),
#'     class = "login_dhis2"
#'   )
#'
#' }
#'
#'
#' print.login_dhis2 <- function(x, ...){
#'   cat("[Server: ", x$path,"]\n", sep = " ")
#'   invisible(x)
#' }
#'
#'
#' #'Retrieve secret from windows
#' #'
#' #'\code{win_secret} returns an invisible decrypted password stored with
#' #'Micrososft data protection API.
#' #'
#' #'Invisible means that the password won't be displayed in the console, but can
#' #'be assigned to a variable or used inline.'Under the hood, \code{win_secret} is
#' #'using \code{\link[keyringr]decrypt_dpapi_pw} to help retrieve and unlock
#' #'passwords stored with the Microsoft data protection API. It Requires
#' #'Powershell to be installed and execution policy set to RemoteSigned.
#' #'@seealso \code{\link[keyringr]{decrypt_dpapi_pw}}
#' #'@param cred_label A string. Usually the name of your secret file.
#' #'@return An invisible string.
#' #'
#' win_secret <- function(cred_label){
#'
#'   credential_path <- paste(Sys.getenv("USERPROFILE"),
#'                            '\\DPAPI\\passwords\\',
#'                            Sys.info()["nodename"],
#'                            '\\', cred_label, '.txt', sep="")
#'   keyringr::decrypt_dpapi_pw(credential_path)
#'
#' }
#'
#' # Retrieve secrets
#' secrets <- function(host,cred_label){
#'
#'   if (.Platform$OS.type == "windows"){
#'     usr <- cred_label
#'     pwd <- win_secret(cred_label = cred_label)
#'   }else{
#'     usr <- keyringr::get_kc_account(host, type = "internet")
#'     pwd <- keyringr::decrypt_kc_pw(host, type = "internet")
#'   }
#'    c(usr,pwd)
#' }
