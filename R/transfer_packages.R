#' Easily transfer a list of all installed packages from one machine to another
#'
#' @param library The location of the library on the current machine to copy.
#' @param output One of `online` (the default), `local` or `github`. See Details for more information.
#' @param expiry Expiry for online file store in days. Weeks can be given with `w`, or months with `m`. Default is 7 days. Will be ignored if `output` is not `online`.
#' @param quiet Don't display output. Defaults to `FALSE`.
#' @param format
#'
#' @details If `output` is `online`, the resulting list of currently installed packages is stored on [https://file.io](https://file.io) for the time specified in `expiry`.
#' If `output` is `local`
#'
#'
#' @importFrom httr POST
#'
#' @return
#' @export
#'
transfer_packages <- function(library = .libPaths()[1], output = "online", expiry="7d", format = "list", quiet = F) {
    # Get list of installed packages
    # Choose location to save
    #   file.io is default, also enable gist and local file (.Rds or csv)
    # Upload, create gist or save file, then output instructions for other computer
    # Also output those packages not installed from CRAN

    pkgs <- unname(installed.packages(lib.loc = library, priority = "NA")[,1])

    if(output == "online") {
        to_install <- paste0("install.packages(c(", paste0('"', pkgs, '"', collapse = ', '), "), repos = 'https://cloud.r-project.org')")
        r <- POST("https://file.io", body = list(text = to_install))
        link <- content(r)$link
    }
    else if(output == "local") {

    }
    else if(output == "gist") {

    }
    else {
        stop("output should be one of online, local or gist")
    }

}
