#' Easily reinstall all currently installed packages on another machine.
#'
#' @param library The location of the library on the current machine to copy.
#' @param output One of `online` (the default), `gist` or `local`. Saves a list of installed packages to the chosen location, and provides instructions on how to use this to (re)install the packages elsewhere.
#' @param expiry Expiry for online file store. Ignored for other save locations.
#' @param format
#' @param quiet Logical. Suppress output if `TRUE`.
#'
#' @importFrom httr POST
#'
#' @return
#' @export
#'
#' @examples
transfer_packages <- function(library = .libPaths()[1], output = "online", expiry="14d", format = "list", quiet = F) {
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
