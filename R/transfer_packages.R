#' Easily reinstall all currently installed packages on another machine or version of R.
#'
#' @param library The location of the library on the current machine to copy.
#' @param output One of `online` (the default), `gist` or `local`. Saves a list of installed packages to the chosen location, and provides instructions on how to use this to (re)install the packages elsewhere. See Details for more information.
#' @param expiry Expiry for online file store. Ignored for other save locations.
#' @param filename Filename for the local output file. Ignored if `output` is not set to `local`.
#' @param quiet Logical (default `FALSE`). Suppress output if `TRUE`.
#'
#' @importFrom httr POST content
#' @importFrom crayon green
#' @importFrom glue glue glue_col glue_collapse single_quote
#'
#' @details If `output` is `online`, the resulting list of currently installed packages is stored on [https://file.io](https://file.io) for the time specified in `expiry`.
#' If `output` is `local`
#'
#' @return
#' @export
#'
#' @examples
#'
transfer_packages <- function(library = .libPaths()[1], output = "online", expiry="14d", filename = "transfer_packages", quiet = F) {
    # Get list of installed packages
    # Choose location to save
    #   file.io is default, also enable gist and local file (.Rds or csv)
    # Upload, create gist or save file, then output instructions for other computer
    # Also output those packages not installed from CRAN

    pkgs <- unname(installed.packages(lib.loc = library, priority = "NA")[,1])
    to_install <- glue::glue("install.packages(c({glue::glue_collapse(glue::single_quote(pkgs), sep = ', ')}), repos = 'https://cloud.r-project.org')")
    green <- crayon::green

    if(output == "online") {
        r <- httr::POST(glue::glue("https://file.io?expires={expiry}"), body = list(text = to_install))
        link <- httr::content(r)$link
        if(!quiet) {
            message(glue::glue_col("Now run {green source({link})} on the other machine to install the packages."))
        }
    }
    else if(output == "local") {
        write(to_install, file = glue::glue("{filename}.R"))
        if(!quiet) {
            message(glue::glue_col("Now copy the file run {filename}.R to the other machine and run {green source('{filename}.R')} to install the packages."))
        }
    }
    else if(output == "gist") {
        # This should only be used by advanced users, do you want to continue? Y/n?
        # Public or private?
    }
    else {
        stop("output should be one of 'online', 'local' or 'gist'")
    }
}
