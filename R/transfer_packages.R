#' Easily reinstall all currently installed packages on another machine or version of R.
#'
#' @param library The location of the library on the current machine to copy.
#' @param output One of `online` (the default), `gist` or `local`. Saves a list of installed packages to the chosen location, and provides instructions on how to use this to (re)install the packages elsewhere. See Details for more information.
#' @param expiry Expiry for online file store in days. Weeks can be given with `w`, or months with `m`. Default is 7 days. Will be ignored if `output` is not `online`.
#' @param filename Filename for the local output file. Ignored if `output` is not set to `local`.
#' @param include_remotes Logical (default `TRUE`). Check for any packages installed from repositories other than CRAN, and output instructions to reinstall.
#' @param quiet Logical (default `FALSE`). Suppress output if `TRUE`.
#'
#' @importFrom httr POST content
#' @importFrom crayon green blue
#' @importFrom glue glue glue_col glue_collapse single_quote
#'
#' @details If `output` is `online`, the resulting list of currently installed packages is stored on [https://file.io](https://file.io) for the time specified in `expiry`.
#' If `output` is `local`, an R script file (`.R`) is saved to the current working directory, which can be transferred manually to another machine.
#'
#' @return
#' @export
#'
#' @examples
#'
transfer_packages <- function(library = .libPaths()[1], output = "online", expiry = "7d", filename = "transfer_packages", include_remotes = TRUE, quiet = FALSE) {
  pkgs <- unname(installed.packages(lib.loc = library, priority = "NA")[, 1])
  to_install <- glue::glue("install.packages(c({glue::glue_collapse(glue::single_quote(pkgs), sep = ', ')}), repos = 'https://cloud.r-project.org')")
  green <- crayon::green
  blue <- crayon::blue

  if (output == "online") {
    r <- httr::POST(glue::glue("https://file.io?expires={expiry}"), body = list(text = to_install))
    link <- httr::content(r)$link
    if (!quiet) {
      message(glue::glue_col("Now run {green source({link})} on the other machine to install the packages."))
    }
  }
  else if (output == "local") {
    write(to_install, file = glue::glue("{filename}.R"))
    if (!quiet) {
      message(glue::glue_col("Now copy the file run {filename}.R to the other machine and run {green source('{filename}.R')} to install the packages."))
    }
  }
  else if (output == "gist") {
    # This should only be used by advanced users, do you want to continue? Y/n?
    # Public or private?
  }
  else {
    stop("output should be one of 'online', 'local' or 'gist'")
  }

  if (include_github) {
    gh <- gh_packages(pkgs)
    if (nrow(gh) > 0 & !quiet) {
      message(glue::glue_col("You have some packages installed from remote sources other than CRAN.
                               They are: {blue {glue::glue_collapse(gh$repo, sep = ', ', last = ' and ')}}"))
      # print(gh, row.names = F)
      message(glue::glue_col("You will need to use the {blue remotes} package to reinstall them."))
      if ("github" %in% gh$type) {
        message("To install those from github, run:")
        output <- paste0("'", gh[which(gh$type == "github"), 2], "/", gh[which(gh$type == "github"), 3], "'", collapse = ", ")
        message(glue::glue_col("To install those from github, run:
                               {blue remotes::install_github(c({output}))}"))
      }
    }
  }
}
