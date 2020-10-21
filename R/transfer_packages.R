#' Easily reinstall all currently installed packages on another machine or version of R.
#'
#' @param library The location of the library on the current machine to copy.
#' @param output One of `online` (the default), `gist` or `local`. Saves a list of installed packages to the chosen location, and provides instructions on how to use this to (re)install the packages elsewhere. See details for more information.
#' @param expiry Expiry for online file store in days. Weeks can be given with `w`, or months with `m`. Default is 7 days. Will be ignored if `output` is not `online`.
#' @param filename Filename for the local output file. Ignored if `output` is not set to `local`.
#' @param list_remotes Logical (default `TRUE`). Check for any packages installed from repositories other than CRAN, and output instructions to reinstall.
#' @param quiet Logical (default `FALSE`). Suppress output if `TRUE`.
#'
#' @importFrom httr POST content
#' @importFrom crayon green blue
#' @importFrom glue glue glue_col glue_collapse single_quote
#'
#' @details If `output` is `online`, the resulting list of currently installed packages is stored on [https://file.io](https://file.io) for the time specified in `expiry`, or until the URL is first accessed.
#' Note that both visiting the URL and sourcing the URL count as access, and it will be removed after either.
#' If `output` is `local`, an R script file (`.R`) is saved to the current working directory, which can be transferred manually to another machine.
#' Beware if using `quiet = TRUE` together with `output = online`, as the source command will not be
#'
#' @return Prints instructions to console if `quiet = FALSE`, and invisibly returns the source command to use on the other machine.
#' @export
#'
transfer_packages <- function(library = .libPaths()[1], output = "online", expiry = "7d", filename = "transfer_packages", list_remotes = TRUE, quiet = FALSE) {
    pkgs <- unname(installed.packages(lib.loc = library, priority = "NA")[, 1])
    gh <- gh_packages(pkgs)
    pkgs <- setdiff(pkgs, gh$repo)

    to_install <- glue::glue("install.packages(c({glue::glue_collapse(glue::single_quote(pkgs), sep = ', ')}), repos = 'https://cloud.r-project.org')")
    to_install <- glue::glue("cat('Packages will now be installed. There will be a large amount of text output, which is normal.\n', fill = T)
Sys.sleep(5)
{to_install}")
    if(list_remotes & nrow(gh)>0) {
        to_install <- glue::glue_col("{to_install}
        message('These pacakges were installed from a repository other than CRAN, and have not been reinstalled: {blue {glue::glue_collapse(gh$repo, sep = ', ', last = ' and ')}}\n', fill = T)")
    }

    green <- crayon::green
    blue <- crayon::blue

    if (output == "online") {
        r <- httr::POST(glue::glue("https://file.io/?expires={expiry}"), body = list(text = to_install))
        link <- httr::content(r)$link
        if (!quiet) {
            cmd <- glue::glue("source('{link}')")
            message(glue::glue_col("Now run {green {cmd}} on the other machine to install the packages."))
        }
    }
    else if (output == "local") {
        write(to_install, file = glue::glue("{filename}.R"))
        if (!quiet) {
            cmd <- glue::glue("source('{filename}.R'")
            message(glue::glue_col("Now copy the file {filename}.R to the other machine and run {green {cmd}} to install the packages."))
        }
    }
    else if (output == "gist") {
        # This should only be used by advanced users, do you want to continue? Y/n?
        # Public or private?
    }
    else {
        stop("output should be one of 'online', 'local' or 'gist'")
    }

    if (list_remotes) {
        # gh <- gh_packages(pkgs)
        if (nrow(gh) > 0 & !quiet) {
            message(glue::glue_col("\n\nYou have some packages installed from remote sources other than CRAN. They are: {blue {glue::glue_collapse(gh$repo, sep = ', ', last = ' and ')}}"))

            message(glue::glue_col("You will need to use the {blue remotes} package to reinstall them."))
            if ("github" %in% gh$type) {
                output <- paste0("'", gh[which(gh$type == "github"), 2], "/", gh[which(gh$type == "github"), 3], "'", collapse = ", ")
                message(glue::glue_col("To install those from github, run:
                               {blue remotes::install_github(c({output}))}"))
            }
        }
    }
    invisible(cmd)
}
