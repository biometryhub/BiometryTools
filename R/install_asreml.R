#' Install ASreml-R package
#'
#' @description A helper function for installing the ASreml-R package, intended to reduce the difficulty of finding the correct version for your operating system and R version.
#'
#' @param library Library location to install ASreml-R. Uses first option in `.libPaths()` by default.
#' @param quiet Logical (default `FALSE`). Should package be installed quietly?
#' @param force Logical (default `FALSE`). Force ASreml-R to install. Useful for upgrading if it is already installed.
#' @param keep_file Should the downloaded asreml package file be kept? Default is `FALSE`. `TRUE` downloads to current directory. A file path can also be provided to save to another directory. See `Details` for more information.
#'
#' @details The package file is downloaded from a shortlink, and if `keep_file` is `TRUE`, the package archive file will be saved in the current directory. If a valid path is provided in `keep_file`, the file will be saved to that path, but no directory will be created. If keep_file does not specify an existing, valid path, an error will be shown.
#'
#' @importFrom utils installed.packages install.packages download.file remove.packages
#' @importFrom httr GET write_disk progress
#' @importFrom glue glue glue_col glue_collapse single_quote
#' @importFrom crayon green
#'
#' @export
#'
#' @return Silently returns `TRUE` if `asreml` installed successfully or already present, `FALSE` otherwise. Optionally prints a confirmation message on success.
#'
install_asreml <- function(library = .libPaths()[1], quiet = FALSE, force = FALSE, keep_file = FALSE) {
    if("asreml" %in% installed.packages()[,1] & !force) {
        if(!quiet) message(crayon::green("ASreml-R is already installed."))
        invisible(TRUE)
    }
    else {
        if(!quiet) {
            message("\nDownloading and installing ASreml-R. This may take some time, depending on internet speed...\n")
        }
        if(force & "asreml" %in% (.packages())) {
            detach("package:asreml", unload = TRUE)
        }

        os <- switch(Sys.info()[['sysname']],
                     Windows = "win",
                     Linux   = "linux",
                     Darwin  = "mac"
        )

        ver <- glue::glue("{os}_{substr(getRversion(), 1, 3)}")

        url <- switch(ver,
                      win_3.5 = {"https://link.biometryhubwaite.com/win-35"},
                      win_3.6 = {"https://link.biometryhubwaite.com/win-36"},
                      win_4.0 = {"https://link.biometryhubwaite.com/win-40"},
                      win_4.1 = {"https://link.biometryhubwaite.com/win-41"},
                      mac_3.5 = {"https://link.biometryhubwaite.com/mac-35"},
                      mac_3.6 = {"https://link.biometryhubwaite.com/mac-36"},
                      mac_4.0 = {"https://link.biometryhubwaite.com/mac-40"},
                      linux_3.5 = {"https://link.biometryhubwaite.com/linux-35"},
                      linux_3.6 = {"https://link.biometryhubwaite.com/linux-36"},
                      linux_4.0 = {"https://link.biometryhubwaite.com/linux-40"}
        )

        # First check if file already exists, both in the current directory and temp folder
        # Need to create a regex to check it's the correct file extension, so tests ignore .R files
        temp_files <- list.files(tempdir(), pattern = "asreml+(([a-zA-Z0-9_.\\-])*)+(.zip|.tar.gz|.tgz)")
        dir_files <- list.files(pattern = "asreml+(([a-zA-Z0-9_.\\-])*)+(.zip|.tar.gz|.tgz)")

        if(length(temp_files) > 0) {
            filename <- temp_files
            save_file <- glue::glue("{tempdir()}/{filename}")

            if(keep_file == TRUE) {
                install_file <- filename
                # Copy to current working directory
                file.copy(save_file, filename)
            }

            else if(is.character(keep_file)) {
                if(!dir.exists(keep_file)) {
                    stop("Directory provided in keep_path does not exist. Please provide a path in the keep_file argument to save the package to.")
                }
                else {
                    install_file <- glue::glue("{keep_file}/{filename}")
                    # If path has a trailing slash remove it
                    # install_file <- gsub("//", "/", install_file)
                    file.copy(save_file, install_file)
                }
            }
            else {
                install_file <- save_file
            }
        }
        else if(length(dir_files) > 0) {
            filename <- dir_files
            save_file <- filename

            if(is.character(keep_file)) {
                if(!dir.exists(keep_file)) {
                    stop("Directory provided in keep_path does not exist. Please provide a path in the keep_file argument to save the package to.")
                }
                else {
                    install_file <- glue::glue("{keep_file}/{filename}")
                    # If path had a trailing slash remove it
                    # install_file <- gsub("//", "/", install_file)
                    file.copy(save_file, install_file)
                    file.remove(save_file)
                }
            }
            else {
                install_file <- filename
            }
        }
        # Can't find file, download
        else {
            #Create a temporary file to save the package
            save_file <- tempfile("asreml_")

            # Use httr to GET the file which also gives the expanded URL
            response <- httr::GET(url = url, httr::write_disk(save_file), if(!quiet){httr::progress()})

            # Find position of the last / in the expanded URL
            pos <- regexpr("\\/[^\\/]*$", response$url)

            # Extract everything after the last / as the filename
            filename <- substr(response$url, pos+1, nchar(response$url))

            # If keep_file is true, copy asreml to current directory
            if(keep_file == TRUE) {
                install_file <- filename
                file.copy(save_file, filename)
            }
            else if(!keep_file) {
                install_file <- glue::glue("{tempdir()}/{filename}")
                file.rename(save_file, install_file)
            }
            else { # Assume keep_file is a path
                if(!dir.exists(keep_file)) {
                    stop("Directory provided in keep_path does not exist. Please provide a valid path in the keep_file argument to save the package to.")
                }
                else {
                    install_file <- glue::glue("{keep_file}/{filename}")
                    file.copy(save_file, install_file)
                }
            }
        }

        # If forcing installation, remove existing version to avoid errors on installation
        if("asreml" %in% installed.packages()[,1] & force) {
            suppressMessages(remove.packages("asreml"))
        }

        # Check dependencies are installed first

        deps <- c("data.table", "ggplot2", "grid", "methods", "jsonlite")
        for(i in seq_along(deps)) {
            if(deps[i] %!in% installed.packages()[,1]) {
                install.packages(deps[i], repos = "https://cloud.r-project.org")
            }
        }

        if("asreml" %in% installed.packages()[,1] & force) {
            suppressMessages(remove.packages("asreml"))
        }

        # Install asreml
        install.packages(install_file, repos = NULL, quiet = quiet, type = ifelse(os == "win", "binary", "source"))

        if("asreml" %in% installed.packages()[,1]) {
            if(!quiet) {
                if (requireNamespace("usethis", quietly = TRUE)) {
                    usethis::ui_done(crayon::green("ASreml-R successfully installed!"))
                }
                else {
                    message(crayon::green("ASreml-R successfully installed!"))
                }
            }
        }
        else {
            if(!quiet) warning("There was a problem with installation and ASreml-R was not successfully installed.")
            invisible(FALSE)
        }
        invisible(TRUE)
    }
}


# Should do this with:
## @importFrom magrittr %>%
## @export
#magrittr::`%>%`

#' Update asreml package
#'
#' @param ... other arguments passed to [BiometryTools::install_asreml()]
#'
#' @export
update_asreml <- function(...) {
    install_asreml(force = T, ...)
}
