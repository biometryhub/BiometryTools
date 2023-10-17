#' Reinstall all currently installed packages
#'
#' @param location Location to check for installed packages and to reinstall to. Defaults to the first option in `.libPaths()`.
#' @param source Logical. Install packages from source that have later source versions than binaries? This should usually be FALSE.
#'
#' @return
#' @export
#'
reinstall_packages <- function(location = .libPaths()[1], source = FALSE) {
    to_install <- rownames(installed.packages(lib.loc = location, priority = "NA"))

    if (source) {
        op <- options(install.packages.compile.from.source = "always")
        on.exit(options(op), add = TRUE)
    }

    install.packages(to_install, lib = location, type = ifelse(source, "both", "binary"))
}
