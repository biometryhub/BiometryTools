#' Checks if a package was installed from GitHub
#'
#' @param pkg A character vector of package names to check.
#'
#' @return A dataframe containing packages installed from remote sources, with the type of remote, account name and repo name.
#' @keywords internal
#'
gh_packages <- function(pkg) {
    list <- which(sapply(pkg, function(x) !is.null(packageDescription(x)$RemoteType)))
    results <- data.frame()
    if (length(list) > 0) {
        type <- sapply(pkg[list], function(x) packageDescription(x)$RemoteType)
        username <- sapply(pkg[list], function(x) ifelse(is.null(packageDescription(x)$RemoteUsername), NA, packageDescription(x)$RemoteUsername))
        repo <- names(list)
        results <- data.frame(type, account = username, repo)
        results <- subset(results, type != "standard")
        rownames(results) <- NULL
    }
    return(results)
}
