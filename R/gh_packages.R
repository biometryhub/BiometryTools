#' Identify packages installed from remote sources
#'
#' This function checks which packages in the provided list were installed from 
#' remote sources (GitHub, GitLab, etc.) rather than CRAN, and returns their 
#' remote source information.
#'
#' @param pkg A character vector of package names to check.
#'
#' @return A data frame containing packages installed from remote sources with columns:
#'   \itemize{
#'     \item \code{type}: The type of remote source (e.g., "github", "gitlab")
#'     \item \code{account}: The account/username of the remote repository
#'     \item \code{repo}: The repository name (same as package name)
#'   }
#'   Returns an empty data frame if no packages are from remote sources.
#'
#' @details 
#' The function examines the DESCRIPTION file of each package to determine if it 
#' was installed from a remote source. Packages installed from CRAN or other 
#' standard repositories will not have RemoteType information.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Check if specific packages are from GitHub
#' gh_packages(c("ggplot2", "devtools", "BiometryTools"))
#' 
#' # Check all installed packages
#' gh_packages(rownames(installed.packages()))
#' }
#'
gh_packages <- function(pkg) {
  # Input validation
  if (!is.character(pkg) || length(pkg) == 0) {
    warning("'pkg' must be a non-empty character vector")
    return(data.frame(type = character(), account = character(), repo = character()))
  }
  
  # Get package descriptions once and store them
  descriptions <- lapply(pkg, function(x) {
    tryCatch(packageDescription(x), 
             error = function(e) NULL,
             warning = function(w) NULL)
  })
  names(descriptions) <- pkg
  
  # Filter to only packages with remote information
  remote_packages <- descriptions[!sapply(descriptions, function(desc) {
    is.null(desc) || is.null(desc$RemoteType)
  })]
  
  # Return empty data frame if no remote packages found
  if (length(remote_packages) == 0) {
    return(data.frame(type = character(), account = character(), repo = character()))
  }
  
  # Extract information for remote packages
  results <- data.frame(
    type = sapply(remote_packages, function(desc) desc$RemoteType %||% NA_character_),
    account = sapply(remote_packages, function(desc) desc$RemoteUsername %||% NA_character_),
    repo = names(remote_packages),
    stringsAsFactors = FALSE
  )
  
  # Filter out "standard" type packages and reset row names
  results <- results[results$type != "standard" & !is.na(results$type), ]
  rownames(results) <- NULL
  
  return(results)
}

# Helper function for null coalescing (similar to %||% in rlang)
`%||%` <- function(x, y) if (is.null(x)) y else x
