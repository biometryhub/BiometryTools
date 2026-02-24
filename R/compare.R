#' Pairwise comparison of BLUEs using LSD or p-values
#'
#' Computes pairwise comparisons among predicted values (typically BLUEs)
#' from an \code{asreml} model for a given classification term. Uses
#' \code{predict(..., sed = TRUE)} to obtain predicted values and the
#' SED (standard error of differences) matrix. Depending on \code{type},
#' returns either a pairwise LSD matrix or a symmetric matrix of
#' pairwise p-values derived from Wald contrasts.
#'
#' @param model An \code{asreml} fitted model object.
#' @param term Character string giving the classification term passed to
#'   \code{predict()}, e.g. \code{"Line"} or an interaction like \code{"Env:Line"}.
#' @param type Character string specifying the comparison type:
#'   \code{"LSD"} or \code{"PVAL"}.
#' @param average.LSD Logical (default \code{FALSE}). If \code{TRUE} and
#'   \code{type = "LSD"}, returns a single \code{ave.LSD} column computed as the
#'   mean of the lower-triangular LSD values instead of a full LSD matrix.
#'
#' @details
#' \strong{LSD:} The least significant difference is computed as
#' \code{LSD = SED * qt(0.025, df = model$nedf, lower.tail = FALSE)}.
#'
#' \strong{PVAL:} All pairwise contrasts are constructed and assessed using
#' a Wald test (via \code{wald.test()}), returning a symmetric p-value matrix.
#'
#' Rows with \code{NA} predicted values are dropped, and the corresponding
#' rows/columns are removed from the SED matrix.
#'
#' @return A \code{data.frame} containing the classification column(s),
#'   predicted values, and either:
#' \itemize{
#'   \item an LSD matrix (or \code{ave.LSD} column), or
#'   \item a symmetric matrix of pairwise p-values.
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' JULES COMPLETE
#' }
compare <- function(model, term = "Line", type = "PVAL", average.LSD = FALSE) {
  pred <- predict(model, classify = term, sed = TRUE)
  nterm <- unlist(strsplit(term, ":"))
  pv <- pred$pvals
  sed <- pred$sed
  if (any(wh <- is.na(pv$predicted.value))) {
    pv <- pv[!wh, ]
    sed <- sed[!wh, !wh]
  }
  if (length(nterm) > 1) {
    labs <- paste(pv[[nterm[1]]], pv[[nterm[2]]], sep = ":")
  } else {
    labs <- pv[[term]]
  }
  if (type %in% "LSD") {
    add <- sed * qt(0.025, df = model$nedf, lower.tail = FALSE)
    dimnames(add)[[2]] <- paste("LSD", labs, sep = ":")
    if (average.LSD) {
      add <- cbind.data.frame(ave.LSD = rep(mean(add[lower.tri(add)]), dim(pv)[1]))
    }
  } else if (type %in% "PVAL") {
    ord <- 1:nrow(pv)
    if (length(nterm) > 1) {
      fix.form <- paste(deparse(model$call$fixed), nterm[1], nterm[2], "1", sep = " - ")
      model <- update(model, fixed. = fix.form, Cfixed = TRUE)
      coefs <- model$coefficients$fixed
      wh <- coefs[, 1] == 0
      cnams <- rownames(coefs)[!wh]
      sp <- strsplit(cnams, ":")
      left <- sapply(sp, "[", 1)[1]
      right <- sapply(sp, "[", 2)[1]
      leftn <- 1
      rightn <- 2
      if (any(grep(nterm[1], right))) {
        leftn <- 2
      }
      rightn <- 1
      left <- gsub(paste(nterm[1], "_", sep = ""), "", sapply(sp, "[", leftn))
      right <- gsub(paste(nterm[2], "_", sep = ""), "", sapply(sp, "[", rightn))
      ord <- pmatch(labs, paste(left, right, sep = ":"))
    } else {
      fix.form <- paste(deparse(model$call$fixed), "1", sep = " - ")
      model <- update(model, fixed. = fix.form, Cfixed = TRUE)
    }
    cb <- t(combn(nrow(pv), 2))
    mat <- matrix(0, nrow = nrow(cb), ncol = nrow(pv))
    mat[cbind(1:nrow(mat), cb[, 1])] <- 1
    mat[cbind(1:nrow(mat), cb[, 2])] <- -1
    lenf <- length(model$coefficients$fixed)
    cc <- list(coef = (1:lenf)[!wh], type = "con", comp = mat)
    wt <- wald.test(model, list(cc))$Contrasts
    pval <- wt$"P-Value"
    add <- matrix(0, nrow = nrow(pv), ncol = nrow(pv))
    add[lower.tri(add)] <- pval
    add <- add + t(add)
    add <- add[ord, ord]
    dimnames(add)[[2]] <- paste("PVAL", labs, sep = ":")
  } else {
    stop("This type is not defined.")
  }
  cbind.data.frame(pv[, 1:(length(nterm) + 1)], add)
}
