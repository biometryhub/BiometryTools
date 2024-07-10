#' BLUEs LSD/p-value comparison function
#'
#' @param model An `asreml` model
#' @param term JULES COMPLETE
#' @param type The type of comparison. Can take values of `PVAL` or `LSD`.
#' @param average.LSD JULES COMPLETE
#'
#' @return
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
