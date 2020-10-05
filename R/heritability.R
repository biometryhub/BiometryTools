#' Heritability for multi/single environment trials
#'
#' @param model
#' @param term
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
herit.asreml <- function(model, term = "SYear:Genotype", ...) {
  dat <- eval(model$call$data)
  if (length(grep(":", term))) {
    terms <- all.vars(as.formula(paste("~ ", term, sep = "")))
    labs <- attr(terms(as.formula(model$call$random)), "term.labels")
    iterm <- labs[grep(paste(terms[1], "*.*", terms[2], sep = ""), labs)]
    uv <- sapply(strsplit(iterm, "\\("), "[", 1)
    if (uv == "fa") {
      pred <- predict(model, classify = term, only = iterm, sed = TRUE, ...)
      sumfa <- fa.asreml(model, trunc.char = NULL)
      gam <- diag(sumfa$gammas[[grep(paste(terms[1], "*.*", terms[2], sep = ""), names(sumfa$gammas))]]$Gmat)
    } else if (uv %in% c("diag", "corh", "corgh", "us")) {
      pred <- predict(model, classify = term, only = term, sed = TRUE, ...)
      if (uv %in% c("diag")) {
        gam <- summary(model, vparameters = TRUE)$vparameters[[term]]
      } else {
        gam <- diag(summary(model, vparameters = TRUE)$vparameters[[term]])
      }
    }
    else {
      stop("The function does not understand this asreml function.")
    }
    site <- pred$pvals[[terms[1]]]
    levs <- levels(site)
    avsed <- c()
    for (i in 1:length(levs)) {
      inds <- (1:length(site))[as.character(site) %in% levs[i]]
      sedm <- pred$sed[inds, inds]
      sedm <- sedm[upper.tri(sedm)]
      avsed[i] <- mean(sedm)
    }
  } else {
    pred <- predict(model, classify = term, only = term, sed = TRUE, ...)
    con <- model$vparameters.con[grep("units\\!R", names(model$vparameters.con))]
    if (length(con) == 0 || (con != 4)) {
      gam <- model$vparameters[grep(term, names(model$vparameters))] * model$sigma2
    } else {
      gam <- model$vparameters[grep(term, names(model$vparameters))]
    }
    avsed <- pred$avsed[2]
    levs <- term
  }
  h2 <- 1 - (avsed^2) / (2 * gam)
  names(h2) <- levs
  h2
}
