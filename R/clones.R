#' Check whether genetic clones are in the experiment
#'
#' @param model An `asreml` model.
#' @param cross JULES COMPLETE
#' @param matching JULES COMPLETE
#' @param Envir JULES COMPLETE
#' @param no.samp JULES COMPLETE
#' @param sep JULES COMPLETE
#'
#' @return A data frame containing JULES COMPLETE
#' @export
#'
#' @examples
#' \dontrun{
#' JULES COMPLETE
#' }
phenClones <- function(model, cross, matching = "Genotype", Envir = NULL, no.samp = 1000, sep = "_") {
  mg <- as.character(cross$pheno[[matching]])
  mgs <- lapply(mg, function(el, sep) {
    el <- unlist(strsplit(el, sep))
    if (length(el) > 1) {
      t(combn(el, 2))
    } else {
      NULL
    }
  }, sep = sep)
  mgs <- mgs[!sapply(mgs, is.null)]
  mgd <- do.call("rbind", mgs)
  if (!is.null(Envir)) {
    iterm <- paste(Envir, matching, sep = ":")
    pvals <- predict(model, classify = iterm, only = iterm)$pvals
    pvlist <- split(pvals, pvals[[Envir]])
    levs <- levels(pvals[[Envir]])
  } else {
    pvlist <- list(predict(model, classify = matching, only = matching)$predictions$pvals)
  }
  corlist <- list()
  for (j in 1:length(pvlist)) {
    pvt <- pvlist[[j]]
    cg1 <- pvt$predicted.value[pmatch(mgd[, 1], pvt[[matching]], duplicates.ok = TRUE)]
    cg2 <- pvt$predicted.value[pmatch(mgd[, 2], pvt[[matching]], duplicates.ok = TRUE)]
    cor.samp <- c()
    for (i in 1:no.samp) {
      ts <- sample(pvt$predicted.value, dim(mgd) * 2, replace = FALSE)
      cor.samp[i] <- cor(ts[1:(length(ts) / 2)], ts[(length(ts) / 2 + 1):length(ts)])
    }
    df <- dim(mgd)[1] - 2
    cs <- c(cor.samp, cor(cg1, cg2, use = "complete.obs"))
    pv <- 1 - pf((cs^2) * df / (1 - cs^2), 1, df)
    pva <- pv[1:(length(pv) - 1)]
    corlist[[j]] <- c(length(pva[pva < 0.05]) / no.samp, cs[length(cs)], pv[length(pv)])
  }
  res <- cbind.data.frame(t(do.call("cbind", corlist)))
  names(res) <- c("Type1", "Correlation", "P-value")
  if (!is.null(Envir)) {
    res <- cbind.data.frame(levs, res)
  }
  res
}

#' Fix clones if they are in the experiment
#'
#' @param data The data frame to fix
#' @param cross JULES COMPLETE
#' @param matching The column name to match on
#' @param sep The separator between JULES COMPLETE
#'
#' @return The JULES COMPLETE
#' @export
#'
#' @examples
#' \dontrun{
#' JULES COMPLETE
#' }
phenfixClones <- function(data, cross, matching = "Genotype", sep = "_") {
  mg <- as.character(cross$pheno[[matching]])
  mgs <- lapply(mg, function(el, sep) {
    el <- unlist(strsplit(el, sep))
    if (length(el) > 1) {
      el
    } else {
      NULL
    }
  }, sep = sep)
  mgs <- mgs[!sapply(mgs, is.null)]
  for (i in 1:length(mgs)) {
    levs <- levels(data[[matching]])
    levels(data[[matching]])[levs %in% mgs[[i]]] <- paste(mgs[[i]], collapse = sep)
  }
  data
}
