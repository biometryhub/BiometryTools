#' Associate BLUEs with BLUPs from predict calls of the same model
#'
#' @param model An `asreml` model.
#' @param ran.term Random terms from the `model`.
#' @param fix.term Fixed terms from the `model`.
#' @param ... Other parameters to be passed to the predict function.
#'
#' @return JULES COMPLETE
#' @export
#'
#' @examples
#' \dontrun{
#' JULES COMPLETE
#' }
#'
associate <- function(model, ran.term = "Treatment:Cultivar", fix.term = "Treatment:Type", ...) {
  rnams <- all.vars(as.formula(paste("~ ", ran.term, sep = "")))
  fnams <- all.vars(as.formula(paste("~ ", fix.term, sep = "")))
  if (length(ran.term) > 1) {
    labs <- attr(terms(as.formula(model$call$random)), "term.labels")
    iterm <- labs[grep(paste(rnams[1], "*.*", rnams[2], sep = ""), labs)]
    uv <- sapply(strsplit(iterm, "\\("), "[", 1)
    if (uv == "fa") {
      predr <- predict(model, classify = ran.term, only = iterm, ...)
    } else if (uv %in% c("diag", "corh", "corgh", "us")) {
      predr <- predict(model, classify = ran.term, only = ran.term, ...)
    }
  } else {
    predr <- predict(model, classify = ran.term, only = ran.term, ...)
  }
  pr <- predr$pvals
  names(pr) <- gsub("predicted.value", "blups", names(pr))
  names(pr) <- gsub("std.error", "blups.std.error", names(pr))
  predf <- predict(model, classify = fix.term, ...)
  pf <- predf$pvals
  pf <- pf[!is.na(pf$predicted.value), ]
  names(pf) <- gsub("predicted.value", "blues", names(pf))
  names(pf) <- gsub("std.error", "blues.std.error", names(pf))
  dat <- eval(model$call$data)
  datr <- dat[, unique(c(rnams, fnams))]
  datr <- datr[!duplicated(datr[, rnams]), ]
  pri <- merge(pr[-ncol(pr)], datr, by = rnams, all.x = TRUE)
  pall <- merge(pri, pf[-ncol(pf)], by = fnams, all.x = TRUE)
  pall
}
