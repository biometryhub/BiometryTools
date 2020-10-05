#' Title
#'
#' @param model
#' @param term
#' @param by
#' @param omit.string
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
hsd <- function(model, term = "Treatment:Genotype", by = "Treatment", omit.string = NULL, ...) {
  pred <- predict(model, classify = term, sed = TRUE, ...)
  pv <- pred$pvals
  inds <- !is.na(pv$predicted.value)
  pv <- pv[inds, ]
  sed <- pred$sed[inds, inds]
  section <- FALSE
  if (!is.null(by)) {
    if (length(grep(":", term))) {
      terms <- unlist(strsplit(term, ":"))
    }
    if (length(grep(":", by))) {
      bys <- unlist(strsplit(by, ":"))
      pv[[by]] <- apply(pv[, bys], 1, function(el) paste(el, collapse = ":"))
      section <- TRUE
    }
    if (all(terms %in% bys)) {
      stop("Argument \"by\" indicates no multiple comparisons are being made.")
    }
    if (!all(bys %in% terms)) {
      stop("Some terms in argument \"by\" are not in \"term\".")
    }
  }
  if (!is.null(omit.string)) {
    oind <- grep(omit.string, as.character(pv[[gnam]]))
    if (length(oind)) {
      pv <- pv[-oind, ]
      sed <- sed[-oind, -oind]
    }
  }
  if (section) {
    sst <- as.character(pv[[by]])
    um <- unique(sst)
    hsd <- c()
    for (k in 1:length(um)) {
      inds <- sst %in% um[k]
      ssed <- sed[inds, inds]
      avsed <- mean(ssed[upper.tri(ssed, diag = FALSE)])
      hsd[k] <- (avsed / sqrt(2)) * qtukey(0.95, length(inds), model$nedf)
    }
    pv$HSD <- rep(hsd, times = table(sst))
  } else {
    pv$HSD <- (pred$avsed[2] / sqrt(2)) * qtukey(0.95, nrow(pv), model$nedf)
  }
  pv
}
