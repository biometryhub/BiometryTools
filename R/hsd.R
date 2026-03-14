#' Compute Tukey-style HSD thresholds from `asreml` predictions
#'
#' Computes a Tukey-style honestly significant difference (HSD) threshold from
#' predicted values obtained from a fitted \code{asreml} model, using the
#' associated standard errors of difference (SEDs).
#'
#' The function can calculate:
#' \itemize{
#'   \item a single HSD threshold across all predicted values, or
#'   \item separate HSD thresholds within groups defined by \code{by}.
#' }
#'
#' This is useful for comparing predicted means or BLUPs from plant breeding
#' models, especially when predictions are made for interaction terms such as
#' \code{Treatment:Genotype}.
#'
#' @param model A fitted \code{asreml} model object.
#' @param term A character string specifying the term to be predicted and
#'   compared, for example \code{"Treatment:Genotype"}.
#' @param by An optional character string specifying the factor(s) within which
#'   multiple comparisons are to be made. This must be a subset of the factors
#'   in \code{term}. For example, if \code{term = "Treatment:Genotype"}, setting
#'   \code{by = "Treatment"} computes HSD values separately within each
#'   treatment.
#'   If \code{NULL}, a single HSD is computed across all predictions.
#' @param omit.string An optional character string used to omit rows whose
#'   comparison factor matches the supplied pattern.
#' @param ... Additional arguments passed to \code{predict.asreml()}.
#'
#' @details
#' The function first obtains predicted values and their SED matrix using
#' \code{predict.asreml()}. It then calculates an average SED, either:
#' \itemize{
#'   \item across all predictions, or
#'   \item within each section defined by \code{by}.
#' }
#'
#' The HSD threshold is computed as:
#' \deqn{
#' HSD = \frac{\bar{SED}}{\sqrt{2}} \times q
#' }
#' where \eqn{\bar{SED}} is the average pairwise SED and \eqn{q} is the Tukey
#' critical value from \code{qtukey()} using the model denominator degrees of
#' freedom.
#'
#' The returned object is the prediction table with an added \code{HSD} column.
#'
#' @return
#' A data frame of predicted values returned by \code{predict.asreml()},
#' with an additional column:
#' \describe{
#'   \item{HSD}{The Tukey-style honestly significant difference threshold,
#'   either global or section-specific depending on \code{by}.}
#' }
#' @export
#'
#' @seealso
#' \code{\link[asreml]{predict.asreml}},
#' \code{\link[stats]{qtukey}}
#'
#' @examples
#' \dontrun{
#' TODO
#' }
#'
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
