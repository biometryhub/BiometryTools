#' Convert Treatment × Site BLUPs into Efficiency and Responsiveness
#'
#' Given an \code{asreml} model with a Treatment × Site × Variety structure,
#' this function extracts BLUPs for a Treatment × Site interaction (classified
#' by Variety) and re-parameterizes them into:
#' \itemize{
#'   \item \strong{Efficiency}: the BLUP under a baseline treatment (\code{levs[1]})
#'   \item \strong{Responsiveness}: the residual BLUP under a second treatment
#'         (\code{levs[2]}) after regressing \code{levs[2]} on \code{levs[1]}
#'         using the random-effect covariance matrix
#' }
#'
#' For each site \eqn{s}, the regression coefficient is:
#'
#' \deqn{
#'   \beta_s = \mathrm{Cov}(T_1, T_2) / \mathrm{Var}(T_1)
#' }
#'
#' and responsiveness BLUPs are computed as:
#'
#' \deqn{
#'   b_{\mathrm{resp}} = b_{T_2} - \beta_s b_{T_1}.
#' }
#'
#' The function also returns the transformed covariance matrix
#' \eqn{G_{\mathrm{trans}} = T G T^\top} corresponding to the
#' efficiency/responsiveness parameterization.
#'
#' \strong{Important:}
#' If the fitted model assumes independent Treatment × Site effects
#' (e.g. \code{random = ~ TSite:Variety}), then the covariance between
#' treatments is zero and \eqn{\beta_s = 0}. In that case,
#' responsiveness reduces to the BLUP under \code{levs[2]}.
#'
#' @param model An \code{asreml} fitted model object.
#' @param Env Character string of the form \code{"<TS_term>:<Variety_term>"},
#'   e.g. \code{"TSite:Variety"}, used in \code{predict(..., classify = Env)}.
#' @param levs Character vector of length 2 giving the two treatment levels.
#'   \code{levs[1]} is treated as the baseline (efficiency).
#' @param sep Separator used in Treatment × Site factor level names.
#' @param ... Additional arguments passed to \code{predict()}.
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{blups}: data frame with columns \code{Site}, \code{Variety},
#'         \code{levs[1]}, \code{levs[2]}, and \code{resp}
#'   \item \code{TGmat}: transformed covariance matrix
#'   \item \code{Gmat}: original covariance matrix
#'   \item \code{beta}: per-site regression coefficient
#'   \item \code{sigr}: per-site residual variance
#'   \item \code{tmat}: transformation matrix
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(asreml)
#' library(agridat)
#'
#' data(besag.met)
#' dat <- besag.met
#'
#' dat$county <- factor(dat$county)
#' dat$gen    <- factor(dat$gen)
#' dat$rep    <- factor(dat$rep)
#' dat$block  <- factor(dat$block)
#'
#' # Create artificial 2-level treatment within each site
#' dat$Treat <- ave(seq_len(nrow(dat)), dat$county, FUN = function(i) {
#'   rep(c("Treat1","Treat2"), length.out = length(i))
#' })
#' dat$Treat <- factor(dat$Treat)
#'
#' dat$TSite   <- interaction(dat$Treat, dat$county, sep = "-")
#' dat$Variety <- dat$gen
#'
#' m <- asreml(
#'   fixed  = yield ~ Treat,
#'   random = ~ rep + block + Variety + TSite:Variety,
#'   data   = dat
#' )
#'
#' out <- conv(
#'   model = m,
#'   Env   = "TSite:Variety",
#'   levs  = c("Treat1","Treat2")
#' )
#'
#' head(out$blups)
#'
#' # In this model, treatments are independent,
#' # so beta will be zero:
#' out$beta
#' }
#'
conv <- function(model,
                 Env  = "TSite:Variety",
                 levs = NULL,
                 sep  = "-",
                 ...) {

  if (is.null(levs) || length(levs) != 2)
    stop("levs must be a character vector of length 2.")

  evnam <- strsplit(Env, ":", fixed = TRUE)[[1]]
  enam  <- evnam[1]
  vnam  <- evnam[2]

  # Extract BLUPs safely (ASReml 4 compatible)
  pred  <- stats::predict(model, classify = Env, ...)
  pvals <- pred$pvals

  pred_col <- grep("predicted\\.value|predicted|estimate",
                   names(pvals), value = TRUE)

  if (length(pred_col) != 1)
    stop("Could not uniquely identify predicted value column.")

  names(pvals)[names(pvals) == pred_col] <- "blup"

  # Ensure interaction column exists
  if (!(enam %in% names(pvals))) {

    # ASReml returns split columns (TSite + Variety)
    if (all(evnam %in% names(pvals))) {
      pvals[[enam]] <- as.character(pvals[[evnam[1]]])
    } else {
      stop("Could not reconstruct interaction column ", enam)
    }
  }

  tsnams <- unique(as.character(pvals[[enam]]))

  if (!any(grepl(sep, tsnams, fixed = TRUE)))
    stop("Separator not found in Treatment by Site levels.")

  st <- strsplit(tsnams, sep, fixed = TRUE)

  if (any(lengths(st) != 2))
    stop("Interaction levels must split into exactly 2 parts.")

  tnam <- vapply(st, `[`, "", 1)
  snam <- vapply(st, `[`, "", 2)

  if (!all(levs %in% tnam))
    stop("Treatment levels not found in interaction levels.")

  usnams <- unique(snam)

  # Extract G structure
  rterm <- attr(terms.formula(model$call$random), "term.labels")
  rterm <- rterm[grep(paste(evnam, collapse = "|"), rterm)]

  # FA Models
  if (length(rterm) == 1 && substring(rterm, 1, 2) == "fa") {

    sumfa <- ASExtras4::fa.asreml(model, trunc.char = NULL)
    Gmat  <- sumfa$gammas[[rterm]]$Gmat

  } else {

    # Standard case: often scalar variance
    Gmat <- summary(model, vparameters = TRUE)$vparameters[[Env]]

    # If scalar, convert to diagonal matrix
    if (length(Gmat) == 1) {
      Gmat <- diag(Gmat, length(tsnams))
      dimnames(Gmat) <- list(tsnams, tsnams)
    }
  }

  # Compute efficiency / Responsiveness
  tmat <- diag(nrow(Gmat))
  beta <- sigr <- numeric(length(usnams))
  blist <- vector("list", length(usnams))

  for (i in seq_along(usnams)) {

    inds <- which(snam == usnams[i])
    names(inds) <- tnam[inds]

    if (!all(levs %in% names(inds))) next

    tind <- inds[levs]
    mat  <- Gmat[tind, tind, drop = FALSE]

    # if covariance available
    if (nrow(mat) == 2 && mat[1,1] != 0) {
      beta[i] <- mat[1,2] / mat[1,1]
      rho     <- mat[1,2] / sqrt(mat[1,1] * mat[2,2])
      sigr[i] <- mat[2,2] * (1 - rho^2)
    } else {
      beta[i] <- 0
      sigr[i] <- mat[2,2]
    }

    tmat[tind[2], tind[1]] <- -beta[i]

    blow  <- pvals$blup[pvals[[enam]] == tsnams[tind[1]]]
    bhigh <- pvals$blup[pvals[[enam]] == tsnams[tind[2]]]
    bresp <- bhigh - beta[i] * blow

    blist[[i]] <- data.frame(blow, bhigh, bresp)
  }

  TGmat <- tmat %*% Gmat %*% t(tmat)

  # relabel matrix
  newnams <- gsub(levs[1], "eff", tsnams)
  newnams <- gsub(levs[2], "resp", newnams)
  dimnames(TGmat) <- list(newnams, newnams)

  blups <- do.call(rbind, blist)
  names(blups) <- c(levs[1], levs[2], "resp")

  glev <- unique(as.character(pvals[[vnam]]))

  blups <- data.frame(
    Site    = rep(usnams, each = length(glev)),
    Variety = rep(glev, length(usnams)),
    blups
  )

  return(list(
    blups = blups,
    TGmat = TGmat,
    Gmat  = Gmat,
    beta  = beta,
    sigr  = sigr,
    tmat  = tmat
  ))
}
