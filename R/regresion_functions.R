#' Conversion function for Efficiency and Responsiveness BLUPs in Treatment x Site x Variety experiments
#'
#' The function assumes you have a Treatment x Site factor that is a composite of treatments and sites. The function requires no specific ordering of the factor levels.
#'
#' @param model An `asreml` object. The final full Treatment x Site x Variety model
#' @param Env Treatment x Site x Variety term as a character.
#' @param levs Named treatment levels used in transformation. e.g. `c("Treat1", "Treat2")` would regress Treat2 on Treat1
#' @param sep separator used for Treat x Site names (if multi-x model), if not present assumes single section
#' @param ... Other parameters passed to [asreml::predict.asreml()].
#'
#' @return
#' @export
#'
#' @examples
randomRegress <- function(model, Env = "TSite:Variety", levs = NULL, sep = "-", ...) {
  if (is.null(levs)) {
    stop("Treatment levels cannnot be NULL.")
  }
  evnam <- unlist(strsplit(Env, ":"))
  enam <- evnam[1]
  vnam <- evnam[2]
  rterm <- attr(terms.formula(model$call$random), "term.labels")
  rterm <- rterm[grep(paste(evnam, collapse = "|"), rterm)]
  print(rterm)
  if (substring(rterm, 1, 2) == "fa") {
    sumfa <- fa.asreml(model, trunc.char = NULL)
    pvals <- sumfa$blups[[rterm]]$blups[, 1:3]
    Gmat <- sumfa$gammas[[rterm]]$Gmat
  }
  else {
    pred <- predict(model, classify = Env, only = Env, ...)
    Gmat <- summary(model, vparameters = TRUE)$vparameters[[Env]]
    pvals <- pred$pvals
    names(pvals)[3] <- "blup"
  }
  tsnams <- dimnames(Gmat)[[2]]
  if (length(grep(sep, tsnams))) {
    st <- strsplit(tsnams, split = sep)
    tnam <- sapply(st, function(el) el[1])
    snam <- sapply(st, function(el) el[2])
    if (!all(levs %in% c(snam, tnam))) {
      stop("Treatment levels do not exist in ", enam)
    }
    if (all(levs %in% snam)) {
      tnam <- snam
      snam <- sapply(st, function(el) el[1])
    }
  } else {
    tnam <- tsnams
    snam <- rep("Single", length(tnam))
  }
  usnams <- unique(snam)
  tmat <- diag(nrow(Gmat))
  beta <- sigr <- c()
  blist <- list()
  for (i in 1:length(usnams)) {
    inds <- (1:length(snam))[snam %in% usnams[i]]
    names(inds) <- tnam[inds]
    whl <- (1:2)[levs %in% names(inds)]
    if (length(whl) == 2) {
      tind <- inds[levs]
      mat <- Gmat[tind, tind]
      beta[i] <- mat[1, 2] / mat[1, 1]
      rho <- mat[1, 2] / sqrt(mat[1, 1] * mat[2, 2])
      sigr[i] <- (mat[2, 2] * (1 - rho^2))
      tmat[tind[2], tind[1]] <- -beta[i]
      blow <- pvals$blup[pvals[[enam]] %in% tsnams[tind[1]]]
      bhigh <- pvals$blup[pvals[[enam]] %in% tsnams[tind[2]]]
      bresp <- bhigh - beta[i] * blow
      blist[[i]] <- cbind.data.frame(blow, bhigh, bresp)
    } else {
      slevs <- levs[whl]
      tind <- inds[slevs]
      if (whl == 1) {
        blist[[i]] <- cbind.data.frame(blow = pvals$blup[pvals[[enam]] %in% tsnams[tind]], bhigh = NA, bresp = NA)
      } else {
        blist[[i]] <- cbind.data.frame(blow = NA, bhigh = pvals$blup[pvals[[enam]] %in% tsnams[tind]], bresp = NA)
      }
    }
  }
  TGmat <- tmat %*% Gmat %*% t(tmat)
  tsnams <- gsub(levs[2], "resp", tsnams)
  tsnams <- gsub(levs[1], "eff", tsnams)
  dimnames(TGmat) <- list(tsnams, tsnams)
  blups <- do.call("rbind.data.frame", blist)
  names(blups)[1:3] <- c(levs, "resp")
  glev <- unique(as.character(pvals[[vnam]]))
  blups <- cbind.data.frame(Site = rep(usnams, each = length(glev)), Variety = rep(glev, length(usnams)), blups)
  list(blups = blups, TGmat = TGmat, Gmat = Gmat, beta = beta, sigr = sigr, tmat = tmat)
}

## BLUEs regression

#' Fixed regression for doing stuff
#'
#' @param model
#' @param term
#' @param levs
#' @param robust
#'
#' @return
#' @export
#'
#' @examples
fixedRegress <- function(model, term = "Treatment:Genotype", levs = c("9 cm", "Control"), robust = TRUE) {
  pred <- predict(model, classify = term, vcov = TRUE)
  terms <- unlist(strsplit(term, ":"))
  tnam <- terms[1]
  gnam <- terms[2]
  wt1 <- pred$pvals[[tnam]] %in% levs[1]
  wt2 <- pred$pvals[[tnam]] %in% levs[2]
  ptreat <- pred$pvals$predicted.value[wt1]
  pcont <- pred$pvals$predicted.value[wt2]
  vc <- as.matrix(pred$vcov)
  s22 <- vc[wt2, wt2]
  if (robust) {
    s11 <- vc[wt1, wt1]
    s12 <- vc[wt1, wt2]
    resp <- ptreat - s12 %*% solve(s22) %*% pcont
    resp.var <- s11 - s12 %*% solve(s22) %*% t(s12)
    rdf <- model$nedf
  } else {
    lmr <- lm(ptreat ~ pcont)
    resp <- lmr$residuals
    xm <- model.matrix(~pcont)
    vmat <- (diag(length(ptreat)) - xm %*% solve(t(xm) %*% xm) %*% t(xm))
    resp.var <- ((vmat) %*% t(vmat)) * (summary(lmr)$sigma^2)
    rdf <- lmr$df.residual
  }
  std.error <- sqrt(diag(resp.var))
  sed <- sqrt(apply(combn(diag(resp.var), 2), 2, sum) - 2 * resp.var[lower.tri(resp.var)])
  respd <- cbind.data.frame(Genotype = levels(pred$pvals[[gnam]]), reponse.index = resp, std.error = std.error)
  respd$HSD <- (mean(sed) / sqrt(2)) * qtukey(0.95, length(ptreat), df = rdf)
  respd
}
