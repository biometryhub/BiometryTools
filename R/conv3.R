#' Conversion function for Efficiency and Responsiveness BLUPs in Treatment x Site x Variety experiments
#'
#' @param model Final full Treatment x Site x Variety model
#' @param Env Treatment x Site x Variety term
#' @param levs Named treatment levels used in transformation. i.e c("Treat1", "Treat2") would regress Treat2 on Treat1
#' @param sep separator used for Treat x Site names. Defaults to `-`
#' @param ...
#'
#' @return Returns a list with BLUPs, and some other stuff JULES COMPLETE
#' @export
#'
#' @examples JULES COMPLETE
#'
conv <- function(model, Env = "TSite:Variety", levs = NULL, sep = "-", ...){
  if(is.null(levs))
    stop("Treatment levels cannnot be NULL.")
  evnam <- unlist(strsplit(Env, ":"))
  enam <- evnam[1]; vnam <- evnam[2]
  rterm <- attr(terms.formula(model$call$random), "term.labels")
  rterm <- rterm[grep(paste(evnam, collapse = "|"), rterm)]
  if(substring(rterm, 1, 2) == "fa"){
    sumfa <- fa.asreml(model, trunc.char = NULL)
    pvals <- sumfa$blups[[rterm]]$blups[,1:3]
    Gmat <- sumfa$gammas[[rterm]]$Gmat
  }
  else {
    pred <- predict(model, classify = Env, only = Env, ...)
    Gmat <- summary(model, vparameters = TRUE)$vparameters[[Env]]
    pvals <- pred$pvals
    names(pvals)[3] <- "blup"
  }
  tsnams <- dimnames(Gmat)[[2]]
  if(!length(grep(sep, tsnams)))
    stop("Separator not found in Treatment by Site factor.")
  st <- strsplit(tsnams, split = sep)
  tnam <- sapply(st, function(el) el[1])
  snam <- sapply(st, function(el) el[2])
  if(!all(levs %in% c(snam, tnam)))
    stop("Treatment levels do not exist in ", enam)
  if(all(levs %in% snam)){
    tnam <- snam
    snam <- sapply(st, function(el) el[1])
  }
  usnams <- unique(snam)
  tmat <- diag(nrow(Gmat))
  beta <- sigr <- c()
  blist <- list()
  for(i in 1:length(usnams)){
    inds <- (1:length(snam))[snam %in% usnams[i]]
    names(inds) <- tnam[inds]
    whl <- (1:2)[levs %in% names(inds)]
    if(length(whl) == 2){
      tind <- inds[levs]
      mat <- Gmat[tind, tind]
      beta[i] <- mat[1,2]/mat[1,1]
      rho <- mat[1,2]/sqrt(mat[1,1]*mat[2,2])
      sigr[i] <- (mat[2,2]*(1 - rho^2))
      tmat[tind[2], tind[1]] <- - beta[i]
      blow <- pvals$blup[pvals[[enam]] %in% tsnams[tind[1]]]
      bhigh <- pvals$blup[pvals[[enam]] %in% tsnams[tind[2]]]
      bresp <- bhigh - beta[i]*blow
      blist[[i]] <- cbind.data.frame(blow, bhigh, bresp)
    } else {
      slevs <- levs[whl]
      tind <- inds[slevs]
      if(whl == 1)
        blist[[i]] <- cbind.data.frame(blow = pvals$blup[pvals[[enam]] %in% tsnams[tind]], bhigh = NA, bresp = NA)
      else blist[[i]] <- cbind.data.frame(blow = NA, bhigh = pvals$blup[pvals[[enam]] %in% tsnams[tind]], bresp = NA)
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
