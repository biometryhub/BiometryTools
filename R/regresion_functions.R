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
#' \dontrun{
#' JULES COMPLETE
#' }
randomRegress <- function(model, Env = "TSite:Variety", levs = NULL, sep = "-", pev = TRUE, ...){
    if(is.null(levs))
        stop("Treatment levels cannnot be NULL.")
    evnam <- unlist(strsplit(Env, ":"))
    enam <- evnam[1]; vnam <- evnam[2]
    penv <- gsub(":",".*", Env)
    rterm <- attr(terms.formula(model$call$random), "term.labels")
    rterm <- rterm[grep(penv, rterm)]
    print(rterm)
    if(substring(rterm, 1, 2) == "fa"){
        sumfa <- ASExtras4::fa.asreml(model, trunc.char = NULL)
        pvals <- sumfa$blups[[rterm]]$blups[,1:3]
        Gmat <- sumfa$gammas[[rterm]]$Gmat
    }
    else {
        pred <- predict(model, classify = Env, only = Env, vcov = TRUE, ...)
        Gmat <- summary(model, vparameters = TRUE)$vparameters[[Env]]
        pvals <- pred$pvals
        names(pvals)[3] <- "blup"
    }
    tsnams <- dimnames(Gmat)[[2]]
    if(length(grep(sep, tsnams))){
        st <- strsplit(tsnams, split = sep)
        tnam <- sapply(st, function(el) el[1])
        snam <- sapply(st, function(el) el[2])
        if(!all(levs %in% c(snam, tnam)))
            stop("Treatment levels do not exist in ", enam)
        if(all(levs %in% snam)){
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
            imat <- diag(2)
            imat[2,1] <- - beta[i]
            blow <- pvals$blup[pvals[[enam]] %in% tsnams[tind[1]]]
            bhigh <- pvals$blup[pvals[[enam]] %in% tsnams[tind[2]]]
            bresp <- bhigh - beta[i]*blow
            blist[[i]] <- cbind.data.frame(blow, bhigh, bresp)
            lowi <- (1:nrow(pvals))[pvals[[enam]] %in% tsnams[tind[1]]]
            highi <- (1:nrow(pvals))[pvals[[enam]] %in% tsnams[tind[2]]]
            pevm <- kronecker(imat, diag(length(blow))) %*% as.matrix(pred$vcov[c(lowi,highi),c(lowi,highi)]) %*% kronecker(t(imat), diag(length(blow)))
            if(!pev)
                pevm <- kronecker(diag(c(mat[1,1],sigr[i])), diag(length(blow))) - pevm
            bvar <- pevm[(length(blow) + 1):ncol(pevm),(length(blow) + 1):ncol(pevm)]
            sed <- apply(combn(diag(bvar), 2), 2, sum) - 2*bvar[lower.tri(bvar)]
            sed[sed < 0] <- NA
            blist[[i]]$HSD <- (mean(sqrt(sed), na.rm = TRUE)/sqrt(2))*qtukey(0.95, length(blow), df = length(blow) - 2)
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

## BLUEs regression

fixedRegress <- function(model, term = "Treatment:Genotype", by = NULL, levs = NULL, simple = TRUE){
    pterm <- term
    if(is.null(levs))
        stop("Treatment levels cannnot be NULL.")
    term <- unlist(strsplit(term, ":"))
    if(length(term) < 2)
        stop("Argument \"term\" needs at least two variables.")
    pred <- predict(model, classify = pterm, vcov = TRUE)
    whna <- !is.na(pred$pvals$predicted.value)
    pv <- pred$pvals[whna,]
    vc <- as.matrix(pred$vcov)[whna,whna]
    wht <- unlist(sapply(pv[,term], function(el, levs) all(levs %in% levels(el)), levs))
    if(!any(wht))
        stop("Some levels specified in \"levs\" do not exist in term variables.")
    tnam <- term[wht]
    if(!is.null(by)){
        bys <- unlist(strsplit(by, ":"))
        if(!all(bys %in% term))
            stop("Some variables in argument \"by\" are not in \"term\".")
        if(tnam %in% bys)
            stop("Levels specified in \"levs\" cannot be in \"by\" variable.")
        rterm <- term[!(term %in% c(tnam, bys))]
        if(!length(rterm))
            stop("There are no variables to form regression between specified levels.")
        if(length(rterm) > 1)
            pv[["regress"]] <- apply(pv[,rterm], 1, function(el) paste(el, collapse = ":"))
        else pv[["regress"]] <- pv[[rterm]]
        if(length(bys) > 1)
            pv[[by]] <- apply(pv[,bys], 1, function(el) paste(el, collapse = ":"))
        uby <- as.character(pv[[by]])
        um <- unique(uby)
    } else {
        uby <- rep(tnam, nrow(pv))
        um <- unique(uby)
        rterm <- term[!(term %in% tnam)]
        pv[["regress"]] <- pv[[rterm]]
    }
    resp.list <- list()
    for(i in 1:length(um)){
        inds <- uby %in% um[i]
        pvt <- pv[inds,]
        pv1 <- pvt[wh1 <- pvt[[tnam]] %in% levs[1],]
        pv2 <- pvt[wh2 <- pvt[[tnam]] %in% levs[2],]
        whr <- intersect(pv1[["regress"]], pv2[["regress"]])
        if(length(whr) > 5){
            wt1 <- pv1[["regress"]] %in% whr
            wt2 <- pv2[["regress"]] %in% whr
            pcont <- pv1$predicted.value[wt1]
            ptreat <- pv2$predicted.value[wt2]
            regt <- pv2[["regress"]][wt2]
            if(!simple){
                vct <- vc[inds, inds]
                s22 <- vct[wh2,wh2]
                s22 <- s22[wt2,wt2]
                s11 <- vct[wh1,wh1]
                s11 <- s11[wt1,wt1]
                s21 <- vct[wh2,wh1]
                s21 <- s21[wt2,wt1]
                resp <- ptreat - s21 %*% solve(s11) %*% pcont
                resp.var <- s22 - s21 %*% solve(s11) %*% t(s21)
                rdf <- model$nedf
            } else {
                lmr <- lm(ptreat ~ pcont)
                resp <- lmr$residuals
                xm <- model.matrix( ~ pcont)
                vmat <- (diag(length(ptreat)) - xm %*% solve(t(xm)%*%xm) %*% t(xm))
                resp.var <- ((vmat) %*% t(vmat))*(summary(lmr)$sigma^2)
                rdf <- lmr$df.residual
            }
            std.error <- sqrt(diag(resp.var))
            sed <- sqrt(apply(combn(diag(resp.var), 2), 2, sum) - 2*resp.var[lower.tri(resp.var)])
            respd <- cbind.data.frame(Split = um[i], Regress.Var = regt)
            respd[[levs[1]]] <- pcont
            respd[[levs[2]]] <- ptreat
            respd$reponse.index <- resp
            respd$std.error <- std.error
            respd$HSD <- (mean(sed)/sqrt(2))*qtukey(0.95, length(ptreat), df = rdf)
            respd$sed <- mean(sed)
            resp.list[[i]] <- respd
        } else warning("Some treatment combinations in ", um[i]," have less than 5 matching observations and have been omitted.\n")
    }
    resp.list <- resp.list[!sapply(resp.list, is.null)]
    do.call("rbind.data.frame", resp.list)
}

compare <- function(model, term = "Treatment:Genotype", by = NULL, omit.string = NULL, type = "HSD", pev = TRUE, fw.method = "none", ...){
    pred <- predict(model, classify = term, vcov = TRUE, ...)
    terms <- unlist(strsplit(term, ":"))
    pv <- pred$pvals
    inds <- !is.na(pv$predicted.value)
    if(!pev & all(terms %in% all.vars(model$call$random))){
        varm <- summary(model, vparameters = TRUE)$vparameters[[term]]
        if(length(terms) > 1)
            len <- table(pv[,1])[1]
        else len <- nrow(pv)
        vara <- kronecker(varm, diag(len)) - pred$vcov
        vara[inds, inds]
    } else vara <- pred$vcov[inds, inds]
    pv <- pv[inds,]
    section <- FALSE
    if(!is.null(by)){
        bys <- unlist(strsplit(by, ":"))
        if(all(terms %in% bys))
            stop("Argument \"by\" indicates no multiple comparisons are being made.")
        if(!all(bys %in% terms))
            stop("Some terms in argument \"by\" are not in \"term\".")
        if(length(bys) > 1)
            pv[[by]] <- apply(pv[,bys], 1, function(el) paste(el, collapse = ":"))
    } else{
        by <- term
        pv[[by]] <- by
    }
    if(!is.null(omit.string)){
        oind <- grep(omit.string, as.character(pv[[gnam]]))
        if(length(oind)){
            pv <- pv[-oind,]
            sed <- sed[-oind,-oind]
        }
    }
    sst <- as.character(pv[[by]])
    um <- unique(sst)
    if(type %in% c("HSD","LSD")){
        tsd <- avsed <- c()
        for(k in 1:length(um)){
            sinds <- sst %in% um[k]
            svar <- vara[sinds, sinds]
            avsed[k] <- sqrt(mean(apply(combn(diag(svar), 2), 2, sum) - 2*svar[lower.tri(svar)]))
            if(type == "HSD")
                tsd[k] <- (avsed[k]/sqrt(2))*qtukey(0.95, length(sinds), model$nedf)
            else tsd[k] <- avsed[k]*qt(0.025, df = model$nedf, lower.tail = FALSE)
        }
        pv <- cbind.data.frame(pv[,1:(length(terms) + 2)])
        pv[[type]] <- rep(tsd, times = table(sst))
        pv[["sed"]] <- rep(avsed, times = table(sst))
    }
    else if(type %in% "PVAL"){
        pvs <- split(pv, pv[[by]])
        yvar <- deparse(model$call$fixed[[2]])
        xvar <- labels(terms(as.formula(model$call$fixed)))
        fix.form <- as.formula(paste(yvar, " ~ ", xvar[length(xvar)], " - 1", sep = ""))
        model <- update(model, fixed. = fix.form, Cfixed = TRUE)
        coefs <- model$coefficients$fixed
        cinds <- grep(paste(terms, collapse = ".*"), rownames(coefs))
        coefs <- coefs[cinds,,drop = FALSE]
        for(k in 1:length(um)){
            umt <- paste(strsplit(um[k], ":")[[1]], collapse = ".*")
            sind <- cinds[grep(umt, rownames(coefs))]
            scf <- coefs[grep(umt, rownames(coefs)),]
            sna <- scf == 0
            aind <- sind[!sna]
            pvt <- pvs[[k]]
            cb <- t(combn(nrow(pvt), 2))
            mat <- matrix(0, nrow = nrow(cb), ncol = nrow(pvt))
            mat[cbind(1:nrow(mat), cb[,1])] <- 1
            mat[cbind(1:nrow(mat), cb[,2])] <- -1
            cc <- list(coef = aind, type = "con", comp = mat)
            wt <- waldTest(model, list(cc))$Contrasts
            pval <- wt$"P-Value"
            add <- matrix(0, nrow = nrow(pvt), ncol = nrow(pvt))
            add[lower.tri(add)] <- stats::p.adjust(pval, method = fw.method)
            add <- add + t(add)
            #            add <- add[ord, ord]
            dimnames(add)[[2]] <- apply(pvt[,terms], 1, function(el) paste(el, collapse = ":"))
            paste(as.character(pvt[[terms[[1]]]]), as.character(pvt[[terms[2]]]), sep = ":")
            pvs[[k]] <- cbind.data.frame(pvs[[k]][,1:(length(terms) + 2)], add)
        }
        pv <- pvs
    } else stop("Please use one of the allowable types, \"HSD\",\"LSD\",\"PVAL\"")
    pv
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
#' \dontrun{
#' JULES COMPLETE
#' }
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
