
## Conversion function for Efficiency and Responsiveness BLUPs in
## Treatment x Site x Variety experiments

## The function assumes you have a Treatment x Site factor that is a composite of
## treatments and sites. The function requires no specific ordering of the factor levels.

## Arguments:
##  model: Final full Treatment x Site x Variety model
##  Env: Treatment x Site x Variety term
##  levs: Named treatment levels used in transformation. i.e c("Treat1", "Treat2")
##  would regress Treat2 on Treat1
##  sep: separator used for Treat x Site names (if multi-x model), if not present assumes single section


randomRegress <- function(model, Env = "TSite:Variety", levs = NULL, sep = "-", ...){
  if(is.null(levs))
      stop("Treatment levels cannnot be NULL.")
  evnam <- unlist(strsplit(Env, ":"))
  enam <- evnam[1]; vnam <- evnam[2]
  rterm <- attr(terms.formula(model$call$random), "term.labels")
  rterm <- rterm[grep(paste(evnam, collapse = "|"), rterm)]
  print(rterm)
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
fixedRegress <- function(model, term = "Treatment:Genotype", levs = c("9 cm","Control"), robust = TRUE){
    pred <- predict(model, classify = term, vcov = TRUE)
    terms <- unlist(strsplit(term, ":"))
    tnam <- terms[1]; gnam <- terms[2]
    wt1 <- pred$pvals[[tnam]] %in% levs[1]
    wt2 <- pred$pvals[[tnam]] %in% levs[2]
    ptreat <- pred$pvals$predicted.value[wt1]
    pcont <- pred$pvals$predicted.value[wt2]
    vc <- as.matrix(pred$vcov)
    s22 <- vc[wt2,wt2]
    if(robust){
        s11 <- vc[wt1,wt1]
        s12 <- vc[wt1,wt2]
        resp <- ptreat - s12 %*% solve(s22) %*% pcont
        resp.var <- s11 - s12 %*% solve(s22) %*% t(s12)
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
    respd <- cbind.data.frame(Genotype = levels(pred$pvals[[gnam]]), reponse.index = resp, std.error = std.error)
    respd$HSD <- (mean(sed)/sqrt(2))*qtukey(0.95, length(ptreat), df = rdf)
    respd
}

hsd <- function(model, term = "Treatment:Genotype", by = "Treatment", omit.string = NULL, ...){
    pred <- predict(model, classify = term, sed = TRUE, ...)
    pv <- pred$pvals
    inds <- !is.na(pv$predicted.value)
    pv <- pv[inds,]
    sed <- pred$sed[inds,inds]
    section <- FALSE
    if(!is.null(by)){
        if(length(grep(":", term)))
            terms <- unlist(strsplit(term, ":"))
        if(length(grep(":", by))){
            bys <- unlist(strsplit(by, ":"))
            pv[[by]] <- apply(pv[,bys], 1, function(el) paste(el, collapse = ":"))
            section <- TRUE
        }
        if(all(terms %in% bys))
            stop("Argument \"by\" indicates no multiple comparisons are being made.")
        if(!all(bys %in% terms))
            stop("Some terms in argument \"by\" are not in \"term\".")
    }
    if(!is.null(omit.string)){
        oind <- grep(omit.string, as.character(pv[[gnam]]))
        if(length(oind)){
            pv <- pv[-oind,]
            sed <- sed[-oind,-oind]
        }
    }
    if(section){
        sst <- as.character(pv[[by]])
        um <- unique(sst)
        hsd <- c()
        for(k in 1:length(um)){
            inds <- sst %in% um[k]
            ssed <- sed[inds, inds]
            avsed <- mean(ssed[upper.tri(ssed, diag = FALSE)])
            hsd[k] <- (avsed/sqrt(2))*qtukey(0.95, length(inds), model$nedf)
        }
        pv$HSD <- rep(hsd, times = table(sst))
    } else pv$HSD <- (pred$avsed[2]/sqrt(2))*qtukey(0.95, nrow(pv), model$nedf)
    pv
}

## associate BLUEs with BLUPs form predict calls of the same model

associate <- function(model, ran.term = "Treatment:Cultivar", fix.term = "Treatment:Type", ...){
    rnams <- all.vars(as.formula(paste("~ ", ran.term, sep = "")))
    fnams <- all.vars(as.formula(paste("~ ", fix.term, sep = "")))
    if(length(ran.term) > 1){
        labs <- attr(terms(as.formula(model$call$random)), "term.labels")
        iterm <- labs[grep(paste(rnams[1], "*.*", rnams[2], sep = ""), labs)]
        uv <- sapply(strsplit(iterm, "\\("), "[", 1)
        if(uv == "fa")
            predr <- predict(model, classify = ran.term, only = iterm, ...)
        else if(uv %in% c("diag","corh","corgh","us"))
            predr <- predict(model, classify = ran.term, only = ran.term, ...)
    } else predr <- predict(model, classify = ran.term, only = ran.term, ...)
    pr <- predr$pvals
    names(pr) <- gsub("predicted.value", "blups", names(pr))
    names(pr) <- gsub("std.error", "blups.std.error", names(pr))
    predf <- predict(model, classify = fix.term, ...)
    pf <- predf$pvals
    pf <- pf[!is.na(pf$predicted.value),]
    names(pf) <- gsub("predicted.value", "blues", names(pf))
    names(pf) <- gsub("std.error", "blues.std.error", names(pf))
    dat <- eval(model$call$data)
    datr <- dat[,unique(c(rnams,fnams))]
    datr <- datr[!duplicated(datr[,rnams]),]
    pri <- merge(pr[-ncol(pr)], datr, by = rnams, all.x = TRUE)
    pall <- merge(pri, pf[-ncol(pf)], by = fnams, all.x = TRUE)
    pall
}

## heritability for multi/single environment trials

herit.asreml <- function(model, term = "SYear:Genotype", ...){
    dat <- eval(model$call$data)
    if(length(grep(":", term))){
        terms <- all.vars(as.formula(paste("~ ", term, sep = "")))
        labs <- attr(terms(as.formula(model$call$random)), "term.labels")
        iterm <- labs[grep(paste(terms[1], "*.*", terms[2], sep = ""), labs)]
        uv <- sapply(strsplit(iterm, "\\("), "[", 1)
        if(uv == "fa"){
            pred <- predict(model, classify = term, only = iterm, sed = TRUE, ...)
            sumfa <- fa.asreml(model, trunc.char = NULL)
            gam <- diag(sumfa$gammas[[grep(paste(terms[1], "*.*", terms[2], sep = ""), names(sumfa$gammas))]]$Gmat)
        } else if(uv %in% c("diag","corh","corgh","us")){
            pred <- predict(model, classify = term, only = term, sed = TRUE, ...)
            if(uv %in% c("diag"))
                gam <- summary(model, vparameters = TRUE)$vparameters[[term]]
            else
                gam <- diag(summary(model, vparameters = TRUE)$vparameters[[term]])
        }
        else stop("The function does not understand this asreml function.")
        site <- pred$pvals[[terms[1]]]
        levs <- levels(site)
        avsed <- c()
        for(i in 1:length(levs)){
            inds <- (1:length(site))[as.character(site) %in% levs[i]]
            sedm <- pred$sed[inds, inds]
            sedm <- sedm[upper.tri(sedm)]
            avsed[i] <- mean(sedm)
        }
    } else {
        pred <- predict(model, classify = term, only = term, sed = TRUE, ...)
        con <- model$vparameters.con[grep("units\\!R", names(model$vparameters.con))]
        if(length(con) == 0 || (con != 4))
            gam <- model$vparameters[grep(term, names(model$vparameters))]*model$sigma2
        else gam <- model$vparameters[grep(term, names(model$vparameters))]
        avsed <- pred$avsed[2]
        levs <- term
    }
    h2 <- 1 - (avsed^2)/(2*gam)
    names(h2) <- levs
    h2
}

## outlier addition function

outlier.down <- function(data, model, cutoff = 3){
    ss <- names(model)
    inds <- 1:nrow(data)
    for(i in 1:length(ss)){
        str <- abs(model[[ss[i]]]$aom$R[,2])
        r <- str > cutoff
        wh <- inds[r]
        wh <- wh[!is.na(wh)]
        if(length(wh)){
            ps <- paste(ss[i], "o", sep = ".")
            num <- 0
            if(length(wt <- grep(ps, names(data)))){
                num <- sapply(strsplit(names(data)[wt], "\\."), function(el) el[length(el)])
                num <- as.numeric(num[length(num)])
            }
            print(wh)
            for(j in 1:length(wh)){
                nam <- paste(ps, j + num, sep = ".")
                v <- rep(0, nrow(data))
                v[wh[j]] <- 1
                data[[nam]] <- v
            }
        }
    }
    data
}

## outlier removal function

outlier.rem <- function(data, model, cutoff = 3){
    ss <- names(model)
    inds <- 1:nrow(data)
    out <- rep(FALSE, length(model))
    names(out) <- ss
    for(i in 1:length(ss)){
        trait <- data[[ss[i]]]
        str <- abs(model[[ss[i]]]$aom$R[,2])
        r <- str > cutoff
        wh <- inds[r]
        wh <- wh[!is.na(wh)]
        if(length(wh)){
            print(wh)
            data[[ss[i]]][wh] <- NA
            out[i] <- TRUE
        }
    }
    list(data = data, out = out)
}

pad.data <- function(data, pattern = "Row:Column", split = "Block", keep = 4, fill = NULL){
    pat <- unlist(strsplit(pattern, ":"))
    if(!(split %in% names(data)))
        stop("split argument not in data")
    if(!all(pat %in% names(data)))
        stop("One or more of the variables in pattern argument not in data")
    spd <- split(data, data[[split]])
    spd <- lapply(spd, function(el, pat){
             temp <- el
             temp <- cbind.data.frame(lapply(temp, function(el){ if(is.factor(el)) factor(el) else el}))
             temp$add <- "old"
             tabs <- table(temp[[pat[1]]], temp[[pat[2]]])
             wh <- which(tabs == 0, arr.ind = TRUE)
             if(dim(wh)[1] > 0){
                 tp <- temp[1:nrow(wh),]
                 tp <- cbind.data.frame(lapply(tp, function(el) rep(NA, length(el))))
                 tp[,keep] <- temp[1:nrow(wh),keep]
                 tp[[pat[1]]] <- factor(rownames(tabs)[wh[,1]])
                 tp[[pat[2]]] <- factor(colnames(tabs)[wh[,2]])
                 if(!is.null(fill))
                     tp[,fill] <- NA
                 tp$add <- "new"
                 temp <- rbind.data.frame(temp, tp)
             }
             temp
         }, pat)
  ad <- do.call("rbind.data.frame", spd)
  ad[[pat[1]]] <- factor(ad[[pat[1]]], levels = as.character(sort(as.numeric(levels(ad[[pat[1]]])))))
  ad[[pat[2]]] <- factor(ad[[pat[2]]], levels = as.character(sort(as.numeric(levels(ad[[pat[2]]])))))
  ad[order(ad[[pat[1]]],ad[[pat[2]]]),]
}

extract <- function(data, pattern = "Row:Column", match = "DH", split = "Block", pad = TRUE, keep = 4, fill = NULL){
    pat <- unlist(strsplit(pattern, ":"))
    if(!(split %in% names(data)))
        stop("split argument not in data")
    if(!all(pat %in% names(data)))
        stop("One or more of the variables in pattern argument not in data")
    spd <- split(data, data[[split]])
    spd <- lapply(spd, function(el, match, pat, pad){
             temp <- el[as.character(el$Type) %in% match,]
             print(dim(temp))
             rr <- range(as.numeric(as.character(temp[,pat[1]])))
             rc <- range(as.numeric(as.character(temp[,pat[2]])))
             print(rr)
             print(rc)
             elr <- (1:nrow(el))[el[[pat[1]]] %in% as.character(rr[1]:rr[2])]
             elc <- (1:nrow(el))[el[[pat[2]]] %in% as.character(rc[1]:rc[2])]
             ela <- intersect(elr, elc)
             temp <- el[ela[order(ela)],]
             temp <- cbind.data.frame(lapply(temp, function(el){ if(is.factor(el)) factor(el) else el}))
             if(pad){
                 temp$add <- "old"
                 tabs <- table(temp[[pat[1]]], temp[[pat[2]]])
                 wh <- which(tabs == 0, arr.ind = TRUE)
                 if(length(wh)){
                     whn <- pmatch(pat, names(temp))
                     tp <- temp[1:nrow(wh),]
                     tp <- cbind.data.frame(lapply(tp, function(el) rep(NA, length(el))))
                     tp[,keep] <- temp[1:nrow(wh),keep]
                     tp[[pat[1]]] <- factor(rownames(tabs)[wh[,1]])
                     tp[[pat[2]]] <- factor(colnames(tabs)[wh[,2]])
                     if(!is.null(fill))
                         tp[,fill] <- "Blank"
                     tp$add <- "new"
                     temp <- rbind.data.frame(temp, tp)
                 }
             }
             temp
         }, match, pat, pad)
  ad <- do.call("rbind.data.frame", spd)
  ad[[pat[1]]] <- factor(ad[[pat[1]]], levels = as.character(sort(as.numeric(levels(ad[[pat[1]]])))))
  ad[[pat[2]]] <- factor(ad[[pat[2]]], levels = as.character(sort(as.numeric(levels(ad[[pat[2]]])))))
  ad[order(ad[[pat[1]]],ad[[pat[2]]]),]
}

## BLUEs LSD/p-value comparison function

compare <- function(model, term = "Line", type = "PVAL", average.LSD = FALSE){
    pred <- predict(model, classify = term, sed = TRUE)
    nterm <- unlist(strsplit(term, ":"))
    pv <- pred$pvals
    sed <- pred$sed
    if(any(wh <- is.na(pv$predicted.value))){
        pv <- pv[!wh,]
        sed <- sed[!wh, !wh]
    }
    if(length(nterm) > 1)
        labs <- paste(pv[[nterm[1]]], pv[[nterm[2]]], sep = ":")
    else labs <- pv[[term]]
    if(type %in% "LSD"){
        add <- sed*qt(0.025, df = model$nedf, lower.tail = FALSE)
        dimnames(add)[[2]] <- paste("LSD", labs, sep = ":")
        if(average.LSD)
            add <- cbind.data.frame(ave.LSD = rep(mean(add[lower.tri(add)]), dim(pv)[1]))
    } else if(type %in% "PVAL"){
        ord <- 1:nrow(pv)
        if(length(nterm) > 1){
            fix.form <- paste(deparse(model$call$fixed), nterm[1], nterm[2], "1", sep = " - ")
            model <- update(model, fixed. = fix.form, Cfixed = TRUE)
            coefs <- model$coefficients$fixed
            wh <- coefs[,1] == 0
            cnams <- rownames(coefs)[!wh]
            sp <- strsplit(cnams, ":")
            left <- sapply(sp, "[", 1)[1]
            right <- sapply(sp, "[", 2)[1]
            leftn <- 1; rightn <- 2
            if(any(grep(nterm[1], right)))
                leftn <- 2; rightn <- 1
            left <- gsub(paste(nterm[1], "_", sep = ""), "", sapply(sp, "[", leftn))
            right <- gsub(paste(nterm[2], "_", sep = ""), "", sapply(sp, "[", rightn))
            ord <- pmatch(labs, paste(left, right, sep = ":"))
        } else {
            fix.form <- paste(deparse(model$call$fixed), "1", sep = " - ")
            model <- update(model, fixed. = fix.form, Cfixed = TRUE)
        }
        cb <- t(combn(nrow(pv), 2))
        mat <- matrix(0, nrow = nrow(cb), ncol = nrow(pv))
        mat[cbind(1:nrow(mat), cb[,1])] <- 1
        mat[cbind(1:nrow(mat), cb[,2])] <- -1
        lenf <- length(model$coefficients$fixed)
        cc <- list(coef = (1:lenf)[!wh], type = "con", comp = mat)
        wt <- wald.test(model, list(cc))$Contrasts
        pval <- wt$"P-Value"
        add <- matrix(0, nrow = nrow(pv), ncol = nrow(pv))
        add[lower.tri(add)] <- pval
        add <- add + t(add)
        add <- add[ord, ord]
        dimnames(add)[[2]] <- paste("PVAL", labs, sep = ":")
    } else stop("This type is not defined.")
    cbind.data.frame(pv[,1:(length(nterm) + 1)], add)
}

## fine map a wgaim object

fineMap <- function(model, intervalObj, mark = NULL, flanking = 50, exclusion.window = 10000, ...){
    resp <- deparse(model$call$fixed[[2]])
    phenoData <- eval(parse(text = paste(resp, ".data", sep = "")))
    if (missing(intervalObj))
        stop("intervalObj is a required argument")
    if (!inherits(intervalObj, "cross"))
        stop("intervalObj is not of class \"cross\"")
    if(is.null(mark))
        stop("mark argument must be non-NULL.")
    if (model$QTL$type == "interval")
        gdat <- lapply(intervalObj$geno, function(el) el$interval.data)
    else gdat <- lapply(intervalObj$geno, function(el) el$imputed.data)
    genoData <- do.call("cbind", gdat)
    gterm <- model$QTL$diag$genetic.term
    state <- model$QTL$diag$state
    method <- model$QTL$method
    dimnames(genoData) <- list(as.character(intervalObj$pheno[[gterm]]), names(state))
    genoData <- genoData[rownames(genoData) %in% as.character(phenoData[[gterm]]),]
    fm <- find.markerpos(intervalObj, mark)
    chrs <- sapply(strsplit(names(state), "\\."), "[", 2)
    chr.ind <- chrs %in% fm$chr
    state.chr <- state[chrs %in% fm$chr]
    mapc <- pull.map(intervalObj, fm$chr)[[1]]
    qind <- (1:length(mapc))[names(mapc) %in% mark]
    mark.qtl <- gsub("Chr\\.", "X.", names(state.chr)[qind])
    ql <- ifelse(qind - flanking <= 0, 1, qind - flanking)
    qr <- ifelse(qind + flanking > length(mapc), length(mapc), qind + flanking)
    state.chr[ql:qr] <- 1
    genoChr <- genoData[,names(state.chr)[ql:qr]]
    colnames(genoChr) <- gsub("Chr\\.", "X.", colnames(genoChr))
    tmp <- cbind.data.frame(rownames(genoData), genoChr)
    colnames(tmp)[1] <- gterm
    phenoData <- phenoData[,!(names(phenoData) %in% mark.qtl)]
    phenoData$ord <- 1:nrow(phenoData)
    phenoData <- merge(phenoData, tmp, by = gterm, all.x = TRUE)
    phenoData <- phenoData[order(phenoData$ord),]
    k <- 1
    pvalue <- lod <- c()
    for(i in ql:qr){
        wind <- abs(mapc[i] - mapc) < exclusion.window
        state.chr[wind] <- 0
        state[chr.ind] <- state.chr
        print(length(state))
        genoSub <- genoData[,as.logical(state)]
        print(dim(genoData))
        cov.env <- wgaim:::constructCM(genoSub)
        covObj <- cov.env$relm
        assign("covObj", covObj, envir = parent.frame())
        mark.i <- colnames(genoChr)[k]
        print(mark.i)
        if(method == "random"){
            temp.form <- update.formula(model$call$random, as.formula(paste("~ . - ", mark.qtl, sep = "")))
            temp.form <- update.formula(temp.form, as.formula(paste("~ . + ", mark.i, sep = "")))
            tempmodel <- wgaim:::vModify(model, gterm)
            tempmodel <- update.asreml(tempmodel, random. = temp.form, data = phenoData, ...)
        }
        else {
            fix.form <- formula(paste(". ~ . +", mark.i, "-", mark.qtl, sep = ""))
            tempmodel <- update.asreml(model, fixed. = fix.form, data = phenoData, ...)
        }
        cf <- tempmodel$coefficients[[method]]
        whr <- grep(mark.i, rownames(cf))
        print(whr)
        mcf <- tempmodel$coefficients[[method]][whr, 1]
        vcf <- tempmodel$vcoeff[[method]][whr]
        print(c(mcf, vcf))
#        zrat <- mcf/sqrt(vcf * tempmodel$sigma2)
        zrat <- mcf/sqrt(vcf)
        pvalue[k] <- round((1 - pchisq(zrat^2, df = 1))/2, 4)
        lod[k] <- round(0.5 * log(exp(zrat^2), base = 10), 4)
        print(c(pvalue[k], lod[k]))
        k <- k + 1
    }
    cbind.data.frame(mark = names(mapc)[ql:qr], dist = mapc[ql:qr], pvalue = pvalue, LOD = lod)
}


## manhattan plot using ggplot

manhattan <- function(mlist, cross, chr.in = NULL, annotate = TRUE, ...){
    nams <- names(mlist)
    outs <- lapply(mlist, function(el){
        temp <- el$QTL$diag$oint[[1]]
        names(temp) <- gsub("Chr\\.","", names(temp))
        temp
    })
    if(!is.null(chr.in)){
        cross <- subset(cross, chr = chr.in)
        for(i in 1:length(outs)){
            onam <- sapply(strsplit(names(outs[[i]]), "\\."), "[", 1)
            outs[[i]] <- outs[[i]][onam %in% chr.in]
        }
    }
    dat <- cbind.data.frame(value = unlist(outs))
    dat$nout <- sapply(outs, names)
    len <- sapply(outs, length)[1]
    dat$Name <- rep(nams, each = len)
    chr <- rep(names(cross$geno), times = nmar(cross))
    dat$chr <- factor(rep(chr, length(nams)))
    dist <- lapply(cross$geno, function(el) {
         tel <- c(100000, diff(el$map))
         names(tel)[1] <- names(el$map)[1]
         tel
    })
    dist <- cumsum(unlist(dist))
    sp <- unlist(lapply(split(dist, chr), function(el) min(el) + diff(range(el))/2))
    spc <- unlist(lapply(split(dist, chr), function(el) max(el) + 500000))
    dat$dist <- rep(dist, length(nams))
    dat$chr.g <- dat$chr
    levels(dat$chr.g) <- c(rep(c("g1","g2"),  10), "g1")
        cols <- brewer.pal(3, "Set1")[1:2]
    gp <- ggplot(dat, aes(x = dist, y = value)) + facet_wrap(~ Name, ncol = 1, scales = "free_y") +
          geom_vline(xintercept = spc, colour = "gray80") +
          geom_point(aes(colour = chr.g)) +
          scale_y_continuous(expand = c(0.02,0), breaks = seq(0,100, by = 10)) +
          scale_x_continuous(breaks = sp, labels = names(cross$geno), expand = c(0.02,0)) +
          xlab("") + ylab("Outlier Statistic") + scale_color_manual(values = cols) +
        theme(legend.position = "none",axis.text = element_text(size = 10), panel.background = element_blank(), panel.border = element_rect(colour = "gray80", fill = NA, size = 1.1), panel.grid.major.y = element_line(colour = "gray90", size = 1.2), panel.grid.minor.y = element_line(colour = "gray90", size = 0.8), panel.grid.major.x = element_blank(), axis.title = element_text(size = 20), strip.text = element_text(size=10))
    if(annotate){
        qtl <- lapply(mlist, function(el) gsub("Chr\\.", "", el$QTL$qtl))
        qtl.dat <- cbind.data.frame(Name = rep(nams, times = sapply(qtl, length)))
        qtl.dat$nout <- unlist(qtl)
        if(!is.null(chr.in)){
            qnam <- sapply(strsplit(as.character(qtl.dat$nout), "\\."), "[", 1)
            print(qnam)
            qtl.dat <- qtl.dat[qnam %in% chr.in,]
        }
        print(qtl.dat)
        subexpr <- paste(dat$Name, dat$nout, sep = ":") %in% paste(qtl.dat$Name, qtl.dat$nout, sep = ":")
        ann <- subset(dat, subexpr)
        print(ann)
        gp + geom_text(data = ann, aes(label = nout), size = 5)
    }
}

## check whether genetic clones are in the experiment

phenClones <- function(model, cross, matching = "Genotype", Envir = NULL, no.samp = 1000, sep = "_"){
     mg <- as.character(cross$pheno[[matching]])
        mgs <- lapply(mg, function(el, sep){
        el <- unlist(strsplit(el, sep))
        if(length(el) > 1)
            t(combn(el, 2))
        else NULL
        }, sep = sep)
     mgs <- mgs[!sapply(mgs, is.null)]
     mgd <- do.call("rbind", mgs)
     if(!is.null(Envir)){
         iterm <- paste(Envir, matching, sep = ":")
         pvals <- predict(model, classify = iterm, only = iterm)$pvals
         pvlist <- split(pvals, pvals[[Envir]])
         levs <- levels(pvals[[Envir]])
     } else
         pvlist <- list(predict(model, classify = matching, only = matching)$predictions$pvals)
     corlist <- list()
     for(j in 1:length(pvlist)){
        pvt <- pvlist[[j]]
        cg1 <- pvt$predicted.value[pmatch(mgd[,1], pvt[[matching]], duplicates.ok = TRUE)]
        cg2 <- pvt$predicted.value[pmatch(mgd[,2], pvt[[matching]], duplicates.ok = TRUE)]
        cor.samp <- c()
        for(i in 1:no.samp) {
            ts <- sample(pvt$predicted.value, dim(mgd)*2, replace = FALSE)
            cor.samp[i] <- cor(ts[1:(length(ts)/2)], ts[(length(ts)/2 + 1):length(ts)])
        }
        df <- dim(mgd)[1] - 2
        cs <- c(cor.samp, cor(cg1, cg2, use = "complete.obs"))
        pv <- 1 - pf((cs^2)*df/(1 - cs^2), 1, df)
        pva <- pv[1:(length(pv) - 1)]
        corlist[[j]] <- c(length(pva[pva < 0.05])/no.samp, cs[length(cs)], pv[length(pv)])
     }
     res <- cbind.data.frame(t(do.call("cbind", corlist)))
     names(res) <- c("Type1","Correlation","P-value")
     if(!is.null(Envir))
         res <- cbind.data.frame(levs, res)
     res
}

## fix clones if they are in the experiment

phenfixClones <- function(data, cross, matching = "Genotype", sep = "_"){
    mg <- as.character(cross$pheno[[matching]])
    mgs <- lapply(mg, function(el, sep){
        el <- unlist(strsplit(el, sep))
        if(length(el) > 1)
            el
        else NULL
    }, sep = sep)
    mgs <- mgs[!sapply(mgs, is.null)]
    for(i in 1:length(mgs)){
        levs <- levels(data[[matching]])
        levels(data[[matching]])[levs %in% mgs[[i]]] <- paste(mgs[[i]], collapse = sep)
    }
    data
}

## FAST: overall performance and stability for intrepreting Factor Analytic models

fast <- function(model, dat = NULL, term = "fa(Site, 4):Genotype", ...){
#    dat <- eval(model$call$data)
    str <- strsplit(term, ":")[[1]]
    sterm <- sapply(strsplit(gsub("fa\\(|\\))", "", str[grep("fa", str)]), ","), "[", 1)
    gterm <- str[-grep("fa", str)]
    sfa <- fa.asreml(model, ...)
    scores <- sfa$blups[[term]]$scores
    lvar <- cbind.data.frame(sfa$gammas[[term]]$"rotated loads", sfa$gammas[[term]]$"specific var")
    scores <- do.call("cbind.data.frame", tapply(scores$blupr, scores[[sterm]], function(el) el))
    names(scores) <- ns <- paste("score", 1:ncol(scores), sep = "")
    nk <- dim(scores)[2]
    scores <- cbind.data.frame(levels(dat[[gterm]]), scores)
    names(scores)[1] <- gterm
    sa <- scores[rep(1:nrow(scores), nrow(lvar)),]
    lvar <- lvar[rep(1:nrow(lvar), each = nrow(scores)),]
    nl <- paste("loads", 1:(ncol(lvar) - 1), sep = "")
    names(lvar) <- c(nl, "spec.var")
    ls <- cbind.data.frame(rep(levels(dat[[sterm]]), each = nrow(scores)), lvar, sa)
    names(ls)[1] <- sterm
    for(i in 1:nk){
        ts <- paste("fitted", i, sep = "")
        ls[[ts]] <- ls[[ns[i]]]*ls[[nl[[i]]]]
    }
    print(names(ls))
    ls$CVE <- rowSums(ls[,grep("fitted", names(ls))])
    ls$VE <- ls$CVE + ls[,"spec.var"]
    ls$OP <- mean(ls$loads1)*ls$score1
    ls$dev <- ls$CVE - ls$fitted1
    ls$stab <- tapply(ls$dev^2, ls$Genotype, mean)[as.character(ls$Genotype)]
    ls
}


## ggplot theme for designs

theme_design <- function (base_size = 11, base_family = "") {
    theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(
        legend.position = "none",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank(),
        axis.line =element_blank(),
        axis.ticks =element_blank(),
        axis.text.y = element_text(),
        axis.text.x = element_text(angle = 45),
        axis.title = element_text(),
        strip.text = element_text()
    )
}

## ggplot theme for heat maps

theme_design_heat <- function (base_size = 11, base_family = "") {
    theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(
#        legend.position = "none",
        strip.text.x = element_text(size = 14), #margin = margin(0.15,0,0.15,0, "cm")),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_text(),
        axis.title = element_text()
    )
}

## ggplot theme for scatter plots

theme_scatter <- function (base_size = 11, base_family = "") {
    theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(
        legend.position = "none",
        legend.text = element_text(size = 26),
        legend.title = element_text(size = 26),
        panel.grid.minor = element_line(colour = "grey80"),
        panel.grid.major = element_line(colour = "grey80"),
        panel.background = element_blank(),
        panel.border = element_rect(fill = NA, size = 1.1, colour = "grey80"),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 20, angle = 90),
        axis.title.x = element_text(size = 20),
#        axis.title = element_blank(),
        strip.text = element_text(size = 16)
    )
}

## ggplot them for bar plots

theme_barplot <- function (base_size = 11, base_family = "") {
    theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
       # legend.position = "none",
        panel.grid.minor = element_line(colour = "grey80"),
        panel.grid.major = element_line(colour = "grey80"),
        panel.background = element_blank(),
        panel.border = element_rect(fill = NA, size = 1.1, colour = "grey80"),
        axis.text.x = element_text(angle = 25, hjust = 1, vjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_blank(),
        strip.text = element_text(size = 15, margin = margin(0.17,0,0.17,0, "cm"))
    )
}

wald.test.asreml <- function(object, cc, keep.fac = TRUE)
{
    if(oldClass(object) != "asreml")
        stop("Requires an object of class asreml\n")
    if(is.null(object$Cfixed)) {
        warning("Requires C matrix from model object. Refitting test model with argument \"Cfixed = T\"\n")
        asreml.options(Cfixed = TRUE)
        object <- update(object)
    }
    vrb <- as.matrix(object$Cfixed)
    tau <- c(object$coefficients$fixed)
    names(tau) <- rownames(object$coefficients$fixed)
    nc <- length(tau)
    sigma2 <- object$sigma2
    vrb <- vrb/sigma2
    ccnams <- names(tau)
    zdf <- cdf <- NULL
    cc <- lapply(cc, function(el, ccnams){
        if(!all(names(el) %in% c("coef","type","comp","group")))
            stop("Inappropriately named argument for comparison object.")
        if(is.numeric(el$coef)) {
            if(max(el$coef) > length(ccnams))
                stop("coefficient subscript out of bounds")
            names(el$coef) <- ccnams[el$coef]
        }
        else {
            if(any(is.na(pmatch(el$coef, ccnams))))
                  stop("Names in contrast do not match the names of coefficients of object")
            temp <- pmatch(el$coef, ccnams)
            names(temp) <- el$coef
            el$coef <- temp
        }
        el
    }, ccnams)
   ## split contrasts and other available tests
    ctype <- unlist(lapply(cc, function(el) el$type))
    if(!all(ctype %in% c("con","zero")))
        stop("Contrast types must be either \"con\" for treatment comparisons or \"zero\" for testing zero equality")
    cons <- cc[ctype %in% "con"]
    zero <- cc[ctype %in% "zero"]
    cse <- ctau <- zwtest <- cwtest <- zpval <- c()
    if(length(cons)) {
       CRows <- lapply(cons, function(el, nc){
           if(length(el) < 3){
               con <- contr.helmert(length(el$coef))[, (length(el$coef) - 1)]
               names(con) <- cnam <- names(el$coef)
               cat("Warning: default contrast being taken for", cnam, "is", con, "\n")
               row <- rep(0, nc)
               row[el$coef] <- con
               row
           }
           else {
               if(is.matrix(el$comp)) {
                   if(length(el$coef) != ncol(el$comp))
                       stop("Length of contrast does not match the number of specified coefficients")
                   cons <- split(el$comp, 1:nrow(el$comp))
                   rows <- lapply(cons, function(ell, first = el$coef, nc){
                       row <- rep(0, nc)
                       row[first] <- ell
                       row
                   }, first = el$coef, nc)
                   rows <- unlist(rows, use.names = F)
                   matrix(rows, nrow = nrow(el$comp), byrow = T)
               }
               else {
                   if(length(el$coef) != length(el$comp))
                       stop("Length of contrast does not match the number of specified coefficients")
                   row <- rep(0, nc)
                   row[el$coef] <- el$comp
                   row
               }
           }
       }, nc)
       Cmat <- do.call("rbind", CRows)
       if(!keep.fac)
           ccnams <- substring(ccnams, regexpr("\\_", ccnams) + 1, nchar(ccnams))
       cnam <- lapply(split(Cmat, 1:nrow(Cmat)), function(el, ccnams){
           namr <- ccnams[ifelse(el < 0, T, F)]
           naml <- ccnams[ifelse(el > 0, T, F)]
           c(paste(naml, collapse = ":"), paste(namr, collapse = ":"))
       }, ccnams)
       Cnam <- do.call("rbind", cnam)
       gnams <- lapply(cons, function(el){
           if(!is.null(el$group)){
               if(!any(names(el$group) %in% c("left","right")))
                   stop("group names must be \"left\" and \"right\".")
               if(is.null(el$group$left)){
                   if(is.matrix(el$comp))
                       el$group$left <- rep(NA, nrow(el$comp))
                   else el$group$left <- NA
               } else {
                   if(is.matrix(el$comp)){
                       if(length(el$group$left) == 1)
                           el$group$left <- rep(el$group$left, nrow(el$comp))
                       if(length(el$group$left) != nrow(el$comp))
                          stop("No. of group names do not match the number of comparisons in object")
                   }
               }
                if(is.null(el$group$right)){
                   if(is.matrix(el$comp))
                       el$group$right <- rep(NA, nrow(el$comp))
                   else el$group$right <- NA
               } else {
                   if(is.matrix(el$comp)) {
                       if(length(el$group$right) == 1)
                           el$group$right <- rep(el$group$right, nrow(el$comp))
                       if(length(el$group$right) != nrow(el$comp))
                          stop("No. of group names do not match the number of comparisons in object")
                   }
               }
           } else {
               if(is.matrix(el$comp))
                   el$group$left <- el$group$right <- rep(NA, nrow(el$comp))
               else el$group$left <- el$group$right <- NA
           }
           cbind(el$group$left, el$group$right)
       })
       Gnam <- do.call("rbind", gnams)
       Cnam[!is.na(Gnam[,1]), 1] <- Gnam[!is.na(Gnam[,1]),1]
       Cnam[!is.na(Gnam[,2]), 2] <- Gnam[!is.na(Gnam[,2]),2]
       for(i in 1:nrow(Cmat)) {
           varmat <- sum(Cmat[i,  ]*crossprod(vrb, t(Cmat)[, i]))
           cse[i] <- sqrt(varmat * sigma2)
           ctau[i] <- sum(Cmat[i,  ]*tau)
           cwtest[i] <- (ctau[i]/cse[i])^2
       }
       cdf <- data.frame(wald = round(cwtest, 6), pval = round(1 - pchisq(cwtest, 1), 6),
                         coef = round(ctau, 6), se = round(cse, 6))
       cat("\nWald Tests: Comparisons\n\n")
       attr(cdf, "names") <- c("Wald Statistic", "P-Value", "Cont. Coef.", "Std. Error")
       attr(cdf, "row.names") <- paste(Cnam[,1], Cnam[,2],  sep = " vs ")
       oldClass(cdf) <- "data.frame"
   }
      if(length(zero)) {
        ZRows <- lapply(zero, function(el, nc){
            rows <- rep(rep(0, nc), length(el$coef))
            dum <- seq(0, (length(el$coef) - 1) * nc, by = nc)
            rows[el$coef + dum] <- 1
            matrix(rows, nrow = length(el$coef), byrow = T)
        }, nc)
        znam <- unlist(lapply(zero, function(el, ccnams) {
            if(is.null(el$group))
                paste(ccnams[el$coef], collapse = ":")
            else el$group
            }, ccnams))
        if(any(table(znam) > 1))
            stop("Duplicate names in group structures for zero equality tests.")
        for(i in 1:length(ZRows)) {
            varmat <- ZRows[[i]] %*% crossprod(vrb, t(ZRows[[i]]))
            Ctau <- ZRows[[i]] %*% tau
            zwtest[i] <- sum(Ctau*crossprod(solve(varmat), Ctau))/sigma2
            zpval[i] <- 1 - pchisq(zwtest[i], nrow(ZRows[[i]]))
        }
        zdf <- data.frame(wald = round(zwtest, 6), pval = round(zpval, 6))
        cat("\nWald Tests: Zero Equality\n\n")
        attr(zdf, "names") <- c("Wald Statistic", "P-Value")
        attr(zdf, "row.names") <- znam
        oldClass(zdf) <- "data.frame"
      }
    res <- list(Contrasts = cdf, Zero = zdf)
    invisible(res)
}

wald.test <- function(object, ...)
    UseMethod("wald.test")
