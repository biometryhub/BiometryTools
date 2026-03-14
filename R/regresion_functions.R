#' Convert treatment-specific BLUPs into efficiency and responsiveness
#'
#' Re-parameterizes treatment-by-site-by-variety BLUPs from a fitted
#' \code{asreml} model into an efficiency/responsiveness representation.
#'
#' The function assumes that the environment term supplied in \code{Env}
#' represents a treatment-by-site factor crossed with variety, for example
#' \code{"TSite:Variety"}, where levels of \code{TSite} combine treatment and
#' site information.
#'
#' Given two treatment levels in \code{levs}, the function treats the first as
#' a baseline ("efficiency") component and expresses the second as a
#' responsiveness component after regression on the first.
#'
#' @param model A fitted \code{asreml} model object containing the full
#'   treatment-by-site-by-variety structure.
#' @param Env A character string giving the environment-by-variety term to be
#'   transformed, for example \code{"TSite:Variety"}.
#' @param levs A character vector of length 2 giving the treatment levels used
#'   in the transformation. The second treatment is regressed on the first.
#' @param sep A character string giving the separator used in composite
#'   treatment-by-site level names. If no separator is present, the function
#'   assumes a single section.
#' @param pev Logical; if \code{TRUE}, use the transformed prediction error
#'   variance matrix. If \code{FALSE}, subtract the transformed prediction error
#'   variance from the corresponding genetic variance structure.
#' @param ... Additional arguments passed to \code{predict.asreml()}.
#'
#' @details
#' For each site (or section), the function extracts the \eqn{2 \times 2}
#' covariance matrix for the two treatment levels and computes:
#' \deqn{
#' \beta = \frac{\mathrm{Cov}(T_1, T_2)}{\mathrm{Var}(T_1)}
#' }
#' and the responsiveness variance:
#' \deqn{
#' \sigma_r^2 = \mathrm{Var}(T_2)(1 - \rho^2),
#' }
#' where \eqn{\rho} is the correlation between the two treatment effects.
#'
#' The transformed responsiveness BLUP is then:
#' \deqn{
#' b_{\mathrm{resp}} = b_{T_2} - \beta b_{T_1}.
#' }
#'
#' The function also returns the transformed covariance matrix
#' \eqn{G_{\mathrm{trans}} = T G T^\top} corresponding to the
#' efficiency/responsiveness parameterization.
#'
#' @return
#' A list with components:
#' \describe{
#'   \item{blups}{A data frame containing site, variety, BLUPs for the two
#'   specified treatment levels, the derived responsiveness value, and an HSD
#'   summary where available.}
#'   \item{TGmat}{The transformed covariance matrix under the
#'   efficiency/responsiveness parameterization.}
#'   \item{Gmat}{The original covariance matrix for the supplied environment
#'   term.}
#'   \item{beta}{Regression coefficients used to regress the second treatment on
#'   the first within each site.}
#'   \item{sigr}{Responsiveness variances within each site.}
#'   \item{tmat}{The linear transformation matrix applied to \code{Gmat}.}
#' }
#'
#' @note
#' This function assumes exactly two treatment levels in \code{levs}. It is
#' primarily intended for treatment-by-site composite factors where the first
#' part or second part of the composite level name identifies treatment.
#'
#' @examples
#' \dontrun{
#' TODO
#'}
#' @export
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


#' Compute a fixed-effect responsiveness index from predicted values
#'
#' Forms a regression-based responsiveness index from predicted values obtained
#' from a fitted \code{asreml} model.
#'
#' The function identifies two treatment levels within a prediction term and
#' compares matched predictions across a remaining regression variable
#' (for example genotype). The resulting responsiveness index is computed either
#' as:
#' \itemize{
#'   \item residuals from a simple linear regression of the second treatment on
#'   the first, or
#'   \item model-based conditional residuals using the prediction covariance
#'   matrix.
#' }
#'
#' @param model A fitted \code{asreml} model object.
#' @param term A character string specifying the prediction term, for example
#'   \code{"Treatment:Genotype"} or \code{"Treatment:Site:Genotype"}.
#' @param by An optional character string specifying variables used to split the
#'   analysis into sections. These variables must be contained in \code{term}.
#' @param levs A character vector of length 2 giving the treatment levels to be
#'   compared.
#' @param simple Logical; if \code{TRUE}, compute responsiveness as residuals
#'   from a simple linear regression of treatment 2 on treatment 1. If
#'   \code{FALSE}, compute responsiveness using the model-based prediction
#'   covariance matrix.
#'
#' @details
#' The function first predicts the full \code{term} using
#' \code{predict.asreml(..., vcov = TRUE)}. It then identifies:
#' \itemize{
#'   \item the factor containing the treatment levels in \code{levs},
#'   \item optional grouping variables in \code{by}, and
#'   \item the remaining variable(s) used to match observations across the two
#'   treatment levels.
#' }
#'
#' For each split defined by \code{by}, matched predictions are extracted for
#' the two treatment levels. If \code{simple = TRUE}, a linear regression
#' \eqn{y_2 \sim y_1} is fitted and the residuals are returned as the
#' responsiveness index. If \code{simple = FALSE}, the responsiveness index is
#' computed from the conditional mean structure implied by the prediction
#' covariance matrix.
#'
#' The function also reports standard errors, an average SED, and a Tukey-style
#' HSD summary for the responsiveness index.
#'
#' @return
#' A data frame containing:
#' \describe{
#'   \item{Split}{The grouping level defined by \code{by}, or a default label if
#'   no grouping is used.}
#'   \item{Regress.Var}{The matching regression unit, for example genotype.}
#'   \item{\code{levs[1]}}{Predicted value under the first treatment level.}
#'   \item{\code{levs[2]}}{Predicted value under the second treatment level.}
#'   \item{reponse.index}{The derived responsiveness index.}
#'   \item{std.error}{Standard error of the responsiveness index.}
#'   \item{HSD}{A Tukey-style HSD summary based on the average pairwise SED.}
#'   \item{sed}{Average pairwise SED of the responsiveness index.}
#' }
#'
#' @note
#' This function assumes exactly two treatment levels in \code{levs}. It also
#' assumes that matched observations across treatments can be identified using
#' the remaining variable(s) in \code{term} after removing the treatment factor
#' and any grouping variables in \code{by}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' TODO
#' }
#'
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
