#' Pairwise comparison of BLUEs using LSD or p-values
#'
#' Computes pairwise comparisons among predicted values (typically BLUEs)
#' from an \code{asreml} model for a given classification term. Uses
#' \code{predict(..., sed = TRUE)} to obtain predicted values and the
#' SED (standard error of differences) matrix. Depending on \code{type},
#' returns either a pairwise LSD matrix or a symmetric matrix of
#' pairwise p-values derived from Wald contrasts.
#'
#' @param model An \code{asreml} fitted model object.
#' @param term Character string giving the classification term passed to
#'   \code{predict()}, e.g. \code{"Line"} or an interaction like \code{"Env:Line"}.
#' @param type Character string specifying the comparison type:
#'   \code{"LSD"} or \code{"PVAL"}.
#' @param average.LSD Logical (default \code{FALSE}). If \code{TRUE} and
#'   \code{type = "LSD"}, returns a single \code{ave.LSD} column computed as the
#'   mean of the lower-triangular LSD values instead of a full LSD matrix.
#'
#' @details
#' \strong{LSD:} The least significant difference is computed as
#' \code{LSD = SED * qt(0.025, df = model$nedf, lower.tail = FALSE)}.
#'
#' \strong{PVAL:} All pairwise contrasts are constructed and assessed using
#' a Wald test (via \code{wald.test()}), returning a symmetric p-value matrix.
#'
#' Rows with \code{NA} predicted values are dropped, and the corresponding
#' rows/columns are removed from the SED matrix.
#'
#' @return A \code{data.frame} containing the classification column(s),
#'   predicted values, and either:
#' \itemize{
#'   \item an LSD matrix (or \code{ave.LSD} column), or
#'   \item a symmetric matrix of pairwise p-values.
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' TODO
#' }
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
