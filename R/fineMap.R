#' Fine-map a QTL region around a nominated marker in a fitted `wgaim` model
#'
#' @description
#' Given a fitted \code{wgaim} object and a \code{qtl::cross} object containing
#' interval- or imputation-based genotype data, \code{fineMap()} scans a window
#' of markers around a nominated marker and refits the model for each candidate
#' marker. For each refit, the function extracts the candidate marker effect
#' (either from the fixed or random component, depending on the model method),
#' computes a Wald-style test statistic, and reports a per-marker p-value and
#' Logarithm of the Odds(LOD).
#'
#' @details
#' The function:
#' \enumerate{
#'   \item Extracts genotype predictors from \code{intervalObj} (either
#'   \code{interval.data} or \code{imputed.data}, depending on \code{model$QTL$type}).
#'   \item Aligns genotype rows to the phenotype rows used by \code{model}, using
#'   the ID column \code{model$QTL$diag$genetic.term}.
#'   \item Defines a window of \code{flanking} markers on each side of \code{mark}.
#'   \item For each candidate marker in the window, removes nearby markers within
#'   \code{exclusion.window} (in the same units as the map positions returned by
#'   \code{qtl::pull.map()}) from the background set used to build the covariance.
#'   \item Constructs a covariance object via \code{wgaim:::constructCM()} and
#'   assigns \code{covObj} into the parent frame (a side-effect required by
#'   downstream \code{wgaim}/\code{asreml} update code).
#'   \item Refits the model with the candidate marker added and the original QTL
#'   marker removed, then extracts an effect estimate and its variance to compute
#'   a test statistic, p-value, and LOD.
#' }
#'
#' @param model A fitted \code{wgaim} object (typically fit with ASReml-R via \code{asreml}).
#' The object must contain \code{model$QTL} components used by \code{wgaim}.
#'
#' @param intervalObj A \code{qtl::cross} object containing genotype data used for
#' interval mapping or genotype imputation. Must inherit class \code{"cross"}.
#'
#' @param mark Character scalar. The name of the focal marker around which to fine-map.
#' This must be a marker present in \code{intervalObj}.
#'
#' @param flanking Integer. Number of markers to include on each side of \code{mark}
#' when defining the scan window. Default is \code{50}.
#'
#' @param exclusion.window Numeric. Distance threshold for excluding nearby markers
#' when constructing the background marker set for each candidate marker. Markers
#' with \code{abs(dist_i - dist_j) < exclusion.window} are excluded. Default is \code{10000}.
#'
#' @param ... Additional arguments passed to \code{update.asreml()} when refitting
#' the model for each candidate marker.
#'
#' @return A data frame with one row per scanned marker in the flanking window and columns:
#' \describe{
#'   \item{mark}{Marker name (as in \code{qtl::pull.map(intervalObj, chr)}).}
#'   \item{dist}{Map position of the marker (units as returned by \code{qtl::pull.map()}).}
#'   \item{pvalue}{Per-marker p-value derived from a Wald-style statistic.}
#'   \item{LOD}{Per-marker LOD score derived from the same statistic.}
#' }
#'
#' @section Note:
#' \code{fineMap()} assigns an object named \code{covObj} into the calling
#' environment (\code{parent.frame()}). This is required by internal \code{wgaim}
#' update routines that expect \code{covObj} to exist during model refits.
#'
#' @import wgaim
#'
#' @export
#'
#' @examples
#' \dontrun{
#' JULES COMPLETE
#' }
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
    state.chri <- state[chrs %in% fm$chr]
    mapc <- pull.map(intervalObj, fm$chr)[[1]]
    qind <- (1:length(mapc))[names(mapc) %in% mark]
    mark.qtl <- gsub("Chr\\.", "X.", names(state.chri)[qind])
    ql <- ifelse(qind - flanking <= 0, 1, qind - flanking)
    qr <- ifelse(qind + flanking > length(mapc), length(mapc), qind + flanking)
    state.chri[ql:qr] <- 1
    genoChr <- genoData[,names(state.chri)[ql:qr]]
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
        wind <- abs(mapc[i] - mapc) <= exclusion.window
        state.chr <- state.chri
        state.chr[wind] <- 0
        state[chr.ind] <- state.chr
        mout <- (1:ncol(genoData))[!as.logical(state)]
        genoSub <- genoData[,-mout]
        if(ncol(genoSub) > nrow(genoSub)){
            cov.env <- wgaim:::constructCM(genoSub)
            covObj <- cov.env$relm
        } else {
            tempObj <- cbind.data.frame(covObj[,1], genoSub)
            names(tempObj)[1] <- names(covObj)[1]
            covObj <- tempObj
        }
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
        mcf <- tempmodel$coefficients[[method]][whr, 1]
        vcf <- tempmodel$vcoeff[[method]][whr]
        zrat <- mcf/sqrt(vcf * tempmodel$sigma2)
        #zrat <- mcf/sqrt(vcf)
        pvalue[k] <- round((1 - pchisq(zrat^2, df = 1)), 4)
        lod[k] <- round(0.5 * log(exp(zrat^2), base = 10), 4)
        print(c(pvalue[k], lod[k]))
        k <- k + 1
    }
    cbind.data.frame(mark = names(mapc)[ql:qr], dist = mapc[ql:qr], pvalue = pvalue, LOD = lod)
}
