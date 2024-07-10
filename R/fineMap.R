#' Fine map a `wgaim` object
#'
#' @param model
#' @param intervalObj
#' @param mark
#' @param flanking
#' @param exclusion.window
#' @param ...
#'
#' @import wgaim
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' JULES COMPLETE
#' }
fineMap <- function(model, intervalObj, mark = NULL, flanking = 50, exclusion.window = 10000, ...) {
  resp <- deparse(model$call$fixed[[2]])
  phenoData <- eval(parse(text = paste(resp, ".data", sep = "")))
  if (missing(intervalObj)) {
    stop("intervalObj is a required argument")
  }
  if (!inherits(intervalObj, "cross")) {
    stop("intervalObj is not of class \"cross\"")
  }
  if (is.null(mark)) {
    stop("mark argument must be non-NULL.")
  }
  if (model$QTL$type == "interval") {
    gdat <- lapply(intervalObj$geno, function(el) el$interval.data)
  } else {
    gdat <- lapply(intervalObj$geno, function(el) el$imputed.data)
  }
  genoData <- do.call("cbind", gdat)
  gterm <- model$QTL$diag$genetic.term
  state <- model$QTL$diag$state
  method <- model$QTL$method
  dimnames(genoData) <- list(as.character(intervalObj$pheno[[gterm]]), names(state))
  genoData <- genoData[rownames(genoData) %in% as.character(phenoData[[gterm]]), ]
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
  genoChr <- genoData[, names(state.chr)[ql:qr]]
  colnames(genoChr) <- gsub("Chr\\.", "X.", colnames(genoChr))
  tmp <- cbind.data.frame(rownames(genoData), genoChr)
  colnames(tmp)[1] <- gterm
  phenoData <- phenoData[, !(names(phenoData) %in% mark.qtl)]
  phenoData$ord <- 1:nrow(phenoData)
  phenoData <- merge(phenoData, tmp, by = gterm, all.x = TRUE)
  phenoData <- phenoData[order(phenoData$ord), ]
  k <- 1
  pvalue <- lod <- c()
  for (i in ql:qr) {
    wind <- abs(mapc[i] - mapc) < exclusion.window
    state.chr[wind] <- 0
    state[chr.ind] <- state.chr
    print(length(state))
    genoSub <- genoData[, as.logical(state)]
    print(dim(genoData))
    cov.env <- wgaim:::constructCM(genoSub)
    covObj <- cov.env$relm
    assign("covObj", covObj, envir = parent.frame())
    mark.i <- colnames(genoChr)[k]
    print(mark.i)
    if (method == "random") {
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
    zrat <- mcf / sqrt(vcf)
    pvalue[k] <- round((1 - pchisq(zrat^2, df = 1)) / 2, 4)
    lod[k] <- round(0.5 * log(exp(zrat^2), base = 10), 4)
    print(c(pvalue[k], lod[k]))
    k <- k + 1
  }
  cbind.data.frame(mark = names(mapc)[ql:qr], dist = mapc[ql:qr], pvalue = pvalue, LOD = lod)
}
