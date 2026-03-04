#' Handle outliers using standardized residuals
#'
#' Utilities to flag outlying observations per trait using standardized residuals
#' from fitted models (typically `asreml`), with a common cutoff rule.
#'
#' `outlier.down()` adds indicator (0/1) columns marking outlying rows, which can
#' be used as covariates or to downweight those observations in a subsequent fit.
#' `outlier.rem()` replaces outlying responses with `NA` (per trait) and returns
#' which traits had any outliers removed.
#'
#' @param data A data frame containing the response columns referenced by `model`.
#' @param model A named list of fitted model objects, one per trait/response.
#'   Each element must contain standardized residuals at `x$aom$R[, 2]`.
#'   Names of `model` are used to match response columns in `data`.
#' @param cutoff Numeric. Absolute standardized residual threshold used to flag
#'   outliers (default `3`).
#'
#' @return
#' - `outlier.down()` returns the input `data` with additional 0/1 indicator
#'   columns appended. New columns are named `"<trait>.o.<k>"`.
#' - `outlier.rem()` returns a list with:
#'   \describe{
#'     \item{data}{`data` with outlying responses set to `NA` (per trait).}
#'     \item{out}{Named logical vector indicating whether each trait had any
#'       outliers removed.}
#'   }
#'
#' @details
#' Outliers are identified for each trait using
#' `abs(model[[trait]]$aom$R[, 2]) > cutoff`.
#'
#' `outlier.down()` will append new indicator columns; if columns with the same
#' prefix already exist (e.g. `"Trait.o.*"`), numbering continues from the
#' highest existing suffix.
#'
#' Both functions print the row indices of detected outliers.
#'
#' @name outliers
NULL

#' Downweight outliers
#'
#' Adds one indicator column per detected outlying observation (per trait).
#'
#' @rdname outliers
#' @export
#'
#' @examples
#' \dontrun{
#'
#' }
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

#' Remove outliers
#'
#' Sets outlying response values to `NA` (per trait) and reports which traits had
#' outliers removed.
#'
#' @rdname outliers
#' @export
#'
#' @examples
#' \dontrun{
#'
#' }
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
