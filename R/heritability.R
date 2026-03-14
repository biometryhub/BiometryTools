#' Estimate heritability from an `asreml` model
#'
#' Computes Cullis-style heritability from a fitted \code{asreml} model using
#' predicted genetic effects and their associated standard errors of difference
#' (SEDs).
#'
#' The function supports both:
#' \itemize{
#'   \item single-term models, such as \code{"Genotype"}, and
#'   \item interaction terms for multi-environment trials, such as
#'   \code{"SYear:Genotype"}.
#' }
#'
#' For interaction terms, heritability is calculated separately for each level
#' of the first factor in \code{term} (for example, each site or site-year),
#' using the diagonal genetic variances extracted from the fitted covariance
#' structure.
#'
#' The heritability is computed as:
#' \deqn{
#' H^2 = 1 - \frac{\bar{SED}^2}{2\sigma_g^2}
#' }
#' where \eqn{\bar{SED}} is the average standard error of difference between
#' genotype predictions, and \eqn{\sigma_g^2} is the corresponding genetic
#' variance.
#'
#' @param model A fitted \code{asreml} model object.
#' @param term A character string giving the random term for which heritability
#'   is to be calculated. This can be a single term such as \code{"Genotype"},
#'   or an interaction term such as \code{"SYear:Genotype"}.
#' @param ... Additional arguments passed to \code{predict.asreml()}.
#'
#' @details
#' For interaction terms, the function attempts to identify the corresponding
#' random-effect structure in the fitted model and currently supports:
#' \code{fa}, \code{diag}, \code{corh}, \code{corgh}, and \code{us}.
#'
#' If \code{term} is an interaction term, the function:
#' \enumerate{
#'   \item obtains predicted values for the interaction,
#'   \item extracts the SED matrix among genotype predictions within each level
#'     of the first factor in \code{term},
#'   \item computes the average pairwise SED for each level, and
#'   \item combines this with the corresponding genetic variance to calculate
#'     heritability.
#' }
#'
#' For a single term, a single heritability estimate is returned.
#'
#' @return
#' A numeric vector of heritability estimates.
#' \itemize{
#'   \item If \code{term} is a single factor, a length-1 named vector is
#'   returned.
#'   \item If \code{term} is an interaction term, a named vector is returned
#'   with one heritability estimate for each level of the first factor in
#'   \code{term}.
#' }
#'
#' @note
#' This function is intended for use with \code{asreml} models fitted with
#' supported variance structures. For factor-analytic models, it relies on
#' \code{ASExtras4::fa.asreml()} to extract the genetic variance matrix.
#'
#' The function assumes that the first component of an interaction term
#' corresponds to the environment-like factor (for example, site, year, or
#' site-year), and the second corresponds to genotype.
#'
#' @seealso
#' \code{\link[asreml]{predict.asreml}},
#' \code{ASExtras4::fa.asreml}
#'
#'@export
#'
#' @examples
#' \dontrun{
#' TODO
#' }
#'
herit.asreml <- function(model, term = "SYear:Genotype", ...){
    dat <- eval(model$call$data) # Unused?
    if(length(grep(":", term))){
        terms <- all.vars(as.formula(paste("~ ", term, sep = "")))
        labs <- attr(terms(as.formula(model$call$random)), "term.labels")
        iterm <- labs[grep(paste(terms[1], "*.*", terms[2], sep = ""), labs)]
        uv <- sapply(strsplit(iterm, "\\("), "[", 1)
        if(uv == "fa"){
            pred <- predict(model, classify = term, only = iterm, sed = TRUE, ...)
            sumfa <- ASExtras4::fa.asreml(model, trunc.char = NULL)
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
