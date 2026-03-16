#' Test Genetic Clone Similarity in an ASReml Model
#'
#' Evaluates whether genetically related clones show significantly
#' correlated predicted values from an \code{asreml} model.
#'
#' Clone relationships are extracted from a \code{cross} object,
#' and a permutation test is used to assess whether observed
#' correlations exceed random expectation.
#'
#' @param model An \code{asreml} fitted model object.
#' @param cross A list-like object containing a \code{$pheno} data frame.
#' @param matching Character string giving the genotype column name
#'   within both the model predictions and \code{cross$pheno}.
#' @param Envir Optional character string specifying an environment
#'   factor. If supplied, clone testing is performed separately
#'   within each environment.
#' @param no.samp Number of random permutations used to estimate
#'   the null distribution (default = 1000).
#' @param sep Character separator used to split clone identifiers
#'   (default = "_").
#'
#' @details
#' For each pair of cloned genotypes:
#' \enumerate{
#'   \item Predicted values are extracted from \code{predict()}.
#'   \item The observed correlation between clone pairs is computed.
#'   \item A permutation test generates random correlations.
#'   \item A p-value is computed from the F-distribution.
#' }
#'
#' The Type1 column reports the proportion of permutation samples
#' with p < 0.05.
#'
#' @return A data frame with columns:
#' \itemize{
#'   \item \code{Type1}: Estimated type I error rate from permutations
#'   \item \code{Correlation}: Observed clone correlation
#'   \item \code{P-value}: Significance test for observed correlation
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' JULES COMPLETE
#' }
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

#' Collapse Cloned Genotypes into Single Levels
#'
#' Modifies a data frame so that cloned genotypes
#' are treated as identical factor levels.
#'
#' @param data A data frame containing genotype factor levels.
#' @param cross A list-like object containing a \code{$pheno} data frame.
#' @param matching Character string specifying genotype column name.
#' @param sep Character separator used to identify clones (default "_").
#'
#' @return A modified data frame where cloned genotype
#' levels are collapsed into single combined labels.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Fake cross object
#' cross <- list(
#' pheno = data.frame(
#' Genotype = c("A_B", "C", "D_E")
#' )
#' )
#' # Fake prediction table
#' fake_model <- list()
#' # Mock predict method
#' predict <- function(model, classify, only = NULL) {
#' data <- data.frame(
#' Genotype = c("A","B","C","D","E"),
#' predicted.value = rnorm(5)
#' )
#' list(pvals = data)
#' }
#' # Test clone fixing
#' df <- data.frame(Genotype = factor(c("A","B","C","D","E")))
#' phenfixClones(df, cross)
#' }
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
