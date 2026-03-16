#' Associate BLUEs with BLUPs form predict calls of the same model
#'
#' Extracts BLUPs (random effects) and BLUEs (fixed effects)
#' from an \code{asreml} fitted model using \code{predict()}
#' and merges them into a single data frame.
#'
#' @param model An \code{asreml} model object.
#'   The model must contain a valid \code{model$call$data} reference.
#'
#' @param ran.term Character string specifying the random-effect term
#'   to classify in \code{predict()}. Examples:
#'   \code{"gen"} or \code{"loc:gen"}.
#'
#' @param fix.term Character string specifying the fixed-effect term
#'   to classify in \code{predict()}. Examples:
#'   \code{"loc"} or \code{"Variety"}.
#'
#' @param ... Additional arguments passed to \code{predict()}.
#'
#' @details
#' The function performs:
#' \enumerate{
#'   \item \code{predict(model, classify = ran.term, only = ran.term)}
#'         to extract BLUPs
#'   \item \code{predict(model, classify = fix.term)}
#'         to extract BLUEs
#' }
#'
#' If \code{ran.term} is an interaction and uses structured
#' variance models (e.g. \code{fa()}, \code{us()}, \code{diag()},
#' \code{corh()}, \code{corgh()}), the appropriate term is passed
#' to the \code{only} argument.
#'
#' @return
#' A \code{data.frame} containing:
#' \itemize{
#'   \item Classification variables
#'   \item \code{blups}
#'   \item \code{blups.std.error}
#'   \item \code{blues}
#'   \item \code{blues.std.error}
#' }
#'
#' @seealso \code{\link[asreml]{predict.asreml}}
#'
#' @export
#' @examples
#' \dontrun{
#' if (requireNamespace("asreml", quietly = TRUE)) {
#'
#'   data(oats)
#'
#'   oats$Blocks   <- factor(oats$Blocks)
#'   oats$Variety  <- factor(oats$Variety)
#'   oats$Nitrogen <- factor(oats$Nitrogen)
#'
#'   # Fit a simple mixed model
#'   m <- asreml::asreml(
#'     fixed  = yield ~ Variety + Nitrogen,
#'     random = ~ Blocks,
#'     data   = oats
#'   )
#'
#'   # Extract BLUPs for Blocks and BLUEs for Variety
#'   associate(
#'     m,
#'     ran.term = "Blocks",
#'     fix.term = "Variety"
#'   )
#'
#'   # Example with interaction BLUPs
#'   m2 <- asreml::asreml(
#'     fixed  = yield ~ Variety,
#'     random = ~ Blocks:Variety,
#'     data   = oats
#'   )
#'
#'   associate(
#'     m2,
#'     ran.term = "Blocks:Variety",
#'     fix.term = "Variety"
#'   )
#' }
#' }
#'
associate <- function(model, ran.term = "Site:Genotype", fix.term = "Site:Type", ...){
    rnams <- all.vars(as.formula(paste("~ ", ran.term, sep = "")))
    fnams <- all.vars(as.formula(paste("~ ", fix.term, sep = "")))
    if(length(rnams) > 1){
        labs <- attr(terms(as.formula(model$call$random)), "term.labels")
        iterm <- labs[grep(paste(rnams[1], "*.*", rnams[2], sep = ""), labs)]
        uv <- sapply(strsplit(iterm, "\\("), "[", 1)
        if(uv == "fa")
            predr <- predict(model, classify = ran.term, only = iterm, ...)
        else if(uv %in% c("diag","corh","corgh","us"))
            predr <- predict(model, classify = ran.term, only = ran.term, ...)
        tab <- table(c(rnams, fnams))
        dnams <- names(tab)[tab == 1]
        snams <- names(tab)[tab == 2]
        rnams <- rnams[rnams != snams]
    } else {
        predr <- predict(model, classify = ran.term, only = ran.term, ...)
        dnams <- c(rnams,fnams)
    }
    pr <- predr$pvals
    names(pr) <- gsub("predicted.value", "blups", names(pr))
    names(pr) <- gsub("std.error", "blups.std.error", names(pr))
    predf <- predict(model, classify = fix.term, ...)
    pf <- predf$pvals
    pf <- pf[!is.na(pf$predicted.value),]
    names(pf) <- gsub("predicted.value", "blues", names(pf))
    names(pf) <- gsub("std.error", "blues.std.error", names(pf))
    dat <- eval(model$call$data)
    datr <- dat[,dnams]
    datr <- datr[!duplicated(datr),]
    pri <- merge(pr[-ncol(pr)], datr, by = rnams, all.x = TRUE)
    pall <- merge(pri, pf[-c(ncol(pf) - 1, ncol(pf))], by = fnams, all.x = TRUE)
    if(any(is.na(pall$blues))){
        if(length(rnams) > 1)
            spv <- pall[[snams]]
        else spv <- factor(rep(1, nrow(pall)))
        palls <- split(pall, spv)
        pall <- do.call("rbind", lapply(palls, function(el){
            nab <- is.na(el$blues)
            el$Type.blue <- as.character(el$Type)
            el$Type.blue[nab] <- "Site Average"
            el$blues[nab] <- mean(el$blues[!nab])
            el}))
    }
    pall
}
