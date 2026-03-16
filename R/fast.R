#' FAST: Overall performance and stability from a factor-analytic MET model
#'
#' Compute FAST-style summaries (overall performance and stability) from an
#' ASReml factor analytic (FA) mixed model fitted to variety-by-environment
#' (VE) effects. The method is based on the latent regression interpretation of
#' the FA model described by Smith & Cullis (2018).
#'
#' @details
#' Consider the common VE (CVE) effects \eqn{\tilde\beta_{ij}} for genotype
#' \eqn{i} in environment \eqn{j} under an FA model with rotated loadings
#' \eqn{\hat\lambda_{rj}} and genotype scores \eqn{\tilde f_{ri}}. Smith & Cullis (2018)
#' separate the first factor from the remainder via the *first latent regression*
#' representation, where \eqn{\tilde\epsilon_{ij}} collects the contributions from factors
#' \eqn{r = 2,\dots,k} (and can be interpreted as deviations about the first
#' latent regression line).
#'
#' When (almost) all rotated loadings for factor 1 are positive, Smith & Cullis (2018)
#' define **overall performance** (OP) for genotype \eqn{i} as the fitted value at the
#' mean of the factor-1 loadings:
#'  \deqn{OP_i = \bar\lambda_1 \tilde f_{1i},}
#' where \eqn{\bar\lambda_1} is the mean of \eqn{\hat\lambda_{1j}} across environments.
#'
#' They define **stability** as the root mean squared deviation (RMSD) about the first
#' latent regression line:
#' \deqn{RMSD_i = \sqrt{\frac{1}{p}\sum_{j=1}^p (\tilde\beta_{ij} - \hat\lambda_{1j}\tilde f_{1i})^2},}
#' where \eqn{p} is the number of environments.
#'
#' This function reconstructs per-environment fitted contributions
#' \eqn{\widehat{\mathrm{fitted}}_{rij} = \hat\lambda_{rj}\tilde f_{ri}} and forms
#' \code{CVE} as their sum across factors. It then computes:
#' \itemize{
#'   \item \code{OP = mean(loads1) * score1} (matches \eqn{OP_i} above),
#'   \item \code{dev = CVE - fitted1} (corresponds to \eqn{\tilde\beta_{ij}-\hat\lambda_{1j}\tilde f_{1i}}),
#'   \item \code{stab = mean(dev^2)} by genotype (this is *MSD*; RMSD is \code{sqrt(stab)}).
#' }
#'
#' @param model An \code{asreml} model object containing the FA term specified by \code{term}.
#' @param dat A data frame used only to obtain factor levels for the environment and genotype terms
#'   named in \code{term}. Must contain those columns as factors (e.g. \code{Site} and \code{Genotype}).
#' @param term Character string giving the FA term of interest, typically of the form
#'   \code{"fa(<Env>, <k>):<Genotype>"} (default \code{"fa(Site, 4):Genotype"}).
#' @param ... Additional arguments passed to \code{ASExtras4::fa.asreml()}.
#'
#' @return A data frame with one row per Environment \eqn{\times} Genotype combination containing:
#' \itemize{
#'   \item the environment factor (e.g. \code{Site}),
#'   \item \code{loads1}, \code{loads2}, ... and \code{spec.var} (from \code{ASExtras4::fa.asreml()}),
#'   \item the genotype factor (e.g. \code{Genotype}),
#'   \item \code{score1}, \code{score2}, ... (scores),
#'   \item \code{fitted1}, \code{fitted2}, ... (per-factor fitted contributions),
#'   \item \code{CVE} (sum of fitted contributions across factors),
#'   \item \code{VE} (\code{CVE + spec.var}; included for convenience),
#'   \item \code{OP} (overall performance; constant within genotype),
#'   \item \code{dev} (deviation from factor-1 fitted contribution),
#'   \item \code{stab} (mean squared deviation by genotype; \code{sqrt(stab)} gives RMSD scale).
#' }
#'
#' @references
#' Smith, A. B. & Cullis, B. R. (2018). Plant breeding selection tools built on factor analytic
#' mixed models for multi-environment trial data. *Euphytica*, 214:143.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' library(asreml)
#' data(oats)
#'
#' oats$Nitrogen <- as.factor(oats$Nitrogen)
#' oats$Variety  <- as.factor(oats$Variety)
#' oats$Block    <- as.factor(oats$Block)
#'
#' m_fa <- asreml(
#'   fixed    = yield ~ Nitrogen,                 # Fixed treatment levels
#'   random   = ~ Block + Block:Variety + fa(Nitrogen, 2):Variety,
#'   residual = ~ units,
#'   data     = oats
#' )
#'
#' out <- fast(m_fa, dat = oats, term = "fa(Nitrogen, 2):Variety")
#'
#' op_by_var   <- tapply(out$OP, out$Variety, unique)
#' msd_by_var  <- tapply(out$stab, out$Variety, unique)
#' rmsd_by_var <- sqrt(msd_by_var)
#'
#' summary_tbl <- data.frame(
#'   Variety = names(op_by_var),
#'   OP      = as.numeric(op_by_var),
#'   MSD     = as.numeric(msd_by_var),
#'   RMSD    = as.numeric(rmsd_by_var),
#'   row.names = NULL
#' )
#'
#' summary_tbl[order(-summary_tbl$OP), ]
#' }
#'
fast <- function(model, term = "fa(Site, 4):Genotype", ...){
    dat <- eval(model$call$data)
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
    ls$CVE <- rowSums(ls[,grep("fitted", names(ls)), drop = FALSE])
    ls$VE <- ls$CVE + ls[,"spec.var"]
    ls$OP <- mean(ls$loads1)*ls$score1
    if(nk > 1){
        ls$dev <- ls$CVE - ls$fitted1
        ls$stab <- sqrt(tapply(ls$dev^2, ls[[gterm]], mean)[as.character(ls[[gterm]])])
    }
    ls
}

#' FAST-IC: FAST summaries within interaction classes from a factor-analytic MET model
#'
#' Compute FAST-style summaries of genotype performance and stability within
#' *interaction classes* (ICs) derived from a factor-analytic (FA) mixed model
#' fitted to multi-environment trial (MET) data using ASReml.
#'
#' @details
#' Factor analytic (FA) linear mixed models are widely used to model
#' genotype-by-environment (G×E) interactions in multi-environment trials.
#' In this framework, the common variety-by-environment effects can be written
#' in terms of environment loadings and genotype scores.
#'
#' Smith & Cullis (2018) introduced the FAST approach, which interprets the FA
#' model using a *latent regression representation*. In this interpretation,
#' the first factor captures the dominant pattern of genotype response across
#' environments, allowing summary measures of:
#'
#' \itemize{
#'   \item \strong{Overall performance (OP)} — the expected genotype response at
#'   the mean of the first factor loadings.
#'
#'   \item \strong{Stability} — typically measured using the root mean squared
#'   deviation (RMSD) of genotype responses from the first latent regression line.
#' }
#'
#' Smith et al. (2021) extended this idea by introducing
#' *interaction classes (iClasses)*. These classes group environments according
#' to the **sign pattern of their FA loadings**, reflecting different
#' patterns of genotype response across environments.
#'
#' For a model with \eqn{k} factors, interaction classes are defined using the
#' sign pattern of the first \code{ic.num} loadings:
#'
#' \itemize{
#'   \item \code{"p"} indicates a positive loading
#'   \item \code{"n"} indicates a negative loading
#' }
#'
#' For example, when \code{ic.num = 2}:
#'
#' \itemize{
#'   \item \code{"pp"} means both loadings are positive
#'   \item \code{"pn"} means loading 1 positive, loading 2 negative
#'   \item \code{"np"} means loading 1 negative, loading 2 positive
#'   \item \code{"nn"} means both loadings are negative
#' }
#'
#' FAST summaries can then be computed **within each interaction class**.
#' This produces class-specific summaries of genotype performance and stability.
#'
#' In this function:
#'
#' \itemize{
#'   \item Genotype scores \eqn{\tilde f_{ri}} are obtained from the FA model.
#'   \item Environment loadings \eqn{\hat\lambda_{rj}} define interaction classes.
#'   \item The fitted FA contributions
#'   \eqn{\widehat{fitted}_{rij} = \hat\lambda_{rj}\tilde f_{ri}} are reconstructed.
#'   \item The common variety-by-environment effect (CVE) is obtained as the
#'   sum of fitted contributions across factors.
#' }
#'
#' Within each interaction class \eqn{c}, overall performance is calculated as:
#'
#' \deqn{
#' OP_{i,c} =
#' \sum_{r=1}^{ic.num}
#' \bar{\lambda}_{r,c} \tilde f_{ri}
#' }
#'
#' where \eqn{\bar{\lambda}_{r,c}} is the mean loading of environments in class
#' \eqn{c}.
#'
#' Stability is summarised using the root mean squared deviation (RMSD):
#'
#' \deqn{
#'   RMSD_{i,c} =
#'   \sqrt{
#'     \frac{1}{|c|}
#'     \sum_{j \in c}
#'     \left(
#'       \tilde\beta_{ij} -
#'       \sum_{r=1}^{ic.num}\hat\lambda_{rj}\tilde f_{ri}
#'     \right)^2
#'   }
#' }
#'
#' where \eqn{\tilde\beta_{ij}} denotes the FA-predicted common VE effect.
#'
#' These summaries allow genotype performance to be compared within groups
#' of environments that share similar G×E response patterns.
#'
#' @param model An \code{asreml} model object containing the FA term specified by \code{term}.
#'
#' @param term Character string specifying the FA term of interest,
#' typically of the form \code{"fa(<Env>, <k>):<Genotype>"}.
#' Default is \code{"fa(Site, 4):Genotype"}.
#'
#' @param ic.num Integer specifying the number of FA factors used to define
#' interaction classes (default = 2).
#'
#' @param ... Additional arguments passed to \code{ASExtras4::fa.asreml()}.
#'
#' @return
#' A data frame containing one row for each Environment × Genotype
#' combination with columns including:
#'
#' \itemize{
#'   \item environment factor (e.g. \code{Site})
#'   \item \code{iclass} — interaction class label
#'   \item FA loadings (\code{loads1}, \code{loads2}, ...)
#'   \item \code{spec.var} — specific variance
#'   \item genotype scores (\code{score1}, \code{score2}, ...)
#'   \item fitted FA contributions (\code{fitted1}, \code{fitted2}, ...)
#'   \item \code{CVE} — common variety-by-environment effect
#'   \item \code{OP} — overall performance within interaction class
#'   \item \code{dev} — deviation from fitted FA contribution
#'   \item \code{RMSD} — stability within interaction class
#' }
#'
#' @references
#' Smith, A., Norman, A., Kuchel, H., & Cullis, B. (2021).
#' Plant variety selection using interaction classes derived from factor analytic
#' linear mixed models: Models with independent variety effects.
#' *Frontiers in Plant Science*, 12, 737462.
#' https://doi.org/10.3389/fpls.2021.737462
#'
#' Smith, A. B., & Cullis, B. R. (2018).
#' Plant breeding selection tools built on factor analytic mixed models for
#' multi-environment trial data.
#' *Euphytica*, 214, 143.
#'
#' @export
#'
#' @examples
#' \dontrun{
# library(asreml)
#
# data(oats)
#
# oats$Nitrogen <- as.factor(oats$Nitrogen)
# oats$Variety  <- as.factor(oats$Variety)
# oats$Block    <- as.factor(oats$Block)
#
# m_fa <- asreml(
#   fixed    = yield ~ Nitrogen,
#   random   = ~ Block + Block:Variety + fa(Nitrogen, 2):Variety,
#   residual = ~ units,
#   data     = oats
# )
#
# out <- fastIC(m_fa, term = "fa(Nitrogen, 2):Variety", ic.num = 2)
#
# # Example: ranking varieties by performance within interaction class
# op_tbl <- aggregate(OP ~ iclass + Variety, out, function(x) unique(x)[1])
# op_tbl[order(op_tbl$iclass, -op_tbl$OP), ]
#
# # Example: stability comparison
# stab_tbl <- aggregate(RMSD ~ iclass + Variety, out, function(x) unique(x)[1])
# stab_tbl[order(stab_tbl$iclass, stab_tbl$RMSD), ]
#' }
fastIC <- function(model, term = "fa(Site, 4):Genotype", ic.num = 2, ...){
    dat <- eval(model$call$data)
    str <- strsplit(term, ":")[[1]]
    sterm <- sapply(strsplit(gsub("fa\\(|\\))", "", str[grep("fa", str)]), ","), "[", 1)
    if(length(grep("vm", str)))
        gterm <- sapply(strsplit(gsub("vm\\(|\\))", "", str[grep("vm", str)]), ","), "[", 1)
    else
        gterm <- str[-grep("fa", str)]
    sfa <- ASExtras4::fa.asreml(model, ...)
    scores <- sfa$blups[[term]]$scores

    scored <- do.call("cbind.data.frame", tapply(scores$blupr, scores[[sterm]], function(el) el))
    names(scored) <- ns <- paste("score", 1:ncol(scored), sep = "")
    nk <- dim(scored)[2]
    scored <- cbind.data.frame(factor(unique(scores[[gterm]])), scored)
    names(scored)[1] <- gterm
    loads <- sfa$gammas[[term]]$"rotated loads"
    spv <- sfa$gammas[[term]]$"specific var"
    lvar <- cbind.data.frame(loads, spv)
    sa <- scored[rep(1:nrow(scored), nrow(lvar)),]
    lvar <- lvar[rep(1:nrow(lvar), each = nrow(scored)),]
    nl <- paste("loads", 1:(ncol(lvar) - 1), sep = "")
    names(lvar) <- c(nl, "spec.var")
    iclass <- apply(loads[,1:ic.num, drop = FALSE], 1, function(el) paste(ifelse(el > 0, "p", "n"), collapse = ""))
    ls <- cbind.data.frame(iclass = factor(rep(iclass, each = nrow(scored))), lvar, sa)
    env <- factor(rownames(loads), levels = rownames(loads))
    ls <- cbind.data.frame(rep(env, each = nrow(scored)), ls)
    names(ls)[1] <- sterm
    for(i in 1:nk){
        ts <- paste("fitted", i, sep = "")
        ls[[ts]] <- ls[[ns[i]]]*ls[[nl[[i]]]]
    }
    ls$CVE <- rowSums(ls[,grep("fitted", names(ls))])
    print(dim(ls))
    ilev <- levels(ls$iclass)
    ics <- lapply(split(ls, ls$iclass), function(el, ic.num, gterm){
        mld <- apply(el[,grep("loads", names(el)), drop = FALSE][,1:ic.num], 2, mean)
        el$OP <- rowSums(t(mld*t(el[,grep("score", names(el)), drop = FALSE][,1:ic.num])))
        el$dev <- el$CVE - rowSums(el[,grep("fitted", names(el)),drop = FALSE][,1:ic.num])
        el$RMSD <- sqrt(tapply(el$dev^2, el[[gterm]], mean)[as.character(el[[gterm]])])
        el
    }, ic.num, gterm)
    icd <- do.call("rbind.data.frame", ics)
    icd[order(icd[[sterm]], icd[[gterm]]),]
}














