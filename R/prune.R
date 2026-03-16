#' Prune a pedigree to individuals present in a dataset
#'
#' Removes individuals from a pedigree that are not represented in a data set,
#' keeping only the subset of the pedigree required for the specified individuals
#' and their ancestors up to a given number of generations.
#'
#' This function is based on the `prune()` implementation from the
#' *pedicure* package developed at the University of Wollongong
#' (David Butler, 2016).
#'
#' @param ped A pedigree data frame. The first column contains individual IDs,
#'   the second column the dam (mother), and the third column the sire (father).
#' @param data A data frame containing individuals to retain. One column must
#'   correspond to the individual IDs in `ped`.
#' @param gen Optional integer specifying the number of generations of ancestors
#'   to retain. If `NULL`, the maximum number of generations present in the
#'   pedigree is used.
#'
#' @return A pruned pedigree data frame containing only the individuals required
#'   for the specified data individuals and their ancestors.
#'
#' @details
#' The function identifies individuals present in `data`, then constructs a
#' pedigree structure and calls `pedigree::trimPed()` to retain the required
#' individuals and their ancestors. Individuals in `data` that are absent from
#' `ped` will trigger a warning.
#'
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ped <- data.frame(
#'   id   = c("A","B","C","D"),
#'   dam  = c(NA, NA, "A", "A"),
#'   sire = c(NA, NA, "B", "B")
#' )
#'
#' data <- data.frame(id = c("C","D"))
#'
#' prune(ped, data)
#' }
prune <- function(ped, data, gen = NULL)
{
    if(is.na(which <- match(names(ped)[1],names(data))))
        stop(paste("Cannot find",names(ped)[1],"in data"))
    if(any(is.na(match(as.character(data[[which]]), as.character(ped[,1])))))
        warning("There are individuals in 'data' that are absent in 'ped'")

    data <- as.numeric(!is.na(match(as.character(ped[,1]),
                                    as.character(data[[which]]))))
    mmd <- data.frame(id = 1:nrow(ped),
                      dam = match(ped[,2],ped[,1],nomatch = 0),
                      sire = match(ped[,3],ped[,1],nomatch = 0))
    print(mmd)
    if(is.null(gen)) {
        gen <- max(countGen(mmd))
    }
    what <- pedigree::trimPed(mmd, data, gen)
    return(ped[what,])
}

#' Count generations in a pedigree (Does this makes sense for plant breeding??)
#'
#' Computes the generation number for each individual in a pedigree structure.
#' Founders (individuals with no recorded parents) are assigned generation 1,
#' and descendants are assigned one plus the maximum generation of their
#' parents.
#'
#' @param ped A data frame or matrix with columns:
#' \describe{
#'   \item{id}{Individual index}
#'   \item{dam}{Index of the dam (mother), or 0 if unknown}
#'   \item{sire}{Index of the sire (father), or 0 if unknown}
#' }
#'
#' @return A numeric vector giving the generation number for each individual.
#'
#' @details
#' The function iteratively propagates generation numbers through the pedigree
#' until all individuals have been assigned a generation level.
#'
#' @keywords internal
countGen <- function(ped)
{
  n <- nrow(ped)
  gen <- rep(NA_integer_, n)

  # founders
  founders <- ped$dam == 0 & ped$sire == 0
  gen[founders] <- 1

  repeat {
    old <- gen

    for (i in seq_len(n)) {
      if (is.na(gen[i])) {

        dam  <- ped$dam[i]
        sire <- ped$sire[i]

        dam.gen  <- if (dam  > 0) gen[dam]  else 1
        sire.gen <- if (sire > 0) gen[sire] else 1

        if (!is.na(dam.gen) && !is.na(sire.gen)) {
          gen[i] <- max(dam.gen, sire.gen) + 1
        }
      }
    }

    if (identical(old, gen)) break
  }

  gen
}
