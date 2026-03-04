#' Pad missing row–column positions within groups
#'
#' Ensures that, within each level of a grouping variable (e.g. a block),
#' every combination of a row factor and a column factor exists.
#' Missing row–column positions are added as new rows with `NA`s.
#'
#' @param data A data frame.
#' @param pattern Character string of the form `"Row:Column"` giving the names of
#'   the two variables that define the row–column layout.
#' @param split Character. Name of the column used to split `data` into groups
#'   (default `"Block"`). Padding is done within each group.
#' @param keep Integer. Column index to copy from existing data into newly added
#'   rows (default `4`). The value copied comes from the first rows of the group
#'   (recycled as needed). This is typically used to keep a constant identifier
#'   (e.g. block label or trial id).
#' @param fill Optional. Integer or character specifying columns to set to `NA`
#'   in newly created rows. Default `NULL` (do nothing).
#'
#' @return A data frame like `data`, with additional rows added for missing
#'   row–column combinations. A column `add` is added, with values `"old"` for
#'   original rows and `"new"` for padded rows. The result is ordered by the
#'   row and column variables in `pattern`.
#'
#' @details
#' For each group defined by `split`, the function forms a contingency table
#' of `Row` by `Column`. For any zero-count cell, it adds a new row where:
#' \itemize{
#'   \item `Row` and `Column` are set to the missing combination,
#'   \item all other columns are `NA` (except `keep`, which is copied),
#'   \item `add` is set to `"new"`.
#' }
#' The row/column factors are then re-leveled to be in increasing numeric order
#' (assuming their levels are numeric strings).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' d <- expand.grid(
#'   Block = factor(1:2),
#'   Row = factor(1:3),
#'   Column = factor(1:3)
#' )
#'
#' # Remove one cell to simulate a missing plot
#' d <- d[-5, ]
#' d # Inspect
#'
#' pad.data(d, pattern = "Row:Column", split = "Block", keep = 1)
#'
#' }
#'

pad.data <- function(data, pattern = "Row:Column", split = "Block", keep = 4, fill = NULL){
    pat <- unlist(strsplit(pattern, ":"))
    if(!(split %in% names(data)))
        stop("split argument not in data")
    if(!all(pat %in% names(data)))
        stop("One or more of the variables in pattern argument not in data")
    spd <- split(data, data[[split]])
    spd <- lapply(spd, function(el, pat){
        temp <- el
        temp <- cbind.data.frame(lapply(temp, function(el){ if(is.factor(el)) factor(el) else el}))
        temp$add <- "old"
        tabs <- table(temp[[pat[1]]], temp[[pat[2]]])
        wh <- which(tabs == 0, arr.ind = TRUE)
        if(dim(wh)[1] > 0){
            tp <- temp[1:nrow(wh),]
            tp <- cbind.data.frame(lapply(tp, function(el) rep(NA, length(el))))
            tp[,keep] <- temp[1:nrow(wh),keep]
            tp[[pat[1]]] <- factor(rownames(tabs)[wh[,1]])
            tp[[pat[2]]] <- factor(colnames(tabs)[wh[,2]])
            if(!is.null(fill))
                tp[,fill] <- NA
            tp$add <- "new"
            temp <- rbind.data.frame(temp, tp)
        }
        temp
    }, pat)
    ad <- do.call("rbind.data.frame", spd)
    ad[[pat[1]]] <- factor(ad[[pat[1]]], levels = as.character(sort(as.numeric(levels(ad[[pat[1]]])))))
    ad[[pat[2]]] <- factor(ad[[pat[2]]], levels = as.character(sort(as.numeric(levels(ad[[pat[2]]])))))
    ad[order(ad[[pat[1]]],ad[[pat[2]]]),]
}
