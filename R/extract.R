#' Extract a rectangular grid region within groups and optionally pad missing cells
#'
#' Given a data frame containing two grid coordinate columns (e.g. Row and Column),
#' this function splits the data into groups (e.g. by Block), identifies the
#' bounding rectangle of rows whose \code{Type} matches \code{match}, then returns
#' all rows within that rectangle for each group. Optionally, missing Row-by-Column
#' combinations inside the rectangle are padded by adding new rows.
#'
#' @param data A data frame containing the grouping column specified by \code{split},
#'   the two coordinate columns specified by \code{pattern}, and a column named
#'   \code{Type} used for matching.
#' @param pattern A length-1 character string of the form \code{"<row>:<col>"} giving
#'   the names of the two coordinate columns (default \code{"Row:Column"}).
#' @param match A character vector of \code{Type} values used to define the
#'   bounding rectangle within each group (default \code{"DH"}).
#' @param split A length-1 character string giving the column name used to split
#'   \code{data} into groups (default \code{"Block"}).
#' @param pad Logical; if \code{TRUE}, pad missing coordinate combinations inside the
#'   rectangle by adding new rows (default \code{TRUE}).
#' @param keep Integer index (or indices) of columns to copy from existing rows into
#'   padded rows. Values are copied from the first \code{n_missing} rows of the extracted
#'   rectangle. Default is \code{4}.
#' @param fill Optional integer index (or indices) of columns to fill with the
#'   string \code{"Blank"} in padded rows. If \code{NULL} (default), nothing is filled.
#'
#' @details
#' For each group defined by \code{split}, rows with \code{Type} in \code{match} are used
#' to determine the minimum and maximum values of the two coordinate columns specified
#' by \code{pattern}. All rows in the group whose coordinates fall within these inclusive
#' bounds are returned.
#'
#' When \code{pad = TRUE}, the function constructs a contingency table of the extracted
#' coordinates and adds rows for any missing Row-by-Column combinations. Existing rows are
#' marked with \code{add = "old"} and padded rows with \code{add = "new"}.
#'
#' @return
#' A data frame containing the extracted (and optionally padded) rows from all groups,
#' sorted by the first and second coordinate columns. If \code{pad = TRUE}, an additional
#' column \code{add} is included to indicate whether a row is original (\code{"old"}) or
#' padded (\code{"new"}).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   Block = rep(1:2, each = 6),
#'   Type  = rep(c("DH", "X"), times = 6),
#'   Row   = rep(c("1","1","2"), times = 4),
#'   Column= rep(c("1","2"), times = 6),
#'   Value = seq_len(12)
#' )
#'
#' # Extract rectangle defined by Type == "DH" within each Block and pad missing cells
#' out <- extract(df, pattern = "Row:Column", match = "DH", split = "Block",
#'                pad = TRUE, keep = 5, fill = 4)
#' head(out)
#' }
extract <- function(data, pattern = "Row:Column", match = "DH", split = "Block", pad = TRUE, keep = 4, fill = NULL){
    pat <- unlist(strsplit(pattern, ":"))
    if(!(split %in% names(data)))
        stop("split argument not in data")
    if(!all(pat %in% names(data)))
        stop("One or more of the variables in pattern argument not in data")
    spd <- split(data, data[[split]])
    spd <- lapply(spd, function(el, match, pat, pad){
        temp <- el[as.character(el$Type) %in% match,]
        print(dim(temp))
        rr <- range(as.numeric(as.character(temp[,pat[1]])))
        rc <- range(as.numeric(as.character(temp[,pat[2]])))
        print(rr)
        print(rc)
        elr <- (1:nrow(el))[el[[pat[1]]] %in% as.character(rr[1]:rr[2])]
        elc <- (1:nrow(el))[el[[pat[2]]] %in% as.character(rc[1]:rc[2])]
        ela <- intersect(elr, elc)
        temp <- el[ela[order(ela)],]
        temp <- cbind.data.frame(lapply(temp, function(el){ if(is.factor(el)) factor(el) else el}))
        if(pad){
            temp$add <- "old"
            tabs <- table(temp[[pat[1]]], temp[[pat[2]]])
            wh <- which(tabs == 0, arr.ind = TRUE)
            if(length(wh)){
                whn <- pmatch(pat, names(temp))
                tp <- temp[1:nrow(wh),]
                tp <- cbind.data.frame(lapply(tp, function(el) rep(NA, length(el))))
                tp[,keep] <- temp[1:nrow(wh),keep]
                tp[[pat[1]]] <- factor(rownames(tabs)[wh[,1]])
                tp[[pat[2]]] <- factor(colnames(tabs)[wh[,2]])
                if(!is.null(fill))
                    tp[,fill] <- "Blank"
                tp$add <- "new"
                temp <- rbind.data.frame(temp, tp)
            }
        }
        temp
    }, match, pat, pad)
    ad <- do.call("rbind.data.frame", spd)
    ad[[pat[1]]] <- factor(ad[[pat[1]]], levels = as.character(sort(as.numeric(levels(ad[[pat[1]]])))))
    ad[[pat[2]]] <- factor(ad[[pat[2]]], levels = as.character(sort(as.numeric(levels(ad[[pat[2]]])))))
    ad[order(ad[[pat[1]]],ad[[pat[2]]]),]
}
