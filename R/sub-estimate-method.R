#' Extract or replace subsets of \code{\link[decisionSupport]{estimate}}
#'
#' subset operator for estimate class from decisionSupport package.
#'
#' @author Issoufou Liman
#' @param x estimate object
#' @param i,j elements to extract or replace by index or dimnames.
#' @param drop logical. If TRUE the result is coerced to the lowest possible dimension. The default is to drop if only one column is left, but not to drop if only one row is left.
#' @details Attempt to drop any of the required estimate columns (i.e distribution, min, max )
#' is silently ignored since estimate object may not make sense without them.
#' @examples
#' ## This example is taken fom decisionSupport
#' library (decisionSupport)
#' # Create an estimate from text (with correlated components):
#' estimateTextMarg<-'variable,  distribution, lower, upper
#' revenue1,  posnorm,      100,   1000
#' revenue2,  posnorm,      50,    2000
#' costs1,    posnorm,      50,    2000
#' costs2,    posnorm,      100,   1000'
#' estimateTextCor<-',         revenue1, costs2
#' revenue1,        1,   -0.3
#' costs2,       -0.3,      1'
#' estimateCor <- as.estimate (read.csv (header=TRUE, text=estimateTextMarg,
#' strip.white = TRUE, stringsAsFactors = FALSE),
#' correlation_matrix = data.matrix (read.csv (text = estimateTextCor,
#' row.names = 1, strip.white = TRUE)))
#' ## extracting the first row.
#' estimateCor [1, ]
#' ## extracting all the rows keep everything intact.
#' estimateCor [1:4, ]
#' ## Trying to drop mandatory column is ignored.
#' estimateCor [, 1]
#' @rdname sub-estimate-method
#' @importFrom decisionSupport estimate
#' @importFrom decisionSupport as.estimate
#' @export
`[.estimate` <- function(x, i, j, drop = FALSE) {
    # removing the class attr
    unclassed_x <- unclass(x)
    # getting the corrrelation matrix embedded in estimate, if any
    cor_x <- unclassed_x$correlation_matrix
    # extracting the data.frame embedded in the estimate
    x <- unclassed_x$marginal
    if (!(missing(j))) {
        if (is.character(j)) {
            # mandatory estimates columns should not be concerned by names matching
            mand_cols <- colnames(x)[colnames(x) %in% c("distribution", "lower", "upper")]
            j <- j[!(j %in% mand_cols)]
            j <- c(j, mand_cols)
        } else if (is.integer(as.integer(j))) {
            # mandatory estimates columns should not be concerned by index matching
            mand_cols <- which(colnames(x) %in% c("distribution", "lower", "upper"))
            j <- unique(c(j, mand_cols))
        }
    } else if (missing(j)) {
        # j <- TRUE J <- missing(j)
        j = 1:ncol(x)
    }
    x <- x[i, j, drop = drop]
    cor_x <- cor_x[rownames(cor_x) %in% rownames(x), colnames(cor_x) %in% rownames(x)]
    if (any(is.null(dimnames(cor_x)))) 
        cor_x <- NULL
    x <- as.estimate(x, correlation_matrix = cor_x)
    return(x)
}
