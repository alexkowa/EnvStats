#' Convert a Long Format Data Set into a Wide Format
#' @description
#' Given a data frame or matrix in long format, convert it to wide format based on
#'   the levels of two variables in the data frame.  This is a simplified version of
#'   the \R function \code{\link{reshape}} with the argument \code{direction="wide"}.
#' @usage
#' longToWide(x, data.var, row.var, col.var,
#'     row.labels = levels(factor(x[, row.var])),
#'     col.labels = levels(factor(x[, col.var])),
#'     paste.row.name = FALSE, paste.col.name = FALSE, sep = ".",
#'     check.names = FALSE, ...)
#' @rawRd
#' \arguments{
#'   \item{x}{
#'   data frame or matrix to convert to wide format.  Must have at least 3 columns
#'   corresponding to the data variable, row variable, and column variable, respectively.
#' }
#'   \item{data.var}{
#'   character string or numeric scalar indicating column variable name in \code{x} for
#'   data values.
#' }
#'   \item{row.var}{
#'   character string or numeric scalar indicating column variable name in \code{x} for
#'   defining rows of output.  The indicated column in \code{x} cannot have missing values.
#' }
#'   \item{col.var}{
#'   character string or numeric scalar indicating column variable name in \code{x} for
#'   defining columns of output.  The indicated column in \code{x} cannot have missing values.
#' }
#'   \item{row.labels}{
#'   optional character vector indicating labels to use for rows.  The default value is the levels
#'   of the variable indicated by \code{row.var} when coerced to a factor.
#' }
#'   \item{col.labels}{
#'   optional character vector indicating labels to use for columns.  The default value is the levels
#'   of the variable indicated by \code{col.var} when coerced to a factor.
#' }
#'   \item{paste.row.name}{
#'   logical scalar indicating whether to paste the name of the variable used to define the row names
#'   (i.e., the value of \code{row.var}) in front of the values defining the row names.
#'   The default value is \code{paste.row.name=FALSE}.
#' }
#'   \item{paste.col.name}{
#'   logical scalar indicating whether to paste the name of the variable used to define the column names
#'   (i.e., the value of \code{col.var}) in front of the values defining the column names.
#'   The default value is \code{paste.col.name=FALSE}.
#' }
#'   \item{sep}{
#'   character string separator used when \code{paste.row.name=TRUE} and/or \cr
#'   \code{paste.col.name=TRUE}.  The default value is \code{sep="."}.
#' }
#'   \item{check.names}{
#'   argument to \code{\link{data.frame}}.  Used to convert the return value to a data frame when
#'   the argument \code{x} is a data frame.  This argument is ignored if \code{x} is a matrix.
#' }
#'
#'   \item{\dots}{
#'   other arguments to \code{\link{data.frame}}.  This argument is ignored if \code{x} is a matrix.
#' }
#' }
#' @rawRd
#' \details{
#'   The combination of values in \code{x[, row.var]} and \code{x[, col.var]} must yield
#'   \eqn{n} unique values, where \eqn{n} is the number of rows in \code{x}.
#' }
#' @rawRd
#' \value{
#'   \code{longToWide} returns a matrix when \code{x} is a matrix and a data frame when \code{x}
#'   is a data frame.  The number of rows is equal to the number of
#'   unique values in \code{x[, row.var]} and the number of columns is equal to the number of
#'   unique values in \code{x[, col.var]}.
#' }
#' @rawRd
#' \author{
#'   Steven P. Millard (\email{EnvStats@ProbStatInfo.com}),
#'   based on a template from Phil Dixon.
#' }
#' @rawRd
#' \seealso{
#'   \code{\link{reshape}}, \code{\link{data.frame}}, \code{\link{matrix}}.
#' }
#' @rawRd
#' \examples{
#'   EPA.09.Ex.10.1.nickel.df
#'   #   Month   Well Nickel.ppb
#'   #1      1 Well.1       58.8
#'   #2      3 Well.1        1.0
#'   #3      6 Well.1      262.0
#'   #4      8 Well.1       56.0
#'   #5     10 Well.1        8.7
#'   #6      1 Well.2       19.0
#'   #7      3 Well.2       81.5
#'   #8      6 Well.2      331.0
#'   #9      8 Well.2       14.0
#'   #10    10 Well.2       64.4
#'   #11     1 Well.3       39.0
#'   #12     3 Well.3      151.0
#'   #13     6 Well.3       27.0
#'   #14     8 Well.3       21.4
#'   #15    10 Well.3      578.0
#'   #16     1 Well.4        3.1
#'   #17     3 Well.4      942.0
#'   #18     6 Well.4       85.6
#'   #19     8 Well.4       10.0
#'   #20    10 Well.4      637.0
#'
#'   longToWide(EPA.09.Ex.10.1.nickel.df,
#'     "Nickel.ppb", "Month", "Well", paste.row.name = TRUE)
#'   #         Well.1 Well.2 Well.3 Well.4
#'   #Month.1    58.8   19.0   39.0    3.1
#'   #Month.3     1.0   81.5  151.0  942.0
#'   #Month.6   262.0  331.0   27.0   85.6
#'   #Month.8    56.0   14.0   21.4   10.0
#'   #Month.10    8.7   64.4  578.0  637.0
#' }
#' @rawRd
#' \keyword{manip}

longToWide <-
function (x, data.var, row.var, col.var, row.labels = levels(factor(x[, 
    row.var])), col.labels = levels(factor(x[, col.var])), paste.row.name = FALSE, 
    paste.col.name = FALSE, sep = ".", check.names = FALSE, ...) 
{
    if (!((is.df <- is.data.frame(x)) || is.matrix(x)) || ncol(x) < 
        3) 
        stop("'x' must be a data frame or matrix with at least 3 columns")
    if (!is.df) 
        x <- data.frame(x)
    names.x <- names(x)
    all.char <- is.character(data.var) && is.character(row.var) && 
        is.character(col.var)
    all.num <- is.numeric(data.var) && is.numeric(row.var) && 
        is.numeric(col.var)
    if (length(data.var) != 1 || length(row.var) != 1 || length(col.var) != 
        1 || !(all.char || all.num) || length(unique(c(row.var, 
        col.var, data.var))) != 3) 
        stop(paste("'data.var', 'row.var', and 'col.var' must be", 
            "all character scalars or all numeric scalars, and must be unique"))
    if (all.char) {
        if (!all(c(row.var, col.var, data.var) %in% names.x)) 
            stop(paste("When 'row.var', 'col.var', and 'data.var' are character scalars,", 
                "they must match column names in 'x'"))
    }
    else {
        if (!all(c(row.var, col.var, data.var) %in% 1:ncol(x))) 
            stop(paste("When 'row.var', 'col.var', and 'data.var' are numeric scalars,", 
                "they must match column numbers in 'x'"))
    }
    row.vals <- x[, row.var]
    col.vals <- x[, col.var]
    if (any(is.na(row.vals)) || any(is.na(col.vals))) 
        stop("Missing values not allowed in the variables used to define rows and columns")
    if (length(unique(paste(row.vals, col.vals, sep = "."))) != 
        nrow(x)) 
        stop("The value combinations using 'row.var' and 'col.var' must be unique")
    if (!missing(row.labels) && !is.character(row.labels) || 
        ((lrl <- length(row.labels)) != length(unique(row.labels)) || 
            lrl != length(levels(factor(row.vals))))) 
        stop(paste("'row.labels' must be a character vector and the number of", 
            "unique values in 'row.labels' must match the number of unique values", 
            "in the column specified by 'row.var'"))
    if (!missing(col.labels) && !is.character(col.labels) || 
        ((lcl <- length(col.labels)) != length(unique(col.labels)) || 
            lcl != length(levels(factor(col.vals))))) 
        stop(paste("'col.labels' must be a character vector and the number of", 
            "unique values in 'col.labels' must match the number of unique values", 
            "in the column specified by 'col.var'"))
    if (paste.row.name || paste.col.name) {
        if (all.num) {
            row.var <- names.x[row.var]
            col.var <- names.x[col.var]
        }
        if (paste.row.name) 
            row.labels <- paste(row.var, row.labels, sep = sep)
        if (paste.col.name) 
            col.labels <- paste(col.var, col.labels, sep = sep)
    }
    ret.val <- tapply(x[, data.var], list(row.vals, col.vals), 
        FUN = function(x) x)
    dimnames(ret.val) <- list(as.character(row.labels), as.character(col.labels))
    if (is.df) 
        ret.val <- data.frame(ret.val, check.names = check.names, 
            ...)
    ret.val
}
