#' Generates X-bar and R plots for a given set of data
#' 
#' XbarRqcc can accept either a column vector with the number of subgroups, or a column
#' vector with the subgroup sample size (assuming all groups the same size), or an mxn matrix with 
#' m subgroups each of size n. XbarRqcc will generate the Xbar and R graphs and return
#' the qcc objects for each graph in a list.
#' XbarRqcc forces the use of R-bar estimates of sigma.
#' @name XbarRqcc
#' @author Thomas Hopper \email{tomhopper@gmail.com}
#' @param x Required. Either a column vector of subgroup observations or a matrix where each row is a subgroup of observations. Must be type double or integer.
#' @param subgroups Optional. If x is a column vector, either subgroups or subgroupsize is required. subgroups is the number of subgroups to use when calculating subgroup averages.
#' @param subgroupsize Optional. If x is a column vector, either subgroups or subgroupsize is required. subgroupsize is the number of observations in each subgroup to use when calculating subgroup averages.
#' @param title.X Optional. The title for the X-bar chart.
#' @param title.R Optional. The title for the moving range chart.
#' @return \item{XbarRqcc()[[1]]}{contains the subgroup (type = xbar) qcc object}
#' @return \item{XbarRqcc()[[2]]}{contains the moving range (type = R) qcc object}
#' @references http://cran.r-project.org/web/packages/qcc/qcc.pdf
#' @references http://www.r-project.org/doc/Rnews/Rnews_2004-1.pdf
#' @references http://www.qualitydigest.com/inside/six-sigma-column/right-and-wrong-ways-computing-limits.html
#' @references http://r-resources.massey.ac.nz/161325examples/examplesli29.html
libresult <- require(qcc)
if(libresult == FALSE)
  stop("Could not load library qcc. XbarRqcc relies on qcc.")

XbarRqcc <- function(x, subgroups=0, subgroupsize=0, title.X="", title.R="", data.name="", ...) {
  # Error checking function stops and ends on error. No graceful exits, here.
  if(!is.integer(x) && !is.double(x))
    stop("Data must be of either type integer or type double.")
  
  # if a column vector is supplied, we need to reshape it into a matrix
  if(NCOL(x) < 2) {
    # Reshape the data into a matrix
    # Error checking
    if (subgroups > 0 && subgroupsize > 0)
      stop("Please supply only one of subgroups or subgroupsize.")
    if(data.name == "")
      data.name <- deparse(substitute(x))
    if(subgroups > 0) {
      x.rows <- subgroups
      x.cols <- length(x)/x.rows
      if(as.integer(x.cols) != x.cols)
        stop("The data cannot be divided evenly into the specifed number of subgroups!")
      x <- matrix(x, nrow = x.rows, ncol = x.cols, byrow=TRUE)
    } else {
      x.cols <- subgroupsize
      x.rows <- length(x)/x.cols
      if(as.integer(x.rows) != x.rows)
        stop("The data cannot be divided evenly into subgroups of size subgroup!")
      x <- matrix(x, nrow = x.rows, ncol = x.cols, byrow=TRUE)
    }
  }
  
  # Force use of R estimate of variation
  std.dev=c("UWAVE-R")
  
  # Get data name
  if(data.name == "")
    data.name <-deparse(substitute(x))
  
  
  # If no X-bar chart title is supplied, create one.
  if(title.X == "") {
    title.X <- paste("X-bar chart for ",(data.name),", using R-bar estimate of sigma")
  }
  
  # If no R chart title supplied, create one.
  if(title.R == "") {
    title.R <- paste("Range chart for ", (data.name),", using R-bar estimate of sigma")
  }
  
  # Using the mxn matrix data, run qcc for the x-bar chart and the R chart.
  x.Xbar <- qcc(x, type="xbar", std.dev=std.dev, title= title.X, data.name=data.name, ...)
  x.R <- qcc(x, type="R", std.dev=std.dev, title= title.R, data.name=data.name, ...)
  
  # Set up the return qcc objects in a list
  XbarRqcc <- list(x.Xbar, x.R)
  
  # Return the qcc objects
  return(XbarRqcc)
}