#' R-based Qualitative Data Analysis package
#'
#' Qualitative Data Analysis based on R language. Current version supports plain text. In addition, it can import PDF highlights.
#'
#' @details
#' The workhorse function for end-user is the RQDA(), you can use RQDA() to start the GUI after library(RQDA). Please Refer to the documentation section of the project homepage for the usage of RQDA.
#'
#' The position of ViewFile widget can be controlled by "widgetCoordinate" options, with default value c(400, 2). You can change it by options("widgetCoordinate"=c(x, y)), where x and y are integers specifying the position.
#'
#' The size of many widgets (e.g. ViewFile widgets) can be controlled by "widgetSize" options, with default value c(550, 700). You can change it by options("widgetSize"=c(x, y)), where x and y are integers specifying width and height.
#'
#' @author Ronggui Huang \email{ronggui.huang@gmail.com}
#'
#' @references
#' Kelle, U. (ed). 1995. "Computer-aided qualitative data analysis: theory, methods and practice." Sage Publications.
#'
#' Lewins, A. & Silver, C.2007. Using Software in Qualitative Research : A Step-by-Step Guide. Sage Publications.
#'
#' Kuckartz, Udo. 2014. Qualitative Text Analysis. Sage Publications.
#'
#' @examples
#' \dontrun{library(RQDA)
#' RQDA()}
#'
#' @name RQDA
#' @docType package
#' @import RGtk2
#' @import gWidgets2
#' @import gWidgets2RGtk2
#' @import RSQLite
#' @importFrom graphics plot
#' @importFrom grDevices colors
#' @importFrom methods getFunction slot
#' @importFrom stats reshape
#' @importFrom utils browseURL citation packageDescription write.csv
#' @include rqda-env.R
NULL

