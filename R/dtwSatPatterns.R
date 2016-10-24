#' The DTWSATPATTERNS class
#'
#' Use this class for representing a DTWSATPATTERNS object
#'
#'
#'
#' @note No notes
#' @name dtwSatPatterns
#' @aliases dtwSatPatterns-class
#' @exportClass dtwSatPatterns
#' @author Alber Sanchez
#' @import methods
#' @import roxygen2
#' @import testthat
setClass (
  Class = "dtwSatPatterns",
  representation = representation(),
  validity = function(object){}
)


#*******************************************************
# CONSTRUCTOR
#*******************************************************
setMethod (
  f="initialize",
  signature="dtwSatPatterns",
  definition=function(.Object){
    validObject(.Object)
    return(.Object)
  }
)
#CONSTRUCTOR (USER FRIENDLY)
#' Creates a dtwSatPatterns object
#'
#' @rdname dtwSatPatterns
#' @docType methods
#' @export
dtwSatPatterns <- function(){
  new (Class="dtwSatPatterns")
}



#*******************************************************
# ACCESSORS
#*******************************************************



#*******************************************************
# FUNCTIONS
#*******************************************************


