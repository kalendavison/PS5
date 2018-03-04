#' Creates a Trapezoid class
#'
#' 
#'
#' @param xvalues A numeric object
#' @param yvalues A numeric object with the same dimensionality as \code{xvalues}.
#' @param area A numeric
#'
#' @return creates Simpson Class
#' @author Kalen Davison
#' @note Creates Trapezoid class with three slots
#' @examples
#' 

#' @seealso \code{\link{integrateIt}}
#' @rdname trapezoidclass
#' @aliases 
#' @export
setClass(Class = "Trapezoid",
         representation = representation(
           xvalues = "numeric",
           yvalues = "numeric",
           area = "numeric"
         ),
         prototype = prototype(
           xvalues = c(),
           yvalues = c(),
           area = c()
         )
)

#' @export
setValidity("Trapezoid", function(object){ 
  areaLength = (length(object@area == 1)) 
  valuesLength = (length(object@xvalues) == length(object@yvalues))
  
  if(!areaLength){
    return("Trapezoid not valid! Area must be of length 1.")
  }
  if(!valuesLength){
    return("Trapezoid is not valid! xvalues and yvalues must be of the same length.")
  }
})

#' @export
setMethod("initialize", "Trapezoid", function(.Object, ...) { 
  value = callNextMethod()
  validObject(value)
  return(value)
})