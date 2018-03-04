#' Creates a Simpson class
#'
#' 
#'
#' @param xvalues A numeric object
#' @param yvalues A numeric object with the same dimensionality as \code{xvalues}.
#' @param area A numeric
#'
#' @return creates Simpson Class
#' @author Kalen Davison
#' @note Creates Simpson class with three slots
#' @examples
#' 

#' @seealso \code{\link{integrateIt}}
#' @rdname simpsonclass
#' @aliases 
#' @export
setClass(Class = "Simpson",
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
setValidity("Simpson", function(object){ 
  areaLength = (length(object@area == 1))
  valuesLength = (length(object@xvalues) == length(object@yvalues))
  
  if(!areaLength){
    return("Simpson not valid! Area must be of length 1.")
  }
  if(!valuesLength){
    return("Simpson is not valid! xvalues and yvalues must be of the same length.")
  }
})

#' @export
setMethod("initialize", "Simpson", function(.Object, ...) {  
  value = callNextMethod()
  validObject(value)
  return(value)
})