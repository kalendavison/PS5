#' Prints an integral approximation
#'
#' 
#'
#' @param x An object of class Trapezoid or Simpson
#'
#' @return A numeric
#' @author Kalen Davison
#' @note Please Work 
#' @examples
#' 

#' @seealso \code{\link{integrateIt}}
#' @rdname printfunction
#' @aliases 
#' @export

setMethod("print", "Trapezoid",
          function(x){
            print(x@area)})

setMethod("print", "Simpson",
          function(x){
            print(x@area)})