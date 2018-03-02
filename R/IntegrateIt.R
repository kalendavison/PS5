#' Calculate definite integrals
#'
#' Calculates definite integrals using Simpson and Trapezoid method
#'
#' @param xvalues A numeric object
#' @param yvalues A numeric object with the same dimensionality as \code{xvalues}.
#' @param start A numeric object of length 1
#' @param end A numeric object of length 1
#' @param rule A character string equal to either "Trap" or "Simpson"
#'
#' @return An object of class Trapezoid or Simpson containing
#'  \item{xvalues}{The first object input}
#'  \item{yvalues}{The second object input}
#'  \item{area}{The area under the curve} 
#' @author Kalen J. Davison
#' @note The estimate may vary depending on the method of integration chosen
#' @examples
#' 
#' myX <- c(1:10) 
#' myY <- c(11:20) 
#' integrateIT(myX, myY, "Trap")
#' @rdname integrateIt
#' @aliases integrateIt
#' @export
setGeneric(name="integrateIt",
           def=function(xvalues, yvalues, start, end, rule)
           {standardGeneric("integrateIt")}
)

#' @export
setMethod("integrateIt",
          definition=function(xvalues, yvalues, start, end, rule){
            start = min(xvalues)
            end = max(xvalues)
            height = (end - start) / (length(xvalues) - 1)
            h = height
            
            if (rule == "Trapezoid"){
              Trap = (h/2)*((2*sum(yvalues))-yvalues[1]-yvalues[length(yvalues)]) 
              return (new("Trapezoid", xvalues = xvalues, yvalues = yvalues, area = Trap_area)) }
              
            if (rule == "Simpson"){
              first = yvalues[1]
              last = yvalues[length(yvalues)]
              second_last = 4*(yvalues[length(yvalues)-1])    
              middle = yvalues[2:(length(xvalues)-1)]
              odd_middle = 4*(middle[seq(1,length(middle),2)])
              even_middle = 2*(middle[seq(2,length(middle),2)])
              
              Simp_area = (h/3) * (sum(first+odd_middle+even_middle+second_last+last))
              return (new("Simpson", xvalues = xvalues, yvalues = yvalues, area = Simp_area)) }
            })

#' @export
setGeneric("print")

#' @export
setMethod("print",
          definition = function(x) {
  