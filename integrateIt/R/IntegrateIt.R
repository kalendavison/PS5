#' Calculate definite integrals
#'
#' Calculates definite integrals using Simpson and Trapezoid method
#'
#' @param xvalues A numeric object
#' @param yvalues A numeric object with the same dimensionality as \code{xvalues}.
#' @param start A numeric object of length 1
#' @param end A numeric object of length 1
#' @param rule A character string equal to either "Trapezoid" or "Simpson"
#'
#' @return An object of class Trapezoid or Simpson containing
#'  \item{xvalues}{The first object input}
#'  \item{yvalues}{The second object input}
#'  \item{area}{The area under the curve} 
#' @author Kalen J. Davison
#' @note The estimate may vary depending on the method of integration chosen
#' @examples
#' 
#' myX <- c(1:20) 
#' myY <- c(21:40) 
#' integrateIt(myX, myY, 3, 18, "Trapezoid")
#' integrateIt(myX, myY, 3, 18, "Simpson")
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
            if (length(xvalues) != length(yvalues)){
              stop("xvalues and yvalues must be of equal length.")
            }
            xvalues = xvalues[start]:xvalues[end]
            yvalues = yvalues[start]:yvalues[end]
            height = (end - start) / (length(xvalues) - 1)
            h = height
            
            if (rule == "Trapezoid"){
              Trap_area = (h/2)*((2*(sum(yvalues))-yvalues[1]-yvalues[length(yvalues)])) 
              return (new("Trapezoid", xvalues = xvalues, yvalues = yvalues, area = Trap_area)) }
              
            if (rule == "Simpson"){
              first = yvalues[1]
              last = yvalues[length(yvalues)]
              middle = yvalues[2:(length(xvalues)-1)]
              odd_middle = 4*(middle[seq(1,length(middle),2)])
              even_middle = 2*(middle[seq(2,length(middle),2)])
              
              Simp_area = (h/3) * (sum(first, sum(odd_middle), sum(even_middle), last))
              return (new("Simpson", xvalues = xvalues, yvalues = yvalues, area = Simp_area)) }
            })
