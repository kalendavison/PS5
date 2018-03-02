# Kalen Davison
# Professor Montgomery
# Problem Set 5
# Due 3/6

### Creating a basic function to calculate definite integrals. The function takes as inputs a numeric 
### vector of xvalues and yvalues,a start and end value with a maximum and minimum default, and the 
### choice of which method to use. It returns the given coordinates, the estimated integral, and the method used.
integrateIt <- function(xvalues, yvalues, start, end, rule){
  start = min(xvalues)
  end = max(xvalues)
  height = (end - start) / (length(xvalues) - 1)
  h = height
  
  if (rule == "Trapezoid"){
    Trap = (h/2)*((2*sum(yvalues))-yvalues[1]-yvalues[length(yvalues)]) 
    return (c(Trap, xvalues, yvalues, rule)) }
  
  if (rule == "Simpson"){
    first = yvalues[1]
    last = yvalues[length(yvalues)]
    second_last = 4*(yvalues[length(yvalues)-1])    
    middle = yvalues[2:(length(xvalues)-1)]
    odd_middle = 4*(middle[seq(1,length(middle),2)])
    even_middle = 2*(middle[seq(2,length(middle),2)])
    
    Simp = (h/3) * (sum(first+odd_middle+even_middle+second_last+last))
    return (c(Simp, xvalues, yvalues, rule)) }
}


integrateIt(1:10, 11:20, 1, 20, "Simpson") 
integrateIt(1:10, 11:20, 1, 20, "Trapezoid") #formatting is good on tests but results are not the same suggesting inaccuracy


setClass(Class = "Trapezoid",
         representation = representation(
           xvalues = "integer",
           yvalues = "integer",
           area = "numeric"
         ),
         prototype = prototype(
           xvalues = c(),
           yvalues = c(),
           area = c()
         )
)

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

new("Trapezoid", xvalues = 1:10, yvalues= 11:20, area = 265)



setClass(Class = "Simpson",
         representation = representation(
           xvalues = "integer",
           yvalues = "integer",
           area = "numeric"
         ),
         prototype = prototype(
           xvalues = c(),
           yvalues = c(),
           area = c()
         )
)


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

new("Simpson", xvalues = 1:10, yvalues= 11:20, area = 265)

#one generic and two methods




setMethod("initialize", "Trapezoid", function(.Object, ...) { #initilize method 
  value = callNextMethod()
  validObject(value)
  return(value)
})
