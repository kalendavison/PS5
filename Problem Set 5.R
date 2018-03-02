# Kalen Davison
# Professor Montgomery
# Problem Set 5
# Due 3/6

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
  check_length1 = (length(objectxvalues >= 1))
  check_length2 = (length(object@yvalues >= 1))
  check_length3 = (length(object@range == 2))
  check_value4 = object@rule == "Trap" | object@rule == "Simpson"

  
  if (!check_length1){return("object@xvalues is not valid, must be of a length greater than 1")}
  if (!check_length2){return("object@yvalues is not valid, must be of a length greater than 1")}
  if (!check_length3){return("object@range must be of length 2")}
  if (!check_value4){return("object@rule must either be 'Trap' or 'Simpson'")}
}
)

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
  check_length1 = (length(objectxvalues >= 1))
  check_length2 = (length(object@yvalues >= 1))
  check_length3 = (length(object@range == 2))
  check_value4 = object@rule == "Trap" | object@rule == "Simpson"
  
  
  if (!check_length1){return("object@xvalues is not valid, must be of a length greater than 1")}
  if (!check_length2){return("object@yvalues is not valid, must be of a length greater than 1")}
  if (!check_length3){return("object@range must be of length 2")}
  if (!check_value4){return("object@rule must either be 'Trap' or 'Simpson'")}
}
)




IntegrateIt <- function(xvalues, yvalues, start, end, rule){
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


IntegrateIt(1:20, 21:40, 1, 20, "Simpson")

#one generic and two methods


