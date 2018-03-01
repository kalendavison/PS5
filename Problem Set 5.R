# Kalen Davison
# Professor Montgomery
# Problem Set 5
# Due 3/6

setClass(Class = "Trapezoid",
         representation = representation(
           xvalues = "integer",
           yvalues = "integer",
           range = "integer",
           rule = "character"
         ),
         prototype = prototype(
           xvalues = c(),
           yvalues = c(),
           range = c(),
           rule = c()
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
           range = "integer",
           rule = "character"
         ),
         prototype = prototype(
           xvalues = c(),
           yvalues = c(),
           range = c(),
           rule = c()
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




IntegrateIt

#one generic and two methods