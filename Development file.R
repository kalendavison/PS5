
## Load libraries and set working directory
library(devtools)
library(roxygen2)
setwd("/Users/kalendavison/Desktop/Applied Statistical Programming/GitHub/PS5") #This will need to be changed to match your directory

## This is run once when the package strcuture is first created

## At this point put the *.R files into the correcto directories and edit the DESCRIPTION file

# Now the NAMESPACE

## This can be run many times as the code is updates
current.code <- as.package("integrateIt")
load_all(current.code)
document(current.code)

## Let's look at a function
integrateIt
getMethod(integrateIt)

## Let's try it out
library(integrateIt)
x<-c(1:10)
y<-c(11:20)
integrateIt(x, y, 2, 8, "Trapezoid")
integrateIt(x, y, 2, 8, "Simpson") #return same or similar answers
test = integrateIt(x, y, 1, 10, "Trapezoid")
class(test)
print(test)

current.code <- as.package("integrateIt")
load_all(current.code) # Load all of the functions so you can use them
document(current.code) # Make the help files
check(current.code) # Run the R checks
install(pkg=current.code, local=TRUE) # Install the package
getwd()
build(current.code, path=getwd()) # Build a local copy for you to share

?integrateIt #help file works
integrateIt(1:10, 2:19, 2, 5, "Trapezoid")
