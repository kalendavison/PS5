
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
integrateIt(x, y, min(x), max(x), "Trapezoid")
test = integrateIt(x, y, min(x), max(x), "Simpson")
print(test)


