
library(devtools)
library(roxygen2)
setwd("/Users/kalendavison/Desktop/Applied Statistical Programming/GitHub/PS5")

current.code <- as.package("integrateIt")
load_all(current.code)
document(current.code)
check(current.code)