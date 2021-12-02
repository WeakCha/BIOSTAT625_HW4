library(testthat)
library(BIOSTAT625)

#data <- read.csv("data/heart.csv")

install.packages("dplyr", repos = "http://cran.us.r-project.org")
install.packages("lattice", repos = "http://cran.us.r-project.org")
install.packages("Matrix", repos = "http://cran.us.r-project.org")
install.packages("survival", repos = "http://cran.us.r-project.org")
install.packages("glmnet", repos = "http://cran.us.r-project.org")

library(dplyr)
library(lattice)
library(Matrix)
library(glmnet)
library(survival)

test_check("BIOSTAT625")
