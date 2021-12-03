# BIOSTAT625_HW4

<!-- badges: start -->
  [![R-CMD-check](https://github.com/WeakCha/BIOSTAT625_HW4/workflows/R-CMD-check/badge.svg)](https://github.com/WeakCha/BIOSTAT625_HW4/actions)
  [![codecov](https://codecov.io/gh/WeakCha/BIOSTAT625_HW4/branch/main/graph/badge.svg?token=QA50NDYI28)](https://codecov.io/gh/WeakCha/BIOSTAT625_HW4)
  <!-- badges: end -->

This package is a linear regression package, whose input is a data (`data.frame`) and output is the prediction result and RMSE. In this package, the input data will be automatically splitted into 2 parts: training data and test data, and then the linear regression model will be trained using training data, while test data is used to do prediction. The prediction result and the RMSE of the test set will be computed. 
