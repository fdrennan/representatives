# global reference to scipy (will be initialized in .onLoad)
#' boto module
boto <- NULL
#' subprocess module
subprocess <- NULL
#' sys module
sys <- NULL
#' @import reticulate
.onLoad <- function(libname, pkgname) {
  # use superassignment to update global reference to scipy
  scipy <<- reticulate::import("scipy", delay_load = TRUE)
  subprocess <<- reticulate::import("subprocess", delay_load = TRUE)
  boto <<- reticulate::import("boto3", delay_load = TRUE)
  sys <<- reticulate::import("sys", delay_load = TRUE)
}


#' configure_aws
#' @importFrom configr read.config
#' @export configure_aws
configure_aws <- function() {

  config_data <- configr::read.config('config.yaml')$aws
  access_key <-
    paste("aws configure set aws_access_key_id", config_data$aws_access_key_id)

  aws_secret_access_key <-
    paste("aws configure set aws_secret_access_key", config_data$aws_secret_access_key)

  default_region <-
    paste("aws configure set default.region", config_data$default.region)

  subprocess$call(access_key, shell=TRUE)
  subprocess$call(aws_secret_access_key, shell=TRUE)
  subprocess$call(default_region, shell=TRUE)

}

#' install_python: sets up python environment for awsR
#' @param method  method argument for py_install
#' @param conda  conda argument for py_install
#' @param envname  a conda or python virtual environment name
#' @export install_python
install_python <- function (method = "auto", conda = "auto", envname = 'r_reticulate')  {
  reticulate::py_install("awscli", method = method, conda = conda, envname = envname)
  reticulate::py_install("boto3", method = method, conda = conda, envname = envname)
}



