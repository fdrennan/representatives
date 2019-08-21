library(representatives)
library(reticulate)
library(tidyverse)
library(dbplyr)
library(RPostgreSQL)
library(tictoc)

if (FALSE) {
  install_python(envname = "reps")
  use_virtualenv('reps')
  configure_aws()
}


if (FALSE) {
  if(!'reps' %in% keyfile_list()$key_name)
    keyfile_create('reps', save_to_directory = TRUE)

  if (!'reps' %in% security_group_list()$group_name) {
    security_group_id <- security_group_create('reps')
  } else {
    security_group_id <- filter(security_group_list(), group_name == 'reps')$group_id
  }

  instance_id <- ec2_instance_create(
    image_id = 'ami-05c1fa8df71875112',
    instance_type='t2.nano',
    min = 1,
    max = 1,
    key_name = 'reps',
    security_group_id = security_group_id,
    instance_storage = 20,
    user_data = ec2_instance_script()
  )

  # ec2_instance_stop(instance_id[[1]]$instance_id, terminate = TRUE)

  public_ip_address <-
    ec2_get_info() %>%
    filter(state == 'running') %>%
    filter(launch_time >= Sys.time() - 500) %>%
    pull(public_ip_address) %>%
    str_replace_all("\\.", "-")

  message(
    paste0(
      'ssh -i "reps.pem" ubuntu@ec2-',public_ip_address,'.us-east-2.compute.amazonaws.com'
    )
  )


}


if(TRUE) {
  repeat({
    try(update_tweets())
    Sys.sleep(900)
  })
}

# str_to_lower(screen_name)
