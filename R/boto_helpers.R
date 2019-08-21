#' boto
#' @export boto3
boto3 <- function() {
  import("boto3")
}

#' keyfile_create
#' @importFrom utils write.table
#' @param keyname name for keyfile
#' @param save_to_directory save to working directory
#' @export keyfile_create
keyfile_create <- function(keyname = NA, save_to_directory = TRUE) {

  if(is.na(keyname)) {
    stop("Please supply a name for the keyfile")
  }

  key_pair <- boto3()$client('ec2')$create_key_pair(KeyName=keyname)

  if(save_to_directory) {
    filename = paste0(keyname, ".pem")
    message(
      "Saving file to ", file.path(getwd(), filename)
    )
    write.table(key_pair$KeyMaterial,
                file = filename,
                row.names = FALSE,
                col.names = FALSE,
                quote = FALSE)

    system(paste("chmod 400 ", filename))
  }
  key_pair$KeyMaterial
}

#' keyfile_list
#' @importFrom purrr map_df
#' @importFrom tibble tibble
#' @export keyfile_list
keyfile_list <- function() {
  key_pair <- boto3()$client('ec2')
  response <- key_pair$describe_key_pairs()$KeyPairs
  response %>%
    map_df(
      function(x) {
        tibble(
          key_name = x$KeyName,
          fingerprint = x$KeyFingerprint
        )
      }
    )
}

#' keyfile_delete
#' @param keyname Keyfile name to deletes
#' @export keyfile_delete
keyfile_delete <- function(keyname = NA) {
  if(is.na(keyname)) {
    stop("Please supply a name for the keyfile")
  }
  try(file.remove(paste0(keyname,".pem")))
  key_pair <- boto3()$client('ec2')
  response <- key_pair$delete_key_pair(KeyName = keyname)
  if(response$ResponseMetadata$HTTPStatusCode == 200) {
    TRUE
  }
}

#' security_group_list
#' @importFrom purrr map_df
#' @importFrom tibble tibble
#' @export security_group_list
security_group_list <- function() {
  resource <- boto3()$resource('ec2')
  client <- boto3()$client('ec2')

  response <- client$describe_security_groups()
  response <- response$SecurityGroups

  security_group_list <-
    map_df(response, function(x) {
      tibble(group_name = x$GroupName,
             group_id = x$GroupId)
    })

  security_group_list
}

#' security_group_create
#' @importFrom purrr keep
#' @importFrom dplyr filter
#' @importFrom dplyr pull
#' @param  group_name Name of security group
#' @export security_group_create
security_group_create <- function(group_key = NA) {

  resource <- boto3()$resource('ec2')
  client <- boto3()$client('ec2')

  security_group_df <- security_group_list()

  if(!any(security_group_df$group_name == group_key)) {
    create_security <-
      resource$create_security_group(
        GroupName=group_key,
        Description='for automated server'
      )

    create_security$authorize_ingress(
      IpProtocol = "tcp",
      CidrIp     = "0.0.0.0/0",
      FromPort   = 0L,
      ToPort     = 65535L
    )

    security_group_id <-
      create_security$id

  } else {
    security_group_id = security_group_df %>%
      filter(group_name == group_key) %>%
      pull(group_id)
  }

  security_group_id
}

#' security_group_delete
#' @param security_group_id  a security group ID
#' @export security_group_delete
security_group_delete <- function(security_group_id) {
  client <- boto3()$client('ec2')
  response = client$delete_security_group(GroupId = security_group_id)
  response$ResponseMetadata$HTTPStatusCode
}

#' ec2_instance_create
#' @param image_id An aws ec2 id: i.e., 'ami-0174e69c12bae5410'
#' @param instance_type See \url{https://aws.amazon.com/ec2/instance-types/}
#' @param min min instances
#' @param max max instances
#' @param key_name A .pem file to ssh
#' @param security_group_id SecurityGroupId of security group you have created in UI
#' @param instance_storage Size of the box in gb
#' @param device_name  "/dev/sda1"
#' @param user_data A shell script that runs on startup
#' @export ec2_instance_create
ec2_instance_create <- function(image_id = NA,
                                instance_type='t2.nano',
                                min = 1,
                                max = 1,
                                key_name = NA,
                                security_group_id = NA,
                                instance_storage = 50,
                                device_name = "/dev/sda1",
                                user_data  = NA) {

  if(is.na(user_data)) {
    user_data <- '#!/bin/bash
                  echo \'hello world \''
  }

  resource = boto3()$resource('ec2')

  resource$create_instances(ImageId      = image_id,
                            InstanceType = instance_type,
                            MinCount     = as.integer(min),
                            MaxCount     = as.integer(max),
                            UserData     = user_data,
                            KeyName      = key_name,
                            SecurityGroupIds = list(security_group_id),
                            BlockDeviceMappings = list(
                              list(
                                Ebs = list(
                                  VolumeSize = as.integer(instance_storage)
                                ),
                                DeviceName = device_name
                              )
                            )
  )

}

#' ec2_instance_start
#' @param instance_id An aws ec2 id: i.e., 'i-034e6090b1eb879e7'
#' @export ec2_instance_start
ec2_instance_start = function(instance_id = NA) {
  client <- boto3()$client('ec2')
  response <- client$start_instances(InstanceIds = list(instance_id))
  if(response$ResponseMetadata$HTTPStatusCode == 200) {
    return(TRUE)
  }
}


#' ec2_instance_stop
#' @param ids An aws ec2 id: i.e., 'i-034e6090b1eb879e7'
#' @param terminate An boolean to specify whether to stop or terminate
#' @export ec2_instance_stop
ec2_instance_stop = function(ids, terminate = FALSE) {

  config_data <- configr::read.config('config.yaml')$protected_ec2
  if(terminate) {
    resp <- readline(prompt="Are you sure you want to terminate this instance? All data will be destroyed - y/n: ")
    if(resp != 'y') {
      stop()
    }
  }
  resource = boto3()$resource('ec2')
  ids = list(ids)
  instances = resource$instances

  if(ids %in% unlist(config_data)) {
    stop("ID is protected. Cannot stop or terminate.")
  }

  if(terminate) {
    instances$filter(InstanceIds = ids)$terminate()
  } else {
    instances$filter(InstanceIds = ids)$stop()
  }
}


#' if_is_null
#' @importFrom dplyr if_else
#' @param x  input
#' @export if_is_null
if_is_null <- function(x) {
  if_else(is.null(x), as.character(NA), x)
}


#' ec2_get_info
#' @importFrom lubridate ymd_hms
#' @export ec2_get_info
ec2_get_info <- function() {
  ec2_con = boto3()$client('ec2')
  instances = ec2_con$describe_instances()
  map_df(instances$Reservations, function(x) {
    launch_time        <-  ymd_hms(paste0(x$Instances[[1]]$LaunchTime))
    state              <-  if_is_null(x$Instances[[1]]$State$Name)
    instance_id        <-  if_is_null(x$Instances[[1]]$InstanceId)
    image_id           <-  if_is_null(x$Instances[[1]]$ImageId)
    public_ip_address  <- if_is_null(x$Instances[[1]]$PublicIpAddress)
    private_ip_address <- if_is_null(x$Instances[[1]]$PrivateIpAddress)
    instance_type      <- if_is_null(x$Instances[[1]]$InstanceType)
    tibble(
      public_ip_address   = public_ip_address,
      priviate_ip_address = private_ip_address,
      image_id            = image_id,
      instance_id         = instance_id,
      launch_time         = launch_time,
      instance_type       = instance_type,
      state               = state
    )
  })
}
