
## This is an example to show how data can be transferred between two instances.
## If this package is not installed, you need to run devtools::load_all() prior to running the example.

library(tidyverse)

devtools::load_all()

modocorp <- Domo$new(client_id='aaa',secret='aaa',scope='data',domain='api.domo.com')
domodomo <- Domo$new()

modo_download <- modocorp$ds_get('45511431-9da5-4cd5-9795-378fb77cc847')
dd_download <- domodomo$ds_get('22189238-b26e-45d4-8822-696d9d4f642a')

# Create a new data set in two instances
tt <- data.frame(x=rnorm(50),y=rnorm(50))
domo_create <- domodomo$ds_create(tt,'THIS IS A TEST RPLUGIN V2')
modo_create <- modocorp$ds_create(tt,'THIS IS A TEST RPLUGIN V2')

# Update with the same data
domo_same <- domodomo$ds_update(domo_create,tt)
modo_same <- modocorp$ds_update(modo_create,tt)

# Change the schema
ttt <- tt %>% mutate(z=rnorm(50))
domo_same <- domodomo$ds_update(domo_create,ttt)
modo_same <- modocorp$ds_update(modo_create,ttt)

# Upload large data set
llg <- data.frame(x=rnorm(1000000),y=rnorm(1000000,mean=5,sd=20))
domo_lg <- domodomo$ds_update(domo_create,llg)
modo_lg <- modocorp$ds_update(modo_create,llg)

# Get ds schema
domo_sc <- domodomo$ds_meta(domo_create)

# All ds in modocorp
mc_ds <- modocorp$ds_list()
