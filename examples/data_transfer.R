
## This is an example to show how data can be transferred between two instances.
## If this package is not installed, you need to run devtools::load_all() prior to running the example.

library(tidyverse)

devtools::load_all()

modocorp <- Domo$new(client_id=Sys.getenv('MODOCORP_CLIENTID'),secret=Sys.getenv('MODOCORP_SECRET'),scope='data',domain='api.domo.com')
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

# Clean up
domodomo$ds_delete(domo_create)
modocorp$ds_delete(modo_create)



# Test upload
ti <- Domo$new(client_id=Sys.getenv('RDOMO_TEST_CLIENT_ID'),secret=Sys.getenv('RDOMO_TEST_SECRET'),domain='api.domo.com',scope=c('user','data'))
rd <- data.frame(matrix(rnorm(100),ncol=2))
new_df <- ti$ds_create(rd,'TEST TEST')

dd <- data.frame(aa=c('Вот эбаут санрайз','Из зере тайм','Вот эбаут килин филдс'),bb=c(1,2,3))
uu_df <- ti$ds_update(new_df,as.data.frame(dd))

ee <- data.frame(aa=rnorm(100))
uu_df <- ti$ds_update(new_df,as.data.frame(ee))

one_col <- ti$ds_create(as.data.frame(ee),'TEST ONE COLUMN')
