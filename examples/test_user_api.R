
test_instance <- Domo$new(client_id=Sys.getenv('RDOMO_TEST_CLIENT_ID'),secret=Sys.getenv('RDOMO_TEST_SECRET'),domain='api.domo.com',scope=c('user','data'))

new_user <- test_instance$users_add(x_name='Jeremy Morris',x_email=paste0(digest::digest(Sys.time()),'@aaa.com'),x_role='Privileged',x_sendInvite=FALSE)

user_obj <- test_instance$users_get(new_user$id)

make_admin <- user_obj
make_admin$role <- 'Admin'
make_admin$roleId <- NULL
admin_now <- test_instance$users_update(new_user$id,make_admin)

change_name <- user_obj
change_name$name <- 'TEST USER'
change_name$roleId <- NULL
change_name$title <- 'BOGUS TITLE'
changed_name <- test_instance$users_update(change_name$id,change_name)

all_users <- test_instance$users_list()

del <- test_instance$users_delete(user_obj$id)

# Try to add the user back via the API
nn <- test_instance$users_add(x_name=new_user$name,x_email=new_user$email,x_role=new_user$role)

all_users <- test_instance$users_list()
