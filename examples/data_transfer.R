
## This is an example to show how data can be transferred between two instances.
## If this package is not installed, you need to run devtools::load_all() prior to running the example.

modocorp <- Domo$new(client_id='96dc3209-89b0-41d5-8944-1f4ebb56e434',secret='fa6592829c048230c5fffd5ce0d1e25950e7bf3b1ebdf09fbaf2f329080f2e55',scope='data',domain='api.domo.com')
domodomo <- Domo$new()

modo_download <- modocorp$ds_get('45511431-9da5-4cd5-9795-378fb77cc847')
dd_download <- domodomo$ds_get('22189238-b26e-45d4-8822-696d9d4f642a')
