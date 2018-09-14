## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- fig.show='hold'----------------------------------------------------
#load( system.file ( 'extdata', 'mydata.rda', package = 'iotables'))
#eurostat_data_example ('create_vocabulary.csv')
#read.csv ( "../inst/extdata/create_vocabulary.csv")
#read.csv ( file.path  ( '..', 'inst', 'extdata', 'create_vocabulary.csv'))
#system.file ( 'extdata', 'create_vocabulary.csv', package = 'iotables')

list.files(system.file('extdata', package = 'iotables'))

load (system.file('extdata', 'naio_10_product_x_product.rda', package = 'iotables') )

