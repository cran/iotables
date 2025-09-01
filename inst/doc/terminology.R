## ----setupvignette, include = FALSE-------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(scipen = 999)

## ----setup--------------------------------------------------------------------
library(iotables)

## ----germany------------------------------------------------------------------
germany_siot <- iotable_get()

## ----firstq-------------------------------------------------------------------
germany_siot[c(1:7), c(1:7)]

## ----outputflow---------------------------------------------------------------
germany_siot[5, c(1, 5:8)]

## ----inputflow----------------------------------------------------------------
germany_siot[c(5:9), c(1, 6)]

## ----inputcoeffmatrix, eval=FALSE---------------------------------------------
# input_coefficient_matrix_create_2(iotable_get(), digits = 2)

