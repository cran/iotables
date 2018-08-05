
<!-- README.md is generated from README.Rmd. Please edit that file -->
iotables
========

The symmetric input-output tables (SIOTs) are complex statistical products that present inter-related statistics in a predefined structure. They are often found in spreadsheets that follow this structure, or in the case Eurostat in a data repository. In both cases they in reproducible research must be downloaded and restructured to programmatically accessible form. Often these highly structured statistics need to be analyzed together with other data, for example, when employment effects and multipliers are calculated. In this case processing the employment data to SIOT conforming format is a significant preprocessing challenge.

The iotables are exactly designed for these tasks. Currently the package downloads and processes standardized European SIOTs conforming to the latest statistical regulations, i.e. SIOTs starting from the year 2010.

The aim of this introduction is not to introduce input-output economics, or SIOTs in detail. The Eurostat manual on input-output tables and the [Eurostat tematic page](http://ec.europa.eu/eurostat/web/esa-supply-use-input-tables/methodology/symmetric-input-output-tables) should be consulted for further information about the data and the metadata.

In order to test the analytical functions of the package and to have a manageable sized example data set, we use the real-life data from the Eurostat manual. The `germany_1990` dataset is a simplified 6x6 sized SIOT taken from the Eurostat SIOT manual. The package function examples can be checked against Jörg Beutel's published results(<http://ec.europa.eu/eurostat/documents/3859598/5902113/KS-RA-07-013-EN.PDF/b0b3d71e-3930-4442-94be-70b36cea9b39?version=1.0>).

Installation
------------

You can install iotools from github with:

``` r
# install.packages("devtools")
devtools::install_github("rOpenGov/iotables")

#with vignettes:
#devtools::install_github("rOpenGov/iotables", build_vignettes = TRUE)
```

YOu can follow changes on the NEWS.md file.

Acquiring data
--------------

Eurostat's data can be downloaded in several tidy, long-form, files, and a lot of filtering is needeed to start working with it. In case you need to work with a SIOT given in Excel, for example, Eurostat's pre-2010 files, which cannot be downloaded with the eurostat package, please consult the Croatia vignette. What you want to do is to arrive to a similar format to Eurostat's long-form.

Currently the following Eurostat SIOTs can be used: `naio_10_cp1700`, `naio_10_cp1750`, `naio_10_pyp1700`, `naio_10_pyp1750`, `naio_cp17_r2`, `naio_17_agg_60_r2`, `naio_17_agg_10_r2`. 

Furthermore, you can get the trade and retail margins with parameters `source = "naio_10_cp1620", stk_dom = "TOTAL"`. If the stk_dom is omitted, it will be added to the query. 

The tables follow a similar structure, but they are differing in two important aspects.

The following example is not run. In this case the `stk_flow` is omitted:

``` r
require (dplyr) ; require (iotables)

#Get the Slovak national IO table.
sk_io <- iotable_get ( source = "naio_cp17_r2", geo = "SK",
                       year = 2010, unit = "MIO_EUR", 
                       labelling = "iotables")

#labelling = 'short' will keep the Eurostat short labels.

#Retrieve the bulk file from the temporary directory
retrieve_from_temp_bulk <- readRDS(paste0(tempdir(), "\\eurostat/naio_cp17_r2_date_code_TF.rds" ))
```

The Eurostat bulk downloader will give you a very long file: all country data in a long-form, with different currencies, and for different years. The `iotable_get` function will filter out a table for you.

-   Eurostat uses different labelling for product x product, product x industry and industry x industry cases. You can preserve the Eurostat metadata vocabulary with choosing the `labelling  = 'short'` option in the preprocessing functions.

-   The default `labelling  = 'iotables'` parameter will change all row and column names to a standard snake\_var\_name format naming. This results in more readable output, but of course you have to keep in mind the differences among the tables.

-   The `unit` parameter is necessary because Eurostat files contain the same data in national currency units and euros. For eurozone member states `unit = MIO_EUR` and `unit = MIO_NAC` is equivalent, because the euro is their national currency.

-   The `year` should be given as a numeric.

-   The `geo` can be inputed with 2 letter country codes or country names. Beware, that Eurostat UK for the United Kingdom instead of GB and EL for Greece instead of GR.

-   The `stk_flow` variable is only used when the 'DOM' and 'IMP' type SIOTs differ in the treatment of iimpots. In the case of SIOTs where the creation process does not differentiate these types, the stk\_flow variable should be left at the default setting.

The code above depends on the `get_eurostat()` function of the Eurostat package. This function call will create a temporary .rds file during your session to avoid too large data volumes.

Preprocessing
-------------

The `use_table_get ()` function extracts only the input-flow matrix from the table. The `output_get ()` function extracts the output vector. Because the Eurostat input-output files are very large, we use the simplified German IO table for the examples. This is also useful for testing the functions as the returned values can be checked against Eurostat manual values.

For some analysis we use the household consumption, for some analysis we do not need it. In all cases, the `households = TRUE` parameter will add the household consumption expenditure column, and in the case of the use table the wages (or compensation) row to maintain the symmetry of the table.

``` r
de_use <- use_table_get ( source = "germany_1990", geo = "DE",
                       year = 1990, unit = "MIO_EUR", 
                       households = FALSE, labelling = "iotables")

de_output <- output_get ( source = "germany_1990", geo = "DE",
                       year = 1990, unit = "MIO_EUR",
                       households = TRUE, labelling = "iotables")
print (de_output[c(1,7,8)])
#>    iotables_row other_services_group consumption_expenditure_household
#> 16    output_bp               508918                           1001060
```

For almost all analytical uses you will need input coefficients and the Leontieff-inverse matrix. The `input_coefficient_matrix_create()` function creates the input\_coefficients, the `leontieff_matrix_create()` function creates the Leontieff-matrix from the appropriate SIOT fields. The `leontieff_inverse_create(L)` creates the inverse of the Leontieff-matrix which is used in all basic equations.

``` r
de_output <- output_get ( source = "germany_1990", geo = "DE",
                       year = 1990, unit = "MIO_EUR",
                       households = FALSE, labelling = "iotables")

de_coeff <- input_coefficient_matrix_create( de_use, de_output, digits = 4)
L_de <- leontieff_matrix_create( technology_coefficients_matrix =
                                de_coeff )
I_de <- leontieff_inverse_create(L_de)
print (L_de[,1:3])
#>              iotables_row agriculture_group manufacturing_group
#> 1       agriculture_group            0.9742             -0.0236
#> 2     manufacturing_group           -0.1806              0.7178
#> 3       construcion_group           -0.0097             -0.0068
#> 4             trade_group           -0.0811             -0.0674
#> 5 business_services_group           -0.0828             -0.0890
#> 6    other_services_group           -0.0353             -0.0139
```

> The Leontieff matrix shown here can be checked against the Eurostat manuals table 15.9 on page 487. (The ordering of the industries is different.) The Leontieff-inverse matrix can be checked against the Eurostat input-output manual Table 15.11 on page 490. (The ordering of the industries is different.)

Analytical functions
--------------------

The most frequently used input-output equations are programmed into convenient R functions. If all elements are in place, working with the elements is easy with R's matrix algebraic operators such as `%*%`.

Because of the high complexity of the data, any violation of matrix algebraic rules causes either an error, or worse, a hard-to-detect calculation error. The iotables functions have some built-in error handling and try to give more meaningful error and warning messages than the matrix operators. Typical errors are typos in the many column names and key colum elements, or a change in the ordering of the columns (for example to alphabetic ordering from NACE ordering), or singularity if there are empty columns.

Using the analytical functions we can calculate the employment multipliers for the German economy in the year 1990. For example, the `multiplier_create()` function performs an `%*%` operation after checking if the name ordering of the input vector and Leontieff-inverse is correct, and returns the results with a key column for consistency with other function returns.

``` r
de_emp <- primary_input_get ( input = "employment_total",
                              source = "germany_1990", geo = "DE",
                              year = 1990,  
                              households = FALSE, labelling = "iotables")

de_emp_indicator <- input_indicator_create (de_emp, de_output)

employment_multipliers <- multiplier_create ( 
  input_vector    = de_emp_indicator,
  Im              = I_de,
  multiplier_name =  "employment_multiplier", 
  digits = 4 )

print (employment_multipliers[1,3])
#> [1] 0.0162
```

Using non-Eurostat input-output tables
--------------------------------------

Another vignette gives some more ideas on how to add data to your analytical equations from other sources.

There is currently no downloader function for the [OECD STAN](https://stats.oecd.org/Index.aspx?DataSetCode=STAN_IO_TOT_DOM_IMP) SIOT databases, which contain the SIOT's of some OECD countries, often overlapping with the EU countries who create their SIOTs, but this is planned in the near future. The code below gets the 2011 Australian SIOT in USD terms in long-form. The `RJSDMX` package requires `rJava`, whicn in turn on Windows machines requires the 64-bit version of Java to be installed on your computer.

The next pre-processing steps that are already available for Eurostat will be included in the 0.3 version of the package. With some data-wrangling you can bring OECD tables to the same format as the Eurostat tables.
