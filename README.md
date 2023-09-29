## ODC Data Quality App - R Package

This repo packages all the functions used in the hosted version of ODC Data Quality App as an R package.

This R package is aimed towards technical users or those who require modularity, reproducability, or batch processing.

> Note: This R package does not install Python nor the required libraries for the EDA Report. However, the code to do so is included.

### To install

```{r}
install.packages("devtools")
devtools::install_github("")
```

### Requirements

The following R libraries (all available on CRAN) are required:

- tidyverse
- DT
- shiny
- shinyjs
- waiter