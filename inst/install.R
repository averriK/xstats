file.remove("NAMESPACE")
# usethis::use_data_raw()
usethis::use_proprietary_license(copyright_holder = "Alejandro Verri Kozlowski")
# source("data-raw/DATASET.R")
devtools::document()
# devtools::build()
devtools::check()
remove.packages("xstats")
# devtools::install()
## push main
# usethis::use_github()

