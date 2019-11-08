devtools::install_github("elizagrames/litsearchr")

library(litsearchr)

TS <- (("graz*" OR "livestock") AND ("exclosure*" OR "exclusion" OR "exclude*" OR "ungrazed" OR "retire*" OR "fallow*"))

naiveimport <- litsearchr::import_results(directory = "data//ReviewerSearch//", filename = NULL, save_dataset = FALSE)