packages = c("tidyverse", "kableExtra", "dplyr", "magrittr", "data.table", "sf", "rnaturalearth", "rnaturalearthdata", "scales", "ggsci", "tinytex", "countrycode", "ggspatial", "magick", "rgeos") # lists all the packages I want to load

temp = lapply(packages, function(x) {if (!x %in% row.names(installed.packages())) install.packages(x)}) # Check to see if the package is installed; if not, install it

temp = lapply(packages, library, character.only = TRUE) # loads library for all packages

if(webshot::is_phantomjs_installed() == FALSE){
  webshot::install_phantomjs()
}
