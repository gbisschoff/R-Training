install_if_missing <- function(packages, ...) {
  for(package in packages){
    if(!require(package, character.only = TRUE)) install.packages(package, ...)
    library(package, character.only = TRUE)
  }
}

dir.create('.cran')
.libPaths('.cran')
install_if_missing(c(
  'devtools',
  'dplyr',
  'plotly',
  'rio',
  'zoo'
), lib = '.cran')

dir.create('.data')
dir.create('.results')