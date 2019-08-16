install_if_missing <- function(packages, ...) {
  for(package in packages){
    if(!require(package)) install.packages(package, ...)
    library(package)
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
), lib = '.cran'

