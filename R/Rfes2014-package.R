#' @rawNamespace useDynLib(libfes); useDynLib(Rfes2014)
#'
#' @aliases NULL Rfes2014 Rfes2014-package
#'
#' @details
#' The Rfes2014 package provides an R wrapper around the FES2014 library made
#' available by Aviso.
#'
#' There are 3 main functions to Rfes2014:
#' \itemize{
#'   \item \code{\link{fes_new}}: Create a new FES2014 instance
#'   \item \code{\link{fes_calculate}}: Request FES2014 to do a calculation
#'   \item \code{\link{fes_delete}}: Clean up the FES2014 instance
#' }
#'
#' @author Craig Williams, AVS Developments Ltd \email{craig@avsdev.uk}
#'
#' @references AVSDev: \url{https://avsdev.uk/R/Rfes}
#' @references FES2014: \url{https://bitbucket.org/cnes_aviso/fes}
#' @references netCDF: \url{https://github.com/Unidata/netcdf-c}
"_PACKAGE"