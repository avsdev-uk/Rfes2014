#' Enables debug print statements showing the process flow of Rfes2014
#'
#' @param enable logical; TRUE to enable, FALSE to disable
#'
#' @return None
#'
#' @examples
#' fes_debug(TRUE)
#' hdl <- fes_new("ocean", "memory", "data/fes2014/eastward_velocity.ini")
#' # Created FES handle: 9c5b5020
#' fes_calculate(hdl, 50.10, -2.40, 1570536000)
#' # Calculating for: 50.100000 -2.400000 25482.500000
#' # Result: -24.965188 0.210749 4
#' fes_delete(hdl)
#' # Destroying FES handle: 9c5b5020
#'
#' \dontrun{
#' fes_debug(TRUE)
#' hdl <- fes_new("ocean", "memory", "data/fes2014/eastward_velocity.ini")
#' # Created FES handle: 8ca5f830
#' q()
#' # Finalizing handle: 8ca5f830
#' # Destroying FES handle: 8ca5f830
#' }
#'
#' @export
fes_debug <- function(enable) {
  .Call("fes2014_debug", as.logical(enable))
  invisible(0)
}


#' Create a handle to a new FES2014 instance
#'
#' FES2014 handles are used in \code{\link{fes_calculate()}} and can either be
#' cleaned up manually with \code{\link{fes_delete()}} or are finalized
#' automatically when the R session is destroyed.
#'
#' DO NOT SAVE THE HANDLE IN RDATA FILES
#'
#' @param tide_type string; one of "ocean", "tide" or "radial"
#' @param access_mode string; one of "io" or "memory"
#' @param ini_file string; path to a fes2014 configuration file
#'
#' @return <extpointer> A handle to a FES2014 instance
#'
#' @seealso \code{\link{fes_delete}}
#'
#' @examples
#' hdl <- fes_new("ocean", "memory", "data/fes2014/eastward_velocity.ini")
#'
#' @export
fes_new <- function(tide_type, access_mode, ini_file) {
  if (!(tide_type %in% c("ocean", "tide", "radial"))) {
    stop("tide_type must be one of c(ocean, radial)")
  }
  if (!(access_mode %in% c("memory", "io"))) {
    stop("access_mode must be one of c(memory, io)")
  }
  if (!(file.exists(ini_file))) {
    stop("ini_file must be a path to a valid ini file")
  }

  tt <- ifelse(tide_type %in% c("ocean", "tide"), 0, 1)
  am <- ifelse(access_mode %in% c("memory"), 1, 0)

  hdl <- .Call(
    "fes2014_new",
    as.integer(tt),
    as.integer(am),
    as.character(ini_file)
  )

  return(hdl)
}

#' Cleans up a FES2014 instance
#'
#' @param hdl <extptr>; a FES2014 handle
#'
#' @return None
#'
#' @seealso \code{\link{fes_new}}
#'
#' @examples
#' hdl <- fes_new("ocean", "memory", "data/fes2014/eastward_velocity.ini")
#' fes_delete(hdl)
#'
#' @export
fes_delete <- function(hdl) {
  if (typeof(hdl) != "externalptr") {
    stop("hdl must be handle created by fes_new")
  }
  .Call("fes2014_delete", hdl)
  invisible(0)
}


#' Performs a calculation with a FES2014 instance
#'
#' @param hdl <extptr>; a FES2014 handle
#'
#' @seealso \code{\link{fes_new}}; \code{\link[base]{as.POSIXct}} for timestamps
#'
#' @examples
#' hdl <- fes_new("ocean", "memory", "data/fes2014/eastward_velocity.ini")
#' res <- fes_calculate(hdl, 50.10, -2.40, 1570536000)
#' print(res)
#' fes_delete(hdl)
#'
#' # To get a unix timestamp from Sys.time():
#' epoch_secs <- as.integer(Sys.time())
#'
#' # To get a unix timestamp from Sys.Date() or a Date() object:
#' epoch_secs <- as.integer(as.POSIXct(Sys.Date(), tx = "UTC"))
#'
#' # fes_calculate can also take data.frames/tibbles:
#' df <- data.frame(
#'   lat = c(50.10, 50.20),
#'   long = c(-2.40, -2.40),
#'   epoch_sec = c(1570536000, 1570536000)
#' )
#' res <- fes_calculate(hld, df, lonCol = "long")
#' res <- fes_calculate(hld, dplyr::as_tibble(df), lonCol = "long")
#'
#' @export
fes_calculate <- function(hdl, ...) {
  params <- as.list(...)
  UseMethod("fes_calculate", params[[1]])
}

#' @rdname fes_calculate
#'
#' @param lat double; a double representing latitudes
#' @param long double; a double representing longitudes
#' @param epoch_sec integer; an integer representing seconds since POSIX
#'   epoch (1970/01/01 00:00:00 UTC). Negative numbers are accepted.
#'
#' @return A list containing h, hLongPeriod and samples for the lat, long,
#'   timestamp tuple
#'
#' @export
fes_calculate.double <- function(hdl, lat, long, epoch_sec) {
  if (typeof(hdl) != "externalptr") {
    stop("hdl must be handle created by fes_new")
  }
  if (!is.numeric(lat)) {
    stop("lat must be numeric")
  }
  if (!is.numeric(long)) {
    stop("long must be numeric")
  }
  if (!is.numeric(epoch_sec) && !is.integer(epoch_sec)) {
    stop("epoch_sec must be numeric")
  }

  ret <- .Call(
    "fes2014_calculate_one",
    hdl,
    as.double(lat), as.double(long),
    as.integer(epoch_sec)
  )

  return(ret)
}

#' @rdname fes_calculate
#'
#' @param lat list; a list of latitudes as doubles
#' @param long list; a list of longitudes as doubles
#' @param epoch_sec list; a list of timestamps (seconds since epoch) as integers
#'
#' @return A data.frame, matrix or tibble containing h, hLongPeriod and samples
#'   for each lat, long, timestamp tuple
#'
#' @export
fes_calculate.list <- function(hdl, lat, long, epoch_sec) {
  if (typeof(hdl) != "externalptr") {
    stop("hdl must be handle created by fes_new")
  }

  if (length(lat) != length(long)) {
    stop("lat and long must be the same length")
  }

  if (length(epoch_sec) != 1 && length(lat) != length(epoch_sec)) {
    stop("length of epoch_sec must either be 1 or length(lat)")
  }
  if (length(epoch_sec) == 1) {
    epoch_sec <- rep(epoch_sec, length(lat))
  }

  if (length(lat) == 0) {
    return(data.frame(
      lat = numeric(0),
      lon = numeric(0),
      epoch_sec = integer(0),
      h = numeric(0),
      hLongPeriod = numeric(0),
      samples = integer(0)
    ))
  }


  if (!is.numeric(lat[[1]])) {
    stop("lat must be numeric")
  }
  if (!is.numeric(long[[1]])) {
    stop("long must be numeric")
  }
  if (!is.numeric(epoch_sec[[1]]) && !is.integer(epoch_sec[[1]])) {
    stop("epoch_sec must be numeric")
  }

  ret <- .Call(
    "fes2014_calculate_many",
    hdl,
    as.double(lat), as.double(long),
    as.integer(epoch_sec),
    as.integer(length(lat))
  )

  return(as.data.frame(cbind(
    lat = lat,
    lon = long,
    epoch_sec = epoch_sec,
    h = ret$h,
    hLongPeriod = ret$hLongPeriod,
    samples = ret$samples
  )))
}

#' @rdname fes_calculate
#'
#' @param data data.frame/tibble/matrix; a data.frame, matrix or tibble
#'   containing 3 columns (lat/lon/epoch_sec) or 2 columns (lat/long) with the
#'   epoch_sec parameter. Column names can be provided as extra parameters.
#' @param latCol string; name of the latitude column in data
#' @param lonCol string; name of the longitude column in data
#' @param epochSecCol string; name of the timestamps column in data
#'
#' @export
fes_calculate.data.frame <- function(
  hdl, data, epoch_sec = NULL,
  latCol = "lat", lonCol = "lon", epochSecCol = "epoch_sec"
) {
  if (typeof(hdl) != "externalptr") {
    stop("hdl must be handle created by fes_new")
  }

  if (!is.null(epoch_sec)) {
    data[,epochSecCol] <- epoch_sec
  }

  if (!(latCol %in% colnames(data))) {
    stop(paste0("lat column missing (", latCol, ")"))
  }
  if (!(lonCol %in% colnames(data))) {
    stop(paste0("lon column missing (", lonCol, ")"))
  }
  if (!(epochSecCol %in% colnames(data))) {
    stop(paste0("epoch_sec column missing (", epochSecCol, ")"))
  }

  return(fes_calculate.list(
    hdl, data[,latCol], data[,lonCol], data[,epochSecCol]
  ))
}

#' @rdname fes_calculate
#' @export
fes_calculate.matrix <- function(
  hdl, data, epoch_sec = NULL,
  latCol = "lat", lonCol = "lon", epochSecCol = "epoch_sec"
) {
  return(as.matrix(fes_calculate.data.frame(
      hdl, as.data.frame(data), epoch_sec,
      latCol, lonCol, epochSecCol
  )))
}

#' @rdname fes_calculate
#' @export
fes_calculate.tbl_df <- function(
  hdl, data, epoch_sec = NULL,
  latCol = "lat", lonCol = "lon", epochSecCol = "epoch_sec"
) {
  if (typeof(hdl) != "externalptr") {
    stop("hdl must be handle created by fes_new")
  }

  if (!is.null(epoch_sec)) {
    data[,epochSecCol] <- epoch_sec
  }

  if (!(latCol %in% colnames(data))) {
    stop(paste0("lat column missing (", latCol, ")"))
  }
  if (!(lonCol %in% colnames(data))) {
    stop(paste0("lon column missing (", lonCol, ")"))
  }
  if (!(epochSecCol %in% colnames(data))) {
    stop(paste0("epoch_sec column missing (", epochSecCol, ")"))
  }

  return(dplyr::as_tibble(fes_calculate.list(
    hdl,
    dplyr::pull(data[,latCol]),
    dplyr::pull(data[,lonCol]),
    dplyr::pull(data[,epochSecCol])
  )))
}