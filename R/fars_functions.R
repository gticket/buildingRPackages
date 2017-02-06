#' Read filename as a data frame tbl.
#'
#' This is a wrapper function to read a file and return it as a data frame tbl.
#' The function stops if the provided filename does not exist.
#' If it does exists it reads the file as a csv file without progress bar
#'  and without returning diagnotstic messages.
#'
#' @param filename The name of the file which is to be read. This file must be a valid csv file.
#'
#' @return  A data frame tbl of the data contained in filename.
#'
#' @importFrom dplyr::tbl_df()
#' @importFrom readr::read_csv()
#'
#' @examples
#' \dontrun{
#' fars_read("accident_2013.csv.bz2")
#' }
#'
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' Create a filename.
#'
#' This function creates a filename.
#'
#' @param year The year which is to be included in the name. This value must be convertible to an integer.
#'
#' @return  A filename in the format "accident_<year>.csv.bz2".
#'
#' @examples
#' \dontrun{
#' make_filename(2017)
#' }
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Makes the data in the year files available as a list.
#'
#' This function reads every accident file for which the year is provided as input
#' and returns a list with the same length as the input vector.
#' An error is thrown if one of the provided years does not have an accident file associated to it.
#'
#' @param years The years for which the accident data is to be read.
#'
#' @return  A list of the data contained in files. Every element of the list contains the data of one year.
#'
#' @importFrom dplyr::mutate()
#' @importFrom dplyr::select()
#'
#' @examples
#' \dontrun{
#' fars_read_years(c(2013,2014))
#' }
#'
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' Makes the data in the year files available as a data frame tbl.
#'
#' This function reads every accident file for which the year is provided as input
#' and returns a data frame tbl where the rows are month and the columns are the years.
#'
#' @param years The years for which the accident data is to be read.
#'
#' @return  A data frame tbl where the rows are month and the columns are the years.
#'
#' @importFrom dplyr::bind_rows()
#' @importFrom dplyr::group_by()
#' @importFrom dplyr::summarize()
#' @importFrom tidyr::spread()
#'
#' @examples
#' \dontrun{
#' fars_summarize_years(c(2013,2014))
#' }
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Displays the data in a year file on the map of the corresponding state.
#'
#' This function reads every accident file for which the year is provided as input
#' and displays a state map (state provided as input) with the accidents of that years.
#'
#' @param year The year for which the accident data is to be displayed.
#' @param state.num The state for which the accident data is to be displayed. The value provided must be
#' an integer representing a state as found in the accident data file.
#'
#' @return  A plot of the statement with the accidents of that years.
#'
#' @importFrom dplyr::filter()
#' @importFrom maps::map()
#' @importFrom graphics::points()
#'
#' @examples
#' \dontrun{
#' fars_map_state(13,2014)
#' }
#'
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
