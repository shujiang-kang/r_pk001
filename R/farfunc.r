#' Read a FARS Data File

#'

#' Reads a single Fatality Analysis Reporting System (FARS) data file from

#' disk and returns its contents as a tibble.

#'

#' @param filename A character string specifying the path to a FARS CSV file.

#'

#' @return A tibble containing the data from the specified file.

#'

#' @details

#' The file is read using `readr::read_csv()` with progress messages

#' suppressed. If the specified file does not exist, the function stops

#' with an informative error message.

#'

#' @examples

#' \dontrun{

#' fars_read("accident_2013.csv.bz2")

#' }

#'

#' @export

#' @importFrom readr read_csv

#' @importFrom dplyr tbl_df

fars_read <- function(filename) {

  if(!file.exists(filename))

    stop("file '", filename, "' does not exist")

  data <- suppressMessages({

    readr::read_csv(filename, progress = FALSE)

  })

  dplyr::tbl_df(data)

}



#' Construct a FARS Data Filename

#'

#' Generates the standard FARS accident filename for a given year.

#'

#' @param year A numeric or character value representing a year.

#'

#' @return A character string in the form

#'   `"accident_<year>.csv.bz2"`.

#'

#' @details

#' The supplied value is coerced to an integer before constructing the

#' filename.

#'

#' @examples

#' make_filename(2013)

#' make_filename("2014")

#'

#' @export

make_filename <- function(year) {

  year <- as.integer(year)

  sprintf("accident_%d.csv.bz2", year)

}



#' Read FARS Data for Multiple Years

#'

#' Reads FARS data files for one or more years and extracts the

#' `MONTH` and `year` variables from each dataset.

#'

#' @param years A vector of years (numeric or character).

#'

#' @return A list of tibbles, one for each successfully read year.

#' Each tibble contains the variables `MONTH` and `year`. If a file

#' cannot be read, a warning is issued and `NULL` is returned for that year.

#'

#' @details

#' For each year, the corresponding filename is generated using

#' `make_filename()`. The file is then read using `fars_read()`.

#' Errors are handled internally so that failure to read one file does not

#' interrupt processing of the remaining years.

#'

#' @examples

#' \dontrun{

#' fars_read_years(c(2013, 2014))

#' }

#' @importFrom dplyr mutate

#' @importFrom dplyr select

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




#' Summarize FARS Accident Counts by Month and Year

#'

#' Produces a summary table of monthly accident counts for one or more

#' years of FARS data.

#'

#' @param years A vector of years (numeric or character).

#'

#' @return A tibble containing one row per month and one column per year.

#' Each value represents the number of accident records observed in the

#' corresponding month and year.

#'

#' @details

#' This function combines data returned by `fars_read_years()`, groups

#' records by year and month, counts the number of observations, and

#' reshapes the result into a wide format with one column per year.

#'

#' @examples

#' \dontrun{

#' fars_summarize_years(c(2013, 2014))

#' }

#' @importFrom dplyr bind_rows

#' @importFrom dplyr summarize

#' @importFrom tidyr spread

#' @export

fars_summarize_years <- function(years) {

  dat_list <- fars_read_years(years)

  dplyr::bind_rows(dat_list) %>%

    dplyr::group_by(year, MONTH) %>%

    dplyr::summarize(n = n()) %>%

    tidyr::spread(year, n)

}



#' Plot FARS Accident Locations for a State

#'

#' Creates a map showing accident locations within a specified U.S. state

#' for a given year.

#'

#' @param state.num A numeric state identifier used in the FARS dataset.

#' @param year A numeric or character value representing a year.

#'

#' @return Invisibly returns `NULL`. The primary purpose of this function is

#' to produce a plot.

#'

#' @details

#' The function reads the corresponding FARS data file, validates the state

#' code, filters records for the selected state, and plots accident

#' locations using longitude and latitude coordinates. Coordinates outside

#' valid ranges are treated as missing values.

#'

#' If no accident records are available for the requested state and year,

#' a message is displayed and no plot is produced.

#'

#' @examples

#' \dontrun{

#' fars_map_state(1, 2013)

#' }

#' @importFrom dplyr filter

#' @importFrom maps map

#' @importFrom graphics points

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
