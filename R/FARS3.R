#' R e v i e w  FARS data
#'
#'
#' FUN #1 This function reads data from .csv file
#'
#'
#' It is fed with internal data from the US National Highway Traffic Safety
#' Administration's Fatality Analysis Reporting System, which is a nationwide
#' census providing the American public yearly data regarding fatal injuries.
#' suffered in motor vehicle traffic crashes.
#' \strong{US National Highway Traffic Safety Administration's}
#'
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#'
#' @param filename a file.
#' @return a data frame.
#'
#'
#' @examples
#' library(dplyr)
#' library(readr)
#' filename <- "./Internal_Data/accident_2013.csv.bz2"
#' data <- fars_read(filename)
#' View(head(data,3))
#' @note To generate file name use: \code{\link{make_filename}}
#' @seealso \link{make_filename}
#'
#'
#' @export
#'
#'
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::data_frame(data)
}


#' FUN #2 Make data file name
#'
#'
#' Make .csv data file name related to the given \code{fars_read}
#'
#'
#' @param year A string or an integer with the input
#'
#'
#' @return a string with the filename.
#'
#' @examples
#' make_filename(2015)
#'
#'
#' @export
#'
#'
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("./Internal_Data/accident_%d.csv.bz2", year)
}


#' FUN #3 Read FARS years
#'
#'
#' Makes a list of month and years
#'
#'
#' function used by \code{fars_summarize_years}
#'
#'
#' @importFrom dplyr mutate_
#' @importFrom dplyr select_
#' @importFrom magrittr "%>%"
#'
#'
#' @param years A vector with a list of years
#'
#'
#' @return A data.frame including entries in data by month, or NULL if the
#'  \code{year} is not valid
#'
#'
#' @seealso \link{fars_read}
#' @seealso \link{make_filename}
#'
#'
#' @examples
#' fars_read_years(2013)
#'
#'
#' @export
#'
#'
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dat <- dat %>% mutate(year = YEAR) %>%
        select(MONTH, year) %>% data.frame()
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}


#' #'FUN #4 Summarize FARS data by years.
#'
#'
#' Makes a summary count per month in particular years.This function summarizes yearly accidents data, by month
#'
#'
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by_
#' @importFrom dplyr summarize_
#' @importFrom tidyr spread_
#'
#'
#' @param years A vector with a list of years to summarize by
#'
#'
#' @return A data.frame with number of accidents by years summarized by month
#'
#'
#' @seealso \link{fars_read_years}
#'
#'
#' @examples
#' fars_summarize_years(2015))
#' fars_summarize_years(c(2015, 2014))
#'
#'
#' @export
#'
#'
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years) %>% bind_rows()
  dat_list <- dat_list %>% group_by(year, MONTH) %>%
    summarise(n = n())
}




#' FUN #5 Display accidents map by state and year
#'
#' Displays a plot with a state map including the accidents location by year


#' @param state.num An Integer with the State Code
#' @param year A string, or an integer, with the input \code{year}
#'
#'
#' @importFrom maps map
#' @importFrom dplyr filter_
#' @importFrom graphics points
#' @return None
#'
#'
#' @seealso \link{fars_read}
#' @seealso \link{make_filename}
#'
#'
#' @examples
#' fars_map_state(49, 2015)
#'
#'
#' @export
#'
#'
#'
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
