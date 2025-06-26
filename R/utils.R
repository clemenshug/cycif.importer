#' Package Imports and Utilities
#'
#' @description
#' This file handles package imports and provides utility functions.
#'
#' @name utils
NULL

# Import the pipe operator from magrittr (via dplyr)
#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

# Import key functions from dplyr to avoid NSE issues
#' @importFrom dplyr filter select mutate group_by summarize bind_rows
#' @importFrom dplyr across any_of where left_join n distinct
#' @importFrom rlang .data
#' @importFrom stats complete.cases
