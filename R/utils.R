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

# Quiets concerns of R CMD check regarding non-standard evaluation
utils::globalVariables(c("slideName", "ROI", "ROIname", "GroupCount",
                         "Xt", "Yt", "CellID", "PanCKp", "CD8ap", "CD4p",
                         "CD103p", "Ki67p", "PD1p", "LAG3p", "GZMBp",
                         "pTBK1p", "pSTAT1p", "pSTAT3p", "HLA_Ep",
                         "CD11cp", "HLADRp", "CD68p"))
