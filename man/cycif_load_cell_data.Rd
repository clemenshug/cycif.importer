% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/file_io.R
\name{cycif_load_cell_data}
\alias{cycif_load_cell_data}
\title{Load cell data from UNMICST files}
\usage{
cycif_load_cell_data(data_dir, slide_filter = NULL)
}
\arguments{
\item{data_dir}{Directory containing *--unmicst_cell.csv(.gz) files}

\item{slide_filter}{Optional vector of slide names to load}
}
\value{
List of data frames by slide name
}
\description{
Loads cell segmentation data from UNMICST CSV files in a directory.
Files should follow the naming pattern "*--unmicst_cell.csv(.gz)".
}
\examples{
\dontrun{
# Load all cell data files
cell_data <- cycif_load_cell_data("/path/to/data")

# Load specific slides only
cell_data <- cycif_load_cell_data("/path/to/data", slide_filter = c("slide1", "slide2"))
}
}
