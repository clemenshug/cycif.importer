#' Apply gates to processed cell data
#'
#' Applies single and double marker gating thresholds to cell data.
#' Returns gated data as list of dataframes by slide.
#'
#' @param data List of dataframes by slide name (output from
#'   cycif_import_and_assign_rois)
#' @param gate_thresholds Data frame with gating thresholds by slide and marker
#'   (optional)
#' @param double_gates Data frame with marker1, marker2 columns for double gates
#'   (optional)
#'
#' @return List of dataframes by slide name with gating results added
#' @export
#'
#' @examples
#' \dontrun{
#' # Apply gates to processed data
#' gated_data <- cycif_apply_gates(
#'   data = data,
#'   gate_thresholds = my_gates
#' )
#' }
cycif_apply_gates <- function(
  data,
  gate_thresholds = NULL,
  double_gates = NULL
) {
  data <- standardize_input_data(data)

  if (is.null(gate_thresholds) && is.null(double_gates)) {
    message("No gates provided, returning data unchanged")
    return(data)
  }

  slide_names <- names(data)
  gated_slides <- list()

  for (slide_name in slide_names) {
    message(sprintf("Applying gates to slide: %s", slide_name))
    data_slide <- data[[slide_name]]

    # Apply gates if thresholds provided
    if (!is.null(gate_thresholds)) {
      data_slide <- apply_single_gates(data_slide, gate_thresholds, slide_name)
    }

    if (!is.null(double_gates)) {
      data_slide <- apply_double_gates(data_slide, double_gates)
    }

    gated_slides[[slide_name]] <- data_slide
  }

  return(gated_slides)
}

#' Apply single marker gates
#'
#' @param data Data frame with marker measurements
#' @param gate_thresholds Data frame with gating thresholds
#' @param slide_name Name of the current slide
#'
#' @return Data frame with gating results added as logical columns
#' @export
apply_single_gates <- function(data, gate_thresholds, slide_name) {
  # Find gate thresholds for this slide
  slide_gates <- gate_thresholds %>%
    dplyr::filter(slideName == slide_name)

  if (nrow(slide_gates) == 0) {
    message(sprintf("  No gate thresholds found for %s", slide_name))
    return(data)
  }

  # Get all marker columns (exclude slideName)
  marker_cols <- setdiff(names(gate_thresholds), "slideName")

  for (marker in marker_cols) {
    if (marker %in% names(data)) {
      gate_value <- slide_gates[[marker]][1]
      if (!is.na(gate_value)) {
        gate_name <- paste0(marker, "p")
        data[[gate_name]] <- data[[marker]] > exp(gate_value)
      }
    }
  }

  return(data)
}

#' Apply double marker gates
#'
#' @param data Data frame with single marker gates
#' @param double_gates_df Data frame with marker1, marker2 columns for double gates
#'
#' @return Data frame with double gates added
#' @export
apply_double_gates <- function(data, double_gates_df) {
  # Use default double gates if not provided
  if (is.null(double_gates_df)) {
    double_gates_df <- get_default_double_gates()
  }

  for (j in seq_len(nrow(double_gates_df))) {
    gate1_name <- paste0(double_gates_df$marker1[j], "p")
    gate2_name <- paste0(double_gates_df$marker2[j], "p")
    double_gate_name <- paste0(gate1_name, gate2_name)

    if (gate1_name %in% names(data) && gate2_name %in% names(data)) {
      data[[double_gate_name]] <- data[[gate1_name]] & data[[gate2_name]]
    }
  }

  return(data)
}


#' Calculate marker combinations using flexible formula approach
#'
#' This function allows you to define complex marker combinations using
#' formula expressions. It supports logical operators (!, &, |) and
#' can automatically generate column names or use custom names.
#'
#' @param data Data frame with marker columns, or list of data frames
#' @param marker_combinations Named list of formulas defining marker combinations.
#'   If names are not provided, they will be auto-generated from the formula.
#'
#' @return Data frame(s) with additional marker combination columns
#' @export
#'
#' @examples
#' \dontrun{
#' # Define marker combinations with automatic naming
#' combinations <- list(
#'   ~CD8ap & !CD103p,
#'   ~CD4p | PD1p
#' )
#'
#' # Define marker combinations with custom names
#' combinations <- list(
#'   resident_t_cells = ~CD8ap & CD103p,
#'   activated_t_cells = ~CD4p & Ki67p
#' )
#'
#' # Apply to data
#' result <- cycif_marker_combinations(my_data, combinations)
#' }
cycif_marker_combinations <- function(data, marker_combinations) {
  data <- standardize_input_data(data)

  if (is.null(marker_combinations) || length(marker_combinations) == 0) {
    return(data)
  }

  # Generate automatic names from formulas
  auto_names <- purrr::map_chr(marker_combinations, clean_formula_label)

  # Use provided names or fall back to auto-generated names
  resolved_names <- if (is.null(names(marker_combinations))) {
    auto_names
  } else {
    purrr::map2_chr(names(marker_combinations), auto_names, function(given_name, auto_name) {
      if (!is.na(given_name) && nzchar(given_name)) {
        given_name
      } else {
        auto_name
      }
    })
  }

  missing_vars_all <- character()
  missing_combinations <- character()

  apply_marker_combinations <- function(data1) {
    # Evaluate each formula and add to data
    new_columns <- list()

    for (i in seq_along(marker_combinations)) {
      formula_expr <- marker_combinations[[i]]
      col_name <- resolved_names[i]

      tryCatch({
        # Extract variables referenced in the formula
        vars_in_formula <- all.vars(formula_expr)
        missing_vars <- setdiff(vars_in_formula, names(data1))

        if (length(missing_vars) > 0) {
          message(sprintf("Skipping %s: missing columns %s",
                          col_name, paste(missing_vars, collapse = ", ")))
          missing_vars_all <<- c(missing_vars_all, missing_vars)
          missing_combinations <<- c(missing_combinations, col_name)
          next
        }

        # Evaluate the formula in the context of the data
        result <- rlang::eval_tidy(formula_expr[[2]], data = data1)
        new_columns[[col_name]] <- result

      }, error = function(e) {
        warning(sprintf("Error evaluating formula for %s: %s", col_name, e$message))
      })
    }

    # Add new columns to data
    if (length(new_columns) > 0) {
      data1 <- data1 %>%
        dplyr::mutate(!!!new_columns)
    }

    return(data1)
  }

  res <- lapply(data, apply_marker_combinations)
  attr(res, "missing_vars") <- unique(missing_vars_all)
  attr(res, "missing_combinations") <- unique(missing_combinations)
  res
}

#' Get default marker combinations
#'
#' Returns a comprehensive set of default marker combinations commonly
#' used in CyCIF analysis, including T cell subsets, IFN pathway markers,
#' and antigen-presenting cell combinations.
#'
#' @return List of default marker combination formulas
#' @export
#'
#' @examples
#' \dontrun{
#' default_combos <- get_default_marker_combinations()
#' data_with_combos <- calculate_marker_combinations(my_data, default_combos)
#' }
get_default_marker_combinations <- function() {
  list(
    # Basic T cell combinations
    CD8apCD103n = ~CD8ap & !CD103p,
    CD8apCD103pPD1n = ~CD8ap & CD103p & !PD1p,
    CD4pKi67p = ~CD4p & Ki67p,
    CD4pPD1p = ~CD4p & PD1p,
    CD4pLAG3p = ~CD4p & LAG3p,
    CD8apLAG3p = ~CD8ap & LAG3p,
    CD8apGZMBp = ~CD8ap & GZMBp,

    # IFN pathway (OR of multiple markers)
    IFNP = ~pTBK1p | pSTAT1p | pSTAT3p | `HLA-Ep`,

    # APC combinations
    CD11cpCD103pHLADRpCD68n = ~CD11cp & CD103p & `HLA DRp` & !CD68p,
    CD11cpCD68nCD103p = ~CD11cp & CD103p & !CD68p,
    CD11cpCD103n = ~CD11cp & !CD103p,
    CD11cpCD68pCD103n = ~CD11cp & CD68p & !CD103p,
    CD11cpCD68nCD103n = ~CD11cp & !CD68p & !CD103p,
    CD11cpCD103nHLADRpCD68p = ~CD11cp & !CD103p & `HLA DRp` & CD68p,
    CD11cpCD103nHLADRpCD68n = ~CD11cp & !CD103p & `HLA DRp` & !CD68p,

    # Activated T cells (complex expression)
    CD8apPD1pKi67pLAG3n = ~((CD8ap & PD1p) | (CD8ap & Ki67p)) & !LAG3p
  )
}

#' Get default double marker gate combinations
#'
#' Returns a data frame with default double marker combinations commonly
#' used in CyCIF analysis.
#'
#' @return Data frame with marker1, marker2 columns defining double gates
#' @export
#'
#' @examples
#' \dontrun{
#' default_double_gates <- get_default_double_gates()
#' data_with_double_gates <- apply_double_gates(my_data, default_double_gates)
#' }
get_default_double_gates <- function() {
  data.frame(
    marker1 = c("CD8a", "CD4", "CD4", "CD4", "CD8a", "CD8a"),
    marker2 = c("CD103", "Ki67", "PD1", "LAG3", "LAG3", "GZMB"),
    stringsAsFactors = FALSE
  )
}

#' Create clean column names from formula expressions
#'
#' @param expr A formula expression
#'
#' @return Character string with cleaned name
#' @keywords internal
clean_formula_label <- function(expr) {
  if (inherits(expr, "formula")) {
    expr_text <- rlang::expr_text(expr[[2]])
  } else {
    expr_text <- rlang::expr_text(expr)
  }

  # Clean up the expression text
  expr_text <- gsub("\\s+", "", expr_text)      # remove spaces
  expr_text <- gsub("\\|", "_or_", expr_text)   # OR operator
  expr_text <- gsub("&", "_and_", expr_text)    # AND operator
  expr_text <- gsub("!", "not_", expr_text)     # NOT operator
  expr_text <- gsub("[()]", "", expr_text)      # remove parentheses

  return(expr_text)
}
