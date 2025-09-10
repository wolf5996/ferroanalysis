#' Load and Process Ferroelectric Simulation Data
#'
#' This function loads ferroelectric simulation data from a CSV file and applies
#' standardized column naming and initial processing.
#'
#' @param file_path Character string specifying the path to the CSV file containing
#'   ferroelectric simulation data. The CSV should contain 8 columns in the order:
#'   time, applied voltage, ferroelectric voltage, polarization, displacement current,
#'   leakage current, total current, depletion capacitance.
#'
#' @return A tibble with standardized column names:
#'   \describe{
#'     \item{time}{Time points (s)}
#'     \item{v_app}{Applied voltage (V)}
#'     \item{v_fe}{Ferroelectric voltage (V)}
#'     \item{polarization}{Polarization (C/m²)}
#'     \item{i_disp}{Displacement current (A)}
#'     \item{i_leak}{Leakage current (A)}
#'     \item{i_total}{Total current (A)}
#'     \item{c_depl}{Depletion capacitance (F)}
#'   }
#'
#' @examples
#' \dontrun{
#' # Load ferroelectric simulation data
#' data <- load_ferroelectric_data("path/to/simulation_data.csv")
#' 
#' # Check data structure
#' glimpse(data)
#' }
#'
#' @export
load_ferroelectric_data <- function(file_path) {
  if (!file.exists(file_path)) {
    stop("File not found: ", file_path)
  }
  
  df <- readr::read_csv(file_path, show_col_types = FALSE)
  
  # Check if we have the expected 8 columns
  if (ncol(df) != 8) {
    stop("Expected 8 columns in the CSV file, found ", ncol(df))
  }
  
  # Apply standardized column names
  colnames(df) <- c("time", "v_app", "v_fe", "polarization", 
                    "i_disp", "i_leak", "i_total", "c_depl")
  
  # Ensure all columns are numeric
  df <- df %>%
    dplyr::mutate(dplyr::across(everything(), as.numeric))
  
  return(df)
}

#' Calculate Derived Variables for Ferroelectric Analysis
#'
#' This function calculates additional derived variables needed for comprehensive
#' ferroelectric device analysis, including voltage partitioning, switching dynamics,
#' and corrected capacitance values.
#'
#' @param df A data frame containing ferroelectric simulation data with columns:
#'   time, v_app, v_fe, polarization, i_disp, i_leak, i_total, c_depl
#'
#' @return The input data frame with additional derived columns:
#'   \describe{
#'     \item{c_fe}{Ferroelectric capacitance correction factor}
#'     \item{c_corrected}{Corrected capacitance (F)}
#'     \item{v_depl}{Depletion voltage (V)}
#'     \item{voltage_efficiency}{Voltage efficiency (v_fe/v_app)}
#'     \item{depl_fraction}{Depletion voltage fraction (v_depl/v_app)}
#'     \item{dp_dt}{Polarization switching rate (C/m²/s)}
#'     \item{di_dt}{Current switching rate (A/s)}
#'     \item{v_app_norm, v_fe_norm, pol_norm}{Normalized variables for correlation analysis}
#'   }
#'
#' @details
#' The function calculates several important derived variables:
#' - Corrected capacitance that accounts for ferroelectric dielectric behavior
#' - Voltage partitioning between ferroelectric and depletion regions
#' - Switching dynamics through temporal derivatives
#' - Normalized variables for statistical analysis
#'
#' @examples
#' \dontrun{
#' # Load and process data
#' data <- load_ferroelectric_data("simulation_data.csv")
#' data_processed <- calculate_derived_variables(data)
#' 
#' # Check new columns
#' colnames(data_processed)
#' }
#'
#' @export
calculate_derived_variables <- function(df) {
  df <- df %>%
    dplyr::mutate(
      # Corrected capacitance based on dielectric behavior (max at ~1V)
      # Using ferroelectric voltage for more accurate capacitance representation
      c_fe = abs(1 / (1 + (v_fe - 1)^2 * 0.1)),  # Peaked at 1V
      c_corrected = c_depl * c_fe,
      
      # Voltage efficiency and partitioning
      v_depl = v_app - v_fe,
      voltage_efficiency = ifelse(v_app != 0, v_fe / v_app, 0),
      depl_fraction = ifelse(v_app != 0, v_depl / v_app, 0),
      
      # Switching dynamics
      dp_dt = c(0, diff(polarization) / diff(time)),
      di_dt = c(0, diff(i_total) / diff(time)),
      
      # Normalize for correlation analysis
      v_app_norm = (v_app - mean(v_app, na.rm = TRUE)) / sd(v_app, na.rm = TRUE),
      v_fe_norm = (v_fe - mean(v_fe, na.rm = TRUE)) / sd(v_fe, na.rm = TRUE),
      pol_norm = (polarization - mean(polarization, na.rm = TRUE)) / sd(polarization, na.rm = TRUE)
    )
  
  return(df)
}

#' Calculate Parametric Variations for Analysis
#'
#' This function adds parametric variations to the dataset for studying frequency
#' response, thickness effects, and doping concentration impacts.
#'
#' @param df A data frame containing processed ferroelectric data
#'
#' @return The input data frame with additional parametric columns:
#'   \describe{
#'     \item{freq_1kHz, freq_100kHz}{Frequency analysis components}
#'     \item{thickness_factor}{Normalized thickness variation factor}
#'     \item{pol_thickness}{Polarization modified by thickness effects}
#'     \item{doping_factor}{Doping concentration factor}
#'     \item{i_leak_doped}{Leakage current modified by doping effects}
#'   }
#'
#' @details
#' This function simulates the effects of various device parameters:
#' - Frequency response at 1 kHz and 100 kHz
#' - Thickness variations and their impact on polarization
#' - Doping concentration effects on leakage current
#'
#' @examples
#' \dontrun{
#' # Add parametric variations
#' data_full <- data_processed %>%
#'   calculate_parametric_variations()
#' }
#'
#' @export
calculate_parametric_variations <- function(df) {
  df <- df %>%
    dplyr::mutate(
      # Frequency analysis parameters (simulated for different frequencies)
      freq_1kHz = sin(2*pi*1000*time) * 0.1,  # 1kHz component
      freq_100kHz = sin(2*pi*100000*time) * 0.05,  # 100kHz component
      
      # Thickness variation simulation (normalized thickness factor)
      thickness_factor = 1 + 0.2 * sin(time * 10000),
      pol_thickness = polarization * thickness_factor,
      
      # Doping concentration effect on leakage
      doping_factor = 1 + 0.3 * (v_fe/max(abs(v_fe), na.rm = TRUE)),
      i_leak_doped = i_leak * doping_factor
    )
  
  return(df)
}

#' Complete Data Processing Pipeline
#'
#' This function provides a complete data processing pipeline that loads ferroelectric
#' data and applies all standard processing steps.
#'
#' @param file_path Character string specifying the path to the CSV file
#'
#' @return A fully processed tibble ready for analysis and visualization
#'
#' @examples
#' \dontrun{
#' # Complete processing in one step
#' data <- process_ferroelectric_data("path/to/simulation_data.csv")
#' }
#'
#' @export
process_ferroelectric_data <- function(file_path) {
  data <- load_ferroelectric_data(file_path) %>%
    calculate_derived_variables() %>%
    calculate_parametric_variations()
  
  return(data)
}