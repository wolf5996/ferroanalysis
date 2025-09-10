#' Create Current-Voltage Characteristics Plot
#'
#' This function creates I-V characteristics plots showing the relationship between
#' voltage and different current components in ferroelectric capacitors.
#'
#' @param df A data frame containing ferroelectric simulation data with columns:
#'   v_app, i_disp, i_leak, i_total
#' @param use_doped_leakage Logical indicating whether to use doped leakage current
#'   if available (i_leak_doped column)
#' @param current_types Character vector specifying which current types to plot.
#'   Options: "displacement", "leakage", "total"
#'
#' @return A ggplot2 object showing I-V characteristics
#'
#' @details
#' The I-V characteristics reveal different conduction mechanisms:
#' - Displacement current: related to polarization switching
#' - Leakage current: background conduction through the dielectric
#' - Total current: sum of displacement and leakage components
#'
#' @examples
#' \dontrun{
#' # Create I-V plot with all current types
#' data <- process_ferroelectric_data("simulation_data.csv")
#' plot_iv_characteristics(data)
#' 
#' # Plot only displacement current
#' plot_iv_characteristics(data, current_types = "displacement")
#' }
#'
#' @export
plot_iv_characteristics <- function(df, use_doped_leakage = TRUE, 
                                   current_types = c("displacement", "leakage", "total")) {
  
  # Prepare current data
  current_data <- df %>%
    dplyr::select(v_app, i_disp, i_total, 
                 if(use_doped_leakage && "i_leak_doped" %in% names(df)) i_leak_doped else i_leak) %>%
    dplyr::rename(
      displacement = i_disp,
      total = i_total,
      leakage = if(use_doped_leakage && "i_leak_doped" %in% names(df)) i_leak_doped else i_leak
    )
  
  # Filter selected current types
  selected_currents <- current_data %>%
    dplyr::select(v_app, all_of(current_types)) %>%
    tidyr::pivot_longer(-v_app, names_to = "type", values_to = "current")
  
  # Create color mapping
  color_mapping <- c(
    "displacement" = "blue",
    "leakage" = "red", 
    "total" = "black"
  )
  
  selected_currents %>%
    ggplot2::ggplot(ggplot2::aes(x = v_app, y = current, color = type)) +
    ggplot2::geom_path(size = 1.2) +
    ggplot2::scale_y_continuous(labels = scales::scientific) +
    ggplot2::scale_color_manual(
      values = color_mapping,
      labels = c(
        "displacement" = "Displacement",
        "leakage" = if(use_doped_leakage) "Leakage (Doped)" else "Leakage",
        "total" = "Total"
      )
    ) +
    ggplot2::labs(
      title = if(use_doped_leakage) "I-V Characteristics (Doping Corrected)" else "I-V Characteristics",
      x = "Voltage (V)",
      y = "Current (A)",
      color = "Current Type"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      legend.position = "bottom"
    )
}

#' Create Capacitance-Voltage Characteristics Plot
#'
#' This function creates C-V characteristics plots showing the voltage-dependent
#' capacitance behavior in ferroelectric capacitors.
#'
#' @param df A data frame containing ferroelectric simulation data with columns:
#'   v_app, c_corrected, time
#' @param color_by Character string specifying coloring scheme: "time" or "none"
#'
#' @return A ggplot2 object showing C-V characteristics
#'
#' @details
#' The C-V characteristics show the voltage dependence of capacitance, which
#' typically exhibits a peak near the coercive voltage due to domain switching.
#' The corrected capacitance accounts for ferroelectric dielectric behavior.
#'
#' @examples
#' \dontrun{
#' # Create C-V plot colored by time
#' plot_cv_characteristics(data)
#' 
#' # Create single-color C-V plot
#' plot_cv_characteristics(data, color_by = "none")
#' }
#'
#' @export
plot_cv_characteristics <- function(df, color_by = "time") {
  p <- df %>%
    ggplot2::ggplot(ggplot2::aes(x = v_app, y = c_corrected))
  
  if (color_by == "time") {
    p <- p +
      ggplot2::geom_path(ggplot2::aes(color = time), size = 1.2) +
      viridis::scale_color_viridis_c(labels = scales::scientific, name = "Time (s)")
  } else {
    p <- p +
      ggplot2::geom_path(color = "orange", size = 1.2)
  }
  
  p +
    ggplot2::labs(
      title = "Corrected C-V Characteristics (Peak at ~1V)",
      x = "Voltage (V)",
      y = "Capacitance (F)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      legend.position = "right"
    )
}

#' Analyze Current Components
#'
#' This function performs detailed analysis of current components, extracting
#' key metrics such as peak values, switching currents, and leakage levels.
#'
#' @param df A data frame containing processed ferroelectric simulation data
#'
#' @return A list containing current analysis results:
#'   \describe{
#'     \item{displacement_peaks}{Peak displacement current values}
#'     \item{leakage_stats}{Leakage current statistics}
#'     \item{switching_currents}{Current values at switching events}
#'     \item{current_ratios}{Ratios between different current components}
#'   }
#'
#' @examples
#' \dontrun{
#' # Analyze current components
#' current_analysis <- analyze_current_components(data)
#' print(current_analysis)
#' }
#'
#' @export
analyze_current_components <- function(df) {
  # Displacement current analysis
  disp_peak_pos <- max(df$i_disp, na.rm = TRUE)
  disp_peak_neg <- min(df$i_disp, na.rm = TRUE)
  
  # Leakage current analysis
  leakage_mean <- mean(df$i_leak, na.rm = TRUE)
  leakage_max <- max(df$i_leak, na.rm = TRUE)
  leakage_min <- min(df$i_leak, na.rm = TRUE)
  
  # Find switching current values (at maximum |dp/dt|)
  max_switch_pos_idx <- which.max(df$dp_dt)
  max_switch_neg_idx <- which.min(df$dp_dt)
  
  switch_current_pos <- df$i_total[max_switch_pos_idx]
  switch_current_neg <- df$i_total[max_switch_neg_idx]
  
  # Calculate current ratios
  peak_ratio <- abs(disp_peak_pos / disp_peak_neg)  # Asymmetry ratio
  avg_disp_to_leak_ratio <- mean(abs(df$i_disp), na.rm = TRUE) / leakage_mean
  
  list(
    displacement_peaks = list(
      positive = disp_peak_pos,
      negative = disp_peak_neg,
      asymmetry_ratio = peak_ratio
    ),
    leakage_stats = list(
      mean = leakage_mean,
      max = leakage_max,
      min = leakage_min,
      range = leakage_max - leakage_min
    ),
    switching_currents = list(
      positive_switching = switch_current_pos,
      negative_switching = switch_current_neg
    ),
    current_ratios = list(
      displacement_to_leakage = avg_disp_to_leak_ratio,
      peak_asymmetry = peak_ratio
    )
  )
}

#' Create Switching Current Analysis Plot
#'
#' This function creates a specialized plot showing displacement current peaks
#' during polarization switching events, highlighting switching dynamics.
#'
#' @param df A data frame containing processed ferroelectric simulation data
#'
#' @return A ggplot2 object showing switching current characteristics
#'
#' @examples
#' \dontrun{
#' # Create switching current plot
#' plot_switching_current(data)
#' }
#'
#' @export
plot_switching_current <- function(df) {
  # Identify switching events (high |dp/dt|)
  switching_threshold <- 0.5 * max(abs(df$dp_dt), na.rm = TRUE)
  
  df %>%
    dplyr::mutate(
      is_switching = abs(dp_dt) > switching_threshold,
      current_type = dplyr::case_when(
        is_switching & dp_dt > 0 ~ "Positive Switching",
        is_switching & dp_dt < 0 ~ "Negative Switching", 
        TRUE ~ "Non-switching"
      )
    ) %>%
    ggplot2::ggplot(ggplot2::aes(x = v_app, y = i_disp)) +
    ggplot2::geom_path(color = "gray70", size = 0.5) +
    ggplot2::geom_point(
      data = . %>% dplyr::filter(is_switching),
      ggplot2::aes(color = current_type),
      size = 2
    ) +
    ggplot2::scale_color_manual(
      values = c("Positive Switching" = "red", "Negative Switching" = "blue")
    ) +
    ggplot2::labs(
      title = "Switching Current Characteristics",
      subtitle = "Displacement current peaks during polarization reversal",
      x = "Applied Voltage (V)",
      y = "Displacement Current (A)",
      color = "Switching Type"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      plot.subtitle = ggplot2::element_text(hjust = 0.5),
      legend.position = "bottom"
    )
}

#' Calculate Conduction Mechanism Parameters
#'
#' This function analyzes the current-voltage relationship to identify different
#' conduction mechanisms such as ohmic, Schottky emission, and Poole-Frenkel.
#'
#' @param df A data frame containing ferroelectric simulation data
#' @param voltage_range Numeric vector of length 2 specifying voltage range for analysis
#'
#' @return A list containing fitted parameters for different conduction mechanisms
#'
#' @examples
#' \dontrun{
#' # Analyze conduction mechanisms in low voltage range
#' conduction_analysis <- calculate_conduction_mechanisms(data, c(-0.5, 0.5))
#' }
#'
#' @export
calculate_conduction_mechanisms <- function(df, voltage_range = c(-1, 1)) {
  # Filter data to specified voltage range
  analysis_data <- df %>%
    dplyr::filter(v_app >= voltage_range[1] & v_app <= voltage_range[2]) %>%
    dplyr::filter(abs(i_leak) > 0)  # Remove zero current points
  
  if (nrow(analysis_data) < 10) {
    warning("Insufficient data points for conduction mechanism analysis")
    return(NULL)
  }
  
  # Ohmic conduction: I = G*V (linear relationship)
  ohmic_fit <- lm(i_leak ~ v_app, data = analysis_data)
  conductance <- coef(ohmic_fit)[2]
  
  # Schottky emission: ln(I) = ln(A) + qV/(kT) - barrier analysis
  # For small voltages, approximate exponential behavior
  schottky_data <- analysis_data %>%
    dplyr::filter(i_leak > 0) %>%
    dplyr::mutate(ln_current = log(abs(i_leak)))
  
  if (nrow(schottky_data) > 5) {
    schottky_fit <- lm(ln_current ~ v_app, data = schottky_data)
    schottky_slope <- coef(schottky_fit)[2]
  } else {
    schottky_slope <- NA
  }
  
  # Poole-Frenkel: ln(I/V) vs sqrt(V)
  pf_data <- analysis_data %>%
    dplyr::filter(abs(v_app) > 0.1 & i_leak > 0) %>%
    dplyr::mutate(
      ln_i_over_v = log(abs(i_leak) / abs(v_app)),
      sqrt_v = sqrt(abs(v_app))
    )
  
  if (nrow(pf_data) > 5) {
    pf_fit <- lm(ln_i_over_v ~ sqrt_v, data = pf_data)
    pf_slope <- coef(pf_fit)[2]
  } else {
    pf_slope <- NA
  }
  
  list(
    ohmic = list(
      conductance = conductance,
      r_squared = summary(ohmic_fit)$r.squared
    ),
    schottky = list(
      slope = schottky_slope,
      r_squared = if (!is.na(schottky_slope)) summary(schottky_fit)$r.squared else NA
    ),
    poole_frenkel = list(
      slope = pf_slope,
      r_squared = if (!is.na(pf_slope)) summary(pf_fit)$r.squared else NA
    ),
    voltage_range = voltage_range
  )
}