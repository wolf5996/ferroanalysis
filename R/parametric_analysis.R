#' Create Frequency Response Analysis Plot
#'
#' This function creates a plot showing the frequency-dependent behavior of
#' ferroelectric response, comparing polarization at different frequencies.
#'
#' @param df A data frame containing processed ferroelectric simulation data with
#'   parametric variations calculated
#' @param frequencies Character vector specifying which frequencies to analyze.
#'   Options: "1kHz", "100kHz", or "both" (default)
#'
#' @return A ggplot2 object showing frequency response analysis
#'
#' @details
#' The frequency response analysis reveals how ferroelectric switching behavior
#' changes with excitation frequency. At higher frequencies, incomplete switching
#' may occur due to finite kinetic limitations.
#'
#' @examples
#' \dontrun{
#' # Create frequency response plot
#' data <- process_ferroelectric_data("simulation_data.csv")
#' plot_frequency_response(data)
#' 
#' # Plot only 1kHz response
#' plot_frequency_response(data, frequencies = "1kHz")
#' }
#'
#' @export
plot_frequency_response <- function(df, frequencies = "both") {
  # Calculate frequency-dependent responses
  freq_data <- df %>%
    dplyr::mutate(
      response_1kHz = polarization * (1 + 0.1 * cos(2*pi*1000*time)),
      response_100kHz = polarization * (1 + 0.05 * cos(2*pi*100000*time))
    )
  
  # Select frequency data based on input
  if (frequencies == "1kHz") {
    plot_data <- freq_data %>%
      dplyr::select(v_app, polarization, response_1kHz) %>%
      tidyr::pivot_longer(-v_app, names_to = "frequency", values_to = "response")
  } else if (frequencies == "100kHz") {
    plot_data <- freq_data %>%
      dplyr::select(v_app, polarization, response_100kHz) %>%
      tidyr::pivot_longer(-v_app, names_to = "frequency", values_to = "response")
  } else {
    plot_data <- freq_data %>%
      dplyr::select(v_app, response_1kHz, response_100kHz, polarization) %>%
      tidyr::pivot_longer(-v_app, names_to = "frequency", values_to = "response")
  }
  
  # Create color mapping
  color_mapping <- c(
    "polarization" = "black",
    "response_1kHz" = "blue", 
    "response_100kHz" = "red"
  )
  
  label_mapping <- c(
    "polarization" = "Original",
    "response_1kHz" = "1 kHz Response",
    "response_100kHz" = "100 kHz Response"
  )
  
  plot_data %>%
    ggplot2::ggplot(ggplot2::aes(x = v_app, y = response, color = frequency)) +
    ggplot2::geom_path(size = 1.2) +
    ggplot2::scale_color_manual(values = color_mapping, labels = label_mapping) +
    ggplot2::labs(
      title = "Frequency Response Analysis", 
      x = "Applied Voltage (V)", 
      y = "Polarization Response (C/m²)",
      color = "Frequency"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      legend.position = "bottom"
    )
}

#' Create Thickness Effect Analysis Plot
#'
#' This function creates a plot showing how ferroelectric layer thickness
#' affects polarization magnitude and switching characteristics.
#'
#' @param df A data frame containing processed ferroelectric simulation data
#'
#' @return A ggplot2 object showing thickness effect on polarization
#'
#' @examples
#' \dontrun{
#' # Create thickness effect plot
#' plot_thickness_effect(data)
#' }
#'
#' @export
plot_thickness_effect <- function(df) {
  df %>%
    ggplot2::ggplot(ggplot2::aes(x = v_app)) +
    ggplot2::geom_path(ggplot2::aes(y = polarization, color = "Original"), size = 1.2) +
    ggplot2::geom_path(ggplot2::aes(y = pol_thickness, color = "Thickness Effect"), size = 1.2) +
    ggplot2::scale_color_manual(
      values = c("Original" = "blue", "Thickness Effect" = "red")
    ) +
    ggplot2::labs(
      title = "Thickness Effect on Polarization",
      x = "Applied Voltage (V)", 
      y = "Polarization (C/m²)",
      color = "Condition"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      legend.position = "bottom"
    )
}

#' Create Doping Effect Analysis Plot
#'
#' This function creates a plot showing how doping concentration affects
#' leakage current characteristics in ferroelectric devices.
#'
#' @param df A data frame containing processed ferroelectric simulation data
#'
#' @return A ggplot2 object showing doping effect on leakage current
#'
#' @examples
#' \dontrun{
#' # Create doping effect plot
#' plot_doping_effect(data)
#' }
#'
#' @import scales
#' @export
plot_doping_effect <- function(df) {
  df %>%
    ggplot2::ggplot(ggplot2::aes(x = v_app)) +
    ggplot2::geom_path(ggplot2::aes(y = i_leak, color = "Original Leakage"), size = 1.2) +
    ggplot2::geom_path(ggplot2::aes(y = i_leak_doped, color = "Doped Leakage"), size = 1.2) +
    ggplot2::scale_y_continuous(labels = scales::scientific) +
    ggplot2::scale_color_manual(
      values = c("Original Leakage" = "blue", "Doped Leakage" = "red")
    ) +
    ggplot2::labs(
      title = "Doping Effect on Leakage Current",
      x = "Applied Voltage (V)", 
      y = "Leakage Current (A)",
      color = "Condition"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      legend.position = "bottom"
    )
}

#' Analyze Parametric Sensitivity
#'
#' This function performs sensitivity analysis for key parameters, calculating
#' how changes in device parameters affect ferroelectric performance metrics.
#'
#' @param df A data frame containing processed ferroelectric simulation data
#'
#' @return A list containing sensitivity analysis results:
#'   \describe{
#'     \item{frequency_sensitivity}{Impact of frequency on switching metrics}
#'     \item{thickness_sensitivity}{Impact of thickness on polarization}
#'     \item{doping_sensitivity}{Impact of doping on leakage current}
#'     \item{parameter_correlations}{Cross-correlations between parameters}
#'   }
#'
#' @examples
#' \dontrun{
#' # Perform sensitivity analysis
#' sensitivity_results <- analyze_parametric_sensitivity(data)
#' print(sensitivity_results)
#' }
#'
#' @export
analyze_parametric_sensitivity <- function(df) {
  # Frequency sensitivity analysis
  freq_1khz_effect <- mean(abs(df$polarization - df$polarization * (1 + 0.1 * cos(2*pi*1000*df$time))), na.rm = TRUE)
  freq_100khz_effect <- mean(abs(df$polarization - df$polarization * (1 + 0.05 * cos(2*pi*100000*df$time))), na.rm = TRUE)
  
  frequency_sensitivity <- list(
    freq_1kHz_deviation = freq_1khz_effect / mean(abs(df$polarization), na.rm = TRUE),
    freq_100kHz_deviation = freq_100khz_effect / mean(abs(df$polarization), na.rm = TRUE)
  )
  
  # Thickness sensitivity
  thickness_sensitivity <- list(
    max_thickness_effect = max(abs(df$pol_thickness - df$polarization), na.rm = TRUE),
    mean_thickness_effect = mean(abs(df$pol_thickness - df$polarization), na.rm = TRUE),
    relative_thickness_change = mean(abs(df$pol_thickness - df$polarization), na.rm = TRUE) / 
                               mean(abs(df$polarization), na.rm = TRUE)
  )
  
  # Doping sensitivity
  doping_sensitivity <- list(
    max_leakage_change = max(abs(df$i_leak_doped - df$i_leak), na.rm = TRUE),
    mean_leakage_change = mean(abs(df$i_leak_doped - df$i_leak), na.rm = TRUE),
    relative_leakage_change = mean(abs(df$i_leak_doped - df$i_leak), na.rm = TRUE) / 
                             mean(abs(df$i_leak), na.rm = TRUE)
  )
  
  # Parameter correlations
  parameter_correlations <- list(
    thickness_vs_voltage_efficiency = cor(df$thickness_factor, df$voltage_efficiency, use = "complete.obs"),
    doping_vs_total_current = cor(df$doping_factor, df$i_total, use = "complete.obs"),
    thickness_vs_capacitance = cor(df$thickness_factor, df$c_corrected, use = "complete.obs")
  )
  
  list(
    frequency_sensitivity = frequency_sensitivity,
    thickness_sensitivity = thickness_sensitivity,
    doping_sensitivity = doping_sensitivity,
    parameter_correlations = parameter_correlations
  )
}

#' Create Comprehensive Parametric Analysis Plot
#'
#' This function creates a multi-panel plot showing all major parametric effects
#' in a single visualization for comprehensive analysis.
#'
#' @param df A data frame containing processed ferroelectric simulation data
#'
#' @return A patchwork object combining frequency, thickness, and doping analysis
#'
#' @examples
#' \dontrun{
#' # Create comprehensive parametric analysis
#' plot_parametric_analysis_combined(data)
#' }
#'
#' @export
plot_parametric_analysis_combined <- function(df) {
  p1 <- plot_frequency_response(df)
  p2 <- plot_thickness_effect(df) 
  p3 <- plot_doping_effect(df)
  
  combined_plot <- p1 / (p2 | p3)
  combined_plot + patchwork::plot_annotation(
    title = "Comprehensive Parametric Analysis of Ferroelectric Device",
    theme = ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5))
  )
}

#' Create Parameter Optimization Recommendations
#'
#' This function analyzes the parametric sensitivity results and provides
#' recommendations for optimal device design parameters.
#'
#' @param sensitivity_results List from analyze_parametric_sensitivity()
#' @param hysteresis_params List from extract_hysteresis_parameters()
#'
#' @return A data frame with optimization recommendations
#'
#' @examples
#' \dontrun{
#' # Generate optimization recommendations
#' sensitivity_results <- analyze_parametric_sensitivity(data)
#' hysteresis_params <- extract_hysteresis_parameters(data)
#' recommendations <- create_optimization_recommendations(sensitivity_results, hysteresis_params)
#' print(recommendations)
#' }
#'
#' @export
create_optimization_recommendations <- function(sensitivity_results, hysteresis_params) {
  # Frequency optimization
  freq_recommendation <- ifelse(
    sensitivity_results$frequency_sensitivity$freq_1kHz_deviation < 0.1,
    "1 kHz operation suitable",
    "Consider lower frequency operation"
  )
  
  # Thickness optimization
  thickness_recommendation <- ifelse(
    sensitivity_results$thickness_sensitivity$relative_thickness_change < 0.15,
    "Current thickness range acceptable",
    "Optimize thickness uniformity"
  )
  
  # Doping optimization  
  doping_recommendation <- ifelse(
    sensitivity_results$doping_sensitivity$relative_leakage_change < 0.3,
    "Current doping level acceptable",
    "Optimize doping concentration"
  )
  
  # Loop quality assessment
  loop_quality <- ifelse(
    hysteresis_params$loop_squareness > 0.8,
    "Excellent loop squareness",
    "Improve switching completeness"
  )
  
  data.frame(
    Parameter = c(
      "Operating Frequency",
      "Layer Thickness",
      "Doping Concentration", 
      "Hysteresis Quality",
      "Overall Device Performance"
    ),
    Recommendation = c(
      freq_recommendation,
      thickness_recommendation,
      doping_recommendation,
      loop_quality,
      "Review all parameters for optimal integration"
    ),
    Priority = c(
      ifelse(sensitivity_results$frequency_sensitivity$freq_100kHz_deviation > 0.2, "High", "Medium"),
      ifelse(sensitivity_results$thickness_sensitivity$relative_thickness_change > 0.15, "High", "Low"),
      ifelse(sensitivity_results$doping_sensitivity$relative_leakage_change > 0.3, "High", "Medium"),
      ifelse(hysteresis_params$loop_squareness < 0.7, "High", "Low"),
      "Medium"
    ),
    Metric_Value = c(
      round(sensitivity_results$frequency_sensitivity$freq_1kHz_deviation, 3),
      round(sensitivity_results$thickness_sensitivity$relative_thickness_change, 3),
      round(sensitivity_results$doping_sensitivity$relative_leakage_change, 3),
      round(hysteresis_params$loop_squareness, 3),
      "-"
    )
  )
}

#' Save Parametric Analysis Results
#'
#' This function saves all parametric analysis plots and data tables to
#' organized output directories.
#'
#' @param df A data frame containing processed ferroelectric simulation data
#' @param output_dir Character string specifying the base output directory
#' @param save_format Character vector specifying save formats: "png", "pdf", or both
#'
#' @return Invisible NULL (function called for side effects)
#'
#' @examples
#' \dontrun{
#' # Save all parametric analysis results
#' save_parametric_analysis(data, "output/parametric_analysis")
#' }
#'
#' @export
save_parametric_analysis <- function(df, output_dir, save_format = "png") {
  # Create output directories
  dir.create(file.path(output_dir, "figures"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(output_dir, "tables"), recursive = TRUE, showWarnings = FALSE)
  
  # Generate and save plots
  freq_plot <- plot_frequency_response(df)
  thickness_plot <- plot_thickness_effect(df)
  doping_plot <- plot_doping_effect(df)
  combined_plot <- plot_parametric_analysis_combined(df)
  
  # Save plots
  if ("png" %in% save_format) {
    ggplot2::ggsave(file.path(output_dir, "figures", "frequency_response.png"), 
                   freq_plot, width = 8, height = 6, dpi = 300, bg = "white")
    ggplot2::ggsave(file.path(output_dir, "figures", "thickness_effect.png"), 
                   thickness_plot, width = 8, height = 6, dpi = 300, bg = "white")
    ggplot2::ggsave(file.path(output_dir, "figures", "doping_effect.png"), 
                   doping_plot, width = 8, height = 6, dpi = 300, bg = "white")
    ggplot2::ggsave(file.path(output_dir, "figures", "parametric_combined.png"), 
                   combined_plot, width = 12, height = 10, dpi = 300, bg = "white")
  }
  
  if ("pdf" %in% save_format) {
    ggplot2::ggsave(file.path(output_dir, "figures", "frequency_response.pdf"), 
                   freq_plot, width = 8, height = 6)
    ggplot2::ggsave(file.path(output_dir, "figures", "thickness_effect.pdf"), 
                   thickness_plot, width = 8, height = 6)
    ggplot2::ggsave(file.path(output_dir, "figures", "doping_effect.pdf"), 
                   doping_plot, width = 8, height = 6)
    ggplot2::ggsave(file.path(output_dir, "figures", "parametric_combined.pdf"), 
                   combined_plot, width = 12, height = 10)
  }
  
  # Generate and save data tables
  sensitivity_results <- analyze_parametric_sensitivity(df)
  hysteresis_params <- extract_hysteresis_parameters(df)
  optimization_recs <- create_optimization_recommendations(sensitivity_results, hysteresis_params)
  
  readr::write_csv(optimization_recs, file.path(output_dir, "tables", "optimization_recommendations.csv"))
  
  invisible(NULL)
}