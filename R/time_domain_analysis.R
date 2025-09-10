#' Create Time-Domain Voltage Plot
#'
#' This function creates a time-domain plot showing both applied and ferroelectric
#' voltages, illustrating voltage partitioning effects in ferroelectric capacitors.
#'
#' @param df A data frame containing ferroelectric simulation data with columns:
#'   time, v_app, v_fe
#'
#' @return A ggplot2 object showing voltage evolution over time
#'
#' @details
#' The plot displays both applied voltage and ferroelectric voltage as a function
#' of time, using different colors to distinguish the signals. Scientific notation
#' is used for time axis formatting.
#'
#' @examples
#' \dontrun{
#' # Create voltage time-domain plot
#' data <- process_ferroelectric_data("simulation_data.csv")
#' plot_voltage_time(data)
#' }
#'
#' @export
plot_voltage_time <- function(df) {
  df %>%
    ggplot2::ggplot(ggplot2::aes(x = time)) +
    ggplot2::geom_line(ggplot2::aes(y = v_app, color = "Applied"), size = 1) +
    ggplot2::geom_line(ggplot2::aes(y = v_fe, color = "Ferroelectric"), size = 1) +
    ggplot2::scale_x_continuous(labels = scales::scientific) +
    ggplot2::scale_color_manual(values = c("Applied" = "blue", "Ferroelectric" = "red")) +
    ggplot2::labs(
      title = "Voltage vs Time", 
      x = "Time (s)", 
      y = "Voltage (V)",
      color = "Signal Type"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = ggplot2::element_text(hjust = 0.5)
    )
}

#' Create Time-Domain Polarization Plot
#'
#' This function creates a time-domain plot showing polarization evolution,
#' highlighting the characteristic switching behavior of ferroelectric devices.
#'
#' @param df A data frame containing ferroelectric simulation data with columns:
#'   time, polarization
#'
#' @return A ggplot2 object showing polarization evolution over time
#'
#' @examples
#' \dontrun{
#' # Create polarization time-domain plot
#' plot_polarization_time(data)
#' }
#'
#' @export
plot_polarization_time <- function(df) {
  df %>%
    ggplot2::ggplot(ggplot2::aes(x = time, y = polarization)) +
    ggplot2::geom_line(color = "darkgreen", size = 1) +
    ggplot2::scale_x_continuous(labels = scales::scientific) +
    ggplot2::labs(
      title = "Polarization vs Time", 
      x = "Time (s)", 
      y = "P (C/m²)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
}

#' Create Time-Domain Current Plot
#'
#' This function creates a time-domain plot showing the evolution of different
#' current components (displacement, leakage, and total current).
#'
#' @param df A data frame containing ferroelectric simulation data with columns:
#'   time, i_disp, i_leak, i_total
#'
#' @return A ggplot2 object showing current components evolution over time
#'
#' @examples
#' \dontrun{
#' # Create current time-domain plot
#' plot_current_time(data)
#' }
#'
#' @export
plot_current_time <- function(df) {
  df %>%
    dplyr::select(time, i_disp, i_leak, i_total) %>%
    tidyr::pivot_longer(-time, names_to = "type", values_to = "current") %>%
    ggplot2::ggplot(ggplot2::aes(x = time, y = current, color = type)) +
    ggplot2::geom_line(size = 1) +
    ggplot2::scale_x_continuous(labels = scales::scientific) +
    ggplot2::scale_y_continuous(labels = scales::scientific) +
    ggplot2::scale_color_manual(
      values = c("i_disp" = "blue", "i_leak" = "red", "i_total" = "black"),
      labels = c("i_disp" = "Displacement", "i_leak" = "Leakage", "i_total" = "Total")
    ) +
    ggplot2::labs(
      title = "Current vs Time", 
      x = "Time (s)", 
      y = "Current (A)",
      color = "Current Type"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = ggplot2::element_text(hjust = 0.5)
    )
}

#' Create Time-Domain Capacitance Plot
#'
#' This function creates a time-domain plot showing the corrected capacitance
#' evolution, illustrating polarization-dependent modulation effects.
#'
#' @param df A data frame containing ferroelectric simulation data with columns:
#'   time, c_corrected (calculated by calculate_derived_variables)
#'
#' @return A ggplot2 object showing capacitance evolution over time
#'
#' @examples
#' \dontrun{
#' # Create capacitance time-domain plot
#' plot_capacitance_time(data)
#' }
#'
#' @export
plot_capacitance_time <- function(df) {
  df %>%
    ggplot2::ggplot(ggplot2::aes(x = time, y = c_corrected)) +
    ggplot2::geom_line(color = "orange", size = 1) +
    ggplot2::scale_x_continuous(labels = scales::scientific) +
    ggplot2::labs(
      title = "Corrected Capacitance vs Time", 
      x = "Time (s)", 
      y = "C (F)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
}

#' Create Combined Time-Domain Analysis Plot
#'
#' This function creates a comprehensive 2x2 layout showing all major time-domain
#' characteristics: voltage, polarization, current, and capacitance.
#'
#' @param df A data frame containing processed ferroelectric simulation data
#'
#' @return A patchwork object combining all time-domain plots
#'
#' @examples
#' \dontrun{
#' # Create comprehensive time-domain analysis
#' plot_time_domain_combined(data)
#' }
#'
#' @export
plot_time_domain_combined <- function(df) {
  p1 <- plot_voltage_time(df)
  p2 <- plot_polarization_time(df)
  p3 <- plot_current_time(df)
  p4 <- plot_capacitance_time(df)
  
  combined_plot <- (p1 | p2) / (p3 | p4)
  combined_plot + patchwork::plot_annotation(
    title = "Time-Domain Analysis of Ferroelectric Capacitor",
    theme = ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5))
  )
}

#' Analyze Switching Dynamics
#'
#' This function calculates key switching parameters from the time-domain data,
#' including switching times, peak currents, and transition rates.
#'
#' @param df A data frame containing processed ferroelectric simulation data
#'
#' @return A list containing switching analysis results:
#'   \describe{
#'     \item{switching_times}{Times at which major polarization transitions occur}
#'     \item{peak_currents}{Peak displacement current values}
#'     \item{transition_rates}{Average switching rates}
#'     \item{coercive_voltages}{Voltages at which switching occurs}
#'   }
#'
#' @examples
#' \dontrun{
#' # Analyze switching dynamics
#' switching_analysis <- analyze_switching_dynamics(data)
#' print(switching_analysis)
#' }
#'
#' @export
analyze_switching_dynamics <- function(df) {
  # Find polarization switching events (large dp/dt values)
  switching_threshold <- 0.8 * max(abs(df$dp_dt), na.rm = TRUE)
  switching_events <- df %>%
    dplyr::filter(abs(dp_dt) > switching_threshold) %>%
    dplyr::arrange(time)
  
  # Find peak currents
  peak_positive_current <- max(df$i_disp, na.rm = TRUE)
  peak_negative_current <- min(df$i_disp, na.rm = TRUE)
  
  # Estimate coercive voltages
  pos_switch <- df %>%
    dplyr::filter(dp_dt == max(dp_dt, na.rm = TRUE)) %>%
    dplyr::slice(1)
  
  neg_switch <- df %>%
    dplyr::filter(dp_dt == min(dp_dt, na.rm = TRUE)) %>%
    dplyr::slice(1)
  
  list(
    switching_times = switching_events$time,
    peak_currents = list(
      positive = peak_positive_current,
      negative = peak_negative_current
    ),
    transition_rates = list(
      max_positive = max(df$dp_dt, na.rm = TRUE),
      max_negative = min(df$dp_dt, na.rm = TRUE)
    ),
    coercive_voltages = list(
      positive = pos_switch$v_fe,
      negative = neg_switch$v_fe
    )
  )
}

#' Create Switching Dynamics Plot
#'
#' This function creates a specialized plot showing polarization and its switching
#' rate (dP/dt) to visualize switching dynamics.
#'
#' @param df A data frame containing processed ferroelectric simulation data
#'
#' @return A ggplot2 object showing switching dynamics
#'
#' @examples
#' \dontrun{
#' # Create switching dynamics plot
#' plot_switching_dynamics(data)
#' }
#'
#' @export
plot_switching_dynamics <- function(df) {
  df %>%
    ggplot2::ggplot(ggplot2::aes(x = time)) +
    ggplot2::geom_line(ggplot2::aes(y = polarization), color = "blue", size = 1) +
    ggplot2::geom_line(ggplot2::aes(y = dp_dt * 1e6), color = "red", alpha = 0.7, size = 1) +
    ggplot2::scale_x_continuous(labels = scales::scientific) +
    ggplot2::labs(
      title = "Switching Dynamics", 
      subtitle = "Blue: Polarization, Red: dP/dt (×1e6)",
      x = "Time (s)", 
      y = "Value"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      plot.subtitle = ggplot2::element_text(hjust = 0.5)
    )
}