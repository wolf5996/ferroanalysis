#' Create Polarization-Voltage Hysteresis Loop Plot
#'
#' This function creates the classic P-V hysteresis loop plot, which is the
#' fundamental characteristic of ferroelectric materials showing the relationship
#' between polarization and applied voltage.
#'
#' @param df A data frame containing ferroelectric simulation data with columns:
#'   v_app, polarization, time
#' @param color_by Character string specifying how to color the hysteresis loop.
#'   Options: "time" (default), "none" for single color
#'
#' @return A ggplot2 object showing the P-V hysteresis loop
#'
#' @details
#' The hysteresis loop reveals key ferroelectric parameters including:
#' - Saturation polarization
#' - Remnant polarization
#' - Coercive voltage
#' - Loop squareness
#' 
#' When colored by time, the plot shows the temporal progression through the loop.
#'
#' @examples
#' \dontrun{
#' # Create basic hysteresis loop
#' data <- process_ferroelectric_data("simulation_data.csv")
#' plot_hysteresis_loop(data)
#' 
#' # Create single-color loop
#' plot_hysteresis_loop(data, color_by = "none")
#' }
#'
#' @export
plot_hysteresis_loop <- function(df, color_by = "time") {
  p <- df %>%
    ggplot2::ggplot(ggplot2::aes(x = v_app, y = polarization))
  
  if (color_by == "time") {
    p <- p +
      ggplot2::geom_path(ggplot2::aes(color = time), size = 1.2) +
      ggplot2::scale_color_viridis_c(labels = scales::scientific, name = "Time (s)")
  } else {
    p <- p +
      ggplot2::geom_path(color = "darkblue", size = 1.2)
  }
  
  p +
    ggplot2::labs(
      title = "P-V Hysteresis Loop", 
      x = "Applied Voltage (V)", 
      y = "Polarization (C/m²)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      legend.position = "right"
    )
}

#' Extract Hysteresis Parameters
#'
#' This function extracts key hysteresis parameters from the P-V loop data,
#' providing quantitative metrics for ferroelectric characterization.
#'
#' @param df A data frame containing ferroelectric simulation data with columns:
#'   v_app, polarization, time
#'
#' @return A list containing extracted hysteresis parameters:
#'   \describe{
#'     \item{Ps_positive}{Positive saturation polarization (C/m²)}
#'     \item{Ps_negative}{Negative saturation polarization (C/m²)}
#'     \item{Pr_positive}{Positive remnant polarization (C/m²)}
#'     \item{Pr_negative}{Negative remnant polarization (C/m²)}
#'     \item{Vc_positive}{Positive coercive voltage (V)}
#'     \item{Vc_negative}{Negative coercive voltage (V)}
#'     \item{loop_squareness}{Loop squareness ratio (Pr/Ps)}
#'     \item{voltage_imprint}{Voltage imprint (V)}
#'   }
#'
#' @examples
#' \dontrun{
#' # Extract hysteresis parameters
#' data <- process_ferroelectric_data("simulation_data.csv")
#' params <- extract_hysteresis_parameters(data)
#' print(params)
#' }
#'
#' @export
extract_hysteresis_parameters <- function(df) {
  # Find saturation polarizations
  Ps_positive <- max(df$polarization, na.rm = TRUE)
  Ps_negative <- min(df$polarization, na.rm = TRUE)
  
  # Find remnant polarizations (P at V = 0)
  # Approximate by finding points closest to zero voltage
  zero_crossing_indices <- which(abs(df$v_app) < 0.05)  # Within 50mV of zero
  
  if (length(zero_crossing_indices) >= 2) {
    zero_crossings <- df[zero_crossing_indices, ]
    Pr_positive <- max(zero_crossings$polarization, na.rm = TRUE)
    Pr_negative <- min(zero_crossings$polarization, na.rm = TRUE)
  } else {
    # Fallback: interpolate at V = 0
    Pr_positive <- approx(df$v_app, df$polarization, xout = 0)$y
    Pr_negative <- Pr_positive  # Same point for single crossing
  }
  
  # Find coercive voltages (V at P = 0)
  # Approximate by finding points closest to zero polarization
  zero_pol_indices <- which(abs(df$polarization) < 0.05 * max(abs(df$polarization)))
  
  if (length(zero_pol_indices) >= 2) {
    zero_pol_points <- df[zero_pol_indices, ]
    Vc_positive <- max(zero_pol_points$v_app, na.rm = TRUE)
    Vc_negative <- min(zero_pol_points$v_app, na.rm = TRUE)
  } else {
    # Fallback: use approximate values based on switching points
    switch_analysis <- analyze_switching_dynamics(df)
    Vc_positive <- switch_analysis$coercive_voltages$positive
    Vc_negative <- switch_analysis$coercive_voltages$negative
  }
  
  # Calculate derived parameters
  loop_squareness <- mean(abs(c(Pr_positive, Pr_negative)) / abs(c(Ps_positive, Ps_negative)))
  voltage_imprint <- (Vc_positive + Vc_negative) / 2
  
  list(
    Ps_positive = Ps_positive,
    Ps_negative = Ps_negative,
    Pr_positive = Pr_positive,
    Pr_negative = Pr_negative,
    Vc_positive = Vc_positive,
    Vc_negative = Vc_negative,
    loop_squareness = loop_squareness,
    voltage_imprint = voltage_imprint,
    loop_area = calculate_hysteresis_area(df)
  )
}

#' Calculate Hysteresis Loop Area
#'
#' This function calculates the area enclosed by the hysteresis loop, which is
#' related to the energy dissipated per switching cycle.
#'
#' @param df A data frame containing ferroelectric simulation data with columns:
#'   v_app, polarization
#'
#' @return Numeric value representing the hysteresis loop area (V·C/m²)
#'
#' @details
#' The area calculation uses the shoelace formula for polygon area, treating
#' the hysteresis loop as a closed polygon in the P-V plane.
#'
#' @examples
#' \dontrun{
#' # Calculate loop area
#' area <- calculate_hysteresis_area(data)
#' }
#'
#' @export
calculate_hysteresis_area <- function(df) {
  # Order data points to form a proper closed loop
  df_ordered <- df[order(df$v_app, df$time), ]
  
  # Apply shoelace formula
  n <- nrow(df_ordered)
  x <- df_ordered$v_app
  y <- df_ordered$polarization
  
  # Close the loop
  x <- c(x, x[1])
  y <- c(y, y[1])
  
  area <- 0.5 * abs(sum((x[1:n] * y[2:(n+1)]) - (x[2:(n+1)] * y[1:n])))
  return(area)
}

#' Create Hysteresis Parameters Summary Table
#'
#' This function creates a formatted summary table of hysteresis parameters
#' suitable for reports and presentations.
#'
#' @param hysteresis_params List of hysteresis parameters from extract_hysteresis_parameters()
#'
#' @return A data frame with formatted parameter values and units
#'
#' @examples
#' \dontrun{
#' # Create parameter summary
#' params <- extract_hysteresis_parameters(data)
#' summary_table <- create_hysteresis_summary(params)
#' print(summary_table)
#' }
#'
#' @export
create_hysteresis_summary <- function(hysteresis_params) {
  data.frame(
    Parameter = c(
      "Saturation Polarization (Ps+)",
      "Saturation Polarization (Ps-)", 
      "Remnant Polarization (Pr+)",
      "Remnant Polarization (Pr-)",
      "Coercive Voltage (Vc+)",
      "Coercive Voltage (Vc-)",
      "Loop Squareness",
      "Voltage Imprint",
      "Loop Area"
    ),
    Value = c(
      round(hysteresis_params$Ps_positive, 3),
      round(hysteresis_params$Ps_negative, 3),
      round(hysteresis_params$Pr_positive, 3),
      round(hysteresis_params$Pr_negative, 3),
      round(hysteresis_params$Vc_positive, 3),
      round(hysteresis_params$Vc_negative, 3),
      round(hysteresis_params$loop_squareness, 3),
      round(hysteresis_params$voltage_imprint, 3),
      round(hysteresis_params$loop_area, 3)
    ),
    Unit = c(
      "C/m²", "C/m²", "C/m²", "C/m²", 
      "V", "V", "-", "V", "V·C/m²"
    ),
    Description = c(
      "Maximum achievable polarization",
      "Maximum negative polarization",
      "Polarization at zero field (positive)",
      "Polarization at zero field (negative)",
      "Positive switching threshold",
      "Negative switching threshold",
      "Pr/Ps ratio (0-1)",
      "Average coercive voltage shift",
      "Energy dissipated per cycle"
    )
  )
}

#' Compare Multiple Hysteresis Loops
#'
#' This function creates a comparison plot of multiple hysteresis loops,
#' useful for studying parameter variations or different materials.
#'
#' @param data_list List of data frames, each containing ferroelectric data
#' @param labels Character vector of labels for each dataset
#'
#' @return A ggplot2 object showing multiple hysteresis loops
#'
#' @examples
#' \dontrun{
#' # Compare different conditions
#' data1 <- process_ferroelectric_data("condition1.csv")
#' data2 <- process_ferroelectric_data("condition2.csv")
#' compare_hysteresis_loops(list(data1, data2), c("Condition 1", "Condition 2"))
#' }
#'
#' @import ggplot2
#' @export
compare_hysteresis_loops <- function(data_list, labels) {
  if (length(data_list) != length(labels)) {
    stop("Number of datasets must match number of labels")
  }
  
  # Combine datasets with labels
  combined_data <- data_list %>%
    purrr::map2(labels, ~dplyr::mutate(.x, dataset = .y)) %>%
    dplyr::bind_rows()
  
  combined_data %>%
    ggplot2::ggplot(ggplot2::aes(x = v_app, y = polarization, color = dataset)) +
    ggplot2::geom_path(size = 1.2, alpha = 0.8) +
    ggplot2::labs(
      title = "Hysteresis Loop Comparison",
      x = "Applied Voltage (V)",
      y = "Polarization (C/m²)",
      color = "Dataset"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      legend.position = "bottom"
    )
}