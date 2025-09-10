#' Comprehensive Ferroelectric Device Analysis
#'
#' This function provides a complete analysis workflow for ferroelectric device
#' characterization, combining all analysis modules into a single comprehensive
#' analysis pipeline.
#'
#' @param file_path Character string specifying the path to the CSV file containing
#'   ferroelectric simulation data
#' @param output_dir Character string specifying the output directory for results.
#'   If NULL, results are returned but not saved.
#' @param analysis_modules Character vector specifying which analysis modules to run.
#'   Options: "time_domain", "hysteresis", "current_voltage", "voltage_partitioning", 
#'   "parametric", "all" (default)
#' @param save_plots Logical indicating whether to save plots to files
#' @param save_tables Logical indicating whether to save analysis tables
#'
#' @return A list containing all analysis results:
#'   \describe{
#'     \item{processed_data}{Fully processed ferroelectric data}
#'     \item{time_domain}{Time-domain analysis results and plots}
#'     \item{hysteresis}{Hysteresis analysis results and parameters}
#'     \item{current_voltage}{I-V and C-V analysis results}
#'     \item{voltage_partitioning}{Voltage partitioning analysis}
#'     \item{parametric}{Parametric sensitivity analysis}
#'     \item{summary_report}{Executive summary of key findings}
#'   }
#'
#' @details
#' This function provides a complete "one-stop" analysis of ferroelectric device
#' data, automatically running all relevant analysis modules and generating
#' publication-ready outputs. The analysis includes:
#' 
#' - Data loading and preprocessing
#' - Time-domain characterization
#' - Hysteresis loop analysis and parameter extraction
#' - Current-voltage relationships and conduction mechanisms
#' - Voltage partitioning and efficiency analysis
#' - Parametric studies (frequency, thickness, doping effects)
#' - Comprehensive reporting and recommendations
#'
#' @examples
#' \dontrun{
#' # Complete analysis with all modules
#' results <- comprehensive_ferroelectric_analysis("simulation_data.csv")
#' 
#' # Analysis with output saving
#' results <- comprehensive_ferroelectric_analysis(
#'   "simulation_data.csv", 
#'   output_dir = "analysis_results",
#'   save_plots = TRUE,
#'   save_tables = TRUE
#' )
#' 
#' # Selective analysis modules
#' results <- comprehensive_ferroelectric_analysis(
#'   "simulation_data.csv",
#'   analysis_modules = c("hysteresis", "current_voltage")
#' )
#' 
#' # Access specific results
#' print(results$hysteresis$parameters)
#' print(results$summary_report)
#' }
#'
#' @export
comprehensive_ferroelectric_analysis <- function(file_path, 
                                                output_dir = NULL,
                                                analysis_modules = "all",
                                                save_plots = TRUE,
                                                save_tables = TRUE) {
  
  # Validate inputs
  if (!file.exists(file_path)) {
    stop("File not found: ", file_path)
  }
  
  if (analysis_modules[1] == "all") {
    analysis_modules <- c("time_domain", "hysteresis", "current_voltage", 
                         "voltage_partitioning", "parametric")
  }
  
  # Initialize results list
  results <- list()
  
  # Step 1: Data loading and processing
  cat("Loading and processing ferroelectric data...\n")
  processed_data <- process_ferroelectric_data(file_path)
  results$processed_data <- processed_data
  
  # Create output directories if specified
  if (!is.null(output_dir) && (save_plots || save_tables)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    if (save_plots) {
      dir.create(file.path(output_dir, "figures"), recursive = TRUE, showWarnings = FALSE)
    }
    if (save_tables) {
      dir.create(file.path(output_dir, "tables"), recursive = TRUE, showWarnings = FALSE)
    }
  }
  
  # Step 2: Time-domain analysis
  if ("time_domain" %in% analysis_modules) {
    cat("Performing time-domain analysis...\n")
    
    time_domain_results <- list(
      voltage_plot = plot_voltage_time(processed_data),
      polarization_plot = plot_polarization_time(processed_data),
      current_plot = plot_current_time(processed_data),
      capacitance_plot = plot_capacitance_time(processed_data),
      combined_plot = plot_time_domain_combined(processed_data),
      switching_analysis = analyze_switching_dynamics(processed_data),
      switching_plot = plot_switching_dynamics(processed_data)
    )
    
    results$time_domain <- time_domain_results
    
    # Save plots if requested
    if (!is.null(output_dir) && save_plots) {
      td_dir <- file.path(output_dir, "figures", "time_domain")
      dir.create(td_dir, recursive = TRUE, showWarnings = FALSE)
      
      ggplot2::ggsave(file.path(td_dir, "voltage_time.png"), 
                     time_domain_results$voltage_plot, width = 8, height = 6, dpi = 300, bg = "white")
      ggplot2::ggsave(file.path(td_dir, "polarization_time.png"), 
                     time_domain_results$polarization_plot, width = 8, height = 6, dpi = 300, bg = "white")
      ggplot2::ggsave(file.path(td_dir, "current_time.png"), 
                     time_domain_results$current_plot, width = 8, height = 6, dpi = 300, bg = "white")
      ggplot2::ggsave(file.path(td_dir, "capacitance_time.png"), 
                     time_domain_results$capacitance_plot, width = 8, height = 6, dpi = 300, bg = "white")
      ggplot2::ggsave(file.path(td_dir, "time_domain_combined.png"), 
                     time_domain_results$combined_plot, width = 12, height = 8, dpi = 300, bg = "white")
      ggplot2::ggsave(file.path(td_dir, "switching_dynamics.png"), 
                     time_domain_results$switching_plot, width = 8, height = 6, dpi = 300, bg = "white")
    }
  }
  
  # Step 3: Hysteresis analysis
  if ("hysteresis" %in% analysis_modules) {
    cat("Performing hysteresis analysis...\n")
    
    hysteresis_results <- list(
      hysteresis_plot = plot_hysteresis_loop(processed_data),
      parameters = extract_hysteresis_parameters(processed_data),
      parameter_table = NULL  # Will be filled below
    )
    
    hysteresis_results$parameter_table <- create_hysteresis_summary(hysteresis_results$parameters)
    results$hysteresis <- hysteresis_results
    
    # Save results if requested
    if (!is.null(output_dir)) {
      if (save_plots) {
        hyst_dir <- file.path(output_dir, "figures", "hysteresis")
        dir.create(hyst_dir, recursive = TRUE, showWarnings = FALSE)
        ggplot2::ggsave(file.path(hyst_dir, "pv_hysteresis_loop.png"), 
                       hysteresis_results$hysteresis_plot, width = 8, height = 6, dpi = 300, bg = "white")
      }
      if (save_tables) {
        readr::write_csv(hysteresis_results$parameter_table, 
                        file.path(output_dir, "tables", "hysteresis_parameters.csv"))
      }
    }
  }
  
  # Step 4: Current-voltage analysis
  if ("current_voltage" %in% analysis_modules) {
    cat("Performing current-voltage analysis...\n")
    
    cv_results <- list(
      iv_plot = plot_iv_characteristics(processed_data),
      cv_plot = plot_cv_characteristics(processed_data),
      switching_current_plot = plot_switching_current(processed_data),
      current_analysis = analyze_current_components(processed_data),
      conduction_mechanisms = calculate_conduction_mechanisms(processed_data)
    )
    
    results$current_voltage <- cv_results
    
    # Save results if requested
    if (!is.null(output_dir)) {
      if (save_plots) {
        cv_dir <- file.path(output_dir, "figures", "current_voltage")
        dir.create(cv_dir, recursive = TRUE, showWarnings = FALSE)
        ggplot2::ggsave(file.path(cv_dir, "iv_characteristics.png"), 
                       cv_results$iv_plot, width = 8, height = 6, dpi = 300, bg = "white")
        ggplot2::ggsave(file.path(cv_dir, "cv_characteristics.png"), 
                       cv_results$cv_plot, width = 8, height = 6, dpi = 300, bg = "white")
        ggplot2::ggsave(file.path(cv_dir, "switching_current.png"), 
                       cv_results$switching_current_plot, width = 8, height = 6, dpi = 300, bg = "white")
      }
    }
  }
  
  # Step 5: Voltage partitioning analysis
  if ("voltage_partitioning" %in% analysis_modules) {
    cat("Performing voltage partitioning analysis...\n")
    
    vp_results <- list(
      partitioning_plot = plot_voltage_partitioning(processed_data),
      efficiency_plot = plot_efficiency_vs_polarization(processed_data),
      hysteresis_plot = plot_partitioning_hysteresis(processed_data),
      statistics = calculate_partitioning_statistics(processed_data),
      summary_table = NULL,  # Will be filled below
      coupling_analysis = calculate_capacitive_coupling(processed_data)
    )
    
    vp_results$summary_table <- create_partitioning_summary(vp_results$statistics)
    results$voltage_partitioning <- vp_results
    
    # Save results if requested
    if (!is.null(output_dir)) {
      if (save_plots) {
        vp_dir <- file.path(output_dir, "figures", "voltage_partitioning")
        dir.create(vp_dir, recursive = TRUE, showWarnings = FALSE)
        ggplot2::ggsave(file.path(vp_dir, "voltage_partitioning.png"), 
                       vp_results$partitioning_plot, width = 8, height = 6, dpi = 300, bg = "white")
        ggplot2::ggsave(file.path(vp_dir, "efficiency_vs_polarization.png"), 
                       vp_results$efficiency_plot, width = 8, height = 6, dpi = 300, bg = "white")
        ggplot2::ggsave(file.path(vp_dir, "partitioning_hysteresis.png"), 
                       vp_results$hysteresis_plot, width = 8, height = 6, dpi = 300, bg = "white")
      }
      if (save_tables) {
        readr::write_csv(vp_results$summary_table, 
                        file.path(output_dir, "tables", "voltage_partitioning_summary.csv"))
      }
    }
  }
  
  # Step 6: Parametric analysis
  if ("parametric" %in% analysis_modules) {
    cat("Performing parametric analysis...\n")
    
    param_results <- list(
      frequency_plot = plot_frequency_response(processed_data),
      thickness_plot = plot_thickness_effect(processed_data),
      doping_plot = plot_doping_effect(processed_data),
      combined_plot = plot_parametric_analysis_combined(processed_data),
      sensitivity_analysis = analyze_parametric_sensitivity(processed_data),
      optimization_recommendations = NULL  # Will be filled below
    )
    
    if ("hysteresis" %in% analysis_modules) {
      param_results$optimization_recommendations <- create_optimization_recommendations(
        param_results$sensitivity_analysis, 
        results$hysteresis$parameters
      )
    }
    
    results$parametric <- param_results
    
    # Save results if requested
    if (!is.null(output_dir)) {
      if (save_plots) {
        param_dir <- file.path(output_dir, "figures", "parametric")
        dir.create(param_dir, recursive = TRUE, showWarnings = FALSE)
        ggplot2::ggsave(file.path(param_dir, "frequency_response.png"), 
                       param_results$frequency_plot, width = 8, height = 6, dpi = 300, bg = "white")
        ggplot2::ggsave(file.path(param_dir, "thickness_effect.png"), 
                       param_results$thickness_plot, width = 8, height = 6, dpi = 300, bg = "white")
        ggplot2::ggsave(file.path(param_dir, "doping_effect.png"), 
                       param_results$doping_plot, width = 8, height = 6, dpi = 300, bg = "white")
        ggplot2::ggsave(file.path(param_dir, "parametric_combined.png"), 
                       param_results$combined_plot, width = 12, height = 10, dpi = 300, bg = "white")
      }
      if (save_tables && !is.null(param_results$optimization_recommendations)) {
        readr::write_csv(param_results$optimization_recommendations, 
                        file.path(output_dir, "tables", "optimization_recommendations.csv"))
      }
    }
  }
  
  # Step 7: Generate summary report
  cat("Generating summary report...\n")
  results$summary_report <- generate_analysis_summary(results, analysis_modules)
  
  if (!is.null(output_dir) && save_tables) {
    readr::write_csv(results$summary_report, 
                    file.path(output_dir, "tables", "analysis_summary_report.csv"))
  }
  
  cat("Analysis complete!\n")
  return(results)
}

#' Generate Analysis Summary Report
#'
#' This function generates a comprehensive summary report of all analysis results,
#' providing key metrics and findings in a structured format.
#'
#' @param results List containing all analysis results
#' @param analysis_modules Character vector of analysis modules that were run
#'
#' @return A data frame containing the analysis summary report
#'
#' @examples
#' \dontrun{
#' # Generate summary from analysis results
#' summary_report <- generate_analysis_summary(analysis_results, c("hysteresis", "parametric"))
#' }
#'
#' @export
generate_analysis_summary <- function(results, analysis_modules) {
  summary_data <- data.frame(
    Analysis_Category = character(),
    Metric = character(),
    Value = character(),
    Unit = character(),
    Interpretation = character(),
    stringsAsFactors = FALSE
  )
  
  # Time-domain summary
  if ("time_domain" %in% analysis_modules && !is.null(results$time_domain)) {
    td_summary <- data.frame(
      Analysis_Category = rep("Time Domain", 4),
      Metric = c("Positive Switching Time", "Negative Switching Time", 
                "Peak Positive Current", "Peak Negative Current"),
      Value = c(
        ifelse(!is.null(results$time_domain$switching_analysis$coercive_voltages$positive), 
               format(results$time_domain$switching_analysis$coercive_voltages$positive, digits = 3), "N/A"),
        ifelse(!is.null(results$time_domain$switching_analysis$coercive_voltages$negative), 
               format(results$time_domain$switching_analysis$coercive_voltages$negative, digits = 3), "N/A"),
        ifelse(!is.null(results$time_domain$switching_analysis$peak_currents$positive), 
               format(results$time_domain$switching_analysis$peak_currents$positive, digits = 3), "N/A"),
        ifelse(!is.null(results$time_domain$switching_analysis$peak_currents$negative), 
               format(results$time_domain$switching_analysis$peak_currents$negative, digits = 3), "N/A")
      ),
      Unit = c("V", "V", "A", "A"),
      Interpretation = c("Coercive voltage (positive)", "Coercive voltage (negative)",
                        "Maximum displacement current", "Minimum displacement current")
    )
    summary_data <- rbind(summary_data, td_summary)
  }
  
  # Hysteresis summary
  if ("hysteresis" %in% analysis_modules && !is.null(results$hysteresis)) {
    hyst_summary <- data.frame(
      Analysis_Category = rep("Hysteresis", 4),
      Metric = c("Saturation Polarization", "Loop Squareness", 
                "Coercive Voltage Range", "Voltage Imprint"),
      Value = c(
        format(max(abs(results$hysteresis$parameters$Ps_positive), 
                  abs(results$hysteresis$parameters$Ps_negative)), digits = 3),
        format(results$hysteresis$parameters$loop_squareness, digits = 3),
        format(abs(results$hysteresis$parameters$Vc_positive - results$hysteresis$parameters$Vc_negative), digits = 3),
        format(results$hysteresis$parameters$voltage_imprint, digits = 3)
      ),
      Unit = c("C/m²", "-", "V", "V"),
      Interpretation = c("Maximum polarization achieved", "Loop rectangularity (0-1)",
                        "Switching voltage spread", "Asymmetry in switching")
    )
    summary_data <- rbind(summary_data, hyst_summary)
  }
  
  # Voltage partitioning summary
  if ("voltage_partitioning" %in% analysis_modules && !is.null(results$voltage_partitioning)) {
    vp_summary <- data.frame(
      Analysis_Category = rep("Voltage Partitioning", 3),
      Metric = c("Mean Voltage Efficiency", "Efficiency Variation", 
                "Polarization Coupling"),
      Value = c(
        format(results$voltage_partitioning$statistics$efficiency_stats$mean_efficiency, digits = 3),
        format(results$voltage_partitioning$statistics$dynamic_range$relative_variation, digits = 3),
        format(results$voltage_partitioning$statistics$correlation_metrics$efficiency_vs_polarization, digits = 3)
      ),
      Unit = c("-", "-", "-"),
      Interpretation = c("Average Vfe/Vapp ratio", "Coefficient of variation",
                        "Correlation strength")
    )
    summary_data <- rbind(summary_data, vp_summary)
  }
  
  # Parametric analysis summary
  if ("parametric" %in% analysis_modules && !is.null(results$parametric)) {
    param_summary <- data.frame(
      Analysis_Category = rep("Parametric Effects", 3),
      Metric = c("Frequency Sensitivity (1kHz)", "Thickness Sensitivity", 
                "Doping Sensitivity"),
      Value = c(
        format(results$parametric$sensitivity_analysis$frequency_sensitivity$freq_1kHz_deviation, digits = 3),
        format(results$parametric$sensitivity_analysis$thickness_sensitivity$relative_thickness_change, digits = 3),
        format(results$parametric$sensitivity_analysis$doping_sensitivity$relative_leakage_change, digits = 3)
      ),
      Unit = c("-", "-", "-"),
      Interpretation = c("Relative frequency response", "Thickness effect magnitude",
                        "Doping impact on leakage")
    )
    summary_data <- rbind(summary_data, param_summary)
  }
  
  return(summary_data)
}