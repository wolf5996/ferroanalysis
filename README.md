# ferroanalysis <img src="man/figures/logo.png" align="right" height="139"/>

<!-- badges: start -->

[![R-CMD-check][]][1] [![CRAN status]][2]

  [R-CMD-check]: https://github.com/yourusername/ferroanalysis/actions/workflows/R-CMD-check.yaml/badge.svg
  [1]: https://github.com/yourusername/ferroanalysis/actions/workflows/R-CMD-check.yaml
  [CRAN status]: https://www.r-pkg.org/badges/version/ferroanalysis
  [2]: https://CRAN.R-project.org/package=ferroanalysis

<!-- badges: end -->

**ferroanalysis** is a comprehensive R package for ferroelectric device characterization and analysis, specifically designed for HfO₂-based ferroelectric capacitors and other ferroelectric materials. The package provides automated analysis pipelines and publication-ready visualizations for ferroelectric device research.

## Features

### 🔬 **Comprehensive Analysis Modules**

-   **Time-Domain Analysis**: Voltage, polarization, current, and capacitance evolution
-   **Hysteresis Characterization**: P-V loop analysis with parameter extraction
-   **Current-Voltage Analysis**: I-V and C-V characteristics with conduction mechanism identification
-   **Voltage Partitioning**: Metal-Ferroelectric-Semiconductor voltage distribution analysis
-   **Parametric Studies**: Frequency, thickness, and doping effect analysis

### 📊 **Key Capabilities**

-   Automated ferroelectric parameter extraction (Ps, Pr, Vc, loop squareness)
-   Switching dynamics analysis and coercive voltage determination
-   Voltage efficiency and partitioning statistics
-   Conduction mechanism identification (Ohmic, Schottky, Poole-Frenkel)
-   Publication-ready visualizations with customizable themes
-   Comprehensive reporting and optimization recommendations

### 🎯 **Designed For**

-   HfO₂-based ferroelectric capacitors (HZO, Y-HfO₂, La-HfO₂)
-   MFS (Metal-Ferroelectric-Semiconductor) device structures
-   Ferroelectric memory and neuromorphic device research
-   Academic research and industrial R&D applications

## Installation

You can install the development version of ferroanalysis from GitHub:

``` r
# Install from GitHub
# install.packages("devtools")
devtools::install_github("wolf5996/ferroanalysis")

# Or using pak
# install.packages("pak")
pak::pak("wolf5996/ferroanalysis")
```

### Dependencies

ferroanalysis requires the following R packages:

-   `dplyr` (\>= 1.0.0)

-   `ggplot2` (\>= 3.3.0)

-   `readr` (\>= 2.0.0)

-   `tidyr` (\>= 1.1.0)

-   `scales` (\>= 1.1.0)

-   `viridis` (\>= 0.6.0)

-   `patchwork` (\>= 1.1.0)

-   `purrr` (\>= 0.3.0)

## Quick Start

### Basic Usage

``` r
library(ferroanalysis)

# Complete analysis pipeline (one-stop solution)
results <- comprehensive_ferroelectric_analysis(
  file_path = "path/to/simulation_data.csv",
  output_dir = "analysis_results",
  save_plots = TRUE,
  save_tables = TRUE
)

# Access specific results
print(results$hysteresis$parameters)
print(results$summary_report)
```

### Individual Analysis Modules

``` r
# Step-by-step analysis
data <- process_ferroelectric_data("simulation_data.csv")

# Time-domain analysis
voltage_plot <- plot_voltage_time(data)
switching_analysis <- analyze_switching_dynamics(data)

# Hysteresis analysis
hysteresis_plot <- plot_hysteresis_loop(data)
hysteresis_params <- extract_hysteresis_parameters(data)

# Current-voltage analysis
iv_plot <- plot_iv_characteristics(data)
current_analysis <- analyze_current_components(data)

# Voltage partitioning
partitioning_plot <- plot_voltage_partitioning(data)
partitioning_stats <- calculate_partitioning_statistics(data)

# Parametric analysis
frequency_plot <- plot_frequency_response(data)
sensitivity_analysis <- analyze_parametric_sensitivity(data)
```

## Data Format

ferroanalysis expects CSV data with 8 columns in the following order:

| Column         | Description           | Units |
|----------------|-----------------------|-------|
| `time`         | Time points           | s     |
| `v_app`        | Applied voltage       | V     |
| `v_fe`         | Ferroelectric voltage | V     |
| `polarization` | Polarization          | C/m²  |
| `i_disp`       | Displacement current  | A     |
| `i_leak`       | Leakage current       | A     |
| `i_total`      | Total current         | A     |
| `c_depl`       | Depletion capacitance | F     |

## Example Output

### Hysteresis Parameters

``` r
# Extract hysteresis parameters
params <- extract_hysteresis_parameters(data)
#> $Ps_positive: 29.0 C/m²
#> $Pr_positive: 26.1 C/m² 
#> $Vc_positive: +1.12 V
#> $loop_squareness: 0.90
```

### Comprehensive Analysis Report

``` r
# Generate analysis summary
summary_report <- results$summary_report
#> Analysis_Category    Metric                    Value  Unit  Interpretation
#> Hysteresis          Saturation Polarization    29.0   C/m²  Maximum polarization achieved
#> Hysteresis          Loop Squareness           0.900   -     Loop rectangularity (0-1)
#> Time Domain         Peak Positive Current     2.31    A     Maximum displacement current
#> Voltage Partitioning Mean Voltage Efficiency  0.67    -     Average Vfe/Vapp ratio
```

## Advanced Features

### Multi-Dataset Comparison

``` r
# Compare multiple conditions
data1 <- process_ferroelectric_data("condition1.csv")
data2 <- process_ferroelectric_data("condition2.csv")

comparison_plot <- compare_hysteresis_loops(
  list(data1, data2), 
  c("Condition 1", "Condition 2")
)
```

### Custom Analysis Workflows

``` r
# Selective analysis modules
results <- comprehensive_ferroelectric_analysis(
  "data.csv",
  analysis_modules = c("hysteresis", "current_voltage"),
  output_dir = "custom_analysis"
)

# Parameter optimization recommendations
recommendations <- create_optimization_recommendations(
  sensitivity_results, 
  hysteresis_params
)
```

## Visualization Examples

ferroanalysis produces publication-ready plots including:

-   **Time-domain plots**: Voltage, polarization, current, and capacitance evolution
-   **Hysteresis loops**: P-V characteristics with time progression coloring
-   **I-V characteristics**: Multi-component current analysis
-   **Voltage partitioning**: Dynamic voltage distribution visualization
-   **Parametric studies**: Frequency, thickness, and doping effect plots
-   **Combined layouts**: Multi-panel comprehensive analysis views

All plots use consistent styling and can be easily customized or saved at publication quality (300 DPI).

## Applications

ferroanalysis has been designed for:

### 🔬 **Research Applications**

-   Ferroelectric memory device characterization
-   Neuromorphic computing device analysis\
-   HfO₂-based ferroelectric material studies
-   MFS capacitor performance optimization

### 📚 **Academic Use**

-   PhD thesis research (ferroelectric device chapters)
-   Research paper data analysis and visualization
-   Course material and educational demonstrations
-   Collaborative research projects

### 🏭 **Industrial Applications**

-   Device parameter screening and optimization
-   Quality control and characterization
-   Process development and validation
-   R&D pipeline integration

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request. For major changes, please open an issue first to discuss what you would like to change.

### Development Setup

``` r
# Clone repository
git clone https://github.com/yourusername/ferroanalysis.git
cd ferroanalysis

# Install development dependencies
devtools::install_dev_deps()

# Generate documentation
devtools::document()

# Run tests
devtools::test()

# Check package
devtools::check()
```

## Citation

If you use ferroanalysis in your research, please cite:

``` bibtex
@software{ferroanalysis2024,
  title = {ferroanalysis: Comprehensive Analysis Tools for Ferroelectric Device Characterization},
  author = {Badran Elshenawy},
  year = {2024},
  url = {https://github.com/yourusername/ferroanalysis},
  version = {0.1.0}
}
```

## Related Work

This package supports research presented in: - Elshenawy, M., Jan, A., Di Martino, G., & Flewitt, A. "Compact Model of a HfO₂ Ferroelectric Capacitor Using a Recursive Approach" - PhD Thesis: "Resistive Random Access Memory (RRAM) and Ferroelectric Device Research"

## License

MIT License. See [LICENSE] file for details.

  [LICENSE]: LICENSE

## Contact

-   **Author**: Badran Elshenawy
-   **Email**: badran.elshenawy\@ndm.ox.ac.uk
-   **GitHub**: [\@yourusername]

  [\@yourusername]: https://github.com/yourusername

------------------------------------------------------------------------

**ferroanalysis** - Empowering ferroelectric device research with comprehensive analysis tools 🔬⚡