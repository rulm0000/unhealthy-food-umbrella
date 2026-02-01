# Updated Analysis for UPFs Umbrella Review

This directory contains the scripts and data used for the "Updated Analysis" portion of the UPFs umbrella review. It handles sensitivity analyses, heterogeneity assessments, and the generation of publication-ready forest plots.

## Analysis Workflow

The analysis is primarily performed in R, with Python used for advanced plot composition (overlaying legends).

### 1. Sensitivity & Heterogeneity Analysis
*   **`Sensitivity_Analysis.R`**: Performs the core sensitivity analysis.
*   **`Sensitivity_Heterogeneity.R`**: Calculates heterogeneity statistics for the sensitivity analysis.
*   **`Sensitivity_Heterogeneity_Report.csv`**: Output data containing heterogeneity metrics (I², k) for both primary and sensitivity analyses.
*   **`Sensitivity_Comparison.csv`**: Output comparison data for the sensitivity forest plot.

### 2. Forest Plot Generation
The forest plots are generated in a two-step process to ensure correct formatting and legend placement.

**Step 1: Generate Base Plots (R)**
Run these scripts to generate the base JPEG images (without legends).
*   **`Sensitivity_Heterogeneity_Forest.R`**: Generates `Heterogeneity_Sensitivity_Forest_Base.jpg`.
*   **`Sensitivity_Forest.R`**: Generates `Sensitivity_Forest_Plot_Base.jpg`.

**Step 2: Create Legend & Overlay (Python)**
These scripts generate a standalone legend and overlay it onto the base plots.
*   **`create_legend.py`**: Generates `Forest_Plot_Legend.png` (transparent background, Arial font).
*   **`combine_plots.py`**: Reads the base plots and the legend, then combines them into the final images:
    *   `Heterogeneity_Sensitivity_Forest.jpg`
    *   `Sensitivity_Forest_Plot.jpg`

## Usage Instructions

To regenerate all plots, run the following commands in order:

```bash
# 1. Generate Base Plots
Rscript "Sensitivity_Heterogeneity_Forest.R"
Rscript "Sensitivity_Forest.R"

# 2. Generate Legend and Overlay
python "create_legend.py"
python "combine_plots.py"
```

## Other Files
*   `count_overlap_stats.py`: Utility script for counting data overlaps.
*   `MHUR_Final_Results.csv`: Final results data.
*   `extracted_Rerun_Analyses.csv`: Data extracted for rerun analyses.
