# Life Expectancy Data Analysis

This project delves into life expectancy data analysis using R, applying advanced visualization and statistical techniques. Key features include data cleaning, creating histograms, scatter plots, boxplots, and correlation matrices, and modeling predictors of life expectancy through linear regression. Insights are drawn on how variables like alcohol consumption, schooling, and immunization impact life expectancy. Each script is designed for clarity, producing detailed visual outputs such as AIC comparison plots, correlation heatmaps, and predictor evaluations, offering a comprehensive understanding of the data.


## Table of Contents

- [Features](#features)
- [Usage](#usage)
- [Code Structure](#code-structure)
- [Requirements](#requirements)
- [File Outputs](#file-outputs)
- [License](#license)
- [Important Note](#important-note)


## Features

- **Data Cleaning and Preprocessing**
  - Handles missing values and renames columns for readability.
  - Generates summaries of key metrics for life expectancy datasets.

- **Visualization Techniques**
  - **Boxplots**: Display the distribution of predictors affecting life expectancy.
  - **Histograms**: Show life expectancy frequency distributions.
  - **Correlation Matrices**: Highlight relationships between variables.
  - **Scatter Plots**: Examine relationships between predictors and life expectancy.

- **Statistical Modeling**
  - Applies linear regression models to evaluate predictors of life expectancy.
  - Compares models using AIC scores.
  - Visualizes predictions against actual values for various models.

## Usage

1. **Prepare the Data**
   - Place the `Life Expectancy Data-1.csv` file in the working directory.

2. **Run the Scripts**
   - Execute the following scripts in RStudio or another R environment:
     - `life_expectancy_visualization.R`
     - `predictor_relationships_analysis.R`
     - `life_expectancy_modeling.R`

3. **Output**
   - Visualizations and models are saved as PDF files or displayed in the console.

## Code Structure

- **life_expectancy_visualization.R**
  - Creates boxplots and histograms to analyze distributions of predictors and life expectancy.

- **predictor_relationships_analysis.R**
  - Generates scatter plots and correlation matrices to explore relationships between variables.

- **life_expectancy_modeling.R**
  - Develops and evaluates multiple linear regression models for predicting life expectancy.

## Requirements

- **R Environment**: Preferably RStudio.
- **Packages**:
  - `GISTools`
  - `RColorBrewer`
  - `Cairo`
  - `corrplot`

## File Outputs

- `life_expectancy_boxplot.pdf`: Boxplots for predictor distributions.
- `life_expectancy_histogram.pdf`: Histograms and a combined boxplot for life expectancy.
- `predictor_correlation_matrix.pdf`: Correlation matrix of predictors.
- `predictor_scatterplots.pdf`: Scatter plots of predictors vs. life expectancy.
- `model_comparison_AIC.pdf`: Visualizations of model predictions and AIC comparisons.
- `test_model_predictions.pdf`: Test model results for life expectancy predictions.

## License

This project is licensed under the MIT License. See the LICENSE file for details.

## Important Note

This repository demonstrates data visualization and statistical analysis techniques using R. It is ideal for educational purposes and exploring the relationships between predictors and life expectancy trends.
