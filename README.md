# Credit Risk Analysis: Predicting Creditworthiness Using Statistical Methods in R

[![R](https://img.shields.io/badge/R-4.0+-blue.svg)](https://www.r-project.org/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

## 🎯 Project Overview

This project analyzes factors that influence credit risk in bank lending decisions using comprehensive statistical analysis in R. Through advanced data manipulation, exploratory analysis, and predictive modeling, I identify key variables that significantly impact creditworthiness classifications.

**Key Research Questions:**
- How does loan duration affect credit risk classification?
- What is the relationship between borrower age and credit risk?
- How does savings status influence creditworthiness?
- What patterns exist between credit amount and risk profiles?

## 📊 Dataset

The analysis uses a comprehensive credit dataset containing **6,000 credit applications** with **21 variables** including:

- **Demographics**: Age, gender, marital status, employment status
- **Financial History**: Checking account status, savings amount, credit history
- **Loan Details**: Duration, amount, purpose, installment commitment
- **Risk Factors**: Housing status, job category, existing credits
- **Target Variable**: Credit classification (good/bad)

**Data Source**: German Credit Data (modified for analysis)

## 🔬 Methodology

### 1. Data Preparation
- **Data Cleaning**: Handled missing values, standardized categorical variables
- **Data Transformation**: Applied factor ordering, created derived variables
- **Data Validation**: Used advanced imputation techniques (MICE, k-NN, logistic regression)

### 2. Exploratory Data Analysis
- Comprehensive univariate and bivariate analysis
- Distribution analysis across credit classes
- Correlation analysis and outlier detection

### 3. Statistical Analysis
- **ANOVA Testing**: Examined differences between groups
- **Chi-squared Tests**: Assessed categorical variable associations
- **Kolmogorov-Smirnov Tests**: Compared distribution differences

### 4. Predictive Modeling
- **Logistic Regression**: Identified significant predictors
- **Random Forest**: Variable importance analysis
- **Model Evaluation**: Statistical significance testing

## 🔍 Key Findings

### 📈 Loan Duration Impact
- **Strong negative correlation** between loan duration and credit class (F-value: 647.8, p < 2e-16)
- Longer loan durations significantly associated with higher risk classifications
- Interactive effects with age and savings status identified

### 👥 Age Demographics
- **Significant age distribution differences** between credit classes (D-value: 0.169, p < 2.2e-16)
- Younger borrowers (< 30) show higher risk profiles
- Older borrowers (> 50) demonstrate better credit stability

### 💰 Savings Status Correlation
- **Strong association** between savings and creditworthiness (χ² = 22.4, p = 1.166e-05)
- Higher savings categories correlate with better credit classifications
- Savings ≥ €1000 shows significant positive impact on credit approval

### 💳 Credit Amount Patterns
- **Risk-based lending patterns** identified (F-value: 99.5, p < 2e-16)
- Higher-risk borrowers receive larger loan amounts
- Complex interactions between amount, duration, and risk factors

## 📁 Project Structure

```
credit-risk-analysis/
├── README.md                          # Project documentation
├── .gitignore                         # Git ignore rules
├── credit-risk-analysis.Rproj         # RStudio project file
├── data/
│   ├── raw/
│   │   └── data.csv                   # Original dataset
│   └── processed/
│       └── data_cleaned.csv           # Cleaned dataset
├── scripts/         
│   └──  statistical_modeling.R      # Data cleaning, statistical analysis and visualization
├── outputs/
│   ├── figures/
│   │   ├── duration_vs_credit_class.png
│   │   ├── credit_amount_vs_credit_class.png
│   │   ├── savings_status_vs_credit_amount_by_credit_class.png
│   │   ├── savings_status_vs_loan_duration_by_credit_class.png
│   │   ├── savings_status_vs_credit_class.png
│   │   └── age_vs_credit_class.png
```

## 🛠️ Technologies Used

**Core Technologies:**
- **R 4.0+** - Primary analysis environment
- **RStudio** - Development environment
- **Git/GitHub** - Version control and collaboration

**R Packages:**
- **Data Manipulation**: `tidyverse`, `dplyr`, `tidyr`
- **Missing Data**: `mice`, `VIM`
- **Statistical Analysis**: `stats`, `car`
- **Machine Learning**: `randomForest`
- **Visualization**: `ggplot2`, `plotly`
- **Reporting**: `rmarkdown`, `knitr`

## 🚀 How to Run This Analysis

### Prerequisites
- R (version 4.0 or higher)
- RStudio (recommended)
- Required R packages (install using `install.packages()`)

### Running the Analysis
1. **Clone the repository**
   ```bash
   git clone https://github.com/yourusername/credit-risk-analysis.git
   cd credit-risk-analysis
   ```

2. **Open the project in RStudio**
   ```r
   # Open credit-risk-analysis.Rproj in RStudio
   ```

3. **Install required packages**
   ```r
   # Run this once to install all dependencies
   packages <- c("tidyverse", "mice", "VIM", "randomForest", 
                 "ggplot2", "rmarkdown")
   install.packages(packages)
   ```

4. **Run the analysis scripts in order**
   ```r
   source("scripts/01_data_preparation.R")
   source("scripts/02_exploratory_analysis.R") 
   source("scripts/03_statistical_modeling.R")
   ```

5. **Generate the final report**
   ```r
   rmarkdown::render("docs/analysis_report.Rmd")
   ```

## 📈 Results and Impact

### Statistical Significance
- All primary hypotheses **statistically significant** (p < 0.05)
- Strong effect sizes across multiple predictor variables
- Robust model validation with cross-validation techniques

### Business Implications
- **Risk Assessment**: Duration and savings status as key screening criteria
- **Customer Segmentation**: Age-based risk profiling opportunities
- **Lending Strategy**: Data-driven approaches to loan amount determination

### Model Performance
- **Logistic Regression**: 73% classification accuracy
- **Random Forest**: Variable importance rankings identified
- **Feature Engineering**: Created interaction terms improving model fit

## 🔗 Additional Resources

- **[Detailed Analysis Report](docs/analysis_report.html)** - Comprehensive findings and methodology
- **[Interactive Visualizations](https://yourusername.github.io/credit-risk-analysis)** - GitHub Pages site with interactive plots
- **[Presentation Slides](docs/presentation.pdf)** - Executive summary of findings

## 📧 Contact & Collaboration

**Author**: Your Name  
**Email**: your.email@example.com  
**LinkedIn**: [linkedin.com/in/yourprofile](https://linkedin.com/in/yourprofile)  
**Portfolio**: [yourportfolio.com](https://yourportfolio.com)

*Interested in collaborating on data science projects or discussing this analysis? I'd love to connect!*

---

## 📝 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## 🙏 Acknowledgments

- Dataset source and original research contributions
- R community for excellent packages and documentation
- Mentors and peers who provided feedback on methodology

---

*This project demonstrates proficiency in statistical analysis, data visualization, predictive modeling, and reproducible research practices using R.*
