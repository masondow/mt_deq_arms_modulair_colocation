# Montana DEQ - ARMS Modulair Collocation Project

## 📌 Project Overview
This project assesses the accuracy of QuantAQ Modulair air quality sensors collocated with Montana DEQ's regulatory-grade PM2.5 monitors. The goal is to evaluate raw sensor performance, identify and understand direction and magnitude of bias, and develop and assess correction models to improve sensor agreement with regulatory-grade data.

The project supports the Montana Department of Environmental Quality’s Air Research and Monitoring Section (ARMS) and was conducted as part of a Master of Science in Business Analytics (MSBA) Capstone project.

## 🎯 Objectives
- Retrieve and process raw sensor data from the QuantAQ Cloud API.
- Evaluate PM2.5 agreement between sensors and FEM instruments across three collocation sites using EPA NSIM metrics.
- Develop correction models to improve sensor accuracy using Tidymodels modeling approaches.

## 📁 Repository Structure
```
📂 data/                     – Final combined datasets for RP, NC, and LT sites.
📂 exploratory analysis/     – R Markdown notebook for exploratory data analysis (EDA).
📂 modeling/                 – R Markdown notebook for modeling and evaluation.
📂 functions/                – Python and R function scripts for querying, cleaning, and analysis.
📂 reports/                  – Knitted HTML outputs from analysis and modeling notebooks.
📄 README.md                 – Overview and usage of the repository.
📄 three_ps_dow.txt          – Weekly project updates and reflections.
📄 requirements.txt          – Python dependencies for API querying and setup.
📄 .gitignore                – Prevents sensitive and temporary files from being tracked.
```

## 🔄 Analysis & Modeling Workflow

- **📄 `exploratory_analysis_dow_20250428_v4.Rmd`**  
  Located in `exploratory analysis/`  
  This R Markdown workbook evaluates the raw accuracy of Modulair PM2.5 measurements relative to FEM monitors across three collocation sites. Performance is benchmarked using EPA NSIM field-testing targets and stratified by both AQI category and FEM monitor reference to understand how accuracy varies across conditions.

- **📄 `model_development_dow_20250312.Rmd`**  
  Located in `modeling/`  
  This R Markdown notebook follows the EPA sensor correction workflow to develop and evaluate candidate correction models. Multiple model types—including linear, interaction, and quadratic forms—are trained, cross-validated, and assessed for performance in improving sensor agreement with FEM data.

## 🔐 API Setup (for re-running QuantAQ queries)

To re-query QuantAQ data:
1. Create a `.env` file in the project root directory:
   ```bash
   QUANTAQ_API_KEY="your_secret_api_key"
   ```
2. Your API key will be used by the Python script `py_retrieve_modulair_resampled_dow_20250304.py` to authenticate and download hourly-resampled data.

> ⚠️ Note: The `.env` file is ignored by `.gitignore` and should **never** be committed to the repo.

## 📊 Data Sources
- **QuantAQ Cloud API** – Resampled PM2.5 data from Modulair sensors  
- **AirVision (MT DEQ internal database)** – Regulatory-grade FEM PM2.5 reference data  
- **PurpleAir SQLite Database (internal)** – EPA-corrected PurpleAir CF1 PM2.5 data

## 📄 Reports
Knitted `.html` outputs for both analysis and modeling are saved in the `/reports` folder. These files serve as shareable and reproducible documentation of results.

## 🙏 Acknowledgments
This project was conducted in partnership with the Montana Department of Environmental Quality (DEQ) Air Research and Monitoring Section (ARMS), as part of the MSBA Capstone Project at the University of Montana.
