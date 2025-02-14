# Montana DEQ - ARMS Modulair Collocation Project

## 📌 Project Overview
This project evaluates PM2.5 concentration measurements from QuantAQ Modulair air quality sensors collocated at regulatory-grade FEM air monitoring sites. The goal is to assess sensor accuracy and develop correction models to improve its data quality.

## 🎯 Objectives
- Compare raw PM2.5 readings from Modulair sensors to FEM data.
- Explore meteorological and particle bin size effects on sensor bias.
- Develop and test correction models for improved accuracy.

## 📂 Repository Structure
📂 data/ – Stores processed datasets (NO raw data stored in GitHub).
📂 notebooks/ – Jupyter/RMarkdown notebooks for EDA & modeling.
📂 scripts/ – Python & R scripts (data retrieval, processing, modeling).
📂 reports/ – Final figures, tables, and project summaries.
📂 environment/ – API credentials, dependencies (e.g., .env, requirements.txt).
📄 README.md – Overview of the project.
📄 .gitignore – Prevents sensitive files from being tracked.


## 🚀 Setup Instructions
1. Clone this repo:  
git clone https://github.com/masondow/mt_deq_arms_modulair_colocation.git cd mt_deq_arms_modulair_colocation


2. Install dependencies:  
pip install -r requirements.txt # For Python


3. **Set up API credentials**:  
- Create a `.env` file with your QuantAQ API key:  
  ```
  QUANTAQ_API_KEY="your_secret_api_key"
  ```
- **DO NOT COMMIT `.env`!** It is ignored by `.gitignore`.

## 📊 Data Sources
- **QuantAQ Modulair API** (retrieves sensor data).
- **AirVision SQL Database** (FEM reference data).

## 📝 License & Attribution
<Optional: Any usage terms, license, or acknowledgments>