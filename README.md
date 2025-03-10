
# Montana DEQ - ARMS Modulair Collocation Project

## 📌 Project Overview
This project evaluates colocated PM2.5 concentration measurements from QuantAQ Modulair air quality sensors collocated at Montana DEQ regulatory grade monitoring sites. The goal is to assess sensor accuracy and develop correction models to improve data quality.

## 🎯 Objectives
- Compare uncorrected PM2.5 readings from Modulair sensors to regulatory-grade data.
- Explore accuracy and bias at different concentration/AQI levels. 
- Explore meteorological effects on sensor bias.
- Develop and test correction models for improved accuracy.

## 📂 Repository Structure
```
📂 data/          – Stores processed datasets (NO raw data stored in GitHub).  
📂 exploratory analysis/     – Jupyter/RMarkdown notebooks for EDA.
📂 modeling/      – Jupyter/RMarkdown notebooks for modeling and assessment.  
📂 functions/     – Functions for data retrieval, processing, modeling.  
📂 reports/       – Final figures, tables, and project summaries.  
📄 README.md      – Overview of the project.  
📄 three_ps_dow.txt     – Weekly project updates  
📄  requirements.txt    - Project dependencies 
📄 .gitignore     – Prevents sensitive files from being tracked.  
```

## 🚀 Setup Instructions
1. Clone this repo:  
   ```bash
   git clone https://github.com/masondow/mt_deq_arms_modulair_colocation.git
   cd mt_deq_arms_modulair_colocation
   ```

2. Install dependencies:  
   ```bash
   pip install -r requirements.txt  # For Python
   ```

3. **Set up API credentials**:  
   - Create a `.env` file with your QuantAQ API key:  
     ```bash
     QUANTAQ_API_KEY="your_secret_api_key"
     ```
   - **DO NOT COMMIT `.env`!** It is ignored by `.gitignore`.

## 📊 Data Sources
- **QuantAQ Modulair API** (retrieves sensor data).
- **AirVision SQL Database** (FEM reference data).

## 📝 License & Attribution
<Optional: Any usage terms, license, or acknowledgments I may want (def acknowledgments)>
