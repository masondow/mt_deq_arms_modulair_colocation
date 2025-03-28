
# Montana DEQ - ARMS Modulair Collocation Project

## 📌 Project Overview
This project collects data from and evaluates colocated PM2.5 concentration measurements from QuantAQ Modulair air quality sensors collocated at Montana DEQ regulatory grade monitoring sites. The goal is to assess sensor accuracy and develop correction models to improve data quality.

## 🎯 Objectives
- Produce reusable functions and scripts for querying and reformatting data from the QuantAQ cloud
- Compare uncorrected PM2.5 readings from Modulair sensors to regulatory-grade data.
- Explore accuracy and bias at different concentration/AQI levels. 
- Explore meteorological effects on sensor bias.
- Develop and test correction models for improved accuracy.

## 📂 Repository Structure
```
📂 data/          – Stores processed datasets (to be ignored in final update). 
📂 exploratory analysis/     – Jupyter/RMarkdown notebooks for EDA.
📂 modeling/      – Jupyter/RMarkdown notebooks for modeling and assessment.  
📂 functions/     – Python and R functions for data retrieval and pre-processing.
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
   
## Example Analysis and Modeling Steps
1.

2.

3.


## 📊 Data Sources
- **QuantAQ Cloud Modulair API** (cloud-hosted sensor data).
- **ARMS AirVision Microsoft SQL Server Database** (FEM reference data, accessed manually through AirVision GUI or via AVconn package, not available to public).
- **ARMS SQLite Database** (ARMS owned, not available to public)

## 📝 License & Attribution
<Optional: Any usage terms, license, or acknowledgments I may want (def acknowledgments!)>
