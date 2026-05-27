# Confidence Analysis in Canadian Institutions (R Project)
This project analyzes public confidence in Canadian institutions using data from Statistics Canada’s Quality of Life Hub. The analysis explores how confidence levels vary across institutions, demographics, age groups, immigration status, gender, and area of residence, using R for data cleaning, transformation, and visualization.

### Project Overview
The goal of this project is to understand how Canadians perceive key public institutions and how confidence differs across demographic groups.

The workflow includes:
1. Data cleaning 
2. Variable extraction and transformation
4. Visualization of confidence patterns
5. Insights across demographic segments
6. Summary of key findings
   
This project was completed entirely in RStudio.

### Tools & Technologies
1. R
2. tidyverse
3. dplyr
4. tidyr
5. ggplot2

### Dataset
- Source: Statistics Canada – Quality of Life Hub
- Topic: Good Governance → Confidence in Institutions
- Variables Used:
1. Confidence Level (Low, Neutral, High)
2. Institution
3. Gender
4. Age Group
5. Immigration Status
6. Area of Residence
- Format: CSV file downloaded from StatsCan
- Data Type: survey responses (Q4 2023 & Q4 2024)

### Data Preparation & Transformation
A.  Cleaning
1. Removed missing and duplicate values
2. Filled down demographic fields (Gender, Sociodemographic)
3. Split the Indicators column into:
-  Institution
-  Confidence_Level
4. Reordered variables for clarity

B. Recoding
1. Converted numeric confidence levels (1–5) into ordinal categories:
- Low
- Neutral
- High

### Key Analyses & Visualizations
1. Confidence Distribution by Institution
- Highest confidence: Police
- Lowest confidence: Federal Parliament
- Neutral confidence is consistently high across institutions

2. Year‑over‑Year Change (2023 → 2024)
- Very minimal change overall
- Police: –1.7%
- Canadian Media: +0.5%

3. Confidence by Immigration Status
- Immigrants show higher confidence
- Non‑immigrants show more Low and Neutral responses

4. Confidence by Age Group
- Clear upward trend
- Younger respondents show lower confidence
- Highest confidence: Age 75+ (54.2%)

5. Confidence by Gender & Area of Residence
- Men show the highest Low confidence (29.6%)
- Rural residents show lower confidence overall
- Urban and rural women show higher Neutral scores

### Key Insights
1. Confidence varies significantly across demographic groups
2. Older Canadians and immigrants consistently show higher trust
3. Police receive the highest confidence overall
4. Federal Parliament receives the lowest
5. Gender and area of residence influence confidence patterns
6. Year‑over‑year changes are small, indicating stable perceptions

### Author
Ke Ping Lo  
Business Insights & Analytics
Toronto, Canada
