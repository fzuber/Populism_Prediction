# Populism_Prediction
This is a university team project within the context of the course "Machine learning in R", investigating the predictors of populist/nationalistic voting behaviour in european countries.  

### Workflow: 
https://docs.google.com/document/d/1C7Bb0xgfPLW3uYFhx71ZYfCoM_PBCfYuaeDE7lDArgs/edit?tab=t.0

#### Main database:  
ESS Multilevel data round 11; URL: https://ess.sikt.no/en/datafile/98b539ee-63f4-4803-a384-a8d05293894c

### Notes concerning the Data Engineering & Cleaning Part:

- Sample Selection: The dataset was restricted to valid voters only. Respondents who did not vote, were not eligible, or refused to answer the voting question (ESS codes 66, 77, 88, 99) were excluded.
  --> Impact: Sample size reduced from ~46k to ~24k observations.

- Populist Classification:
  - I linked individual voting data from ESS to 'The PopuList' using the PartyFacts crosswalk table (Döring & Regel, 2019). This crosswalk translates country-specific ESS party codes into standardized PartyFacts IDs.
  - Source: Popu-List.org & Extended Crosswalk (Hill, 2020)
    - URL to the PopuList: https://popu-list.org
    - URL to the crosswalk table: https://github.com/sophieehill/ess-partyfacts-crosswalk

- Missing Data (Imputation):
  - Performed using the missRanger package (Random Forest imputation).
  - Methodology: Imputation was run on the full dataset prior to the test/traing split. The target variable (is_populist_voter) was included as a predictor to preserve correlations but was not imputed itself (as it had no NAs by design).

- Variable Processing:
  - Dropped: trstun (Trust in the united nation) was removed since it had many NA's and I decided its not very     relevant anyways (compared to trstpe and the other trust variables).
  - Encoding: cntry was One-Hot Encoded (dummy variables) for ML compatibility.
  - Scales: Income (hinctnta) and Education (eduyrs) were preserved as numeric/ordinal scales; "Refusal" codes (77/88) were converted to NA and imputed.
  - A list of the currently included variables can be accessed by looking at the codebook

- Final Data:
   - path from this directory: "final_data/ESS11_ML_Ready.csv"
   - path for reference dataset that still includes meta variables (most imporantly ID): "data/ESS11_ReferenceDS_including_meta.rds"

 
