
# Data engineering script (for ML Project)
## removing irrelevant variables
## claning data: removing NA'S; adopting scales
## Preprocessing data
## creating codebook
## load the "PopuList" and use it to translate voted partys into populist (yes/no) variable

library(tidyverse)
library(sjlabelled) 

# step 1: load dataset (ESS) ----------------------------
ess_data <- read.csv("data/ESS11MD_e01_2.csv")
str(ess_data)

# step 2: select relevant variables ------------
df_selected <- ess_data %>%
  select(
    # --- ID & Weights ---
    idno, cntry, anweight, pspwght,
    
    # --- H1: Education ---
    eisced,    # Highest education (Categorical)
    eduyrs,    # Years of education (Continuous)
    
    # --- H2: Economic ---
    hincfel,   # Feeling about income (Subjective insecurity)
    
    # --- Targets (Populism/Nationalism Proxies) ---
    trstplt,   # Trust in politicians
    trstprt,   # Trust in parties
    imueclt,   # Cultural life undermined/enriched
    imwbcnt,   # Immigrants make country worse/better
    stfdem,    # Satisfaction with democracy (CRITICAL for populism)
    
    # --- Demographic Controls ---
    gndr,      # Gender
    agea,      # Age
    domicil,   # Urban/Rural (1=Big city, 5=Farm)
    brncntr,   # Born in country (1=Yes, 2=No)
    
    # --- Social & Psychological Controls (The "Why") ---
    ppltrst,   # Most people can be trusted (Social Trust)
    happy,     # How happy are you
    health,    # Subjective health
    rlgdgr,    # How religious are you
    lrscale,   # Left-Right self placement
    
    # --- Employment ---
    pdwrk,     # Paid work in last 7 days (0/1)
    
  )


str(df_selected)


# step 3: Data cleaning

# Function to fix ESS codes (77, 88, 99 -> NA)
clean_ess_codes <- function(x) {
  # For 0-10 scales
  if(is.numeric(x) && max(x, na.rm=TRUE) > 10) {
    x[x %in% c(66, 77, 88, 99)] <- NA
  }
  return(x)
}

df_clean <- df_selected %>%
  mutate(
    # 1. Apply generic cleaning to 0-10 scales
    across(c(hincfel, trstplt, trstprt, imueclt, imwbcnt, stfdem, 
             ppltrst, happy, rlgdgr, lrscale, agea, eduyrs), 
           ~ replace(., . %in% c(66, 77, 88, 99), NA)),
    
    # 2. Clean Categorical Specifics
    eisced = replace(eisced, eisced %in% c(0, 55, 77, 88, 99), NA),
    gndr = replace(gndr, gndr == 9, NA),
    domicil = replace(domicil, domicil %in% c(7, 8, 9), NA),
    brncntr = replace(brncntr, brncntr %in% c(7, 8, 9), NA),
    health = replace(health, health %in% c(7, 8, 9), NA)
  )

# counting remaining NA's
lapply(df_clean, function(x) sum(is.na(x)))
lapply(df_clean, function(x) summary(x))

# step 4: Pre-Processing for ML -------

df_final <- df_clean %>%
  # Convert Binary/Categorical to Factors or 0/1 for ML
  mutate(
    cntry = as.factor(cntry),
    
    # Gender: 0=Male, 1=Female (Standard ML practice)
    gndr_bin = ifelse(gndr == 2, 1, 0), 
    
    # Born in country: 1=Yes, 0=No
    born_native = ifelse(brncntr == 1, 1, 0),
    
    # Domicil: Reverse it so High Number = Urban? 
    # Currently 1=Big City, 5=Farm. Let's keep it but treat as numeric 
    # or factor depending on model. Let's leave as numeric for Trees.
    
    # Income Insecurity: 
    # hincfel is 1 (Comfortable) to 4 (Difficult).
    # Let's make it intuitive: "Financial Stress Score"
    # No change needed, just remember: High Value = High Stress.
  ) %>%
  # Drop original raw categorical columns if you created binary versions
  select(-gndr, -brncntr) %>% 
  # Drop rows with NAs (ML models cannot handle NAs usually)
  drop_na()

# ---------------------------------------------------------
# STEP 5: EXPORT
# ---------------------------------------------------------
write_csv(df_final, "ESS11_Expanded_ML_Ready.csv")
print(paste("Final dataset has", ncol(df_final), "variables and", nrow(df_final), "respondents."))