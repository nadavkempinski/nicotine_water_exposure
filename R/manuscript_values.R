## Purpose: Create a map of CA-based nicotine (and metabolite) concentrations found in drinking (or source) water

### Import libraries
pacman::p_load(tidyverse, janitor, kableExtra, FSA)

#### Intake data, remove duplicates & QA/QC flagged samples ####
data_path = here::here("publication_materials_zotero_linked", "Table S2-S5.xlsx")
data_file = readxl::read_excel(data_path, sheet = 2, skip=1) 

## Prepare data for analysis
data_clean = data_file |> # cleaned, omitted lat & lon only
  clean_names() |> # normalize column names
  mutate(
    keep = coalesce(keep, "Not a Duplicate"),
    detection_method = na_if(detection_method, "-"),
    sample_unit = tolower(sample_unit), # normalize units
    sample_date = as.Date(sample_date),
    detection_limit_unit = tolower(detection_limit_unit), 
    longitude = as.numeric(longitude), # set data values for lat & lon to help with plotting
    latitude = as.numeric(latitude),
    detection_method = coalesce(detection_method, "Unknown"),
    sample_concentration = if_else(
      sample_unit == "ug/l",
      sample_concentration * 1000,
      sample_concentration),
    sample_unit = "ng/l",
    detection_limit = if_else(
      detection_limit_unit == "ug/l",
      detection_limit * 1000,
      detection_limit),
    detection_limit_unit = "ng/l",
    analyte = as.factor(analyte), water_body = as.factor(water_body)) |> # for making factors of the analytes
  rename(sample_concentration_normalized = sample_concentration_normalized_ng_l)

# list of non-duplicated data (before QA/QC fix)
data_zeros_no_qa = data_clean |> filter(keep !="remove") # includes non-detects

### Determine samples with QA/QC issues
flag_terms <- data_clean |> filter(qa_issues == 1, !is.na(qa_term)) |> select(qa_term) |> unique() |> pull(qa_term)
added_terms = c("received warm", "has been exceeded", "time exceeded", "warm when received")
to_flag = unique(c(added_terms, flag_terms))

## apply dictionary to all samples & flag matches
regex_terms <- paste0("(", paste(to_flag, collapse = "|"), ")")
data_zeros_flagged = data_zeros_no_qa |>
  mutate(qa_flag = str_detect(notes, regex(regex_terms, ignore_case=TRUE)),
         qa_flag_term = str_extract(notes, regex(regex_terms, ignore_case = TRUE))
  )

## count number of samples flagged for QA/QC issues
count(data_zeros_flagged |> filter(qa_flag)) # 246 - flagged for QA/QC issues
count(data_zeros_flagged |> filter(!qa_flag)) # 3588 - not flagged for QA/QC issues

# check non-flagged for false-negatives
set.seed(42) # for repeatable results
sample_qa = data_zeros_flagged |> filter(!qa_flag) |> slice_sample(n=100) |> select(sample_id, notes)

cat(
  paste0(
    sprintf("%d/%d) ID=", seq_len(nrow(sample_qa)), nrow(sample_qa)),
    sample_qa$sample_id, "\n",
    sample_qa$notes
  ),
  sep = "\n\n---\n\n"
)
# Note: A manual step was conducted using the $qa_flag column to determine if the results found had QA/QC issues 
n_unflagged = sum(!data_zeros_flagged$qa_flag)
upper_lim = binom.test(0,100)$conf.int[2]
ceiling(n_unflagged * upper_lim) # 95% CI: 130 

### Keep those not flagged for data analysis, with zeros included
data_zeros = data_zeros_flagged |> filter(!qa_flag)

# Only samples with detected analytes
data_detects = data_zeros |> # detections only
  filter(sample_concentration_normalized > 0) # remove all 0 dots so we can just see the detected results

#### PAPER DATA ####

#### Abstract ####
# % detections of analytes by water body
data_zeros %>%
  filter(analyte=="Nicotine") |>
  group_by(water_body) %>%
  summarize(
    total = n(),
    detected = sum(sample_concentration_normalized > 0, na.rm = TRUE),
    detection_rate = detected / total
  ) |>
  arrange(desc(detection_rate))

data_zeros %>%
  filter(analyte=="Cotinine") |>
  group_by(water_body) %>%
  summarize(
    total = n(),
    detected = sum(sample_concentration_normalized > 0, na.rm = TRUE),
    detection_rate = detected / total
  ) |>
  arrange(desc(detection_rate))

#### Methods ####
### Data Compilation from Available Datasets
# total number of samples downloaded
count(data_clean)

# number of duplicates identified: 3689
count(data_clean |> filter(keep == "remove"))
# % duplicates of total: 49.0%
count(data_clean |> filter(keep == "remove")) / count(data_clean)

# number of flagged samples
count(data_zeros_flagged |> filter(qa_flag)) # 246
# % flagged of total: 3.26%
count(data_zeros_flagged |> filter(qa_flag)) / count(data_clean)

# number of remaining samples
count(data_zeros) # 3,588

# number of detects
count(data_detects) # 587
# % of detects
count(data_detects) / count(data_zeros) # 16.4%

# number of non-detects: 3001
count(data_zeros |> filter(sample_concentration_normalized<=0)) 
# % of non-detects: 83.6%
count(data_zeros |> filter(sample_concentration_normalized<=0)) / count(data_zeros)

#### Results ####
### Compiled Contaminant Data
# number of remaining samples
count(data_zeros) # 3,588

# % detections per body - # most is GW = 2,772
data_zeros %>%
  group_by(water_body) %>%
  summarize(
    total = n(),
    share = total / count(data_zeros),
    detected = sum(sample_concentration_normalized > 0, na.rm = TRUE),
    detection_rate = detected / total
  )

# where were samples collected from to analyze nicotine?
data_zeros %>%
  filter(analyte=="Nicotine") |>
  select(water_body) |>
  unique() |> pull()

# number of non-detects
data_zeros |>
  filter(sample_concentration_normalized==0) |>
  count()
# of of non-detects
data_zeros |>
  filter(sample_concentration_normalized==0) |>
  count() / count(data_zeros)

# % detections per body
data_zeros %>%
  filter(analyte=="Nicotine")|>
  group_by(water_body) %>%
  summarize(
    total = n(),
    share = total / count(data_zeros),
    detected = sum(sample_concentration_normalized > 0, na.rm = TRUE),
    detection_rate = detected / total
  )

# % detections per body for cotinine
data_zeros %>%
  filter(analyte=="Cotinine")|>
  group_by(water_body) %>%
  summarize(
    total = n(),
    share = total / count(data_zeros),
    detected = sum(sample_concentration_normalized > 0, na.rm = TRUE),
    detection_rate = detected / total
  ) |>
  arrange(desc(detection_rate))

data_zeros %>%
  group_by(water_body) %>%
  summarize(
    total = n(),
    share = total / count(data_zeros),
    detected = sum(sample_concentration_normalized > 0, na.rm = TRUE),
    detection_rate = detected / total
  ) |>
  arrange(desc(detection_rate))

# number of outliers: 226
detect_outliers <- function(x) {
  Q1 <- quantile(x, 0.25)
  Q3 <- quantile(x, 0.75)
  IQR <- IQR(x)

  x < (Q1 - 1.5*IQR) | x > (Q3 + 1.5*IQR)
}

n_outliers = data_zeros %>%
  group_by(analyte, water_body) %>%
  summarize(n_total=n(),
            min = min(sample_concentration_normalized, na.rm=TRUE),
            max=max(sample_concentration_normalized, na.rm=TRUE),
            n_outliers = sum(detect_outliers(sample_concentration_normalized),na.rm=TRUE),
            outlier_pct = round(100 * n_outliers / n_total, 2),
            min_outlier = min(sample_concentration_normalized[detect_outliers(sample_concentration_normalized)], na.rm = TRUE),
            max_outlier = max(sample_concentration_normalized[detect_outliers(sample_concentration_normalized)], na.rm = TRUE),
            .groups = "drop"
  ) |> 
  summarize(n_outliers=sum(n_outliers))
n_outliers
# % of outliers of total sample set: 6.3%
n_outliers / count(data_zeros)

# Comparison to global values
limits <- tibble::tribble(
  ~analyte,   ~water_body,        ~limit,
  "Nicotine", "Surface Water",    350,
  "Nicotine", "Groundwater",      164,
  "Nicotine", "Finished Water",    14,
  "Cotinine", "Surface Water",    210,
  "Cotinine", "Groundwater",      130,
  "Cotinine", "Drinking Water",    25
)

data_zeros %>%
  left_join(limits, by = c("analyte", "water_body")) %>%
  mutate(above_limit = sample_concentration_normalized > limit) |>
  filter(!is.na(limit), above_limit) %>%
  count()

# get highest concentrations for each analyte
data_detects %>%
  group_by(analyte, water_body) %>%
  summarize(
    min_conc = min(sample_concentration_normalized, na.rm = TRUE),
    max_conc = max(sample_concentration_normalized, na.rm = TRUE),
    median_conc = median(sample_concentration_normalized, na.rm=TRUE),
    n_detects = n()
  ) |> 
  slice_max(order_by = max_conc, n=2)

# get range: 0.59-2100 ng/L
data_detects %>%
  group_by(analyte, water_body) %>%
  summarize(
    min_conc = min(sample_concentration_normalized, na.rm = TRUE),
    max_conc = max(sample_concentration_normalized, na.rm = TRUE),
    median_conc = median(sample_concentration_normalized, na.rm=TRUE),
    n_detects = n(),
    .groups = "drop"
  ) |> 
  summarize(min_conc = min(min_conc),
            max_conc = max(max_conc))

# get CV range for the detects: 0.5-3.8
data_detects |>
  group_by(analyte, water_body) |>
  summarize(mean = mean(sample_concentration_normalized, na.rm= TRUE),
            sd = sd(sample_concentration_normalized, na.rm= TRUE),
            cv = round(sd/mean,1),
            n = n(),
            .groups = "drop"
  ) |> 
  arrange(desc(cv))

# detections of cotinine: 254
data_zeros |> filter(analyte=="Cotinine", sample_concentration_normalized>0) |> count()
# % detections of cotinine: 9.3%
data_zeros |> filter(analyte=="Cotinine", sample_concentration_normalized>0) |> count() / count(data_zeros |> filter(analyte=="Cotinine"))

# detections of nicotine: 333
data_zeros |> filter(analyte=="Nicotine", sample_concentration_normalized>0) |> count()
# % detections of Nicotine: 38.4%
data_zeros |> filter(analyte=="Nicotine", sample_concentration_normalized>0) |> count() / count(data_zeros |> filter(analyte=="Nicotine"))

# median values
data_detects %>%
  group_by(analyte, water_body) %>%
  summarize(
    min_conc = min(sample_concentration_normalized, na.rm = TRUE),
    max_conc = max(sample_concentration_normalized, na.rm = TRUE),
    median_conc = median(sample_concentration_normalized, na.rm=TRUE),
    n_detects = n(),
    .groups = "drop"
  ) |> arrange(desc(median_conc))


#### STAT: differences in mean concentration between analytes ####
water_bodies <- unique(data_detects$water_body)

wilcox_test_results <- lapply(water_bodies, function(water) {
  df_subset <- data_detects %>%
    filter(water_body == water)
  
  if (length(unique(df_subset$analyte)) == 2) {
    result <- wilcox.test(sample_concentration_normalized ~ analyte, data = df_subset)
    print(result$p.value)
    tibble(
      water_body = water,
      p_value = result$p.value,
      median_cotinine = median(df_subset$sample_concentration_normalized[df_subset$analyte == "Cotinine"]),
      median_nicotine = median(df_subset$sample_concentration_normalized[df_subset$analyte == "Nicotine"]),
      n_cotinine = sum(df_subset$analyte == "Cotinine"),
      n_nicotine = sum(df_subset$analyte == "Nicotine")
    )
  } 
}) %>% bind_rows()
wilcox_test_results |> kable() |> kable_styling()
# GW: p=1.39e-14. SW: p=1.52e-5

## STAT: Difference in water body mean for same analyte
# For Cotinine only
cotinine_data <- data_detects %>%
  filter(analyte == "Cotinine", !is.na(sample_concentration_normalized), !is.na(water_body))

pairwise.wilcox.test(
  x = cotinine_data$sample_concentration_normalized,
  g = cotinine_data$water_body
)

nicotine_data <- data_detects %>%
  filter(analyte == "Nicotine", !is.na(sample_concentration_normalized), !is.na(water_body))

pairwise.wilcox.test( #2e-7
  x = nicotine_data$sample_concentration_normalized,
  g = nicotine_data$water_body,
  p.adjust.method = "holm"
)

## Kruskal-Wallis test for regional differences by HUC
# note: uses analysis_df from figure_3.R
nicotine_huc = analysis_df |> filter(analyte == "Nicotine")
cotinine_huc = analysis_df |> filter(analyte == "Cotinine")

# HUC: p<2.2e-16
kw_nicotine <- kruskal.test(sample_concentration ~ factor(HR_NAME), data = nicotine_huc)
kw_cotinine <- kruskal.test(sample_concentration ~ factor(HR_NAME), data = cotinine_huc)
# water body: p=4.3e-9, <2.2e-16
kw_nicotine <- kruskal.test(sample_concentration ~ factor(water_body), data = nicotine_huc)
kw_cotinine <- kruskal.test(sample_concentration ~ factor(water_body), data = cotinine_huc)
# HUC & Water body: p<2.2e-16
analysis_multi <- analysis_df %>%
  mutate(Combined_Group = paste(HR_NAME, water_body, sep = "_")) %>%
  filter(!is.na(HR_NAME), !is.na(water_body), !is.na(sample_concentration))
kw_nicotine <- kruskal.test(sample_concentration ~ factor(Combined_Group), data = analysis_multi |> filter(analyte == "Nicotine"))
kw_cotinine <- kruskal.test(sample_concentration ~ factor(Combined_Group), data = analysis_multi |> filter(analyte == "Cotinine"))
# print outcomes. Repeat for each test
print(kw_nicotine)
print(kw_cotinine)

## differences between groups
# get group sizes (to calculate r)
hr_nic_counts <- analysis_multi |> 
  filter(analyte == "Nicotine", !is.na(sample_concentration)) |> 
  count(HR_NAME)
# conduct Dunn test
dunn_nicotine_multi <- FSA::dunnTest(sample_concentration ~ factor(HR_NAME), 
                                data = analysis_multi |> filter(analyte == "Nicotine"), 
                                method = "holm")
# calculate effect size (r)
dunn_nic_effect_sizes <- dunn_nicotine_multi$res |>
  # The Comparison column looks like "Region A - Region B". We split it into two columns.
  mutate(
    Group1 = sub(" - .*", "", Comparison),
    Group2 = sub(".* - ", "", Comparison)
  ) |>
  # Join the sample sizes for Group 1
  left_join(hr_nic_counts, by = c("Group1" = "HR_NAME")) |>
  rename(n1 = n) |>
  # Join the sample sizes for Group 2
  left_join(hr_counts, by = c("Group2" = "HR_NAME")) |>
  rename(n2 = n) |>
  # Calculate r and its magnitude
  mutate(
    N_total = n1 + n2,
    effect_size_r = abs(Z) / sqrt(N_total),
    # Add a qualitative label based on Cohen's guidelines
    magnitude = case_when(
      effect_size_r >= 0.5 ~ "Large",
      effect_size_r >= 0.3 ~ "Moderate",
      effect_size_r >= 0.1 ~ "Small",
      TRUE ~ "Negligible"
    )
  ) |>
  # Clean up the final table for viewing
  select(Comparison, Z, P.unadj, P.adj, effect_size_r, magnitude, N_total)

# get p<.01 results
significant_nicotine_hotspots <- dunn_nic_effect_sizes %>%
  filter(P.adj < 0.01) %>%
  arrange(P.adj) # Sort by most significant
View(significant_nicotine_hotspots)


hr_cot_counts <- analysis_multi |> 
  filter(analyte == "Cotinine", !is.na(sample_concentration)) |> 
  count(HR_NAME)
dunn_cotinine_multi <- dunnTest(sample_concentration ~ factor(HR_NAME), 
                                data = analysis_multi |> filter(analyte == "Cotinine"), 
                                method = "holm")
# calculate effect size (r)
dunn_cot_effect_sizes <- dunn_cotinine_multi$res |>
  # The Comparison column looks like "Region A - Region B". We split it into two columns.
  mutate(
    Group1 = sub(" - .*", "", Comparison),
    Group2 = sub(".* - ", "", Comparison)
  ) |>
  # Join the sample sizes for Group 1
  left_join(hr_cot_counts, by = c("Group1" = "HR_NAME")) |>
  rename(n1 = n) |>
  # Join the sample sizes for Group 2
  left_join(hr_counts, by = c("Group2" = "HR_NAME")) |>
  rename(n2 = n) |>
  # Calculate r and its magnitude
  mutate(
    N_total = n1 + n2,
    effect_size_r = abs(Z) / sqrt(N_total),
    # Add a qualitative label based on Cohen's guidelines
    magnitude = case_when(
      effect_size_r >= 0.5 ~ "Large",
      effect_size_r >= 0.3 ~ "Moderate",
      effect_size_r >= 0.1 ~ "Small",
      TRUE ~ "Negligible"
    )
  ) |>
  # Clean up the final table for viewing
  select(Comparison, Z, P.unadj, P.adj, effect_size_r, magnitude, N_total)

# get p<.01 results
significant_cotinine_hotspots <- dunn_cot_effect_sizes %>%
  filter(P.adj < 0.01) %>%
  arrange(P.adj) # Sort by most significant
View(significant_cotinine_hotspots)

# get share of samples by region
n_total = nrow(analysis_df)
analysis_df %>%
  group_by(HR_NAME) %>%
  summarize(n = n(), 
            pct_total = n/n_total*100,
            .groups = "drop") |>
  arrange(desc(pct_total))

# rank results of combined group for nicotine
analysis_multi |> 
  filter(analyte == "Nicotine") %>%
  group_by(water_body, HR_NAME) %>%
  summarize(
    Count = n(),
    Median_Conc = median(sample_concentration, na.rm = TRUE),
    Mean_Conc = mean(sample_concentration, na.rm = TRUE),
    Max_Conc = max(sample_concentration, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(Median_Conc))


# rank results of combined group
analysis_multi |> 
  filter(analyte == "Cotinine") %>%
  group_by(water_body, HR_NAME) %>%
  summarize(
    Count = n(),
    Median_Conc = median(sample_concentration, na.rm = TRUE),
    Mean_Conc = mean(sample_concentration, na.rm = TRUE),
    Max_Conc = max(sample_concentration, na.rm = TRUE)
  ) %>%
  arrange(desc(Median_Conc)) |> View()

### Detection Methods for TPW Chemicals in Waterways
# compilation: LC-MS/MS count - 2,988
count(data_zeros |> filter(phase_type == "LC-MS/MS"))

# compilation: GC-MS count - 152
count(data_zeros |> filter(phase_type == "GC-MS"))

# compilation: unknown count - 448
count(data_zeros |> filter(phase_type == "Unknown"))

# compilation: no detection limit - 31
count(data_zeros |> filter(is.na(detection_limit)))

# compilation: sw8270C use - 28
count(data_zeros |> filter(detection_method == "SW8270C"))

# detection limits
data_zeros |> 
  group_by(analyte, detection_limit_unit) |>
  summarize(min_d = min(detection_limit, na.rm=TRUE),
            max_d = max(detection_limit, na.rm=TRUE),
            .groups = "drop")

#### DISCUSSION ####
# % cotinine of total - 75.8%
count(data_zeros |> filter(analyte == "Cotinine")) / count(data_zeros)
count(data_zeros |> filter(analyte == "Cotinine")) # total count - n=2721

# Nic detections in GW (# & %)
count(data_detects |> filter(analyte=="Nicotine", water_body == "Groundwater")) # 274
count(data_detects |> filter(analyte=="Nicotine", water_body == "Groundwater")) /
  count(data_zeros |> filter(analyte=="Nicotine", water_body == "Groundwater")) # 42.9%

# Nic detections in SW (# & %)
count(data_detects |> filter(analyte=="Nicotine", water_body == "Surface Water")) # 274
count(data_detects |> filter(analyte=="Nicotine", water_body == "Surface Water")) /
  count(data_zeros |> filter(analyte=="Nicotine", water_body == "Surface Water")) # 42.9%

# Cot detections in estuarine (# & %)
count(data_detects |> filter(analyte=="Cotinine", water_body == "Estuary")) # 274
count(data_detects |> filter(analyte=="Cotinine", water_body == "Estuary")) /
  count(data_zeros |> filter(analyte=="Cotinine", water_body == "Estuary")) # 42.9%

# Nic detections in stormwater (# & %)
count(data_detects |> filter(analyte=="Nicotine", water_body == "Stormwater")) # 274
count(data_detects |> filter(analyte=="Nicotine", water_body == "Stormwater")) /
  count(data_zeros |> filter(analyte=="Nicotine", water_body == "Stormwater")) # 42.9%

# nic detections below NOEC 2ng/L: 0
data_detects |>
  filter(analyte=="Nicotine", sample_concentration_normalized <= 2) |>
  count()
# number of nic detections above NOEC: 333
data_detects |>
  filter(analyte=="Nicotine", sample_concentration_normalized > 2) |>
  count()

# cot detections above 50pg/L: 0
data_detects |>
  filter(analyte=="Cotinine", sample_concentration_normalized <= 0.05) |>
  count()
# number of cot detections above 50pg/L: 254
data_detects |>
  filter(analyte=="Cotinine", sample_concentration_normalized > 0.05) |>
  count()

# nic detections below NOEC 1ug/L: 331
data_detects |>
  filter(analyte=="Nicotine", sample_concentration_normalized <= 1000) |>
  count()
# number of nic detections above NOEC 1ug/L: 2 
data_detects |>
  filter(analyte=="Nicotine", sample_concentration_normalized > 1000) |>
  count()
# % of nic detections above NOEC 1ug/L: 0.6% 
data_detects |>
  filter(analyte=="Nicotine", sample_concentration_normalized > 1000) |>
  count() /
  count(data_detects |> filter(analyte=="Nicotine"))*100

# number of nic samples with detect limits > 1ug/L: 28
data_zeros |>
  filter(analyte=="Nicotine", detection_limit>1000) |>
  count() 
# % nic samples with detect limits > 1ug/L: 3.2%
data_zeros |>
  filter(analyte=="Nicotine", detection_limit>1000) |>
  count() / data_zeros |> filter(analyte=="Nicotine") |> count()*100
# number of nic samples with detect limits > 2ng/L: 28
data_zeros |>
  filter(analyte=="Nicotine", detection_limit>2) |>
  count() 
# % nic samples with detect limits > 1ug/L: 3.2%
data_zeros |>
  filter(analyte=="Nicotine", detection_limit>2) |>
  count() / data_zeros |> filter(analyte=="Nicotine") |> count()*100
