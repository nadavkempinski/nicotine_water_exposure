## Purpose: Create a map of CA-based nicotine (and metabolite) concentrations found in drinking (or source) water

### Pseudocode:
# 1. Intake concentrations data
# 1a. Clean data
# 1b. Geocode data
# 2. Create a map of CA
# 3. Insert points on the map
# 3a. Visually show on map: concentration level, contaminant type, water type
# 3b. Insert legends for each visual scale
# 4. Save map as image for export

## 0. Import libraries
library(tidyverse)
library(here)
library(janitor)
library(glue) # for good looking message writing!
library(patchwork) # putting together plots
library(sf) # for map-making
library(rnaturalearth) # map-making
library(tigris) # for sf layers
options(tigris_use_cache = TRUE) # to save sf layers instead of re-downloading
library(googlesheets4) # to import concentrations data. Note: requires authorization. In the future, concentration data will be stored with the project
library(tsibble) # time series analysis
library(fable)
library(lubridate)
library(feasts)
library(kableExtra)
library(gtExtras)
library(clipr) # for saving to clipboard

library(report)
cite_packages()

## 1. Intake data
data_file = read_sheet("https://docs.google.com/spreadsheets/d/1r73-RLY5JXLARDwIof56cgKEzioUuLcAKdD_s3ZkhYU/edit?gid=1804529984#gid=1804529984", sheet = "Table: CA Concentrations") # using gSheets as I work on this online. using na="-" messes up the detection_method column
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
    analyte = as.factor(analyte), water_body = as.factor(water_body)) # for making factors of the analytes 

# list of non-duplicated data (before QA/QC fix)
data_zeros_no_qa = data_clean |> filter(keep !="remove") # includes non-detects

# review: samples with QA/QC issues
flag_terms <- data_clean |> filter(qa_issues == 1, !is.na(qa_term)) |> select(qa_term) |> unique() |> pull(qa_term)
added_terms = c("received warm", "has been exceeded", "time exceeded", "warm when received")
to_flag = unique(c(keepers, flag_terms))

# apply dictionary to all samples & flag matches
regex_terms <- paste0("(", paste(to_flag, collapse = "|"), ")")
data_zeros_flagged = data_zeros_no_qa |>
  mutate(qa_flag = str_detect(notes, regex(regex_terms, ignore_case=TRUE)),
         qa_flag_term = str_extract(notes, regex(regex_terms, ignore_case = TRUE))
  )

# count totals
count(data_zeros_flagged |> filter(qa_flag)) # 246 - flagged for QA/QC issues
count(data_zeros_flagged |> filter(!qa_flag)) # 3588 - not flagged for QA/QC issues

# check non-flagged for false-negatives
set.seed(42)
sample_qa = data_zeros_flagged |> filter(!qa_flag) |> slice_sample(n=100) |> select(sample_id, notes)

cat(
  paste0(
    sprintf("%d/%d) ID=", seq_len(nrow(sample_qa)), nrow(sample_qa)),
    sample_qa$sample_id, "\n",
    sample_qa$notes
  ),
  sep = "\n\n---\n\n"
)

n_unflagged = sum(!data_zeros_flagged$qa_flag)
upper_lim = binom.test(0,100)$conf.int[2]
ceiling(n_unflagged * upper_lim) # 95% CI: 130 

data_detects = data_zeros |> # detections only
  filter(sample_concentration_normalized > 0) # remove all 0 dots so we can just see the detected results

### Keep those not flagged for data analysis, with zeros included
data_zeros = data_zeros_flagged |> filter(!qa_flag)

#### PAPER DATA ####

# --- EXEC SUMM: Frequency
data_zeros |> #16.36% detection
  mutate(detected = ifelse(sample_concentration_normalized > 0, TRUE, FALSE)) |>
  group_by(analyte, detected) |>
  summarize(count=n())

# % detection across all analytes
mean(data_zeros$sample_concentration_normalized > 0, na.rm = TRUE) # 16.4%

# total count
count(data_zeros) # 3588

## EXEC SUMM TABLE: detection counts by body
detection_ranges <- data_detects %>%
  group_by(analyte, water_body) %>%
  summarize(
    min_conc = min(sample_concentration_normalized, na.rm = TRUE),
    max_conc = max(sample_concentration_normalized, na.rm = TRUE),
    n_detects = n(),
    .groups = "drop"
  )
detection_ranges

### Duplicates -- where do we use this?
count(data_clean |> filter(keep == "remove")) # 3689 

#### Paper stats ####

# extraction: data without lat/lon - 16
count(data_zeros |> filter(is.na(latitude) | is.na(longitude)))

# compilation: # of data points
count(data_clean) # 7523

# compilation: dup data to remove
count(data_clean |> filter(keep == "remove")) # 3,689
count(data_clean |> filter(keep == "remove")) / count(data_clean) # 49.0%

# compilation: # of non-dups
count(data_zeros) # 3,588

# compilation: QA/QC 95% CI
n_unflagged = sum(!data_zeros_flagged$qa_flag)
upper_lim = binom.test(0,100)$conf.int[2]
ceiling(n_unflagged * upper_lim) # 95% CI: 130 

# compilation: all flagged samples
count(data_zeros_flagged |> filter(qa_flag)) # 246

# compilation: detects
count(data_detects) # 610
count(data_detects) / count(data_zeros) # 16.4%

# compilation: non-detects - 
count(data_zeros |> filter(sample_concentration_normalized<=0)) # - 3,001
count(data_zeros |> filter(sample_concentration_normalized<=0)) / count(data_zeros) # - 83.6%


# --- Contaminant Detection

# % detections per body - # most is GW = 2,772
data_zeros %>%
  group_by(water_body) %>%
  summarize(
    total = n(),
    share = total / count(data_zeros),
    detected = sum(sample_concentration_normalized > 0, na.rm = TRUE),
    detection_rate = detected / total
  )

# % detections per analyte-body - # N: 100% in stormwater, 42.9% in GW. C: 81.0% recycled, 77.8% stormwater 
data_zeros %>%
  filter(analyte=="Cotinine") |>
  group_by(analyte, water_body) %>%
  summarize(
    total = n(),
    detected = sum(sample_concentration_normalized > 0, na.rm = TRUE),
    detection_rate = detected / total
  ) |>
  arrange(desc(detection_rate))

### FIGURE: boxplots of concentrations by analyte-water body
library(ggplot2)
library(dplyr)
library(patchwork)

## Prep patchwork theme for publication
theme_set(
  theme_minimal(base_size = 7, base_family = "Arial")
)
patchwork_safe_theme <- theme(
  text        = element_text(family = "Arial", size = 7),
  axis.text   = element_text(size = 7),
  axis.title  = element_text(size = 8),
  plot.title  = element_text(size = 9, face = "bold"),
  strip.text  = element_text(size = 8),
  legend.title = element_text(size = 8),
  legend.text  = element_text(size = 7),
  panel.grid.minor = element_blank(),
  plot.margin = margin(4, 4, 4, 4)
)


# Filter valid values
# Get sample sizes per group
sample_sizes <- data_detects %>%
  group_by(analyte, water_body) %>%
  summarize(n = n(), .groups = "drop")

# Color palette by analyte
analyte_colors <- c(
  "Cotinine" = "#F8766D",  # reddish
  "Nicotine" = "#00BFC4"   # teal
)

# Function to generate plot per analyte
make_boxplot <- function(analyte_name) {
  # Filter and rename estuary to brackish
  
  label_df <- filter(sample_sizes, analyte == analyte_name) %>%
    mutate(
      water_body = ifelse(water_body == "Estuary", "Brackish Water", as.character(water_body))
    )
  
  plot_df <- filter(data_detects, analyte == analyte_name) %>%
    mutate(
      water_body = ifelse(water_body == "Estuary", "Brackish Water", as.character(water_body))
    ) %>%
    left_join(label_df, by = c("analyte", "water_body")) %>%
    mutate(
      water_body_labeled = paste0(water_body, " (n=", n, ")"),
      method = ifelse(n < 5, "strip", "box")
    )
  
  label_df <- label_df %>%
    mutate(water_body_labeled = paste0(water_body, " (n=", n, ")"))
  # Build plot
  ggplot(plot_df, aes(x = water_body_labeled, y = sample_concentration_normalized)) +
    geom_boxplot(
      data  = subset(plot_df, method == "box"),
      fill  = analyte_colors[[analyte_name]],
      alpha = 0.6,
      outlier.shape = NA,
      colour = "black",
      size   = 0.3           # thinner outline
    ) +
    geom_jitter(
      width = 0.15,
      size  = 0.5,           # slightly smaller dots
      alpha = 0.8,           # a bit more transparent
      colour = "black"      # softer than pure black
    ) +
    scale_y_log10(
      breaks = c(1, 10, 100, 1000),
      labels = c("1", "10", "100", "1000"),
      limits = c(10^ymin, 10^ymax)
    ) +
    labs(
      x = "Water Sampled",
      y = "Concentration (ng/L, log10 scale)",
      title = paste(analyte_name)
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(hjust = 0.5)
    )
}

# get y-mix & y-max for the boxplots
ymin <- min(data_detects$sample_concentration_normalized, na.rm = TRUE)
ymax <- max(data_detects$sample_concentration_normalized, na.rm = TRUE)
ymin <- floor(log10(ymin))
ymax <- ceiling(log10(ymax))

# Create one plot per analyte
plots <- lapply(unique(data_detects$analyte), make_boxplot)

# Combine plots with a centered main title
boxplots = wrap_plots(plots) + 
  plot_annotation(
    title = "Detected Analyte Concentrations by Waters Sampled (ng/L)",
    theme = theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
  )
boxplots # add ridgeline for Figure 4

ggsave(
  filename = "boxplots_only.png",
  plot     = boxplots,
  width    = 3.3,      # inches  (504 pt – valid 2-column width)
  height   = 3.5,    # tweak for aspect ratio, just keep < 9.17"
  units    = "in",
  dpi      = 600
)

### FIGURE: Ridgeplot of analyte x water sampled (good for highlighting distribution)
library(ggridges)

ridgeplots = data_detects |>
  filter(water_body != "Finished Water") |> # not enough finished concentrations to show  
  mutate(water_body = forcats::fct_drop(water_body)) |>
  ggplot(aes(x = sample_concentration_normalized, y = water_body, fill = analyte)) +
  ggridges::geom_density_ridges(
    scale = 1.2,
    alpha = 0.6,
    rel_min_height = 0.01,
    linewidth = 0.3,
    color = "black"
  ) +
  scale_x_log10(
    breaks = c(1, 10, 100, 1000),
    labels = c("1", "10", "100", "1000")
  ) +
  labs(
    x = "Concentration (ng/L, log10 scale)",
    y = "Sampled Water Source",
    title = "Analyte Concentration Distribution",
    fill = "Analyte"
  ) +
  scale_fill_manual(values = c("Cotinine" = "#F8766D", "Nicotine" = "#00BFC4")) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold"),
    legend.position = "right",
    plot.title.position = "panel",
    plot.title = element_text(hjust = 0.5)
  )

library(patchwork)
boxplots = boxplots + theme(
    axis.text.x = element_text(angle = 45, hjust=1, size=7),
    plot.margin = margin(r=20)
  )
boxplots
fig4 = (boxplots | ridgeplots) +
  plot_layout(widths = c(3, 1)) 
fig4 = fig4 & patchwork_safe_theme
fig4

# save for publication
ggsave(
  filename = "Fig_boxplot_ridge.png",
  plot     = fig4,
  width    = 7,      # inches  (504 pt – valid 2-column width)
  height   = 5,    # tweak for aspect ratio, just keep < 9.17"
  units    = "in",
  dpi      = 600
)

## TABLE: Range by analyte-body
data_detects %>%
  group_by(analyte, water_body) %>%
  summarize(
    min_conc = min(sample_concentration_normalized, na.rm = TRUE),
    max_conc = max(sample_concentration_normalized, na.rm = TRUE),
    median_conc = median(sample_concentration_normalized, na.rm=TRUE),
    n_detects = n(),
    .groups = "drop"
  ) |> arrange(desc(median_conc))

## STAT: SD for each analyte-source
data_zeros |>
  group_by(analyte, water_body) |>
  summarize(mean = mean(sample_concentration_normalized, na.rm= TRUE),
            sd = sd(sample_concentration_normalized, na.rm= TRUE),
            cv = sd/mean,
            n = n(),
            .groups = "drop"
  ) |> 
  arrange(desc(cv)) 

## TABLE: detection counts by body
detection_ranges <- data_detects %>%
  group_by(analyte, water_body) %>%
  summarize(
    min_conc = min(sample_concentration_normalized, na.rm = TRUE),
    max_conc = max(sample_concentration_normalized, na.rm = TRUE),
    n_detects = n(),
    .groups = "drop"
  )
detection_ranges

## STAT: differences in mean concentration between analytes
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
      mean_cotinine = mean(df_subset$sample_concentration_normalized[df_subset$analyte == "Cotinine"]),
      mean_nicotine = mean(df_subset$sample_concentration_normalized[df_subset$analyte == "Nicotine"]),
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

# --- TPW Chem Detection Methods

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

## use table 5 code for detection limits

#### Non-targeted analysis of chemicals ####
## check: by source, spread of analytes
data_zeros |> # 58% are cotinine-only
  # filter(str_detect(data_source, "\\(20")) |>
  group_by(data_source, analyte) |>
  summarize(count=n(), .groups="drop") |>
  group_by(data_source) |>
  summarize(analytes=n(), .groups="drop") |>
  arrange(desc(analytes)) |>
  kable() |> kable_styling()

#### DISCUSSION ####
# % cotinine of total - 75.8%
count(data_zeros |> filter(analyte == "Cotinine")) / count(data_zeros)


#### Tables & Figures ####

## Table 3: Counts by Analyte x Water
# all samples
table_sources_zeros <- data_zeros %>%
  group_by(analyte, water_body) %>%
  summarize(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = water_body, values_from = count, values_fill = 0) |>
  mutate(`Total` = rowSums(across(-analyte)))
table_sources_zeros |> kable() |> kable_styling()

table_sources_zeros2 = table_sources_zeros |>
  summarize(across(where(is.numeric), sum)) |>
  mutate(analyte = "TOTAL") |>
  relocate(analyte)
table_sources_zeros3 = bind_rows(table_sources_zeros, table_sources_zeros2)
table_sources_zeros3 |> kable() |> kable_styling() # total samples

# detections
table_sources_detects <- data_detects %>%
  group_by(analyte, water_body) %>%
  summarize(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = water_body, values_from = count, values_fill = 0) |>
  mutate(`Total` = rowSums(across(-analyte)))

table_sources_detects2 = table_sources_detects |>
  summarize(across(where(is.numeric), sum)) |>
  mutate(analyte = "TOTAL") |>
  relocate(analyte)
table_sources_detects3 = bind_rows(table_sources_detects, table_sources_detects2)
table_sources_detects3 |> kable() |> kable_styling() # detections

## Table 4: Ranges by analyte-body
data_detects %>%
  group_by(analyte, water_body) %>%
  summarize(
    min_conc = min(sample_concentration_normalized, na.rm = TRUE),
    max_conc = max(sample_concentration_normalized, na.rm = TRUE),
    median_conc = median(sample_concentration_normalized, na.rm=TRUE),
    n_detects = n(),
    .groups = "drop"
  ) |> arrange(desc(median_conc))

## Table 4: SD for each analyte-source
data_zeros |>
  group_by(analyte, water_body) |>
  summarize(mean = mean(sample_concentration_normalized, na.rm= TRUE),
            sd = sd(sample_concentration_normalized, na.rm= TRUE),
            cv = sd/mean,
            n = n(),
            .groups = "drop"
  ) |> 
  arrange(desc(cv)) 

## Table 5: Literature concentration ranges of N&C -- no data sci

## Table 6: Detection method limits
detection_ranges_table <- data_zeros %>% # de-duped w/ non-detects
  filter(!is.na(detection_limit), !is.na(detection_limit_unit)) |> # remove where detection limit isn't defined, n=48. These two clauses overlap
  group_by(analyte, phase_type) %>%
  summarize(
    min_limit = min(detection_limit),
    max_limit = max(detection_limit),
    Methods = str_c(sort(unique(detection_method)), collapse = "; "),
    `Detection Range` = paste0(min(detection_limit), " - ", max(detection_limit)),    
    Sources = str_c(sort(unique(data_source)), collapse = ", "),
    Count=n(),
    Detections = sum(sample_concentration_normalized > 0),
    .groups = "drop"
  ) %>%
  rename(Analyte = analyte, 
         `Phase Type` = phase_type) |>  
  select(Analyte, `Phase Type`, `Detection Range`, Methods, Sources, Count, Detections) |>
  arrange(Analyte)
detection_ranges_table |> kable() |> kable_styling()



#### NOT USED IN PAPER ####
# mean of each analyte-water
data_detects |>
  group_by(analyte, water_body) |>
  summarize(mean = mean(sample_concentration_normalized), .groups="drop") |>
  kable() |> kable_styling()

# general sense of data
data_detects |> gt_plt_summary()

# count: missing lat&lon (9, all from Masoner et al. (2019))
data_clean |>
  group_by(is.na(latitude) || is.na(longitude)) |>
  summarize(count=n())

# find duplicates (4,177, 2,077 to keep)
data_clean |>
  group_by(keep != "") |>
  summarize(count=n())

# get 0's (5772 of total)
data_clean |> 
  group_by(sample_concentration == 0) |>
  summarize(count = n(), 
            .groups = "drop")

# get non-dup 0's
data_clean |>
  group_by(keep, sample_concentration 
           > 0) |>
  summarize(count=n(), .groups = "drop") |>
  kable() |>
  kable_styling()


# # View how many duplicates & 0's we have
# data_null = data_file |>
#   mutate(status = case_when(
#     keep == "remove" ~ "duplicate",
#     sample_concentration == 0 ~ "zero",
#     TRUE ~ "detected"
#   ))

# time series?
data_ts = data_detects |>
  as_tsibble(index=sample_date, key=c(analyte, sample_concentration_normalized, latitude, longitude, site_id))
# create a set of plots showing concentration based on various variables

# Concentration - Water body
ggplot(data, aes(x = water_body, y = sample_concentration, fill = water_body)) +
  geom_violin(trim = FALSE) +
  geom_jitter(width = 0.1, alpha = 0.3) +
  scale_y_log10() +
  theme_minimal() +
  labs(
    title = "Distribution of TRC Concentration by Water Body",
    x = "Water Body",
    y = "Concentration (log scale)"
  ) +
  theme(legend.position = "none")

# Concentration - Analyte
ggplot(data_detects, aes(x = analyte, y = sample_concentration, color=data_source)) +
  geom_boxplot() +
  #geom_jitter(width = 0.1, alpha = 0.3) +
  scale_y_log10() +
  theme_minimal() +
  labs(
    title = "Distribution of TRC Concentration by Water Body",
    x = "Water Body",
    y = "Concentration (log scale)"
  ) +
  theme(legend.position = "right")



# to see where we removed data from (bar graph)
ggplot(data_null, aes(x = data_source, fill=status)) +
  geom_bar(position="dodge") +
  theme_minimal() +
  labs(
    title = "Distribution of Removed Data Points Across Data Sources",
    x = "Source",
    y = "Count"
  ) +
  theme(legend.position = "right", axis.text.x = element_text(angle = 45, hjust = 1)
)

# to see how many data points are removed from each dataset due to duplication
table_null = data_null |>
  count(data_source, status) |>
  pivot_wider(names_from = status, values_from = n, values_fill = 0) |>
  kable() |>
  kable_styling()
table_null

# helped figure out that i can find site ID for CEDEN data, and to fix concentration units for CEC: CEDEN data
View(data_null |> filter(data_source %in% c("CEDEN", "CEC: CEDEN")))


# show off distribution of sample concentrations for each analyte & water body
ggplot(data_zeros, aes(y=sample_concentration, )) +
  geom_bar()+
  facet_wrap(~ water_body)

ggplot(data_zeros, aes(x = water_body, y = sample_concentration, fill = water_body)) +
  geom_violin(trim = FALSE) +
  # geom_jitter(width = 0.1, alpha = 0.3) +
  scale_y_log10() +
  theme_minimal() +
  labs(
    title = "Distribution of TRC Concentration by Water Body",
    x = "Water Body",
    y = "Concentration (log scale)"
  ) +
  theme(legend.position = "none",  axis.text.x = element_text(angle = 45, hjust = 1))

# view distribution of data
ggplot(data_zeros, aes(x = analyte, y = sample_concentration, color=data_source)) +
  geom_boxplot() +
  #geom_jitter(width = 0.1, alpha = 0.3) +
  scale_y_log10() +
  theme_minimal() +
  labs(
    title = "Distribution of TRC Concentration by Water Body",
    x = "Water Body",
    y = "Concentration (log scale)"
  ) +
  theme(legend.position = "right",  axis.text.x = element_text(angle = 45, hjust = 1))

cotinine_hist = ggplot(data = data_zeros |> filter(analyte=="Cotinine"), aes(x=sample_concentration))+
  geom_histogram(position="dodge")+
  scale_x_log10(labels = scales::label_number())+
  theme_minimal()+
  labs(title = "Cotinine Concentration Data",
       x = "Sample Concentration (ng/l)",
       y = "Count")+
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
nicotine_hist = ggplot(data = data_zeros |> filter(analyte=="Nicotine"), aes(x=sample_concentration))+
  geom_histogram(position="dodge")+
  scale_x_log10(labels = scales::label_number())+
  theme_minimal()+
  labs(title = "Nicotine Concentration Data",
       x = "Sample Concentration (ng/l)",
       y = "Count")+
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

analyte_hist = nicotine_hist / cotinine_hist
analyte_hist


# check - data sources (why is stormwater 100%?)
summary_table <- data_zeros %>%
  group_by(analyte, water_body) %>%
  summarize(
    n = n(),
    data_sources = paste(sort(unique(data_source)), collapse = "; "),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = water_body,
    values_from = c(n, data_sources),
    values_fill = list(n = 0, data_sources = "")
  ) %>%
  kable(caption = "Summary of Sample Counts and Data Sources by Analyte and Water Body") %>%
  kable_styling(full_width = FALSE, position = "left")
summary_table

## FIGURE: violinplots per analyte & body
ggplot(data_detects, aes(x = water_body, y = sample_concentration_normalized, fill = analyte)) +
  geom_boxplot(position = position_dodge(width = 0.75), outlier.shape = NA) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75), alpha = 0.2) +
  scale_y_log10() +
  labs(
    x = "Water Body",
    y = "Concentration (ng/L)",
    title = "Analyte Concentration Distribution by Water Body"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



## STATS: compare detection method/phase type by analyte/water source
data_binary <- data_zeros %>%
  filter(phase_type %in% c("LC-MS/MS", "GC-MS")) %>%
  mutate(
    detected = sample_concentration_normalized > 0
  )

detection_summary <- data_binary %>%
  group_by(analyte, water_body, phase_type) %>%
  summarise(
    n_total = n(),
    n_detect = sum(detected, na.rm = TRUE),
    detection_rate = round(100 * n_detect / n_total, 1),
    .groups = "drop"
  )
detection_summary

test_detection_method <- function(df) {
  tbl <- table(df$phase_type, df$detected)
  
  if (all(dim(tbl) == c(2, 2))) {
    test <- fisher.test(tbl)
    return(tibble(
      p_value = test$p.value,
      method = "Fisher",
      total_n = sum(tbl),
      table = list(tbl)
    ))
  } else {
    return(tibble(p_value = NA, method = NA, total_n = sum(tbl), table = list(tbl)))
  }
}

# Run test per group
test_results <- data_binary %>%
  group_by(analyte, water_body) %>%
  group_modify(~ test_detection_method(.x)) %>%
  ungroup()

summary_combined <- detection_summary %>%
  pivot_wider(
    names_from = phase_type,
    values_from = c(n_total, n_detect, detection_rate)
  ) %>%
  left_join(test_results, by = c("analyte", "water_body")) |>
  mutate(significant = ifelse(!is.na(p_value) & p_value < 0.05, "Yes", "No")) |>
  filter(`n_total_LC-MS/MS` > 0, `n_total_GC-MS` > 0)

summary_combined |> kable() |> kable_styling()


## count: # samples per body
data_zeros |>
  group_by(water_body, analyte) |>
  summarize(n=n(), .groups = "drop") |>
  kable() |> kable_styling()
