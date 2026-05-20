# Graveyard :) 

#### Plot map, including 0's

# get all zeroes to help with the fill of the map points
data_zeros <- data_zeros %>%
  mutate(
    fill_conc = ifelse(sample_concentration_normalized == 0, NA, sample_concentration_normalized),
  )
# Split data
nondetects <- data_zeros %>% filter(is.na(fill_conc))   # NA values first
detects <- data_zeros %>% filter(!is.na(fill_conc)) %>% arrange(fill_conc)  # low to high

ca_data_map_zeroes = ca_base_map +
  geom_point(
    data = nondetects,
    aes(x = longitude, y = latitude, shape = analyte),
    fill = "gray90",
    color = "black",
    stroke = 0.6,
    size = 2.5,
    alpha = 0.9
  ) +  
  geom_point(
    data = detects,
    aes(x = longitude, y = latitude, shape = analyte, fill = fill_conc),
    color = "black",
    stroke = 0.6,
    size = 2.5,
    alpha = 0.9
  ) +
  # scale_fill_gradient2(name = "Concentrations (ng/l)", low="yellow", mid = "red", high="black", midpoint = 10, na.value = "gray90", trans="log10") +
  scale_fill_viridis_c(name = "Concentrations (ng/l)", option = "D", direction = -1, trans="log10") +
  # scale_fill_manual(name = "Detection Status", values = c("Zero" = "white", "Detected" = "pink"))+
  scale_shape_manual(name = "Analyte", values = c("Cotinine" = 21, "Nicotine" = 24))

ca_data_map_zeroes

# for creating a table of detections


# 2. Create totals row
totals <- data_wide %>%
  summarise(across(-analyte, sum)) %>%
  mutate(analyte = "Total") %>%
  select(analyte, everything())

# 3. Combine totals and original
data_combined <- bind_rows(data_wide, totals)

# 4. Sort columns (excluding 'analyte') by total descending
sorted_cols <- colSums(data_combined %>% select(-analyte, -Total)) %>%
  sort(decreasing = TRUE) %>%
  names()

# 6. Reorder columns: analyte + sorted water bodies + Total last
final_table <- data_combined %>%
  select(analyte, all_of(sorted_cols), Total) %>%
  rename(`Analyte` = analyte)
kable(caption="") %>%
  kable_styling()

final_table




# Insert points on map of CA
ca_detect_map = ca_nondetects_map +
  geom_point(data=data_zeros, aes(x=longitude,y=latitude, shape=analyte, fill=sample_concentration_normalized), size=2, color="black", alpha=0.7) +
  scale_fill_gradientn(name = "Concentrations (ng/l)", 
                       colors = c("skyblue","blue", "red", "black"),
                       values = scales::rescale((log10(c(1,1e1,1e2,1e3)))),
                       #trans= "log", 
                       breaks = c(1, 10, 100, 1000),
                       labels = c("1", "10", "100", "1000")) +
  scale_shape_manual(name = "Analyte", values = c("Cotinine" = 21, "Nicotine" = 24))
ca_detect_map

ca_nondetects_map = ca_base_map +
  geom_point(data=data_zeros |> filter(sample_concentration_normalized==0), aes(x=longitude, y=latitude, shape=analyte), size = 2, color="gray95", fill="black", alpha=0.7)
ca_all_samples_map
# find data that's far away
library(geosphere)

# CA approximate centroid
ca_center <- c(-119.4179, 36.7783)

# Calculate distance in meters
data_detects$dist <- distHaversine(cbind(data_detects$longitude, data_detects$latitude), ca_center)

# Find points farther than, say, 500km (~310 miles)
outliers <- data_detects %>% filter(dist > 500000)
print(outliers)
unique(outliers$row_index)

#### Unused from data_EDA

# time series?
data_ts = data_detects |>
  as_tsibble(index=sample_date, key=c(analyte, sample_concentration_normalized, latitude, longitude, site_id))
# create a set of plots showing concentration based on various variables


# Concentration - Water body
ggplot(data_zeros, aes(x = water_body, y = sample_concentration, fill = water_body)) +
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

table_null = data_clean |>
  count(data_source, keep) |>
  pivot_wider(names_from = keep, values_from = n, values_fill = 0) |>
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


#### Unused from data_gaps.R

## Check for rural v. urban data for each sampled point
library(tigris)  # optional for loading census layers
options(tigris_use_cache = TRUE)

# Download or load shapefile (manually or via tigris)
urban_areas <- st_read(here::here("data/census_rural_data/tl_rd22_us_uac20.shp"))

# Filter for California (if needed)
urban_areas_ca <- urban_areas[grepl("CA", urban_areas$NAME20), ] |>
  st_transform(crs=st_crs(4326))

sample_data$urban_class <- ifelse(
  lengths(st_intersects(sample_data, urban_areas_ca)) > 0, 
  "Urban", 
  "Rural"
)

# check our results
sample_data |>
  group_by(urban_class) |>
  summarize(count=n())


## check by hydrologic area: gis_hydrologic_regions
gis_hydrologic_regions = gis_hydrologic_regions |> st_transform(crs=st_crs(4326))

sf::sf_use_s2(FALSE) # due to duplicate vertex
samples_with_hydro <- st_join(sample_data, gis_hydrologic_regions[, c("HR_NAME")], left = TRUE) |>
  filter(water_body == "Groundwater")
sf::sf_use_s2(TRUE) # make sure it's back for later

samples_with_hydro |>
  group_by(HR_NAME) |>
  summarize(count=n()) |>
  kable() |>
  kable_styling()

## visualize what areas we have
ggplot() +
  geom_sf(data = basins_sf, color = "gray30", alpha = 0.3) +
  geom_sf(data = samples_with_basins, aes(color = basin_major), size = 1, alpha = 0.8) +
  scale_fill_viridis_d(option = "turbo", name = "Major Basin") +
  scale_color_viridis_d(option = "turbo", name = "Major Basin") +
  theme_minimal() +
  labs(title = "Sample Points by Major Groundwater Basin (1–9)",
       subtitle = "Based on CA Bulletin 118 Basin Numbering",
       x = "Longitude", y = "Latitude")

### how often are sites re-used?
sample_data |>
  group_by(site_id) |>
  summarize(count=n(), .groups = "drop") |>
  count(count, name = "n_sites") |>
  kable() |>
  kable_styling()


#### boxplots.R - Unused Figure 2 Boxplot Code ####
library(dplyr)
library(ggplot2)
library(stringr)
library(forcats)
library(tidyr)

# -----------------------------
# 1) Clean data
# -----------------------------
plot_df <- data_detects %>%
  mutate(
    water_body = ifelse(water_body == "Estuary", "Brackish Water", as.character(water_body)),
    sample_concentration_normalized = as.numeric(sample_concentration_normalized)
  ) %>%
  filter(
    !is.na(analyte),
    !is.na(water_body),
    !is.na(sample_concentration_normalized)
  ) %>%
  filter(!grepl("ocean", water_body, ignore.case = TRUE)) %>%
  droplevels()

# -----------------------------
# 2) Detection summary
# -----------------------------
summary_df <- plot_df %>%
  mutate(detected = sample_concentration_normalized > 0) %>%
  group_by(water_body, analyte) %>%
  summarise(
    n = n(),
    pct_detect = round(100 * sum(detected, na.rm = TRUE) / n),
    .groups = "drop"
  )

# -----------------------------
# 3) Order water bodies
# -----------------------------
wb_order <- plot_df %>%
  filter(sample_concentration_normalized > 0) %>%
  group_by(water_body) %>%
  summarise(med = median(sample_concentration_normalized, na.rm = TRUE), .groups = "drop") %>%
  arrange(med) %>%
  pull(water_body)

plot_df <- plot_df %>%
  mutate(water_body = factor(water_body, levels = wb_order))

summary_df <- summary_df %>%
  mutate(water_body = factor(water_body, levels = wb_order))

# -----------------------------
# 4) Build compact x-axis labels
# -----------------------------
short_names <- c(
  "Finished Water" = "Finished",
  "Recycled Water" = "Recycled",
  "Brackish Water" = "Brackish",
  "Stormwater" = "Stormwater",
  "Surface Water" = "Surface",
  "Groundwater" = "Groundwater"
)

label_df <- summary_df %>%
  mutate(
    analyte_short = case_when(
      analyte == "Cotinine" ~ "C",
      analyte == "Nicotine" ~ "N",
      TRUE ~ substr(analyte, 1, 1)
    )
  ) %>%
  select(water_body, analyte_short, pct_detect) %>%
  pivot_wider(
    names_from = analyte_short,
    values_from = pct_detect
  ) %>%
  mutate(
    water_body_short = short_names[as.character(water_body)],
    axis_label = paste0(
      water_body_short, "\n",
      "C:", ifelse(is.na(C), "-", paste0(C, "%")),
      "  N:", ifelse(is.na(N), "-", paste0(N, "%"))
    )
  )

axis_labels <- setNames(label_df$axis_label, as.character(label_df$water_body))

# -----------------------------
# 5) Colors
# -----------------------------
analyte_colors <- c(
  "Cotinine" = "#E8A19B",
  "Nicotine" = "#67C5CC"
)

# -----------------------------
# 6) Plot
# -----------------------------
p <- ggplot(
  plot_df %>% filter(sample_concentration_normalized > 0),
  aes(x = water_body, y = sample_concentration_normalized, fill = analyte)
) +
  geom_boxplot(
    aes(group = interaction(water_body, analyte)),
    position = position_dodge(width = 0.72),
    width = 0.62,
    alpha = 0.6,
    outlier.shape = NA,
    colour = "black",
    size = 0.28
  ) +
  geom_point(
    aes(group = analyte),
    position = position_jitterdodge(
      jitter.width = 0.08,
      dodge.width = 0.72,
      seed = 1
    ),
    size = 0.45,
    alpha = 0.35,
    colour = "black"
  ) +
  scale_fill_manual(values = analyte_colors, drop = TRUE) +
  scale_x_discrete(labels = axis_labels, drop = TRUE) +
  scale_y_log10(
    breaks = c(1, 10, 100, 1000),
    labels = c("1", "10", "100", "1000"),
    limits = c(10^xmin, 10^xmax)
  ) +
  labs(
    x = "Water Sampled",
    y = "Concentration (ng/L, log10 scale)"
  ) +
  theme_minimal(base_size = 9) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(size = 8, lineheight = 0.9),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    plot.margin = margin(t = 8, r = 8, b = 8, l = 8)
  )

print(p)


#### Table 2: 
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




## prepare graph 
# get sample sizes
n_data <- analysis_df %>%
  group_by(HR_NAME, analyte) %>%
  summarize(n = n(), .groups = "drop") %>%
  mutate(y_pos = -0.5) 
# add sample sizes
plot_data_final <- analysis_df %>%
  left_join(region_n, by = "HR_NAME") %>%
  mutate(Region_Label = paste0(HR_NAME, "\n(n=", total_n, ")"))
# get medians to make the graph appealing
medians <- plot_data_final %>% filter(analyte == "Nicotine") %>%
  group_by(HR_NAME) %>% summarize(med = median(sample_concentration, na.rm=TRUE)) %>% arrange(desc(med))
plot_data_final$HR_NAME <- factor(plot_data_final$HR_NAME, levels = medians$HR_NAME)

# plot concentrations by region
ggplot(plot_data_final, aes(x = HR_NAME, y = sample_concentration, fill = analyte, color = analyte)) +
  
  # The points and boxes (same as before)
  geom_point(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75), 
             alpha = 0.4, size = 1.5) +
  geom_boxplot(position = position_dodge(width = 0.75), 
               alpha = 0.7, outlier.shape = NA, color = "black") +
  
  # --- NEW: Add the sample size text for each bar ---
  # We use the n_data summary we created in Step 1.
  # position_dodge must match the width of the boxplot dodge (0.75) so they align perfectly.
  geom_text(data = n_data, 
            aes(x = HR_NAME, y = y_pos, label = paste0("n=", n)), 
            position = position_dodge(width = 0.75), 
            size = 3,       # Adjust text size as needed
            color = "black", # Make sure it's readable
            vjust = 1,      # Push text slightly down
            angle = 90) +   # Rotate text 90 degrees if it's too wide
  
  # The pseudo-log scale
  scale_y_continuous(trans = pseudo_log_trans(sigma = 1, base = 10),
                     breaks = c(0, 1, 10, 100, 1000), labels = comma) +
  
  scale_fill_manual(values = c("Nicotine" = "#1f77b4", "Cotinine" = "#ff7f0e")) +
  scale_color_manual(values = c("Nicotine" = "#1f77b4", "Cotinine" = "#ff7f0e")) +
  
  theme_minimal() +
  labs(
    title = "Nicotine and Cotinine Concentrations by Hydrologic Region",
    x = "Hydrologic Region",
    y = "Concentration (ng/L, pseudo-log scale)",
    fill = "Analyte"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    legend.position = "top",
    panel.grid.minor = element_blank()
  ) +
  guides(color = "none") 

# plot by region with faceted water bodies
library(dplyr)
library(ggplot2)
library(scales)

# 1. Calculate the specific sample sizes for EACH bar (Region + Analyte + Water Body combination)
n_data_facet <- analysis_df %>%
  filter(!is.na(water_body),  water_body!="Ocean: Coastal") %>% # Remove samples without a defined water body
  group_by(HR_NAME, analyte, water_body) %>%
  summarize(n = n(), 
            y_pos = max(analysis_df$sample_concentration, na.rm = TRUE) * 5, 
            .groups = "drop") 

# 2. Filter your main plot data to exclude NAs in water_body so the facet panels are clean
plot_data_facet <- analysis_df %>%
  filter(!is.na(water_body), !is.na(HR_NAME), water_body!="Ocean: Coastal")

# 3. Create the multi-panel plot
ggplot(plot_data_facet, aes(x = reorder(HR_NAME, sample_concentration, FUN=median, na.rm=TRUE), y = sample_concentration, fill = analyte, color = analyte)) +
  
  # Add the points (jittered behind the boxes)
  geom_point(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75), 
             alpha = 0.4, size = 1.2) +
  
  # Add the boxplots
  geom_boxplot(position = position_dodge(width = 0.75), 
               alpha = 0.7, outlier.shape = NA, color = "black") +
  
  # Add the sample sizes (n=X) to the base of each bar
  geom_text(data = n_data_facet, 
            aes(x = HR_NAME, y = Inf, label = paste0("n=", n), group = analyte), 
            position = position_dodge(width = 0.75), 
            size = 2.5,     # slightly smaller text to fit in the panels
            color = "black", 
            vjust = 1,
            hjust = 1.3,
            angle = 75) +   
  
  # The pseudo-log scale (handles the 0s perfectly)
  scale_y_continuous(trans = pseudo_log_trans(sigma = 1, base = 10),
                     breaks = c(0, 1, 10, 100, 1000), labels = comma) +
  
  scale_fill_manual(values = c("Nicotine" = "#1f77b4", "Cotinine" = "#ff7f0e"),
                    labels = c("Nicotine" = "Nicotine", "Cotinine" = "Cotinine")) +
  scale_color_manual(values = c("Nicotine" = "#1f77b4", "Cotinine" = "#ff7f0e")) +
  
  # --- NEW: Facet by water_body to create separate panels ---
  facet_wrap(~ water_body, scales = "free") + 
  expand_limits(y = max(plot_data_facet$sample_concentration, na.rm = TRUE) * 2) +
  theme_bw() + # A clean theme with borders around the facets
  labs(
    title = "",
    x = "",
    y = "Concentration (ng/L, pseudo-log scale)",
    fill = "Analyte"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    legend.position = "top",
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "grey90"), # Makes the facet headers pop
    strip.text = element_text(face = "bold", size = 12)
  ) +
  guides(color = "none")

### View counts of samples by HUC
total = nrow(analysis_df)
analysis_df |>
  group_by(HR_NAME) |>
  summarize(n=n(), 
            pct = n()/total*100,
            .groups="drop") |>
  arrange(desc(n))


### EXEC SUMM: Frequency   
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




#### STAT: SD for each analyte-source ####
data_zeros |>
  group_by(analyte, water_body) |>
  summarize(mean = mean(sample_concentration_normalized, na.rm= TRUE),
            sd = sd(sample_concentration_normalized, na.rm= TRUE),
            cv = sd/mean,
            n = n(),
            .groups = "drop"
  ) |> 
  arrange(desc(cv)) 

#### cotinine median values
data_zeros |>
  mutate(detected = sample_concentration_normalized > 0) |>
  #group_by(analyte, detected) |> # get % detects
  group_by(analyte, detected, water_body) |> # get median per body
  summarize(n=n(),
            median = median(sample_concentration_normalized, na.rm=TRUE),.groups = "drop_last") |>
  arrange(analyte, desc(median))  

## TABLE: detection counts by body
detection_ranges <- data_detects %>%
  group_by(analyte, water_body) %>%
  summarize(
    min_conc = min(sample_concentration_normalized, na.rm = TRUE),
    median_conc = median(sample_concentration_normalized, na.rm=TRUE),
    max_conc = max(sample_concentration_normalized, na.rm = TRUE),
    n_detects = n(),
    .groups = "drop"
  )
detection_ranges

## Number of non-detects
nrow(data_zeros) - nrow(data_detects) # Num = 3001
(nrow(data_zeros) - nrow(data_detects)) / nrow(data_zeros) # % = 83.6%

#### number of outliers ####
data_zeros %>%
  group_by(analyte, water_body) %>%
  summarize(n_total=n(),
            min = min(sample_concentration_normalized, na.rm=TRUE),
            max=max(sample_concentration_normalized, na.rm=TRUE),
            n_outliers = sum(detect_outliers(sample_concentration_normalized),na.rm=TRUE),
            outlier_pct = round(100 * n_outliers / n_total, 2),
            min_outlier = min(sample_concentration_normalized[detect_outliers(sample_concentration_normalized)], na.rm = TRUE),
            max_outlier = max(sample_concentration_normalized[detect_outliers(sample_concentration_normalized)], na.rm = TRUE),
            .groups = "drop"
  )

detect_outliers <- function(x) {
  Q1 <- quantile(x, 0.25)
  Q3 <- quantile(x, 0.75)
  IQR <- IQR(x)
  
  x < (Q1 - 1.5*IQR) | x > (Q3 + 1.5*IQR)
}

# skew?
data_zeros |>
  ggplot(aes(x=sample_concentration_normalized+1e-6))+
  geom_histogram()+
  facet_wrap(~analyte + water_body, scales="free")+
  theme_minimal()+
  scale_y_log10()
scale_x_log10()



# samples of nic in GW & SW
data_detects |> 
  filter(analyte == "Nicotine", water_body %in% c("Groundwater","Surface Water")) |>
  group_by(water_body) |>
  summarize(n=n())

# samples of cot in gw & sw
data_detects |> 
  filter(analyte == "Cotinine", water_body %in% c("Groundwater","Surface Water")) |>
  group_by(water_body) |>
  summarize(n=n())