### Import Libraries
pacman::p_load(tidyverse, gt)

#### Tables & Figures ####

#### Table 1: Sample counts & concentration ranges by water source ####
# Note: Doesn't include references to where the data originated. Table 1 was manually written using this code as reference
table_1 <- data_zeros |>
  group_by(water_body, analyte) |>
  summarize(
    total_n = n(),
    detected_n = sum(sample_concentration_normalized > 0, na.rm = TRUE),
    
    # Filter only detected samples for these calculations
    detects_only = list(sample_concentration_normalized[sample_concentration_normalized > 0]),
    min_conc = min(unlist(detects_only), na.rm = TRUE),
    max_conc = max(unlist(detects_only), na.rm = TRUE),
    
    # Calculate Standard Deviation and Mean to get CV
    sd_conc = sd(unlist(detects_only), na.rm = TRUE),
    mean_conc = mean(unlist(detects_only), na.rm = TRUE),
    cv = sd_conc / mean_conc,
    
    .groups = "drop"
  ) |>
  mutate(
    min_conc = ifelse(is.infinite(min_conc), NA, round(min_conc, 2)),
    max_conc = ifelse(is.infinite(max_conc), NA, round(max_conc, 2)),
    cv = ifelse(is.na(cv), NA, round(cv, 2)),
    
    # Format: Total (Detected)
    Count_Str = paste0(total_n, " (", detected_n, ")"),
    
    # Format: Min-Max (CV)
    Conc_Str = case_when(
      detected_n == 0 ~ "-",
      detected_n == 1 ~ as.character(min_conc), # No CV if only 1 detect
      TRUE ~ paste0(min_conc, "-", max_conc, "\n(", cv, ")")
    )
  ) |>
  select(water_body, analyte, Count_Str, Conc_Str) |>
  pivot_wider(
    names_from = analyte,
    values_from = c(Count_Str, Conc_Str),
    names_glue = "{analyte}_{.value}"
  ) |>
  select(water_body, Nicotine_Count_Str, Nicotine_Conc_Str, Cotinine_Count_Str, Cotinine_Conc_Str) 
table_1_ready = table_1 |> 
  # filter out media we're not reporting on
  filter(water_body %in% c("Stormwater", "Surface Water", "Groundwater", "Finished Water")) |>
  # Manually append the asterisks/footnotes to the water_body names to match the image
  mutate(
    water_body = case_when(
      water_body == "Stormwater" ~ "Stormwater**",
      TRUE ~ as.character(water_body)
    )
  ) |>
  gt() |>
  # 1. Spanning Headers
  tab_spanner(
    label = md("**Nicotine**"),
    columns = c(Nicotine_Count_Str, Nicotine_Conc_Str)
  ) |>
  tab_spanner(
    label = md("**Cotinine**"),
    columns = c(Cotinine_Count_Str, Cotinine_Conc_Str)
  ) |>
  
  # 2. Column Labels (with superscript + and superscript *)
  cols_label(
    water_body = "",
    Nicotine_Count_Str = md("**Count**<sup>+</sup>"),
    Nicotine_Conc_Str = md("**Concentration<br>Range (ng/L)**<sup>*</sup>"),
    Cotinine_Count_Str = md("**Count**<sup>+</sup>"),
    Cotinine_Conc_Str = md("**Concentration<br>Range (ng/L)**<sup>*</sup>")
  ) |>
  
  # 3. Alignment
  cols_align(align = "left", columns = water_body) |>
  cols_align(align = "center", columns = -water_body) |>
  
  # 4. Main Title
  tab_header(
    title = "Table 1. Nicotine & cotinine sample counts and detected concentration ranges in California source waters."
  ) |>
  
  # 5. Add the Caveats as Source Notes at the bottom
  tab_source_note(source_note = "Raw data are tabulated in Table S4.") |>
  tab_source_note(source_note = "+ Samples analyzed are shown with positive detections in parentheses.") |>
  tab_source_note(source_note = "* Coefficient of variation (CV = standard deviation / mean) is shown in parentheses to provide a sense of range in deviation between water sources.") |>
  tab_source_note(source_note = "** Stormwater is not a direct source of drinking water.") |>
  tab_source_note(source_note = "{1} CV was not calculated for cotinine in finished waters as only one detection was reported.") |>
  
  # 6. Styling
  opt_table_lines("all") |>
  opt_table_outline() |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = water_body)
  ) |>
  
  # Allow line breaks (\n) in the cells to render properly
  fmt_markdown(columns = c(Nicotine_Conc_Str, Cotinine_Conc_Str))

table_1_ready
