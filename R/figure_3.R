#### Consider geographic distribution ####
library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)

# 1. Load the shapefile
hydro_regions <- st_read("C:/Users/nadav/OneDrive/Desktop/Research/R/data/i03_Hydrologic_Regions/i03_Hydrologic_Regions.shp")

# 2. Get the exact CRS from the loaded shapefile
# It's always safer to transform the points to the complex shapefile CRS, rather than the other way around
target_crs <- st_crs(hydro_regions)

# 3. Create the spatial points from your data and immediately project them
samples_sf = data_zeros |>
  filter(!is.na(latitude), !is.na(longitude)) |>
  # 4326 is the raw lat/lon datum
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |> 
  # Transform to match the hydro regions perfectly
  st_transform(crs = target_crs)

# 4. Perform a geometric intersection join
# By omitting the 'join' argument, it defaults to st_intersects, 
# meaning "only match if the point is physically inside the polygon"
samples_with_hr <- st_join(samples_sf, hydro_regions)

### Figure 3: boxplots of concentrations by region

# take data with HUCs
analysis_df <- samples_with_hr %>%
  st_drop_geometry() %>%
  filter(!is.na(HR_NAME), !is.na(sample_concentration))

# 1. Filter the dataset to only Surface Water and Groundwater
analysis_df_factor <- analysis_df |> 
  mutate(HR_NAME = factor(HR_NAME, levels = c("Central Coast", "Colorado River", "North Coast", 
                                              "North Lahontan", "Sacramento River", 
                                              "San Francisco Bay", "San Joaquin River", 
                                              "South Coast", "South Lahontan", "Tulare Lake")))

analysis_df_filtered <- analysis_df_factor |>
  filter(water_body %in% c("Surface Water", "Groundwater")) |>
  mutate(water_body = droplevels(water_body),
         water_body = factor(water_body, levels = c("Surface Water", "Groundwater")))

# 2. Generate the missing analyte lanes
missing_analyte_combos_y <- expand_grid(
  water_body = unique(analysis_df_filtered$water_body),
  HR_NAME = levels(analysis_df_filtered$HR_NAME),
  analyte = c("Cotinine", "Nicotine")
) |>
  left_join(
    analysis_df_filtered |> count(water_body, HR_NAME, analyte),
    by = c("water_body", "HR_NAME", "analyte")
  ) |>
  filter(is.na(n) | n == 0)

# 3. Calculate sample sizes, ensuring (0) appears when an analyte is missing
sample_sizes_filtered <- analysis_df_filtered |>
  group_by(water_body, HR_NAME, analyte) |>
  summarise(n = n(), .groups = "drop") |>
  complete(nesting(water_body, HR_NAME), analyte, fill = list(n = 0))

# 4. Define your exact colors
analyte_colors <- c("Cotinine" = "#F8766D", "Nicotine" = "#00BFC4")

# 5. Build the Plot
figure_3 <- ggplot(analysis_df_filtered, aes(y = HR_NAME, x = sample_concentration + 0.1)) +
  
  # A. The Analyte-Specific Gray Lanes
  geom_tile(
    data = missing_analyte_combos_y,
    aes(y = HR_NAME, x = 1, group = analyte), 
    fill = "gray85",    # Hardcoded OUTSIDE of aes() to keep the legend clean
    width = Inf, 
    height = 0.75,      
    alpha = 0.6,       
    position = position_dodge(width = 0.75),
    inherit.aes = FALSE
  ) +
  
  # B. Reporting Limit Line (Uncomment and adjust xintercept if you want this)
  # geom_vline(xintercept = 0.5, linetype = "dashed", color = "gray40", linewidth = 0.5) +
  
  # C. Boxplots
  geom_boxplot(
    aes(fill = analyte),   
    color = "black",       
    outlier.shape = 21,    
    outlier.colour = "transparent", 
    alpha = 0.8, 
    outlier.size = 1.2,    
    outlier.alpha = 0.6, 
    linewidth = 0.3, 
    fatten = 3,
    position = position_dodge(width = 0.75, preserve = "single")
  ) +
  
  # D. Sample Size Labels
  geom_text(
    data = sample_sizes_filtered,
    aes(y = HR_NAME, x = Inf, label = paste0("(", n, ") "), color = analyte),
    size = 2.5,        
    hjust = 1,         
    vjust = 0.5,       
    position = position_dodge(width = 0.75), 
    show.legend = FALSE,
    inherit.aes = FALSE
  ) +
  
  # E. Side-by-side facets for water body
  facet_grid(. ~ water_body) +
  
  # F. Scales
  scale_y_discrete(limits = rev(levels(analysis_df_filtered$HR_NAME))) + 
  scale_fill_manual(values = analyte_colors) +
  scale_color_manual(values = analyte_colors) +
  scale_x_log10(
    breaks = c(0.1, 1, 10, 100, 1000),
    labels = c("ND", "1", "10", "100", "1,000"),
    limits = c(0.1, 8000),
    expand = expansion(mult = c(0, 0.05)) # Removes left padding, keeps right padding
  ) +
  
  # G. Theme and Layout
  labs(
    y = "", 
    x = "Concentration (ng/L, log10 scale)",
    fill = "Analyte",
    color = "Analyte" 
  ) +
  theme_bw(base_size = 9) +
  theme(
    # Trims white space off the left edge of the TIFF
    plot.margin = margin(t = -3, r = 2, b = 4, l = -8), 
    
    # Axis formatting
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(margin = margin(r = 2)),
    
    # Facet headers
    strip.background = element_rect(fill = "gray90"),
    strip.text.x = element_text(face = "bold"), 
    
    # Grid lines (hide horizontal lines so gray lanes stand out clearly)
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(), 
    panel.spacing = unit(0.3, "lines"),
    
    # Legend formatting (transparent backgrounds to prevent white overlap!)
    legend.position = "top",
    legend.box.margin = margin(t = 0, r = 4, b = -10, l = 0),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.key = element_rect(fill = "transparent", color = NA),
    legend.title = element_text(size = 9, face = "bold"),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.8, "lines")
  )

# Print it to preview
print(figure_3)

# 6. Save as high-res single-column figure
ggsave(
  filename = "Figure 3.tiff", 
  plot = figure_3, 
  width = 3.33, 
  height = 4.5,
  units = "in",
  dpi = 600, 
  compression = "lzw"
)

#### Unused ####
### map of all samples with coloring by HR
ggplot() +
  # Add the California state outline as the base layer
  geom_sf(data = ca_border, fill = "gray95", color = "gray50") +
  
  # Add the Groundwater Basins (semi-transparent blue)
  geom_sf(data = ca_hydrologic_regions_sf, fill = "lightblue", color = "steelblue", alpha = 0.5) +
  
  # Add the sample points, colored by whether they fall in a basin
  geom_sf(data = samples_with_hr, 
          aes(color = HR_NAME), 
          size = 2, 
          alpha = 0.8) +
  theme_minimal() +
  labs(
    title = "Sample Locations vs. CA Hydro Regions",
    x = "",
    y = "",
    color = "Sample Location",
  ) +
  theme(legend.position = "bottom")
