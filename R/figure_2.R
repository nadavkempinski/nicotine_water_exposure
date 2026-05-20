#### single boxplot of all concentrations by water body ####
library(ggplot2)
library(dplyr)
library(tidyr)

analyte_colors <- c("Cotinine" = "#F8766D", "Nicotine" = "#00BFC4")

# Get y-axis limits
ymin <- min(data_detects$sample_concentration_normalized, na.rm = TRUE)
ymax <- max(data_detects$sample_concentration_normalized, na.rm = TRUE)
ymin <- floor(log10(ymin))
ymax <- ceiling(log10(ymax))

# 1. Data Prep
plot_df <- data_detects %>%
  filter(!grepl("ocean", water_body, ignore.case = TRUE)) %>%
  mutate(water_body = ifelse(water_body == "Estuary", "Brackish Water", as.character(water_body))) %>%
  group_by(water_body, analyte) %>%
  mutate(n_group = n()) %>% # This checks if n < 3 for the specific analyte
  ungroup()

label_df <- plot_df %>%
  group_by(water_body) %>%
  summarize(
    n_cot = sum(analyte == "Cotinine", na.rm = TRUE),
    n_nic = sum(analyte == "Nicotine", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(water_body_labeled = paste0(water_body, "\n(n=", n_cot, "; ", n_nic, ")"))

plot_df_combined <- plot_df %>%
  left_join(label_df, by = "water_body") %>%
  # TRICK: This ensures missing groups (like 0 Nicotine in Brackish Water) 
  # still take up invisible space so dodging alignment is 100% perfect.
  tidyr::complete(water_body_labeled, analyte)

# 3. Build the Plot
combined_boxplot <- ggplot(plot_df_combined, aes(x = water_body_labeled, y = sample_concentration_normalized, fill = analyte)) +
  
  # A. Jittered points (in background)
  geom_point(
    shape = 21,          # Shape 21 allows a filled center with a distinct border
    color = "black",     # Point border color
    stroke = 0.3,        # Point border thickness
    position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.75),
    size = 0.8,
    alpha = 0.8,
    show.legend = FALSE  # Prevent duplicate legend keys
  ) +
  
  # B. Whiskers/Error bars (Only for n >= 3)
  stat_boxplot(
    data = plot_df_combined %>% filter(n_group >= 3),
    geom = "errorbar",
    width = 0.2, 
    linewidth = 0.3,
    position = position_dodge(width = 0.75, preserve = "single") # Forces it to hold the space
  ) +
  
  # 3. Boxes - ADD preserve = "single"
  geom_boxplot(
    data = plot_df_combined %>% filter(n_group >= 3),
    alpha = 0.6,
    outlier.shape = NA,
    colour = "black",
    linewidth = 0.3,
    fatten = 4, 
    position = position_dodge(width = 0.75, preserve = "single") # Forces it to hold the space
  )+
  
  scale_fill_manual(values = analyte_colors) +
  scale_y_log10(
    breaks = c(1, 10, 100, 1000),
    labels = c("1", "10", "100", "1000"),
    limits = c(10^(ymin), 10^ymax)
  ) +
  labs(
    x = "Water Sampled (n=Cotinine; Nicotine)", 
    y = "Concentration (ng/L, log10 scale)",
    fill = "Analyte"
  ) +
  theme_minimal(base_size = 7, base_family = "Arial") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 8), 
    axis.title = element_text(size = 9),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 11),
    legend.position = "top", 
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    plot.margin = margin(t = 10, r = 15, b = 15, l = 10)
  )

combined_boxplot


ggsave(
  filename = "Figure 2.tiff",
  plot     = combined_boxplot,
  width = 3.33,
  height = 3,
  units = "in",
  dpi = 600
)

#### Prior fig2 plots ####
{
#   
#   #### FIGURE: boxplots of concentrations by analyte-water body ####
#   library(ggplot2)
#   library(dplyr)
#   library(patchwork)
#   
#   ## Prep patchwork theme for publication
#   theme_set(
#     theme_minimal(base_size = 7, base_family = "Arial")
#   )
#   patchwork_safe_theme <- theme(
#     text        = element_text(family = "Arial", size = 7),
#     axis.text   = element_text(size = 7),
#     axis.title  = element_text(size = 8),
#     plot.title  = element_text(size = 9, face = "bold"),
#     strip.text  = element_text(size = 8),
#     legend.title = element_text(size = 8),
#     legend.text  = element_text(size = 7),
#     panel.grid.minor = element_blank(),
#     plot.margin = margin(4, 4, 4, 4)
#   )
#   
#   
#   # Filter valid values
#   # Get sample sizes per group
#   sample_sizes <- data_detects %>%
#     group_by(analyte, water_body) %>%
#     summarize(n = n(), .groups = "drop")
#   
#   # Color palette by analyte
#   analyte_colors <- c(
#     "Cotinine" = "#F8766D",  # reddish
#     "Nicotine" = "#00BFC4"   # teal
#   )
#   
#   # get y-mix & y-max for the boxplots
#   ymin <- min(data_detects$sample_concentration_normalized, na.rm = TRUE)
#   ymax <- max(data_detects$sample_concentration_normalized, na.rm = TRUE)
#   ymin <- floor(log10(ymin))
#   ymax <- ceiling(log10(ymax))
#   
#   make_boxplot <- function(analyte_name) {
#     
#     label_df <- filter(sample_sizes, analyte == analyte_name) %>%
#       mutate(water_body = ifelse(water_body == "Estuary", "Brackish Water", as.character(water_body)))
#     
#     plot_df <- filter(data_detects, analyte == analyte_name) %>%
#       mutate(water_body = ifelse(water_body == "Estuary", "Brackish Water", as.character(water_body))) %>%
#       left_join(label_df, by = c("analyte", "water_body")) %>%
#       mutate(
#         water_body_labeled = paste0(water_body, " (n=", n, ")"),
#         method = ifelse(n < 5, "strip", "box")
#       )
#     
#     ggplot(plot_df, aes(x = water_body_labeled, y = sample_concentration_normalized)) +
#       geom_boxplot(
#         data  = subset(plot_df, method == "box"),
#         fill  = analyte_colors[[analyte_name]],
#         alpha = 0.6,
#         outlier.shape = NA,
#         colour = "black",
#         linewidth = 0.3 # Use linewidth instead of size
#       ) +
#       geom_jitter(
#         width = 0.15,
#         size  = 0.5,
#         alpha = 0.8,
#         colour = "black"
#       ) +
#       scale_y_log10(
#         breaks = c(1, 10, 100, 1000),
#         labels = c("1", "10", "100", "1000"),
#         limits = c(10^ymin, 10^ymax) # Using ymin/ymax you defined
#       ) +
#       labs(
#         x = "",
#         y = "Concentration (ng/L, log10 scale)",
#         title = analyte_name
#       ) +
#       theme_minimal() 
#     # Notice: we removed the theme(axis.text) here, we will apply it globally below!
#   }
#   
#   # Create one plot per analyte
#   plots <- lapply(unique(data_detects$analyte), make_boxplot)
#   
#   boxplots = plots[[1]] + plots[[2]] + #wrap_plots(plots) + 
#     plot_annotation(
#       # title = "Detected Analyte Concentrations by Waters Sampled (ng/L)",
#       theme = theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
#     )
#   boxplots # add ridgeline for Figure 4
#   
#   ### to save without ridgeplots
#   ggsave(
#     filename = "boxplots_only.png",
#     plot     = boxplots,
#     width    = 3.3,      # inches  (504 pt – valid 2-column width)
#     height   = 3.5,    # tweak for aspect ratio, just keep < 9.17"
#     units    = "in",
#     dpi      = 600
#   )
#   
#   #### FIGURE: Ridgeplot of analyte x water sampled (good for highlighting distribution) ####
#   library(ggridges)
#   
#   ridgeplots = data_detects |>
#     filter(water_body != "Finished Water") |> # not enough finished concentrations to show  
#     mutate(water_body = forcats::fct_drop(water_body)) |>
#     ggplot(aes(x = sample_concentration_normalized, y = water_body, fill = analyte)) +
#     ggridges::geom_density_ridges(
#       scale = 1.2,
#       alpha = 0.6,
#       rel_min_height = 0.01,
#       linewidth = 0.3,
#       color = "black"
#     ) +
#     scale_x_log10(
#       breaks = c(1, 10, 100, 1000),
#       labels = c("1", "10", "100", "1000")
#     ) +
#     labs(
#       x = "Concentration (ng/L, log10 scale)",
#       y = "Sampled Water Source",
#       title = "Analyte Concentration Distribution",
#       fill = "Analyte"
#     ) +
#     scale_fill_manual(values = c("Cotinine" = "#F8766D", "Nicotine" = "#00BFC4")) +
#     theme_minimal(base_size = 12) +
#     theme(
#       strip.text = element_text(face = "bold"),
#       legend.position = c(0.45, -0.25), 
#       # legend.position = "bottom",
#       legend.direction = "horizontal",
#       plot.title.position = "panel",
#       plot.title = element_text(hjust = 0.5)
#     )
#   
#   ridgeplots
#   
#   library(patchwork)
#   boxplots = boxplots + theme(
#     axis.text.x = element_text(angle = 45, hjust=1, size=7),
#     plot.margin = margin(r=20)
#   )
#   boxplots
#   
#   fig2 <- plots[[1]] | plots[[2]] | ridgeplots
#   fig2 <- fig2 & 
#     theme(
#       # Fix 1: Uniform angles with vjust=1 to anchor perfectly
#       axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 7),
#       
#       # Fix 2: Extra right and bottom margin to prevent text clipping
#       plot.margin = margin(t = 5, r = 15, b = 10, l = 5),
#       
#       # Fix 3: Coherent, centered headers across all 3 subplots
#       plot.title = element_text(hjust = 0.5, face = "bold", size = 9)
#     )
#   
#   fig2
#   
#   # save for publication
#   ggsave(
#     filename = "Figure 2.png",
#     plot     = fig2,
#     width    = 7,      # inches  (504 pt – valid 2-column width)
#     height   = 3,    # tweak for aspect ratio, just keep < 9.17"
#     units    = "in",
#     dpi      = 600
#   )
}