#Lineages_2024
# ----- Libraries -----
library(tidyverse)
library(readxl)
library(ozmaps)
library(scatterpie)
library(grid)
library(cowplot)

# ----- Step 1: Read and clean data -----
df <- read_excel("Lineage_isolates_2024.xlsx") %>%
  filter(!is.na(long) & !is.na(lat)) %>%
  rename(temp = long) %>%
  rename(long = lat) %>%
  rename(lat = temp)

lineage_cols <- as.character(1:19)

df <- df %>%
  select(any_of(c("long", "lat", "state", lineage_cols))) %>%
  mutate(across(all_of(lineage_cols), ~as.numeric(.))) %>%
  filter(rowSums(across(all_of(lineage_cols)), na.rm = TRUE) > 0)

# ----- Step 2: Collapse lineages -----
lineage_sums <- colSums(df[, lineage_cols], na.rm = TRUE)
top_lineages <- names(sort(lineage_sums, decreasing = TRUE))[1:9]
other_lineages <- setdiff(lineage_cols, top_lineages)

df_pie <- df %>%
  mutate(Other = rowSums(across(all_of(other_lineages)), na.rm = TRUE)) %>%
  select(long, lat, state, all_of(top_lineages), Other)

lineage_cols_reduced <- c("1", "5", "6", "9", "11", "14", "16", "18", "19")

# ----- Step 3: Base map -----
states <- ozmap("states")

# ----- Step 4: Color palette -----
lineage_colors <- c(
  "1" = "#C77CFF", "5" = "#FFCD00", "6" = "#0072B2", "9" = "#9467BD",
  "11" = "#619CFF", "14" = "#e7298a", "16" = "#00BFC4",
  "18" = "#F8766D", "19" = "#1b1b1b"
)

# ----- Step 5a: Map layer -----
p_map <- ggplot() +
  geom_sf(data = states, fill = "white", color = "grey30", linewidth = 0.5) +
  coord_sf(xlim = c(110, 155), ylim = c(-45, -10), expand = FALSE) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold")) +
  theme(legend.position = "none")

# ----- Step 5b: Pie chart layer (no legend) -----
missing_cols <- setdiff(lineage_cols_reduced, colnames(df_pie))
if (length(missing_cols) > 0) {
  for (cc in missing_cols) df_pie[[cc]] <- 0
}

df_pie$total  <- rowSums(df_pie[, lineage_cols_reduced, drop = FALSE], na.rm = TRUE)
df_pie$radius <- scales::rescale(df_pie$total, to = c(1.4, 3.6))

p_pie <- ggplot() +
  geom_scatterpie(
    data = df_pie,
    aes(x = long, y = lat, r = radius),       
    cols = lineage_cols_reduced,
    color = NA, lwd = 0
  ) +

  coord_equal(xlim = c(110, 155), ylim = c(-45, -10), expand = FALSE) +
  scale_fill_manual(values = lineage_colors, breaks = lineage_cols_reduced, name = "Lineage") +
  theme_void() +
  theme(legend.position = "none") +           
  guides(fill = "none")                       

# ----- Step 5c: Add state + total labels -----
df_pie <- df_pie %>%
  mutate(
    label_dx = case_when(
      state == "WA"  ~ 4.5,   
      state == "NSW" ~  2.5,   
      state == "VIC" ~  2.1,   
      TRUE ~  2.0              
    ),
    label_dy = case_when(
      state == "VIC" ~ -0.5,   
      TRUE ~ 0
    )
  )

p_pie <- p_pie +
  geom_text(
    data = df_pie,
    aes(
      x = long + label_dx,
      y = lat + label_dy,
      label = paste0(state, "\n", "n=", total)
    ),
    size = 3.8,
    hjust = 0,  
    vjust = 0.4,
    fontface = "bold",
    lineheight = 0.95
  )

# ----- Step 6: Dummy legend plot-----
dummy_legend_data <- tibble(
  lineage = factor(lineage_cols_reduced, levels = lineage_cols_reduced),
  value = 1
)

legend_plot <- ggplot(dummy_legend_data, aes(x = lineage, y = value, fill = lineage)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = lineage_colors, name = "Lineage",
                    breaks = lineage_cols_reduced,
                    labels = paste0("L", lineage_cols_reduced)) +
  theme_void() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text  = element_text(size = 10),
    legend.key.size = unit(0.32, "cm")
  )

legend_grob <- cowplot::get_legend(legend_plot)

# ----- Step 7: Combine using ggdraw for tight control -----
final_plot <- cowplot::ggdraw() +
  draw_plot(p_map, x = 0, y = 0, width = 0.88, height = 1) +     
  draw_plot(p_pie, x = 0, y = 0, width = 0.88, height = 1) +    
  draw_plot(legend_grob, x = 0.80, y = 0.28, width = 0.18, height = 0.52)


#Lineage_2022-2024_Collections
# ----- Libraries -----
library(tidyverse)
library(readxl)
library(ozmaps)
library(scatterpie)
library(grid)
library(cowplot)
library(scales)

# ----- Step 1: Read and clean data -----
df <- read_excel("Lineage_isolates_2022-2024.xlsx") %>%
  filter(!is.na(long) & !is.na(lat)) %>%
  rename(temp = long) %>%
  rename(long = lat) %>%
  rename(lat = temp)

lineage_cols <- as.character(1:19)

df <- df %>%
  select(any_of(c("long", "lat", "state", lineage_cols))) %>%
  mutate(across(all_of(lineage_cols), ~as.numeric(.))) %>%
  filter(rowSums(across(all_of(lineage_cols)), na.rm = TRUE) > 0)

# ----- Step 2: Collapse lineages -----
df <- df %>%
  mutate(across(all_of(lineage_cols), ~as.numeric(trimws(.)))) %>%
  filter(rowSums(across(all_of(lineage_cols)), na.rm = TRUE) > 0)

df$total  <- rowSums(df[, lineage_cols], na.rm = TRUE)

df$radius <- scales::rescale(df$total, to = c(1.5, 3.6))

# ----- Step 3: Base map -----
states <- ozmap("states")

# ----- Step 4: Color palette -----
lineage_colors <- c(
  "1" = "#C77CFF", "2" = "#8DD3C7", "3" = "#BEBADA", "4" = "#80B1D3",
  "5" = "#FFCD00", "6" = "#0072B2", "7" = "#FDB462", "8" = "#999999",
  "9" = "#9467BD", "10" = "#A6761D", "11" = "#619CFF", "12" = "#CCEBC5",
  "13" = "#D9D9D9", "14" = "#e7298a", "15" = "#FFED6F", "16" = "#00BFC4",
  "17" = "#B3DE69", "18" = "#F8766D", "19" = "#1b1b1b"
)

# ----- Step 5a: Map layer -----
p_map <- ggplot() +
  geom_sf(data = states, fill = "white", color = "grey30", linewidth = 0.5) +
  coord_sf(xlim = c(110, 155), ylim = c(-45, -10), expand = FALSE) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"))

# ----- Step 5b: Pie chart layer (no legend) -----
p_pie <- ggplot() +
  geom_scatterpie(
    data = df,
    aes(x = long, y = lat, r = radius),
    cols = lineage_cols,
    color = NA, lwd = 0
  ) +
  coord_equal(xlim = c(110, 155), ylim = c(-45, -10), expand = FALSE) +
  scale_fill_manual(values = lineage_colors, name = "Lineage", breaks = lineage_cols) +
  theme_void() +
  guides(fill = "none")

# ----- Step 5c: Add state + total labels -----
df <- df %>%
  mutate(
    label_dx = case_when(
      state == "WA"  ~ 4.2,
      state == "NSW" ~ 3.0,
      state == "VIC" ~ 2.1,
      state == "QLD" ~ 2.0,
      TRUE ~ 2.0
    ),
    label_dy = case_when(
      state == "VIC" ~ -0.5,
      TRUE ~ 0
    )
  )

p_pie <- p_pie +
  geom_text(
    data = df,
    aes(
      x = long + label_dx,
      y = lat + label_dy,
      label = paste0(state, "\n", "n=", total)
    ),
    size = 3.6,
    hjust = 0,
    vjust = 0.4,
    fontface = "bold",
    lineheight = 0.92
  )

# ----- Step 6: Dummy legend plot -----
present_lineages <- df %>%
  summarise(across(all_of(lineage_cols), ~sum(.x, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "lineage", values_to = "total") %>%
  filter(total > 0) %>%
  pull(lineage)

lineage_colors_present <- lineage_colors[present_lineages]

dummy_legend_data <- tibble(
  lineage = factor(present_lineages, levels = present_lineages),
  value = 1
)

legend_plot <- ggplot(dummy_legend_data, aes(x = lineage, y = value, fill = lineage)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(
    values = lineage_colors_present,
    name = "Lineage",
    breaks = present_lineages,
    labels = paste0("L", present_lineages)
  ) +
  theme_void() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text  = element_text(size = 9),
    legend.key.size = unit(0.3, "cm")
  )

legend_grob <- cowplot::get_legend(legend_plot)

# ----- Step 7: Combine using ggdraw -----
final_plot <- cowplot::ggdraw() +
  draw_plot(p_map, x = 0, y = 0, width = 0.88, height = 1) +
  draw_plot(p_pie, x = 0, y = 0, width = 0.88, height = 1) +
  draw_plot(legend_grob, x = 0.80, y = 0.28, width = 0.18, height = 0.52)



