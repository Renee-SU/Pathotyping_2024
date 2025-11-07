# ---------- Libraries ----------
library(tidyverse)
library(readxl)
library(ggpubr)

# ---------- Paths ----------
file_path <- "isolate_phenotypes_All_Pc_genes_2022-2024.xlsx"
key <- setNames(0:9, c("0", "0;", ";", ";C", "1;", "1", "2", "3", "3+", "4"))

# ---------- Process data ----------
process_sheet <- function(sheet_name) {
  df <- read_excel(file_path, sheet = sheet_name)
  df %>%
    pivot_longer(-Differential, names_to = "Isolate", values_to = "Score") %>%
    mutate(
      Score_numeric = suppressWarnings(as.numeric(key[Score])),
      Year = sheet_name,
      State = gsub("^[0-9]+([A-Z]+).*", "\\1", Isolate)
    ) %>%
    filter(!is.na(Score_numeric))
}

df_all <- bind_rows(
  process_sheet("2022"),
  process_sheet("2023"),
  process_sheet("2024")
)

# ---------- Filter ----------
df_all <- df_all %>%
  filter(!(Year %in% c("2023", "2024") & State == "QLD")) %>%
  droplevels()

df_all$Year <- factor(df_all$Year, levels = c("2022", "2023", "2024"))

# ---------- Wilcoxon tests (exact = FALSE avoids p=1 issue) ----------
w_22_23 <- wilcox.test(df_all$Score_numeric[df_all$Year == "2022"],
                       df_all$Score_numeric[df_all$Year == "2023"],
                       exact = FALSE)
w_22_24 <- wilcox.test(df_all$Score_numeric[df_all$Year == "2022"],
                       df_all$Score_numeric[df_all$Year == "2024"],
                       exact = FALSE)
w_23_24 <- wilcox.test(df_all$Score_numeric[df_all$Year == "2023"],
                       df_all$Score_numeric[df_all$Year == "2024"],
                       exact = FALSE)

cat("2022 vs 2023:", w_22_23$p.value, "\n")
cat("2022 vs 2024:", w_22_24$p.value, "\n")
cat("2023 vs 2024:", w_23_24$p.value, "\n")

# ---------- Colors ----------
year_colors <- c("2022" = "#00BFC4", "2023" = "#F8766D", "2024" = "#009E73")

# ---------- Build plot ----------
p <- ggplot(df_all, aes(x = Year, y = Score_numeric, fill = Year)) +
  geom_violin(trim = FALSE, scale = "area", alpha = 0.8, color = NA) +
  geom_jitter(width = 0.15, size = 1, color = "black", alpha = 0.3) +
  
  stat_summary(
    fun.data = mean_cl_normal,   
    geom = "errorbar",
    width = 0.15,
    size = 0.6,
    color = "grey"
  ) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3,
               fill = "white", color = "black") +
  
  scale_fill_manual(values = year_colors) +
  labs(
    title = "Virulence distribution by year across Australia",
    x = "Year",
    y = "Virulence score (0–9)"
  ) +
  theme_classic(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 13),
    legend.position = "none",
    axis.text = element_text(color = "black")
  ) +
  
  # —— clean significance (only stars, no lines) ——
  stat_pvalue_manual(
    data = data.frame(
      group1 = c("2022", "2022", "2023"),
      group2 = c("2023", "2024", "2024"),
      y.position = c(9, 9.6, 10.2),
      p.signif = c(
        ifelse(w_22_23$p.value < 0.05, "****", "ns"),
        ifelse(w_22_24$p.value < 0.05, "****", "ns"),
        ifelse(w_23_24$p.value < 0.05, "****", "ns")
      )
    ),
    label = "p.signif",
    hide.ns = TRUE,
    bracket.size = 0,
    tip.length = 0,
    bracket.nudge.y = 0,
    size = 5
  )
