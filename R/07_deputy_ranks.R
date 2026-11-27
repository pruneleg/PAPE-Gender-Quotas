# figure: share of women by deputy mayor rank, 2014

source("R/00_config.R")

officials <- readRDS(file.path(PATH_PROCESSED, "officials_individual.rds"))
panel     <- readRDS(file.path(PATH_PROCESSED, "analysis_panel.rds"))

# same window as the deputy RD in Table 2
bw_deputies <- optimal_bw(filter(panel, stable, year == 2014), "share_women_deputies")

rank_labels <- c("1st deputy", "2nd deputy", "3rd deputy", "4th deputy")

by_rank <- officials %>%
  filter(year == 2014, rank_deputy %in% 1:4,
         abs(population - CUTOFF) <= bw_deputies) %>%
  mutate(group = if_else(population >= CUTOFF, "treated", "control")) %>%
  group_by(rank_deputy, group) %>%
  summarise(share_women = 100 * mean(female), n = n(), .groups = "drop") %>%
  pivot_wider(names_from = group, values_from = c(share_women, n)) %>%
  mutate(rank = factor(rank_labels[rank_deputy], levels = rev(rank_labels)),
         gap  = share_women_treated - share_women_control)

col_control <- "#3B75AF"
col_treated <- "#E4572E"

figure1 <- ggplot(by_rank, aes(y = rank)) +
  geom_vline(xintercept = 50, linetype = "dashed", colour = "grey45") +
  geom_segment(aes(x = share_women_control, xend = share_women_treated, yend = rank),
               colour = "grey75", linewidth = 1.3) +
  geom_point(aes(x = share_women_control), colour = col_control, size = 4) +
  geom_point(aes(x = share_women_treated), colour = col_treated, size = 4) +
  geom_text(aes(x = (share_women_control + share_women_treated) / 2,
                label = sprintf("+%.1f pp", gap)),
            vjust = -1.1, size = 3.4, colour = "grey25") +
  annotate("text", x = 50.5, y = 4.45, label = "parity", hjust = 0,
           size = 3.2, colour = "grey45") +
  scale_x_continuous(limits = c(20, 65), labels = function(x) paste0(x, "%")) +
  labs(
    title    = "Gender gap by deputy-mayor rank, 2014",
    subtitle = sprintf("Share of women among deputy mayors: <span style='color:%s'>**control (< 1,000)**</span> vs. <span style='color:%s'>**treated (≥ 1,000)**</span>",
                       col_control, col_treated),
    x = "Share of women", y = NULL,
    caption = sprintf("Source: BRÉF. Municipalities within %d inhabitants of the 1,000 threshold (RD bandwidth).",
                      round(bw_deputies))
  ) +
  theme_minimal(base_size = 13) +
  theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(size = 11),
        plot.title.position = "plot")

# coloured subtitle needs ggtext
if (requireNamespace("ggtext", quietly = TRUE)) {
  figure1 <- figure1 + theme(plot.subtitle = ggtext::element_markdown(size = 11))
} else {
  figure1 <- figure1 + labs(subtitle = "Share of women among deputy mayors: control (< 1,000, blue) vs. treated (≥ 1,000, red)")
}

save_figure(figure1, "figure1_deputy_rank_gap", width = 9, height = 5.5)
print(by_rank)
