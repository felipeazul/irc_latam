
library(tidyverse)
library(showtext)
library(plotly)

font_add_google(name = "Roboto Mono", family = "Roboto Mono")
font_add_google(name = "Inter", family = "Inter")
showtext_auto()

model_data_06_overlap <- read_csv("data/modeled_data_latam.csv")
latam_sector_refs <- read_csv("data/latam_sector_refs.csv")

format_number <- function(x) {
  if (x >= 1e6) {
    paste0(round(x / 1e6, 1), "M clients")
  } else if (x >= 1e3) {
    paste0(round(x / 1e3, 1), "K clients")
  } else {
    paste0(x, " clients")
  }
}

results <- model_data_06_overlap %>%
  dplyr::select(
    sim, fy, region, country, EDU.007_f:SRH.028_f, health_f = adjusted_health_f,
    health_m = adjusted_health_m, health_total = adjusted_health_total, clients_f = total_clients_f,
    clients_m = total_clients_m, clients_total = total_clients
  ) %>%
  pivot_longer(
    cols = EDU.007_f:clients_total,
    names_to = "type",
    values_to = "clients"
  ) %>%
  filter(clients > 0) %>%
  mutate(
    fy = factor(fy),
    type2 = case_when(
      str_detect(type, "clients_") ~ type,
      str_detect(type, "health_") ~ type,
      str_detect(type, "_f") ~ str_remove(type, "_f"),
      str_detect(type, "_m") ~ str_remove(type, "_m"),
      TRUE ~ type
    )
  ) %>%
  left_join(
    latam_sector_refs %>% dplyr::select(-sap_sector, -n, -value),
    by = c("type2" = "data_element_code")
  ) %>%
  mutate(
    outcome = case_when(
      str_detect(type, "clients_") ~ "total",
      str_detect(type, "health_") ~ "health_total",
      TRUE ~ outcome
    )
  ) %>%
  separate(
    type,
    into = c("indicator", "gender"),
    sep = "_",
    remove = FALSE
  ) %>%
  group_by(sim, fy, region, country, gender, outcome) %>%
  summarise(clients = sum(clients)) %>%
  ungroup()

results_mean <- results %>%
  bind_rows(
    results %>%
      group_by(sim, fy, region, outcome, gender) %>%
      summarise(clients = sum(clients, na.rm = TRUE), .groups = "drop") %>%
      mutate(country = "Latin America")  # new label to distinguish these rows
  ) %>%
  filter(gender != "total") %>%
  group_by(fy, region, country, outcome, gender) %>%
  summarise(clients = mean(clients), .groups = "drop") %>%
  group_by(fy, region, country, outcome) %>%
  mutate(total = sum(clients)) %>%
  pivot_wider(
    names_from = gender,
    values_from = clients,
    values_fill = 0
  ) %>%
  pivot_longer(
    cols = total:m,
    names_to = "gender",
    values_to = "clients"
  ) %>%
  mutate(
    clients_round = round(clients, digits = 0),
    label = sapply(clients_round, format_number),
    label_num = str_remove(label, " clients")
  )


chart_lines_sectors <- results_mean %>%
  filter(outcome != "total", outcome != "health - catchment", gender == "total", country == "Latin America") %>%
  mutate(
    Sector = factor(case_when(
      outcome == "child protection" ~ "Child Protection",
      outcome == "education" ~ "Education",
      outcome == "erd" ~ "Economic Recovery & Development",
      outcome == "governance" ~ "Governance",
      outcome == "protection rule of law" ~ "Protection & Rule of Law",
      outcome == "women's protection & empowerment" ~ "Women's Protection & Empowerment",
      outcome == "health - environmental health" ~ "Environmental Health",
      outcome == "health - nutrition" ~ "Nutrition",
      outcome == "health - primary health care" ~ "Primary Healthcare",
      outcome == "health - sexual & reproductive health" ~ "Sexual & Reproductive Health",
      outcome == "health_total" ~ "Total Health Clients",
      outcome == "total" ~ "Total Clients",
      TRUE ~ outcome
    ),
    levels = c(
      "Child Protection", "Women's Protection & Empowerment", "Protection & Rule of Law", "Governance",
      "Economic Recovery & Development", "Education", "Primary Healthcare", "Sexual & Reproductive Health",
      "Nutrition", "Environmental Health", "Total Health Clients"
    ))
  ) %>%
  ggplot(
    aes(x = fy, y = clients, colour = Sector, group = Sector,
        text = paste0(fy, "<br>", label, "<br>", Sector)
    )) +
  geom_line(
    linewidth = 1.2,
    alpha = .7
  ) +
  guides(fill = "none") +
  labs(
    x = NULL,
    y = NULL,
    colour = "Sector"
  ) +
  scale_y_continuous(
    labels = scales::label_number(scale_cut = scales::cut_short_scale(), accuracy = 1),
    expand = expansion(mult = c(.01, .05))
  ) +
  scale_colour_manual(values = c(
    "Child Protection" = "#0052CC",
    "Women's Protection & Empowerment" = "#6554C0",
    "Protection & Rule of Law" = "#FFAB00",
    "Governance" = "#172B4D",
    "Economic Recovery & Development" = "#FF5630",
    "Education" = "#FF7452",
    "Primary Healthcare" = "#006644",
    "Sexual & Reproductive Health" = "#00875A",
    "Nutrition" = "#36B37E",
    "Environmental Health" = "#ABF5D1",
    "Total Health Clients" = "#00B8D9"
  )) +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.title.y = element_text(family = "Inter", size = 18, margin = margin(t = 15)),
    axis.title.x = element_text(family = "Inter", size = 18, margin = margin(t = 15)),
    legend.title = element_blank(),
    axis.text.y = element_text(family = "Inter", size = 12),
    axis.text.x = element_text(family = "Roboto Mono", size = 12),
    legend.text = element_text(family = "Inter", size = 9)
  )

gg_chart_lines_sectors <- ggplotly(chart_lines_sectors, tooltip = "text")
