
library(readr)
library(dplyr)
library(stringr)
library(lubridate)
library(zoo)
library(prismatic)
library(ggplot2)
library(ggtext)
library(ggforce)
library(ggiraph)
library(gdtools)
library(plotly)

colors <- c("#008075", "#660068")

bikes <- readr::read_csv(
  "https://cedricscherer.com/data/london-bikes.csv",
  col_types = "Dcfffilllddddc"
) |> 
  arrange(day_night, date) |> 
  mutate(
    color_text = if_else(day_night == "day", colors[1], colors[2]),
    before = lag(n, n = 1),
    tooltip = paste0(
      "<b style='font-size:13pt;font-weight:700;color:", 
      prismatic::clr_darken(color_text, .25), ";'>", 
      stringr::str_to_upper(day_night), " PERIOD</b><br>",
      "<b style='font-size:18pt;font-weight:700;font-family:asap;'>", 
      lubridate::stamp("Sun, Jan 1, 2000")(date), 
      "</b><br><br>",
      "Bikes rented: <b>&ensp;", format(n, big.mark = ","), "</b><br>",
      "Temperature: <b>&ensp;", sprintf("%2.1f", temp), "°C</b>"
    ),
    tooltip = paste0("<span style='font-family:asap condensed;'>", tooltip, "</span>")
  )

bikes_smoothed <- 
  bikes |> 
  group_by(day_night) |> 
  mutate(roll_n = zoo::rollmean(n, 30, align = 'center', fill = NA)) |> 
  ungroup()

gdtools::register_gfont("Asap")
gdtools::register_gfont("Asap Condensed")
gdtools::addGFontHtmlDependency(family = "Asap", "Asap Condensed")

theme_set(theme_minimal(base_size = 12, base_family = "Asap"))
theme_update(
  panel.grid = element_blank(),
  axis.text = element_text(color = "grey40"),
  axis.text.x = element_text(hjust = -.1, margin = margin(-10, 0, 0, 0)),
  axis.line = element_line(color = "grey80", linewidth = .4),
  axis.line.y = element_line(arrow = arrow(length = unit(2, "mm"))),
  axis.ticks = element_line(color = "grey80", linewidth = .4),
  axis.ticks.length.x = unit(1, "lines"),
  plot.title = ggtext::element_markdown(margin = margin(2, 0, 15, 0)),
  plot.title.position = "plot",
  plot.caption = ggtext::element_markdown(hjust = 0, color = "grey40", 
                                          margin = margin(24, 0, 0, 0), lineheight = 1.2),
  plot.caption.position = "plot",
  plot.background = element_rect(fill = "white", color = "white")
)

p_base <- 
  ggplot(bikes, aes(x = date, y = n)) +
  coord_cartesian(clip = "off") +
  #coord_fixed(clip = "off", ratio = 1/150) +
  scale_x_date(
    date_breaks = "3 months", date_labels = "%b '%y", expand = expansion(add = c(4, 14)),
    limits = lubridate::as_date(c("2015-01-01", "2016-12-31"))
  ) +
  scale_y_continuous(
    breaks = 0:5*10000, labels = c(0, paste0(1:5*10, "K")), 
    limits = c(0, NA), expand = expansion(add = c(0, 2000))
  ) +
  scale_color_manual(values = colors, guide = "none") +
  labs(
    x = NULL, y = NULL,
    title = "Registered TfL bike shares by *<b style='color:#008075;'>day</b>* and *<b style='color:#660068;'>night</b>*",
    caption = "Source: Transport for London, 2015-2016 (with modifications)  
               **Powered by TfL Open Data**"
  ) 

p_annotations <- list(
  annotate(
    geom = "text",
    family = "Asap Condensed",
    label = "Rolling average\n(30-day window)",
    x = lubridate::as_date("2016-11-05"),
    y = 34000,
    size = 3,
    color = "grey30",
    lineheight = .95,
    hjust = 0
  ),
  annotate(
    geom = "curve",
    x = lubridate::as_date("2016-12-08"),
    y = 31700,
    xend = c(lubridate::as_date("2016-10-28"),
             lubridate::as_date("2016-11-06")),
    yend = c(20500, 7400),
    linewidth = .5,
    arrow = arrow(length = unit(2, "mm"), type = "closed"),
    curvature = -.2,
    color = "grey30"
  ),
  ggforce::geom_mark_hull(
    aes(label = "Tube Network Strikes 2015", filter = n > 40000,
        color = stage(day_night, after_scale = prismatic::clr_lighten(color, .4))),
    description = "Commuters had to deal with severe disruptions in public transport on July 9 and August 6.",
    label.family = c("Asap", "Asap Condensed"), label.fontsize = c(12.5, 9.2),
    expand = unit(3, "mm"), linewidth = 1.2, con.cap = unit(0, "mm"), 
    label.fill = "transparent", con.colour = "#60B6AB"
  ),
  ggforce::geom_mark_hull(
    aes(label = "", filter = n > 18000 & day_night == "night",
        color = stage(day_night, after_scale = prismatic::clr_lighten(color, .4))),
    description = "The disruption also led to a considerable increase in bike rentals during the late hours.",
    label.family = "Asap Condensed", label.fontsize = 9.2,
    expand = unit(3, "mm"), linewidth = 1.2, con.cap = unit(0, "mm"), 
    label.fill = "transparent", con.colour = "#AA6AAC", label.minwidth = unit(58, "mm")
  )
)

p_ggplot <- 
  p_base + 
#  p_annotations +
  geom_point(
    aes(color = stage(day_night, after_scale = prismatic::clr_lighten(color, .2)), 
        fill = after_scale(clr_lighten(color, .6))), 
    size = 1.8, alpha = .7, shape = 21
  ) +
  geom_line(
    data = bikes_smoothed,
    aes(y = roll_n, color = stage(day_night, after_scale = prismatic::clr_darken(color, .15))), 
    linewidth = 1
  )

p_plotly <- plotly::ggplotly(p_ggplot, height = 550, width = 900)

p_ggiraph <- 
  p_base +
  p_annotations +
  geom_point_interactive(
    aes(color = stage(day_night, after_scale = prismatic::clr_lighten(color, .2)), 
        fill = after_scale(prismatic::clr_lighten(color, .6)),
        tooltip = date, data_id = date), 
    size = 1.8, alpha = .7, shape = 21
  ) +
  geom_line(
    data = bikes_smoothed,
    aes(y = roll_n, color = stage(day_night, after_scale = prismatic::clr_darken(color, .15))), 
    linewidth = 1
  )

# girafe(ggobj = p_interactive, width_svg = 9, height_svg = 5.5)

p_ggiraph_css <- 
  p_base +
  p_annotations +
  geom_point_interactive(
    aes(color = stage(day_night, after_scale = prismatic::clr_lighten(color, .2)), 
        fill = after_scale(prismatic::clr_lighten(color, .6)),
        tooltip = tooltip, data_id = date), 
    size = 1.8, alpha = .7, shape = 21
  ) +
  geom_line(
    data = bikes_smoothed,
    aes(y = roll_n, color = stage(day_night, after_scale = prismatic::clr_darken(color, .15))), 
    linewidth = 1
  )

girafe(
  ggobj = p_ggiraph_css, width_svg = 9, height_svg = 5.5,
  options = list(
    opts_tooltip(
      use_fill = TRUE, offx = 18, offy = -35,
      css = "font-size:16pt;font-weight:500;padding:12px;font-family:asap condensed;"
    ),
    opts_hover(
      css = "opacity:1;stroke-width:3px;r:5px;transition:all 0.2s ease;"
    ),
    opts_hover_inv(css = "opacity:0.3;")
    )
  )




# Version from dashboard
library(tidyverse)
library(ggiraph)
library(gdtools)

model_data_06_overlap <- read_csv("data/modeled_data_latam.csv")

gdtools::register_gfont("Roboto Mono")
gdtools::register_gfont("Inter")
gdtools::addGFontHtmlDependency(family = c("Roboto Mono", "Inter"))

format_number <- function(x) {
  if (x >= 1e6) {
    paste0(round(x / 1e6, 1), "M clients")
  } else if (x >= 1e3) {
    paste0(round(x / 1e3, 1), "K clients")
  } else {
    paste0(x, " clients")
  }
}

#### Prepare data ####
results <- model_data_06_overlap %>%
  dplyr::select(
    sim, fy, region, country, edu_f, edu_m, edu_total, health_f = adjusted_health_f,
    health_m = adjusted_health_m, health_total = adjusted_health_total, power_f, power_m, power_total,
    safety_f, safety_m, safety_total, wb_f, wb_m, wb_total, clients_f = total_clients_f,
    clients_m = total_clients_m, clients_total = total_clients
  ) %>%
  pivot_longer(
    cols = edu_f:clients_total,
    names_to = "type",
    values_to = "clients"
  ) %>%
  mutate(fy = factor(fy))

results_median <- results %>%
  bind_rows(
    results %>%
      group_by(sim, fy, region, type) %>%
      summarise(clients = sum(clients, na.rm = TRUE), .groups = "drop") %>%
      mutate(country = "Latin America")  # new label to distinguish these rows
  ) %>%
  group_by(fy, region, country, type) %>%
  summarise(clients = median(clients), .groups = "drop") %>%
  separate(
    type,
    into = c("sector", "gender"),
    sep = "_",
    remove = FALSE
  ) %>%
  mutate(
    gender = case_when(
      gender == "f" ~ "Female",
      gender == "m" ~ "Male",
      gender == "total" ~ "Total",
      TRUE ~ gender
    ),
    clients_round = round(clients, digits = 0),
    label = sapply(clients_round, format_number),
    label_num = str_remove(label, " clients")
  ) %>%
  dplyr::select(-type)

gg_chart_overview <- ggplot(
  data = results_median %>%
    filter(sector == "clients", gender == "Total", country == "Latin America") %>%
    mutate(
      fill_col = factor(case_when(
        fy == "FY24" ~ "#0052CC",
        TRUE ~ "#00B8D9")
      )
    ),
  mapping = aes(
    x = factor(fy), y = clients, fill = fill_col,
    tooltip = paste0(fy, "<br>", label), data_id = label
    )
  ) +
  geom_col_interactive(alpha = .7, hover_nearest = FALSE) +
  scale_y_continuous(
    labels = scales::label_number(scale_cut = scales::cut_short_scale(), accuracy = 1),
    expand = expansion(mult = c(.01, .05))
  ) +
  labs(
    x = NULL,
    y = NULL
  ) +
  scale_fill_identity() +
  theme_minimal() +
  theme(
    legend.position = "none", 
    axis.title.y = element_text(family = "Inter", size = 18, margin = margin(t = 15)),
    axis.title.x = element_text(family = "Inter", size = 18, margin = margin(t = 15)),
    axis.text.y = element_text(family = "Inter", size = 12),
    axis.text.x = element_text(family = "Roboto Mono", size = 12)
  )

set_girafe_defaults(
  opts_hover = opts_hover(css = css_default_hover),
  opts_zoom = opts_zoom(min = 1, max = 4),
  opts_tooltip = opts_tooltip(css = "padding:3px;background-color:#333333;color:white;"),
  opts_sizing = opts_sizing(rescale = FALSE),
  opts_toolbar = opts_toolbar(saveaspng = FALSE, position = "bottom", delay_mouseout = 5000)
)

girafe(
  ggobj = gg_chart_overview
)


results_labelled <- results %>%
  group_by(sim, fy, region, type) %>%
  summarise(clients = sum(clients, na.rm = TRUE), .groups = "drop") %>%
  separate(
    type,
    into = c("sector", "gender"),
    sep = "_",
    remove = FALSE
  ) %>%
  mutate(
    gender = case_when(
      gender == "f" ~ "Female",
      gender == "m" ~ "Male",
      gender == "total" ~ "Total",
      TRUE ~ gender
    ),
    clients_round = round(clients, digits = 0),
    label = sapply(clients_round, format_number),
    label_num = str_remove(label, " clients")
  ) %>%
  dplyr::select(-type) %>%
  filter(sector == "clients", gender == "Total")

dotplot_data <- results_labelled %>%
  filter(fy == "FY24", region == "Latin America", sector == "clients", gender == "Total")

median_estimate <- median(dotplot_data$clients)
max_value <- max(dotplot_data$clients)
binwidth_val <- (max(dotplot_data$clients) - min(dotplot_data$clients)) / 30
#  range_est <- range(plot_data$estimate)
#  dynamic_binwidth <- diff(range_est) / 33

dotplot_data <- dotplot_data %>%
  mutate(
    q05 = quantile(clients, 0.05),
    q95 = quantile(clients, 0.95),
    median = median(clients),
    colour_cat = case_when(
      clients == median ~ "median",
      clients < q05 | clients > q95 ~ "outlier",
      TRUE ~ "middle"
    ),
    colour_cat = factor(colour_cat, levels = c("outlier", "middle", "median")) # draw order
  )

chart_dotplot <- dotplot_data %>%
  ggplot(aes(x = clients, fill = colour_cat, colour = colour_cat, tooltip = label_num)) +
  geom_dotplot_interactive(
    alpha = 1,
    method = "histodot",
    dotsize = .8,
    binwidth = binwidth_val,
    stackdir = "centerwhole"
  ) +
  scale_colour_manual(values = c(
    outlier = "grey70",
    middle = "#00B8D9",
    median = "#6554C0"
  )) +
  scale_fill_manual(values = c(
    outlier = "grey70",
    middle = "#00B8D9",
    median = "#6554C0"
  )) +
  labs(x = NULL) +
  scale_x_continuous(
    limits = c(340000, 410000),
    breaks = c(340000, 350000, 360000, 370000, 380000, 390000, 400000, 410000),
    labels = scales::label_number(scale_cut = scales::cut_short_scale(), accuracy = 1),
    expand = expansion(mult = c(.02, .03))
  ) +
  scale_y_continuous(
    limits = c(-.3, .3)
  ) +
  theme_minimal() +
  theme(
    legend.position = "none", 
    axis.title.y = element_blank(),
    axis.title.x = element_text(family = "Inter", size = 14, margin = margin(t = 15)),
    axis.text.y = element_blank(),
    axis.text.x = element_text(family = "Roboto Mono", size = 8)
  ) +
  annotate(
    "curve", 
    x = median_estimate + 100, 
    y = 0.02, 
    xend = ((median_estimate + max_value) / 2) - ((median_estimate + max_value) / 2) * .002,
    yend = 0.24, 
    color = "grey40", 
    curvature = -.3, 
    arrow = arrow(type = "closed", length = unit(0.1, "inches")),
    lineend = "round", 
    linewidth = .8
  ) +
  annotate(
    "text", 
    x = (median_estimate + max_value) / 2, 
    y = 0.23,
    label = format_number(median_estimate),
    family = "Inter", 
    size = 3,
    color = "grey40",
    hjust = 0, 
    vjust = 0
  )



