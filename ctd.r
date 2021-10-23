# CTD Data Analysis ----
library(here)
library(tidyverse)
library(glue)
library(ggh4x)
library(oce)
library(ggrepel)
source("functions/functions.R")

# Meta Data ----
files <- tribble(
    ~"short", ~"deep", ~"file",
    "BAB", 150, "BAB_AB332_5th_Oct_2021",
    "ISA", 65, "ISA_AB332_6th_Oct_2021",
    "ISG", 250, "IsG_AB332_7th_Oct_21",
    "ISK", 250, "ISK_Ab332_6th_oct_2021"
)

# Loading Files -----
data <- list()
for (i in seq_len(nrow(files))) {
    row <- files[i, ]
    data[[i]] <- read_tsv(glue("data/{row$file}.txt"), skip = 3, locale = locale(encoding = "latin1")) %>%
        janitor::clean_names() %>%
        add_column(
            location = row$short,
            deep = row$deep
        )
}
data <- bind_rows(data)

# Data Wranling ----
ctd <- data %>%
    group_by(location) %>%
    mutate(
        direction = ifelse(press > lead(press, 2), "up", "down")
    ) %>%
    ungroup() %>%
    # mutate(
    #    press = round(press, 1)
    # ) %>%
    filter(direction == "down" & press >= 0.8) %>%
    glimpse() %>%
    # group_by(location, press) %>%
    # mutate(
    #    across(where(is.double), mean)
    # ) %>%
    # ungroup() %>%
    mutate(
        depth = oce::swDepth(press, latitude = 78)
    ) %>%
    arrange(press) %>%
    glimpse() %>%
    select(sal:press, location:depth)


# Long Format Plotting all Variables
ctd_long <- ctd %>%
    select(-direction) %>%
    pivot_longer(-c("press", "location", "deep", "depth"))

# for (i in seq_len(nrow(files))) {
#    row <- files[i, ]
#    p <- ctd_long %>%
#        filter(location == row$short) %>%
#        ggplot(aes(y = press, x = value)) +
#        geom_hline(yintercept = 0, linetype = "dashed", color = "#E69F00") +
#        geom_hline(yintercept = 15, linetype = "dashed", color = "#E69F00") +
#        geom_hline(yintercept = row$depth, linetype = "dashed", color = "#E69F00") +
#        geom_path(col = "black") +
#        scale_y_reverse(breaks = scales::breaks_pretty()) +
#        scale_x_continuous(position = "top", breaks = scales::breaks_pretty()) +
#        theme_bw() +
#        facet_wrap(~name, scales = "free_x", ncol = 4) +
#        labs(
#            title = row$short,
#            y = "Pressure (dbar)",
#            x = ""
#        )
#    fSaveImages(p, row$short, h = 8)
# }

# Plot Temp VS Salinity
## Secondary Axis
sec <- ggh4x::help_secondary(
    ctd,
    primary = sal,
    secondary = temp,
    name = "Temperature [°C]",
    method = "range",
    breaks = c(-1, 0, 1, 2, 3, 4, 5)
)
## Wide Format Plotting
p <- ctd %>%
    ggplot(aes(y = depth)) +
    geom_path(aes(x = sal, color = "Salinity"), show.legend = FALSE) +
    geom_path(aes(x = sec$proj(temp), color = "Temperature"), show.legend = FALSE) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "#CC79A7") +
    geom_hline(yintercept = 15, linetype = "dashed", color = "#CC79A7") +
    geom_hline(aes(yintercept = deep), linetype = "dashed", color = "#CC79A7") +
    scale_y_reverse(breaks = scales::breaks_pretty()) +
    scale_x_continuous(
        limits = c(NA, 35),
        breaks = seq(32, 35, 0.5),
        # position = "top",
        sec.axis = sec
    ) +
    scale_color_manual(
        values = c("Temperature" = "#D55E00", "Salinity" = "#009E73"),
    ) +
    theme_bw() +
    theme(
        axis.line.x.bottom = element_line(color = "#009E73"),
        axis.ticks.x.bottom = element_line(color = "#009E73"),
        axis.text.x.bottom = element_text(color = "#009E73"),
        axis.title.x.bottom = element_text(color = "#009E73", size = 14),
        axis.line.x.top = element_line(color = "#D55E00"),
        axis.ticks.x.top = element_line(color = "#D55E00"),
        axis.text.x.top = element_text(color = "#D55E00"),
        axis.title.x.top = element_text(color = "#D55E00", size = 14),
        panel.grid = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 15),
        panel.grid.major.y = element_line(linetype = "dotted", color = "gray90")
    ) +
    labs(
        y = "Depth [m]",
        x = "Practical Salinity"
    )
p_fixed <- p +
    facet_wrap(
        ~location,
        scales = "fixed",
        strip.position = "right"
    )
p_free_y <- p +
    facet_wrap(
        ~location,
        scales = "free_y",
        strip.position = "right"
    )
fSaveImages(p_fixed, "salinity_temp_fixed", h = 6)
fSaveImages(p_free_y, "salinity_temp_free", h = 6)

# Get Sample Depth Mean Values
ctd_sample <- ctd %>%
    group_by(location) %>%
    mutate(
        sample_group = case_when(
            between(depth, 0, 1.5) ~ "0",
            between(depth, 14, 16) ~ "15",
            between(depth, first(deep) - 1, first(deep) + 1) ~ "deep",
            TRUE ~ "NA"
        )
    ) %>%
    filter(sample_group != "NA") %>%
    group_by(location, sample_group) %>%
    summarise(
        across(where(is.double), function(x) {
            mean(x) %>% round(2)
        })
    ) %>%
    glimpse() # %>%
# mutate(
#    sample_group = forcats::fct_relevel(sample_group, "deep", "15", "0")
# )

p <- ctd_sample %>%
    select(sample_group, location, sal, temp, opmg_l, f_mg_l) %>%
    pivot_longer(-c("sample_group", "location")) %>%
    group_by(name) %>%
    mutate(
        error = sqrt(var(value) / length(value)),
        label = case_when(
            name == "sal" ~ "Practical Salinity",
            name == "temp" ~ "Temperature [°C]",
            name == "f_mg_l" ~ "Chlorophyll [g/ml]",
            name == "opmg_l" ~ "Oxygen Partial Pressure [g/ml]"
        )
    ) %>%
    # filter(location == "ISA") %>%
    ggplot(
        aes(y = value, x = sample_group, fill = location)
    ) +
    geom_col(position = "dodge") +
    geom_errorbar(
        aes(ymin = value - error, ymax = value + error),
        width = 0.2,
        position = position_dodge(width = 0.9)
    ) +
    theme_bw() +
    scale_fill_manual(
        values = c(
            "#E69F00", "#56B4E9", "#009E73", "#CC79A7"
        ),
        guide = guide_legend("Location")
    ) +
    scale_y_continuous(breaks = scales::pretty_breaks()) +
    labs(
        y = "",
        x = "Sample Depth [m]"
    ) +
    theme(
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted", color = "gray90")
    ) +
    facet_wrap(~label, scale = "free_y")

fSaveImages(p, "sample_params", h = 6)

# Water masses
# Temp Y
# Salinity X

watermasses <- tribble(
    ~ext, ~long, ~short, ~temp_low, ~temp_high, ~salinity_low, ~salinity_high,
    "External", "Atlantic Water", "AW", 3, 5, 34.9, 35,
    "External", "Arctic Water", "ArW", -2, 0, 34.3, 34.8,
    "Local", "Surface Water", "SW", 1, 5, 30, 34,
    "Local", "Local Water", "LW", -2, 1, 32, 35,
    "Local", "Winter-Cooled Water", "WCW", -2, -0.50, 34.4, 35,
    "Mixed", "Intermediate Water", "IW", 1.0, 5, 34, 34.7,
    "Mixed", "Transformed Atlantic Water", "TAW", 1, 3, 34.7, 34.9
) %>%
    mutate(
        long = forcats::fct_relevel(long, "Arctic Water", "Atlantic Water", "Surface Water", "Local Water", "Winter-Cooled Water", "Intermediate Water", "Transformed Atlantic Water")
    )

#    sample_group = forcats::fct_relevel(sample_group, "deep", "15", "0")


p <- ggplot(ctd_sample, aes(y = temp, x = sal)) +
    geom_rect(
        data = watermasses %>% filter(ext != "External"),
        mapping = aes(ymin = temp_low, ymax = temp_high, xmin = salinity_low, xmax = salinity_high, fill = long),
        inherit.aes = FALSE,
        alpha = 1
    ) +
    geom_rect(
        data = watermasses %>% filter(ext == "External" | short %in% c("WCW", "TAW")),
        mapping = aes(ymin = temp_low, ymax = temp_high, xmin = salinity_low, xmax = salinity_high, fill = long),
        inherit.aes = FALSE,
        alpha = 1
    ) +
    geom_point(
        aes(shape = location),
        color = "black",
        size = 3
    ) +
    scale_fill_manual(
        values = colorBlindBlack8,
        name = "Water masses"
    ) +
    scale_shape_discrete(name = "Locations") +
    scale_y_continuous(breaks = c(-2:5)) +
    scale_x_continuous(breaks = c(30:35)) +
    ggrepel::geom_label_repel(
        aes(label = paste0(location, " (", sample_group, ")")),
        color = "black"
    ) +
    labs(
        x = "Salinity [psu]",
        y = "Temperature [°C]"
    )

fSaveImages(p, "water_masses", h = 7, w = 10)

# Plot Fluoresence
# f_gml = Chlorophyl based on fluoresence g/ml
p <- ctd_long %>%
    filter(name %in% c("f_mg_l")) %>%
    ggplot(aes(y = depth, x = value)) +
    geom_path() +
    facet_nested(~location, scales = "fixed") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "#CC79A7") +
    geom_hline(yintercept = 15, linetype = "dashed", color = "#CC79A7") +
    geom_hline(aes(yintercept = deep), linetype = "dashed", color = "#CC79A7") +
    scale_x_continuous(
        breaks = scales::breaks_pretty(),
        sec.axis = ~.
    ) +
    scale_y_reverse(
        breaks = scales::breaks_pretty()
    ) +
    labs(
        y = "Depth [m]",
        x = "Chlorophyll [g/ml]"
    ) +
    theme_bw() +
    theme(
        strip.placement = "outside",
        axis.title.x.bottom = element_text(size = 14),
        panel.grid = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 15),
        panel.grid.major.y = element_line(linetype = "dotted", color = "gray90"),
        panel.grid.major.x = element_line(linetype = "dotted", color = "gray90")
    )
fSaveImages(p, "chlorophyll", h = 4)

# Plot Oxygen
# f_gml = Chlorophyl based on fluoresence g/ml
p <- ctd_long %>%
    filter(name %in% c("opmg_l")) %>%
    ggplot(aes(y = depth, x = value)) +
    geom_path() +
    facet_nested(~location, scales = "fixed") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "#CC79A7") +
    geom_hline(yintercept = 15, linetype = "dashed", color = "#CC79A7") +
    geom_hline(aes(yintercept = deep), linetype = "dashed", color = "#CC79A7") +
    scale_x_continuous(
        breaks = scales::breaks_pretty(),
        sec.axis = ~.
    ) +
    scale_y_reverse(
        breaks = scales::breaks_pretty()
    ) +
    labs(
        y = "Depth [m]",
        x = "Oxygen Partial Pressure [g/ml]"
    ) +
    theme_bw() +
    theme(
        strip.placement = "outside",
        axis.title.x.bottom = element_text(size = 14),
        panel.grid = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 15),
        panel.grid.major.y = element_line(linetype = "dotted", color = "gray90"),
        panel.grid.major.x = element_line(linetype = "dotted", color = "gray90")
    )
fSaveImages(p, "oxygen", h = 4)

# If we want to play with oce package
# ctdRaw <- with(ctd[ctd$location == "ISA", ], as.ctd(
#    salinity = sal,
#    pressure = press,
#    temperature = temp,
#    # time = time
#    # deploymentType = "profile",
#    # sampleInterval = 2
# ))
#
# summary(ctdRaw)
# plotScan(ctdRaw)
#
# plotScan(ctdTrim(ctdRaw, "range",
#    parameters = list(item = "scan", from = 600, to = 800)
# ))
# ctdTrimmed <- ctdDecimate(ctdTrim(ctdRaw))
# plotScan(ctdTrimmed)
# plot(ctdTrimmed)
# plot(ctdRaw)