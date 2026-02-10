# ============================================
# PWT (Singapore): GDP per capita growth plot
# ============================================

# Packages
library(readxl)
library(ggplot2)
library(scales)

# 1) Read Penn World Table (make sure sheet = "Data")
pwt <- read_xlsx("pwt110.xlsx", sheet = "Data")

# 2) Filter Singapore and sort by year
sgp <- pwt[pwt$countrycode == "SGP", ]
sgp <- sgp[order(sgp$year), ]

# 3) Real GDP per capita (PWT: rgdpe / pop)
sgp$gdp_pc <- sgp$rgdpe / sgp$pop

# 4) Year-by-year GDP per capita growth rate (log growth, %)
sgp$gdp_pc_g <- c(NA, 100 * diff(log(sgp$gdp_pc)))
theme_set(theme_bw(base_size = 12))
# 5) Plot
p <- ggplot(sgp, aes(x = year, y = gdp_pc_g)) +
  geom_line(color = "darkred", linewidth = 0.5) +
  geom_hline(yintercept = 0) +
  labs(
    title = "Singapore: Real GDP per Capita Growth (PWT)",
    x = "Year",
    y = "Growth rate (%)"
  ) +
  scale_x_continuous(breaks = breaks_pretty(10)) +
  theme_bw(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5))

print(p)
