remove(list = ls())
graphics.off()

library(ggplot2)
library(openxlsx)
library(scales)
library(tidyr)
library(janitor)

pwt <- read_xlsx("pwt110.xlsx", sheet = "Data")
wb <- read_xlsx("API_SGP_DS2_en_excel_v2_1861.xlsx")

sgp <- pwt[pwt$countrycode == "SGP", ]
sgp <- sgp[order(sgp$year), ]

# GDP per capita
sgp$gdp_pc <- sgp$rgdpe / sgp$pop

# GDP per capita growth (log growth)
sgp$gdp_pc_g <- 100 * (log(sgp$gdp_pc) - log(c(NA, sgp$gdp_pc[-nrow(sgp)])))

# Capital per capita
sgp$k_pc <- sgp$rkna / sgp$pop

# Population growth
sgp$pop_g <- 100 * (log(sgp$pop) - log(c(NA, sgp$pop[-nrow(sgp)])))

library(ggplot2)
theme_set(theme_bw(base_size = 12))

pLine_GDP_per_capita <- ggplot(sgp, aes(year, gdp_pc)) +  
  geom_line(color = "darkred", linewidth = 0.5) +
  labs(title = "Singapore: Real GDP per Capita",
       x = "Year", y = "rgdpe / population") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = breaks_pretty(10)) +
  scale_y_continuous(breaks = breaks_pretty(5))
plot(pLine_GDP_per_capita)


pLine_GDP_per_capita_Growth_rate <- ggplot(sgp, aes(year, gdp_pc_g)) +
  geom_line(color = "darkred", linewidth = 0.5) +
  geom_hline(yintercept = 0) +
  labs(title = "Singapore: GDP per Capita Growth Rate",
       x = "Year", y = "Percent") +   
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = breaks_pretty(10)) +
  scale_y_continuous(breaks = breaks_pretty(5))
plot(pLine_GDP_per_capita_Growth_rate)

pLine_capital_stock_per_capita <- ggplot(sgp, aes(year, k_pc)) +
  geom_line(color = "darkred", linewidth = 0.5) +
  labs(
    title = "Singapore: Capital Stock per Capita (PWT)",
    x = "Year",
    y = "Capital stock per capita (rkna / pop)")
plot(pLine_capital_stock_per_capita )

sgp$pop_growth <- 100 * (log(sgp$pop) - log(c(NA, sgp$pop[-nrow(sgp)])))

pLine_Population_growth_rate <- ggplot(sgp, aes(year, pop_growth)) +
  geom_line(color = "darkred", linewidth = 0.5) +
  geom_hline(yintercept = 0) +
  labs(
    title = "Singapore: Population Growth Rate (PWT)",
    x = "Year",
    y = "Population growth rate (%)"
  )
plot(pLine_Population_growth_rate)

avg_pop_growth <- mean(sgp$pop_growth, na.rm = TRUE)
avg_pop_growth

pLine_Stock_Human_Capital <-ggplot(sgp, aes(year, hc)) +
  geom_line(color = "darkred", linewidth = 0.5) +
  labs(
    title = "Singapore: Stock of Human Capital (PWT)",
    x = "Year",
    y = "Human capital index (hc)"
  )
plot(pLine_Stock_Human_Capital)

# Singapore only
sgp <- pwt[pwt$countrycode == "SGP", ]
sgp <- sgp[order(sgp$year), ]

# Extract TFP
sgp$tfp <- sgp$rtfpna

# Plot TFP over time
library(ggplot2)

ggplot(sgp, aes(year, tfp)) +
  geom_line(color = "maroon", linewidth = 0.5) +
  labs(
    title = "Singapore: Total Factor Productivity (PWT)",
    x = "Year",
    y = "Total Factor Productivity (rtfpna)"
  ) +
  theme(plot.title = element_text(hjust = 0.5))