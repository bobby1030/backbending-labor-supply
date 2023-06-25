library(tidyverse)
library(labelled)
library(haven)
library(ggpubr)

variables <- c(
    workers_all = "a6_70",
    hours_all = "a7_70",
    overtime_all = "a8_70",
    wage_basic_all = "a10_70",
    wage_overtime_all = "a11_70",
    workers_ft = "a6_3",
    hours_ft = "a7_3",
    overtime_ft = "a8_3",
    wage_basic_ft = "a10_3",
    wage_overtime_ft = "a11_3",
    workers_pt = "a6_4",
    hours_pt = "a7_4",
    overtime_pt = "a8_4",
    wage_basic_pt = "a10_4",
    wage_overtime_pt = "a11_4"
)

salary <- read_dta("./salary108.dta") %>% select(all_of(variables))

salary <- salary %>% pivot_longer(everything(), names_to = c(".value", "emp_type"), names_pattern = "(workers|hours|overtime|wage_basic|wage_overtime)_(all|ft|pt)")

# Calculate wage rate and labor hours per firm
labor_supply <- salary %>%
    mutate(
        hours_per_labor = hours / workers,
        hourly_wage = wage_basic / hours,
        overtime_per_labor = overtime / workers,
        hourly_wage_overtime = wage_overtime / overtime
    ) %>%
    mutate(
        emp_type = ordered(emp_type, levels = c("all", "ft", "pt"), labels = c("所有受僱員工", "全職員工", "部分工時員工"))
    ) %>% 
    filter(
        hourly_wage <= 500,
        hourly_wage_overtime <= 500
    )

# Labor supply in ordinary hours
plot_labor_supply <- ggplot(labor_supply, aes(x = hours_per_labor, y = hourly_wage)) +
    facet_wrap(~ emp_type, nrow = 3) +
    stat_summary_bin(fun = mean, orientation = "y") +
    geom_smooth(method = "gam", se = FALSE, orientation = "y") +
    labs(x = "每人正常工時", y = "時薪")

# Labor supply in overtime hours
plot_labor_supply_overtime <- ggplot(labor_supply, aes(x = overtime_per_labor, y = hourly_wage_overtime)) +
    facet_wrap(~ emp_type, nrow = 3) +
    stat_summary_bin(fun = mean, orientation = "y") +
    geom_smooth(method = "gam", se = FALSE, orientation = "y") +
    labs(x = "每人加班工時", y = "時薪")

# Combine two plots
plot <- ggarrange(plot_labor_supply, plot_labor_supply_overtime, ncol = 2, nrow = 1, label.x = c("正常工作", "加班"))

ggsave("./labor_supply.pdf", plot, width = 210, height = 297, units = "mm", device = cairo_pdf)
ggsave("./labor_supply.png", plot, width = 210, height = 297, units = "mm")
