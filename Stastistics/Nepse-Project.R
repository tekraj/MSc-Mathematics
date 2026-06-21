# ============================================================
# NEPSE Seasonal Trading Strategies - Complete Analysis
# ============================================================

# --- Load Libraries ---
library(dplyr)
library(ggplot2)
library(lubridate)
library(moments)
library(boot)
library(WRS2)
library(lmPerm)
library(tidyr)
library(purrr)

# --- Load and Prepare Data ---
nepse_data <- read.csv("./data/nepse.csv")
nepse_data <- nepse_data |> mutate(Date = as.Date(Date)) |> arrange(Date)
head(nepse_data, 5)

# ============================================================
# Exploratory Data Analysis
# ============================================================

# --- Box Plot of Closing Prices by Sector ---
ggplot(nepse_data, aes(x = Symbol, y = Close, fill = Symbol)) + geom_boxplot() + labs(title = "Box Plot of Close Price by Symbol", x = "Symbol", y = "Close Price") + theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

# --- Histogram of Closing Prices by Sector ---
ggplot(nepse_data, aes(x = Close, fill = Symbol)) + geom_histogram(bins = 30, color = "white") + facet_wrap(~Symbol, scales = "free") + labs(title = "Histogram of Close Price by Symbol", x = "Close Price", y = "Frequency") + theme_minimal()

# --- Q-Q Plot of Closing Prices by Sector ---
ggplot(nepse_data, aes(sample = Close, color = Symbol)) + stat_qq() + stat_qq_line() + facet_wrap(~Symbol, scales = "free") + labs(title = "Q-Q Plot of Close Price by Symbol", x = "Theoretical Quantiles", y = "Sample Quantiles") + theme_minimal()

# ============================================================
# Probability Distribution of Returns
# ============================================================

# --- Calculate Daily Percentage Returns ---
sector_returns <- nepse_data |> group_by(Symbol) |> arrange(Date) |> mutate(Return = (Close - lag(Close)) / lag(Close)) |> filter(!is.na(Return))

# --- Distribution Statistics (Mean, SD, Skewness, Kurtosis) ---
distribution_stats <- sector_returns |> summarise(Mean = mean(Return), SD = sd(Return), Skewness = skewness(Return), Kurtosis = kurtosis(Return))
print(distribution_stats)

# --- Monthly Average Returns with 95% Bootstrap CIs (Bar Plot) ---
monthly_stats <- sector_returns %>% mutate(Month = month(Date, label = TRUE)) %>% group_by(Symbol, Month) %>% summarise(Mean = mean(Return), CI_lower = quantile(Return, 0.025), CI_upper = quantile(Return, 0.975), .groups = "drop")
ggplot(monthly_stats, aes(x = Month, y = Mean, fill = Symbol)) + geom_col(position = position_dodge(0.9), color = "black", alpha = 0.7) + geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), position = position_dodge(0.9), width = 0.3) + geom_hline(yintercept = 0, linetype = "dashed", color = "red") + facet_wrap(~Symbol, scales = "free_y") + labs(title = "Monthly Average Daily Returns with 95% Bootstrap CIs", x = "Month", y = "Mean Return") + theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

# --- Density Plots of Returns with Normal Overlay ---
norm_params <- sector_returns %>% group_by(Symbol) %>% summarise(mu = mean(Return), sigma = sd(Return))
ggplot(sector_returns, aes(x = Return, fill = Symbol)) + geom_density(alpha = 0.4, adjust = 1.5) + geom_vline(data = norm_params, aes(xintercept = mu), linetype = "dashed", color = "black") + geom_function(fun = dnorm, args = list(mean = first(norm_params$mu), sd = first(norm_params$sigma)), color = "red", linetype = "dotted", size = 1) + facet_wrap(~Symbol, scales = "free") + labs(title = "Return Density Plots with Normal Reference (dotted red)", x = "Daily Return", y = "Density") + theme_minimal() + theme(legend.position = "none")

# --- Time Series of Cumulative Returns by Sector ---
cumulative_returns <- sector_returns %>% group_by(Symbol) %>% arrange(Date) %>% mutate(Cumulative = cumsum(Return)) %>% ungroup()
ggplot(cumulative_returns, aes(x = Date, y = Cumulative, color = Symbol)) + geom_line(size = 0.8) + labs(title = "Cumulative Daily Returns by Sector (2021-2026)", x = "Date", y = "Cumulative Return") + theme_minimal() + theme(legend.position = "bottom")

# --- Heatmap of Mean Returns by Sector and Month ---
heatmap_data <- sector_returns %>% mutate(Month = month(Date, label = TRUE)) %>% group_by(Symbol, Month) %>% summarise(Mean_Return = mean(Return), .groups = "drop")
ggplot(heatmap_data, aes(x = Month, y = Symbol, fill = Mean_Return)) + geom_tile(color = "white") + scale_fill_gradient2(low = "red", mid = "white", high = "steelblue", midpoint = 0, name = "Mean Return") + geom_text(aes(label = round(Mean_Return, 3)), color = "black", size = 3) + labs(title = "Heatmap of Average Daily Returns by Sector and Month", x = "Month", y = "Sector") + theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

# --- Normality Testing Using Shapiro-Wilk Test ---
normality_sw <- sector_returns %>% group_by(Symbol) %>% summarise(p_value = shapiro.test(Return)$p.value)
print(normality_sw)

# ============================================================
# Mathematical Modeling
# ============================================================

# --- Simple Linear Regression: Return ~ Date ---
linear_model <- lm(Return ~ as.numeric(Date), data = sector_returns)
summary(linear_model)

# --- Non-linear (Quadratic) Regression: Return ~ Date + Date^2 ---
nonlinear_model <- lm(Return ~ poly(as.numeric(Date), 2), data = sector_returns)

# --- Multiple Linear Regression: Return ~ Lag_Return + Sector ---
model_data <- sector_returns %>% group_by(Symbol) %>% mutate(Lag_Return = lag(Return)) %>% filter(!is.na(Lag_Return))
multi_reg <- lm(Return ~ Lag_Return + Symbol, data = model_data)
summary(multi_reg)

# ============================================================
# Confirmatory Analysis
# ============================================================

# --- Chi-Square Test: Return Direction (Gain/Loss) vs Month ---
chi_data <- sector_returns %>% mutate(Direction = ifelse(Return > 0, "Gain", "Loss"), Month = month(Date, label = TRUE))
ctable <- table(chi_data$Month, chi_data$Direction)
chisq_test <- chisq.test(ctable)
print(chisq_test)

# --- ANOVA: Return by Month ---
anova_month <- aov(Return ~ factor(month(Date)), data = sector_returns)
summary(anova_month)

# --- ANOVA: Return by Sector ---
anova_sector <- aov(Return ~ Symbol, data = sector_returns)
summary(anova_sector)

# --- Kruskal-Wallis Test: Comparing Returns across Sectors ---
kw_test <- kruskal.test(Return ~ Symbol, data = sector_returns)
print(kw_test)

# --- Mann-Whitney U Test: Banking vs Others ---
subset_data <- sector_returns %>% filter(Symbol %in% c("BANKING", "OTHERS"))
mw_test <- wilcox.test(Return ~ Symbol, data = subset_data)
print(mw_test)

# ============================================================
# Seasonal Trading Strategies
# ============================================================

# --- 1. Monthly Ranking: Best & Worst Months ---
monthly_returns <- sector_returns %>% mutate(Month = month(Date, label = TRUE)) %>% group_by(Symbol, Month) %>% summarise(Mean_Return = mean(Return, na.rm = TRUE), Median_Return = median(Return, na.rm = TRUE), CI_Lower = quantile(Return, 0.025, na.rm = TRUE), CI_Upper = quantile(Return, 0.975, na.rm = TRUE), Win_Ratio = sum(Return > 0) / n(), Sharpe = mean(Return) / sd(Return), .groups = "drop") %>% arrange(Symbol, Month)
best_months <- monthly_returns %>% group_by(Symbol) %>% slice_max(Mean_Return, n = 2) %>% ungroup()
cat("\n===== Best months per sector =====\n")
print(best_months)
worst_months <- monthly_returns %>% group_by(Symbol) %>% slice_min(Mean_Return, n = 2) %>% ungroup()
cat("\n===== Worst months per sector =====\n")
print(worst_months)

# --- 2. Z-Score Entry/Exit Framework ---
zscore_analysis <- sector_returns %>% mutate(Month = month(Date, label = TRUE)) %>% group_by(Symbol, Month) %>% mutate(Monthly_Mean = mean(Return, na.rm = TRUE), Monthly_SD = sd(Return, na.rm = TRUE), Z_Score = (Return - Monthly_Mean) / Monthly_SD) %>% ungroup()
signals <- zscore_analysis %>% filter(abs(Z_Score) > 1.5) %>% mutate(Signal = ifelse(Z_Score < -1.5, "ENTRY", "EXIT")) %>% group_by(Symbol, Signal, Month) %>% summarise(Frequency = n(), Avg_Z = mean(Z_Score), .groups = "drop")
cat("\n===== Z-score signals (|Z|>1.5) =====\n")
print(signals)

# --- 3. Cumulative Return by Day of Year ---
cumulative_pattern <- sector_returns %>% mutate(DayOfYear = yday(Date)) %>% group_by(Symbol, DayOfYear) %>% summarise(Mean_Return = mean(Return, na.rm = TRUE), .groups = "drop") %>% group_by(Symbol) %>% mutate(Cumulative_Mean = cumsum(Mean_Return))
optimal_exit <- cumulative_pattern %>% group_by(Symbol) %>% slice_max(Cumulative_Mean, n = 1, with_ties = FALSE) %>% mutate(Exit_Day = DayOfYear, Exit_Date_approx = as.Date(paste0("2024-", Exit_Day), format = "%Y-%j"))
cat("\n===== Optimal exit day-of-year per sector =====\n")
print(optimal_exit %>% select(Symbol, Exit_Day, Cumulative_Mean))

# --- Cumulative Return Plot (Combined) ---
p_cum <- ggplot(cumulative_pattern, aes(x = DayOfYear, y = Cumulative_Mean, color = Symbol)) + geom_line() + labs(title = "Cumulative mean return by day of year", x = "Day of year", y = "Cumulative return") + theme_minimal()
print(p_cum)

# --- Cumulative Return Plot (Faceted by Sector) ---
ggplot(cumulative_pattern, aes(x = DayOfYear, y = Cumulative_Mean, color = Symbol)) + geom_line(linewidth = 0.8) + facet_wrap(~Symbol, scales = "free_y") + labs(title = "Cumulative Mean Returns by Day of Year", subtitle = "Comparing performance trends across NEPSE sectors", x = "Day of Year (1-366)", y = "Cumulative Return") + theme_minimal() + theme(legend.position = "none")

# --- 4. Year-by-Year Consistency (>=80% Positive Years) ---
consistency_test <- sector_returns %>% mutate(Year = year(Date), Month = month(Date, label = TRUE)) %>% group_by(Symbol, Month, Year) %>% summarise(Monthly_Return = sum(Return, na.rm = TRUE), .groups = "drop") %>% group_by(Symbol, Month) %>% summarise(Positive_Years = sum(Monthly_Return > 0), Total_Years = n(), Consistency_Rate = Positive_Years / Total_Years, .groups = "drop") %>% filter(Total_Years >= 3)
reliable_seasons <- consistency_test %>% filter(Consistency_Rate >= 0.8)
cat("\n===== Reliable seasonal months (>=80% positive years) =====\n")
print(reliable_seasons)

# --- 5. Bootstrap Confidence Intervals (All Sector-Month Pairs) ---
sector_months <- sector_returns %>% distinct(Symbol, Month = month(Date)) %>% mutate(Returns_list = map2(Symbol, Month, ~ { sector_returns %>% filter(Symbol == .x, month(Date) == .y) %>% pull(Return) }))
boot_mean <- function(x, indices) { mean(x[indices], na.rm = TRUE) }
bootstrap_results <- sector_months %>% mutate(Boot = map(Returns_list, ~ { if (length(.x) < 2) return(tibble(CI_lower = NA, CI_upper = NA)); set.seed(123); b <- boot(.x, boot_mean, R = 1000); ci <- tryCatch(boot.ci(b, type = "perc")$percent[4:5], error = function(e) c(NA, NA)); tibble(CI_lower = ci[1], CI_upper = ci[2]) })) %>% unnest(Boot) %>% select(-Returns_list)
cat("\n===== Bootstrap percentile CIs for all sector-month pairs =====\n")
print(head(bootstrap_results, 12))

# --- 5b. Bootstrap CI: BANKING in Kartik (Month 8) ---
banking_kartik_returns <- sector_returns %>% filter(Symbol == "BANKING", month(Date) == 8) %>% pull(Return)
if (length(banking_kartik_returns) >= 2) {
  set.seed(123)
  boot_bank_kart <- boot(banking_kartik_returns, boot_mean, R = 5000)
  ci_bank_kart <- boot.ci(boot_bank_kart, type = "perc")
  cat("\n===== Specific CI for BANKING in Kartik (month 8) =====\n")
  print(ci_bank_kart)
}

# --- 6. Robust ANOVA (20% Trimmed Means) - Month Effect ---
robust_anova_month <- t1way(Return ~ factor(month(Date)), data = sector_returns, tr = 0.2)
cat("\n===== Robust ANOVA (20% trimmed means) - Month effect =====\n")
print(robust_anova_month)

# --- Permutation ANOVA - Month Effect ---
set.seed(123)
perm_anova_month <- aovp(Return ~ factor(month(Date)), data = sector_returns, np = 1000)
cat("\n===== Permutation ANOVA - Month effect =====\n")
summary(perm_anova_month)

# --- 7. Two-Way Interaction ANOVA: Symbol x Month ---
sector_returns <- sector_returns %>% mutate(MonthFactor = factor(month(Date)))
interaction_anova <- aov(Return ~ Symbol * MonthFactor, data = sector_returns)
cat("\n===== Two-way ANOVA (Symbol x Month) =====\n")
summary(interaction_anova)

# --- 8. Monthly Means with Confidence Intervals ---
print(head(monthly_returns %>% select(Symbol, Month, Mean_Return, CI_Lower, CI_Upper, Win_Ratio), 12))
