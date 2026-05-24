# ==========================================================
# BAJAJ FINANCE STOCK ANALYSIS PROJECT
# USING YOUR EXCEL FILE
# ==========================================================

# ==========================================================
# INSTALL REQUIRED PACKAGES (Run only once)
# ==========================================================

install.packages(c(
  "readxl",
  "ggplot2",
  "dplyr",
  "zoo",
  "lubridate",
  "tidyr",
  "patchwork",
  "forecast",
  "viridis",
  "TTR"
))

# ==========================================================
# LOAD LIBRARIES
# ==========================================================

library(readxl)
library(ggplot2)
library(dplyr)
library(zoo)
library(lubridate)
library(tidyr)
library(patchwork)
library(forecast)
library(viridis)
library(TTR)

# ==========================================================
# PART 01:
# FINANCIAL DATA ACQUISITION & HANDLING
# ==========================================================

# Read Bajaj Finance Excel File
stock_data <- read_excel("C:/Users/Anjali/OneDrive/Documents/Anjali/Bajaj Finance Data 2021-2025.xlsx")

# View Data
View(stock_data)

# Check Column Names
colnames(stock_data)

# ==========================================================
# CLEANING DATA
# ==========================================================

# Remove missing values
stock_data <- na.omit(stock_data)

# Rename columns if needed
colnames(stock_data) <- c(
  "Date",
  "Open",
  "High",
  "Low",
  "Close",
  "Volume"
)

# Convert Date column
stock_data$Date <- as.Date(stock_data$Date)

# Sort by Date
stock_data <- stock_data %>%
  arrange(Date)

# Check missing values
sum(is.na(stock_data))

# Structure of dataset
str(stock_data)

# Summary Statistics
summary(stock_data)

# ==========================================================
# DATA VISUALIZATION
# LINE CHART - CLOSING PRICE
# ==========================================================

p1 <- ggplot(stock_data,
             aes(x = Date,
                 y = Close)) +
  
  geom_line(color = "blue",
            linewidth = 1) +
  
  labs(
    title = "Bajaj Finance Closing Price",
    x = "Date",
    y = "Closing Price"
  ) +
  
  theme_minimal()

p1

# ==========================================================
# AREA CHART
# YEAR-WISE AVERAGE CLOSING PRICE
# ==========================================================

stock_data$Year <- factor(year(stock_data$Date))

year_avg_close <- stock_data %>%
  group_by(Year) %>%
  summarise(
    AvgClose = mean(Close, na.rm = TRUE)
  )

p2 <- ggplot(year_avg_close,
             aes(x = Year,
                 y = AvgClose,
                 group = 1)) +
  
  geom_area(fill = "steelblue",
            alpha = 0.7) +
  
  geom_line(color = "darkblue",
            linewidth = 1) +
  
  geom_point(color = "darkblue",
             size = 2) +
  
  labs(
    title = "Year-wise Average Closing Price",
    x = "Year",
    y = "Average Closing Price"
  ) +
  
  theme_minimal()

p2

# ==========================================================
# BAR CHART
# DIVIDEND ANALYSIS
# ==========================================================

stock_data$Dividend <- stock_data$Close * 0.005

year_total_dividend <- stock_data %>%
  group_by(Year) %>%
  summarise(
    TotalDividend = sum(Dividend, na.rm = TRUE)
  )

p3 <- ggplot(year_total_dividend,
             aes(x = Year,
                 y = TotalDividend,
                 fill = Year)) +
  
  geom_bar(stat = "identity",
           color = "white") +
  
  labs(
    title = "Year-wise Total Dividend Amount",
    x = "Year",
    y = "Dividend Amount"
  ) +
  
  theme_minimal() +
  
  theme(
    legend.position = "none"
  ) +
  
  scale_fill_viridis_d()

p3

# ==========================================================
# DOUBLE LINE CHART
# OPENING VS CLOSING PRICE
# ==========================================================

year_avg_prices <- stock_data %>%
  group_by(Year) %>%
  summarise(
    AvgOpen = mean(Open, na.rm = TRUE),
    AvgClose = mean(Close, na.rm = TRUE)
  )

long_year_avg_prices <- year_avg_prices %>%
  pivot_longer(
    cols = c(AvgOpen, AvgClose),
    names_to = "PriceType",
    values_to = "AveragePrice"
  )

p4 <- ggplot(long_year_avg_prices,
             aes(x = Year,
                 y = AveragePrice,
                 color = PriceType,
                 group = PriceType)) +
  
  geom_line(linewidth = 1) +
  
  geom_point(size = 2) +
  
  labs(
    title = "Opening vs Closing Price",
    x = "Year",
    y = "Average Price"
  ) +
  
  theme_minimal()

p4

# ==========================================================
# PART 02:
# ALGORITHMIC TRADING
# MOVING AVERAGE STRATEGY
# ==========================================================

stock_data <- stock_data %>%
  mutate(
    
    SMA20 = rollmean(
      Close,
      k = 20,
      fill = NA,
      align = "right"
    ),
    
    SMA50 = rollmean(
      Close,
      k = 50,
      fill = NA,
      align = "right"
    )
  )

# ==========================================================
# BUY / SELL SIGNALS
# ==========================================================

stock_data <- stock_data %>%
  mutate(
    
    Signal = case_when(
      
      lag(SMA20) < lag(SMA50) &
        SMA20 >= SMA50 ~ "Buy",
      
      lag(SMA20) > lag(SMA50) &
        SMA20 <= SMA50 ~ "Sell",
      
      TRUE ~ NA_character_
    )
  )

# ==========================================================
# RECENT 2 YEARS DATA
# ==========================================================

recent_ma_data <- stock_data %>%
  filter(Date >= (max(Date) - years(2)))

# ==========================================================
# MOVING AVERAGE TRADING SIGNALS PLOT
# ==========================================================

p5 <- ggplot(recent_ma_data,
             aes(x = Date)) +
  
  geom_line(
    aes(y = Close,
        color = "Close Price"),
    linewidth = 0.8
  ) +
  
  geom_line(
    aes(y = SMA20,
        color = "SMA20"),
    linewidth = 1,
    linetype = "dashed"
  ) +
  
  geom_line(
    aes(y = SMA50,
        color = "SMA50"),
    linewidth = 1,
    linetype = "dotdash"
  ) +
  
  geom_point(
    data = filter(recent_ma_data,
                  Signal == "Buy"),
    
    aes(y = Close),
    color = "green",
    size = 4
  ) +
  
  geom_point(
    data = filter(recent_ma_data,
                  Signal == "Sell"),
    
    aes(y = Close),
    color = "red",
    size = 4
  ) +
  
  labs(
    title = "Moving Average Trading Signals",
    x = "Date",
    y = "Price"
  ) +
  
  theme_minimal()

p5

# ==========================================================
# FINANCIAL RANGE VISUALIZATION
# HIGH - LOW - OPEN - CLOSE
# ==========================================================

recent_data <- stock_data %>%
  filter(year(Date) ==
           max(year(Date)))

p6 <- ggplot(recent_data,
             aes(x = Date)) +
  
  geom_linerange(
    aes(
      ymin = Low,
      ymax = High
    ),
    color = "grey70"
  ) +
  
  geom_point(
    aes(y = Open),
    color = "forestgreen",
    size = 2
  ) +
  
  geom_point(
    aes(y = Close),
    color = "firebrick",
    size = 2
  ) +
  
  labs(
    title = "Daily High-Low Open-Close Prices",
    x = "Date",
    y = "Price"
  ) +
  
  theme_minimal()

p6

# ==========================================================
# SHOW ALL CHARTS TOGETHER
# ==========================================================

(p1 | p2) /
  (p3 | p4) /
  (p5 | p6)

# ==========================================================
# BASIC TIME SERIES ANALYSIS
# ==========================================================

ts_close <- ts(
  stock_data$Close,
  frequency = 252
)

# ==========================================================
# ARIMA MODEL
# ==========================================================

arima_model <- auto.arima(ts_close)

# Model Summary
summary(arima_model)

# ==========================================================
# FORECAST NEXT 30 DAYS
# ==========================================================

forecast_data <- forecast(
  arima_model,
  h = 30
)

# Forecast Plot
autoplot(forecast_data) +
  
  labs(
    title = "ARIMA Forecast for Bajaj Finance",
    x = "Time",
    y = "Predicted Price"
  ) +
  
  theme_minimal()

# ==========================================================
# SEASONAL DECOMPOSITION
# ==========================================================

decomp <- decompose(ts_close)

autoplot(decomp) +
  theme_minimal()

# ==========================================================
# DAILY RETURNS ANALYSIS
# ==========================================================

stock_data <- stock_data %>%
  mutate(
    Daily_Return =
      (Close - lag(Close)) / lag(Close)
  )

# Remove NA rows
daily_return_data <- na.omit(stock_data)

p7 <- ggplot(daily_return_data,
             aes(x = Date,
                 y = Daily_Return)) +
  
  geom_line(color = "purple") +
  
  labs(
    title = "Daily Returns of Bajaj Finance",
    x = "Date",
    y = "Daily Return"
  ) +
  
  theme_minimal()

p7

# ==========================================================
# VOLATILITY ANALYSIS
# ==========================================================

stock_data <- stock_data %>%
  mutate(
    Volatility =
      rollapply(
        Daily_Return,
        width = 20,
        FUN = sd,
        fill = NA,
        align = "right"
      )
  )
# Remove NA rows
volatility_data <- na.omit(stock_data)

p8 <- ggplot(stock_data,
             aes(x = Date,
                 y = Volatility)) +
  
  geom_line(color = "darkorange") +
  
  labs(
    title = "20-Day Rolling Volatility",
    x = "Date",
    y = "Volatility"
  ) +
  
  theme_minimal()

p8

# ==========================================================
# FINAL MESSAGE
# ==========================================================

print("Bajaj Finance Project Completed Successfully")