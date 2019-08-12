library(tidyverse)
library(gridExtra)
library(viridis)
library(lubridate)
library(date)
library(scales)
library(data.table)
library(fastDummies)
library(caret)
library(glmnet)
library(tools)
library(knitr)
library(DT)
#-----------------------------------------------------------------------------------------------
# EDA
# Read in dataframe
df <- read.table('brazil_gas_prices.tsv', sep = '\t', header = TRUE)

# Translate column labels
df <- df %>%
  rename(
    'initial_date'    = DATA.INICIAL,
    'final_date'      = DATA.FINAL,
    'region'          = REGIÃO,
    'state'           = ESTADO,
    'product'         = PRODUTO,
    'n_stations'      = NÚMERO.DE.POSTOS.PESQUISADOS,
    'measurement'     = UNIDADE.DE.MEDIDA,
    'mean_price'      = PREÇO.MÉDIO.REVENDA,
    'month'           = MÊS,
    'year'            = ANO
  )

# Translate region/fuel type
df <- df %>%
  select(initial_date, final_date, region, state, product, n_stations, measurement,
         mean_price, month, year) %>%
  mutate(region = fct_recode(region,
    'Central-West'   = 'CENTRO OESTE',
    'Northeast' = 'NORDESTE',
    'North'     = 'NORTE',
    'Southeast' = 'SUDESTE',
    'South'     = 'SUL')) %>%
  mutate(product = fct_recode(product,
    'Common Gas'  = 'GASOLINA COMUM',
    'Diesel'      = 'ÓLEO DIESEL',
    'Ethanol'     = 'ETANOL HIDRATADO',
    'Diesel S10'  = 'ÓLEO DIESEL S10')) %>%
  mutate(final_date = as.Date(final_date)) %>%
  # Analysis will focus on common gas, diesel, diesel s10, and ethanol
  filter(product != 'GNV', product != 'GLP', final_date <= '2019-06-01')

# Convert states from all caps to title case
df$state <- tolower(df$state)
df$state <- toTitleCase(df$state)

# Check for implicit NAs in fuel prices
checkNAs <- df %>%
  select(final_date, year, month, mean_price, product, region, state) %>%
  group_by(product, region, state) %>%
  spread(product, mean_price) %>%
  rename('Common_Gas' = 'Common Gas', 'Diesel_S10'= 'Diesel S10')

checkNAs <- checkNAs %>%
  arrange(!is.na(Ethanol))
# Quite a few NAs for ethanol prices, mostly in Amapa

checkNAs <- checkNAs %>%
  arrange(!is.na(Common_Gas))
# None for common gas

checkNAs <- checkNAs %>%
  arrange(!is.na(Diesel))
# None for diesel

checkNAs <- checkNAs %>%
  arrange(!is.na(Diesel_S10))
# Diesel S10 was introduced in 2013, only a handful of NAs in early 2013
# Likely due to states introducing the fuel at different times

# Plotting functions
# By fuel type
plot_byFuel <- function(fuel_type, beg_yr, end_yr) {
  df %>%
    select(final_date, year, month, mean_price, product, region, measurement) %>%
    group_by(final_date, product) %>%
    filter(product == fuel_type, between(year, beg_yr, end_yr)) %>%
    mutate(price = mean(mean_price)) %>%
    ggplot(aes(as.Date(final_date), price, color = price)) +
    geom_line() +
    scale_color_viridis(option = 'inferno') +
    ggtitle(fuel_type) + xlab(NULL) + 
    ylab(paste0('Price ', '(',df$measurement[df$product == fuel_type],')')) +
    theme(legend.position = 'none')
}

# By fuel type and region
plot_byReg <- function(fuel_type, beg_yr, end_yr) {
  df %>%
    select(final_date, year, month, mean_price, product,
           region, state, measurement) %>%
    group_by(final_date, region) %>%
    filter(product == fuel_type, between(year, beg_yr, end_yr)) %>%
    mutate(price = mean(mean_price)) %>%
    ggplot(aes(as.Date(final_date), price, color = price)) +
    geom_line() +
    scale_color_viridis(option = 'inferno') +
    ggtitle(fuel_type) + xlab(NULL) + 
    ylab(paste0('Price ', '(',df$measurement[df$product == fuel_type],')')) +
    theme(legend.position = 'none') +
    facet_wrap(~region)
}

# By fuel type, region, and state
# Useful for regions with few states (South, Southeast)
# Difficult to visualize for regions with many states (Northeast)
plot_byState <- function(fuel_type, area, beg_yr, end_yr) {
  df %>%
    select(final_date, year, month, mean_price, product, region, state, measurement) %>%
    group_by(final_date, region, state) %>%
    filter(product == fuel_type, region == area,
           between(year, beg_yr, end_yr)) %>%
    ggplot(aes(as.Date(final_date), mean_price, color = state)) +
    geom_line() +
    ggtitle(fuel_type,
            subtitle = paste0('Region: ', area)) + xlab(NULL) + 
    ylab(paste0('Price ', '(',df$measurement[df$product == fuel_type],')')) +
    labs(fill = 'State')
}

# Percent increase by fuel type, region, and state (clearer visualization by state)
plot_pctInc <- function(date1, date2, fuel_type, area) {
  df %>%
    select(final_date, year, month, mean_price, product, region, state) %>%
    group_by(product, region, state) %>%
    filter(final_date == date1 | final_date == date2,
           product == fuel_type, region == area) %>%
    summarize(beg = mean_price[final_date == date1],
              end = mean_price[final_date == date2],
              pct_inc = (end - beg) / beg) %>%
    ggplot(aes(reorder(state, pct_inc), pct_inc, fill = state)) +
    geom_bar(color = 'black', stat = 'identity') +
    scale_fill_viridis(option = 'inferno', discrete = TRUE) +
    ggtitle(fuel_type, subtitle = paste0('Region: ', area)) +
    scale_x_discrete(name = NULL, labels = NULL) +
    scale_y_continuous(name = '% Increase', labels = percent)
}

# Ethanol supply shortages
plot_byFuel('Ethanol', 2005, 2013)

# By Region
plot_byReg('Ethanol', 2005, 2013)

# 1st supply shortage, 2005-2006
plot_byFuel('Ethanol', 2005, 2006)

# By Region
plot_byReg('Ethanol', 2005, 2006)

# Major supply shortage, 2009-2013
plot_byFuel('Ethanol', 2009, 2013)

# By Region
plot_byReg('Ethanol', 2009, 2013)

# By State
min(as.Date(df$final_date)[df$year == 2009]) # 2009-01-10
max(as.Date(df$final_date)[df$year == 2013]) # 2014-01-04
plot_pctInc('2009-01-10', '2014-01-04', 'Ethanol', 'North') +
  labs(fill = 'State', caption = paste0(' % Increase in Prices: 1/10/09 - 1/4/14'))

ess <- df %>%
  select(final_date, year, month, mean_price, product, region, state) %>%
  group_by(region, state) %>%
  filter(final_date == '2009-01-10' | final_date == '2014-01-04',
         product == 'Ethanol') %>%
  mutate(beg = mean_price[final_date == '2009-01-10'],
            end = mean_price[final_date == '2014-01-04'],
            pct_inc = round(((end - beg) / beg) * 100, 2)) %>%
  select(region, state, beg, end, pct_inc) %>%
  arrange(desc(pct_inc)) %>%
  distinct() %>%
  rename('Region' = 'region', 'State' = 'state', 'Price Before' = 'beg',
         'Price After' = 'end', '% Increase' = 'pct_inc') 

# Petrobras scandal/economic crisis
grid.arrange(plot_byFuel('Common Gas', 2014, 2016), plot_byFuel('Ethanol', 2014, 2016),
             plot_byFuel('Diesel', 2014, 2016), plot_byFuel('Diesel S10', 2014, 2016))

# By Region
plot_byReg('Common Gas', 2014, 2016)

# By State
min(as.Date(df$final_date)[df$year == 2014]) # 2014-01-11
max(as.Date(df$final_date)[df$year == 2016]) # 2016-12-31
plot_pctInc('2014-01-11', '2016-12-31', 'Common Gas', 'North') +
  labs(fill = 'State', caption = paste0(' % Increase in Prices: 1/11/14 - 12/31/16'))

recession <- df %>%
  select(final_date, year, month, mean_price, product, region, state) %>%
  group_by(product, region, state) %>%
  filter(final_date == '2014-01-11' | final_date == '2016-12-31') %>%
  mutate(beg = mean_price[final_date == '2014-01-11'],
         end = mean_price[final_date == '2016-12-31'],
         pct_inc = round(((end - beg) / beg) * 100, 2)) %>%
  select(product, region, state, beg, end, pct_inc) %>%
  arrange(desc(pct_inc)) %>%
  distinct() %>%
  rename('Fuel Type' = 'product', 'Region' = 'region', 'State' = 'state', 'Price Before' = 'beg',
         'Price After' = 'end', '% Increase' = 'pct_inc') 

# Post-fuel tax
grid.arrange(plot_byFuel('Common Gas', 2017, 2019), plot_byFuel('Ethanol', 2017, 2019),
             plot_byFuel('Diesel', 2017, 2019), plot_byFuel('Diesel S10', 2017, 2019))

post_tax <- df %>%
  filter(year == 2007, month == 7, region == 'North')


# By region
plot_byReg('Ethanol', 2017, 2019)

# By state
# Tax went into effect 7/23/17
# Will use week ending in 7/22/17 as last pre tax observation
grid.arrange(plot_byFuel('Common Gas', 2017, 2017), plot_byFuel('Ethanol', 2017, 2017),
               plot_byFuel('Diesel', 2017, 2017), plot_byFuel('Diesel S10', 2017, 2017))

plot_pctInc('2017-07-22', '2019-06-01', 'Common Gas', 'Southeast') +
  labs(fill = 'State', caption = paste0(' % Increase in Prices: 7/22/17 - 6/1/19'))

post_tax <- df %>%
  select(final_date, year, month, mean_price, product, region, state) %>%
  group_by(product, region, state) %>%
  filter(final_date == '2017-07-22' | final_date == '2019-06-01') %>%
  mutate(beg = mean_price[final_date == '2017-07-22'],
         end = mean_price[final_date == '2019-06-01'],
         pct_inc = round(((end - beg) / beg) * 100, 2)) %>%
  select(product, region, state, beg, end, pct_inc) %>%
  arrange(desc(pct_inc)) %>%
  distinct() %>%
  rename('Fuel Type' = 'product', 'Region' = 'region', 'State' = 'state', 'Price Before' = 'beg',
         'Price After' = 'end', '% Increase' = 'pct_inc') 
# ------------------------------------------------------------------------------------------------
# Model
# Import Petrobras stock prices
petrobras <- read.csv('PBR.csv', header = TRUE)
petrobras <- petrobras %>%
  select(Date, Open) %>%
  rename('date' = Date, 'stock_price' = Open) %>%
  filter(as.Date(date) >= '2004-05-10' & as.Date(date) <= '2019-06-01') %>%
  mutate(date = as.Date(date, format = '%Y-%m-%d'), week = floor_date(date, 'week'),
         stock_price = as.numeric(as.character(stock_price)))
# Mean prices by week
petrobras <- aggregate(stock_price ~ week, FUN = mean, data = petrobras)
petrobras <- petrobras %>%
  rename('final_date' = 'week') %>%
  # Change date from beg to end of week
  mutate(final_date = as.Date(final_date, format = '%Y-%m-%d'), final_date = final_date + 6)
  
# Import Brazil GDP
b_gdp <- read.csv('brazil_gdp.csv', header = TRUE)
b_gdp <- b_gdp %>%
  rename('date' = DATE, 'b_gdp' = BRALORSGPNOSTSAM) %>%
  mutate(date = as.Date(date, format = '%Y-%m-%d')) %>%
  filter(date >= '2004-05-01' & date <= '2019-05-01')

# Import China GDP
c_gdp <- read.csv('china_gdp.csv', header = TRUE)
c_gdp <- c_gdp %>%
  rename('date' = DATE, 'c_gdp' = CHNLORSGPNOSTSAM) %>%
  mutate(date = as.Date(date, format = '%Y-%m-%d')) %>%
  filter(date >= '2004-05-01' & date <= '2019-05-01')

# Import crude oil prices
# WTI
wti <- read.csv('wti_prices.csv', header = TRUE)
wti <- wti %>%
  rename('date' = DATE, 'wti_price' = DCOILWTICO) %>%
  filter(wti_price != '.') %>%
  mutate(date = as.Date(date, format = '%Y-%m-%d'), week = floor_date(date, 'week'),
         wti_price = as.numeric(as.character(wti_price)))
# Mean prices by week
wti <- aggregate(wti_price ~ week, FUN = mean, data = wti)
wti <- wti %>%
  rename('final_date' = 'week') %>%
  # Change date from beg to end of week
  mutate(final_date = as.Date(final_date, format = '%Y-%m-%d'), final_date = final_date + 6)

# Brent
brent <- read.csv('brent_prices.csv', header = TRUE)
brent <- brent %>%
  rename('date' = DATE, 'brent_price' = DCOILBRENTEU) %>%
  filter(brent_price != '.') %>%
  mutate(date = as.Date(date, format = '%Y-%m-%d'), week = floor_date(date, 'week'),
         brent_price = as.numeric(as.character(brent_price)))
# Mean prices by week
brent <- aggregate(brent_price ~ week, FUN = mean, data = brent)
brent <- brent %>%
  rename('final_date' = 'week') %>%
  # Change date from beg to end of week
  mutate(final_date = as.Date(final_date, format = '%Y-%m-%d'), final_date = final_date + 6)

# Import sugar prices
sugar <- read.csv('sugar_prices.csv', header = TRUE)
sugar <- sugar %>%
  rename('daily_price' = 'value') %>%
  filter(as.Date(date) >= '2004-05-10' & as.Date(date) <= '2019-06-01') %>%
  mutate(date = as.Date(date, format = '%Y-%m-%d'), week = floor_date(date, 'week'))
  # Mean prices by week
sugar <-  aggregate(daily_price ~ week, FUN = mean, data = sugar)
sugar <- sugar %>%
  rename('final_date' = 'week', 'sugar_price' = 'daily_price') %>%
  # Change date from beg to end of week
  mutate(final_date = as.Date(final_date, format = '%Y-%m-%d'), final_date = final_date + 6)

# Merge dataframes by dates for stock/oil prices, by month for gdp
df$ym_date <- make_date(year = df$year, month = df$month)
df$final_date <- as.Date(df$final_date, format = '%Y-%m-%d')

df <- df %>%
  left_join(b_gdp, by = c('ym_date' = 'date')) %>%
  left_join(c_gdp, by = c('ym_date' = 'date')) %>%
  left_join(petrobras, by = 'final_date') %>%
  left_join(wti, by = 'final_date') %>%
  left_join(brent, by = 'final_date') %>%
  left_join(sugar, by = 'final_date')

# Model to predict stock prices
model <- df %>%
  select(final_date, state, product, mean_price, month, year, b_gdp, c_gdp,
         stock_price, wti_price, brent_price, sugar_price) %>%
  group_by(state, product) %>%
  filter(year <= 2018) %>%
  dummy_cols(select_columns = c('state', 'product'), remove_first_dummy = TRUE)

# Create partition, train/test sets
partition <- createDataPartition(model$mean_price, p = 0.8,  list = FALSE)
bgpTrain <- model[partition,]
bgpTest  <- model[-partition,]
fitControl <- trainControl(method = 'repeatedcv', number = 10, repeats = 10)

# Bootstrapped linear regression
set.seed(540)
lm1 <- train(mean_price ~. - final_date - state - product - mean_price + (year * month) +
               (stock_price * b_gdp * c_gdp * wti_price * brent_price * sugar_price),
          data = bgpTrain,
          method = 'lm')

# Regularization using glmnet
set.seed(540)
glmnet1 <- train(mean_price ~. - final_date - state - product - mean_price + (year * month) +
                   (stock_price * b_gdp * c_gdp * wti_price * brent_price * sugar_price),
                 data = bgpTrain,
                 method = 'glmnet', trControl = fitControl)

# Calculate rmse on held out test set
lm1_pred <- predict(lm1, bgpTest)
glm1_pred <- predict(glmnet1, bgpTest)

rmse <- function(mod_pred) {
  mod_rmse <- vector('double', length = nrow(bgpTest))
  for (i in seq_along(mod_rmse)) {
    mod_rmse[[i]] <- RMSE(mod_pred[[i]], bgpTest$mean_price[[i]])
  }
  rmse_var <- var(mod_rmse)
  rmse_mean <- mean(mod_rmse)
  cat('RMSE Mean:', round(rmse_mean, digits = 2),
      'RMSE Variance:',round(rmse_var, digits = 2),sep='\n')
}
rmse(lm1_pred)
rmse(glm1_pred)

lm1_df <- data.frame(lm1$results)
lm1_df$intercept <- NULL
lm1_df <- round(lm1_df, 3)
glm1_df <- data.frame(round(glmnet1$results, 3))

summary(lm1)
