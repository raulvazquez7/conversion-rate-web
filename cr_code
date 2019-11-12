
#### CONVERSION RATE PROJECT ####

## Import Packages and read CSV ##

install.packages("pacman")
pacman::p_load(readr, lubridate, dplyr, plotly, corrplot, tidyr, reshape2)

df <- read_csv("~/Raúl Vázquez/Personal/Test Metriplica/results_df.csv")

# Checking Na's

sapply(df, function(x) sum(is.na(x)))

## Understanding Data and some Data Wrangling ##

str(df)
summary(df)

df$date <- lubridate:: ymd(df$date) #data types

df$channelGrouping <- sub("(^[^-]+)-.*", "\\1", df$channelGrouping) #keep values before "-"

col_names <- c("channelGrouping","userAgeBracket","userType")
df[,col_names] <- lapply(df[,col_names] , factor) # converting some columns to factor
rm(col_names)

df$conversionRate <- (df$transactions/df$sessions)*100

## Data Exploratory Analysis ##

mean_df <- aggregate(df$conversionRate, df[,2:4] , mean)
colnames(mean_df)[4] <- "conversionRate"
mean_df[order(mean_df$conversionRate, decreasing = TRUE), ]

# Detecting and exploring high CR

high_cr <- df %>% dplyr::filter(conversionRate >= 30)
head(high_cr, 11)

plot_ly(df, x = ~conversionRate, type = "histogram")

# Distribution of audience

# CR per Audience

mean_channel <- aggregate(list(conversionRate = df$conversionRate), list(channelGrouping = factor(df$channelGrouping)), mean) 

plot_ly(mean_channel,x = mean_channel$channelGrouping, y = mean_channel$conversionRate ,type = "scatter", mode = "markers",
        name = "Conversion Rate") %>%
  add_trace(df,x = df$channelGrouping, y = df$sessions ,type = "scatter", mode = "markers",
            name = "Visits", yaxis = "y2",
            transforms = list(list(
              type = "aggregate",
              groups = df$channelGrouping,
              aggregations = list(list(target = "y", func = "sum",enabled = TRUE))))) %>%
  layout(title = "Conversion (avg) per Channel",
         xaxis = list(title = "Channel Group"),
         yaxis = list(showline = TRUE, side = "left",title = "Conversion Rate (avg)", ticksuffix = "%"),
         yaxis2 = list(showline = TRUE,side = "right",overlaying = "y",title = "Sessions (M)")) #CR and Sessions per Channel

mean_age <- aggregate(list(conversionRate = df$conversionRate), list(userAgeBracket = factor(df$userAgeBracket)), mean) 

plot_ly(mean_age,x = mean_age$userAgeBracket, y = mean_age$conversionRate ,type = "scatter", mode = "markers",
        name = "Conversion Rate") %>%
  add_trace(df,x = df$userAgeBracket, y = df$sessions ,type = "scatter", mode = "markers",
            name = "Visits", yaxis = "y2",
            transforms = list(list(
              type = "aggregate",
              groups = df$userAgeBracket,
              aggregations = list(list(target = "y", func = "sum",enabled = TRUE))))) %>%
  layout(title = "Conversion (avg) per Channel",
         xaxis = list(title = "Channel Group"),
         yaxis = list(showline = TRUE, side = "left",title = "Conversion Rate (avg)", ticksuffix = "%"),
         yaxis2 = list(showline = TRUE,side = "right",overlaying = "y",title = "Sessions (M)")) #CR and Sessions per Age

mean_type <- aggregate(list(conversionRate = df$conversionRate), list(userType = factor(df$userType)), mean) 

plot_ly(mean_type,x = mean_type$userType, y = mean_type$conversionRate ,type = "scatter", mode = "markers",
        name = "Conversion Rate") %>%
  add_trace(df,x = df$userType, y = df$sessions ,type = "scatter", mode = "markers",
            name = "Visits", yaxis = "y2",
            transforms = list(list(
              type = "aggregate",
              groups = df$userType,
              aggregations = list(list(target = "y", func = "sum",enabled = TRUE))))) %>%
  layout(title = "Conversion (avg) per Channel",
         xaxis = list(title = "Channel Group"),
         yaxis = list(showline = TRUE, side = "left",title = "Conversion Rate (avg)", ticksuffix = "%", range = c(0.4, 1.6)),
         yaxis2 = list(showline = TRUE,side = "right",overlaying = "y",title = "Sessions (M)")) #CR and Sessions per User Type


# CR of Total by day, week and month

byday <- df %>% dplyr::group_by(date=floor_date(date, "day")) %>%
  dplyr::summarize_at(vars(sessions, transactions), funs(sum)) #grouping by day

byday_y2 <- df %>% dplyr::group_by(date=floor_date(date, "day")) %>%
  dplyr::summarize_at(vars(sessions, transactions), funs(sum)) #grouping by week for the second y axes
byday_y2$conversionRate <- byday_y2$transactions/byday_y2$sessions*100

plot_ly(byday_y2,x = byday_y2$date, y = byday_y2$conversionRate ,type = "scatter", mode = "lines",
        name = "Conversion Rate") %>%
  add_trace(byday,x = byday$date, y = byday$sessions ,type = "scatter", mode = "lines",
            name = "Sessions", yaxis = "y2",
            transforms = list(list(
              type = "aggregate",
              groups = byday$date,
              aggregations = list(list(target = "y", func = "sum",enabled = TRUE))))) %>%
  layout(title = "Conversion (mean) and Sessions in 2019 by month (all audience)",
         xaxis = list(title = "Time"),
         yaxis = list(showline = TRUE, side = "left",title = "Conversion Rate (mean)", ticksuffix = "%", range = c(0.1, 2.4)),
         yaxis2 = list(showline = TRUE,side = "right",overlaying = "y",
                       title = "Sessions (Millions)", range = c(10000, 130000))) #CR and Sessions per User Type

byweek <- df %>% dplyr::group_by(date=floor_date(date, "week")) %>%
  dplyr::summarize_at(vars(sessions, transactions), funs(sum)) #grouping by week for the first y axes

byweek_y2 <- df %>% dplyr::group_by(date=floor_date(date, "week")) %>%
  dplyr::summarize_at(vars(sessions, transactions), funs(sum)) #grouping by week for the second y axes
byweek_y2$conversionRate <- byweek_y2$transactions/byweek_y2$sessions*100

plot_ly(byweek_y2,x = byweek_y2$date, y = byweek_y2$conversionRate ,type = "scatter", mode = "lines",
        name = "Conversion Rate") %>%
  add_trace(byweek,x = byweek$date, y = byweek$sessions ,type = "scatter", mode = "lines",
            name = "Sessions", yaxis = "y2",
            transforms = list(list(
              type = "aggregate",
              groups = byweek$date,
              aggregations = list(list(target = "y", func = "sum",enabled = TRUE))))) %>%
  layout(title = "Conversion (mean) and Sessions in 2019 by week (all audience)",
         xaxis = list(title = "Time"),
         yaxis = list(showline = TRUE, side = "left",title = "Conversion Rate (mean)", ticksuffix = "%", range = c(0.2, 2)),
         yaxis2 = list(showline = TRUE,side = "right",overlaying = "y",
                       title = "Sessions (Miles)", range = c(25000, 700000))) #CR and Sessions per User Type

bymonth <- df %>% dplyr::group_by(date=floor_date(date, "month")) %>%
  dplyr::summarize_at(vars(sessions, transactions), funs(sum)) #grouping by month

bymonth_y2 <- df %>% dplyr::group_by(date=floor_date(date, "month")) %>%
  dplyr::summarize_at(vars(sessions, transactions), funs(sum)) #grouping by week for the second y axes
bymonth_y2$conversionRate <- bymonth_y2$transactions/bymonth_y2$sessions*100


plot_ly(bymonth_y2,x = bymonth_y2$date, y = bymonth_y2$conversionRate ,type = "scatter", mode = "lines",
        name = "Conversion Rate") %>%
  add_trace(bymonth,x = bymonth$date, y = bymonth$sessions ,type = "scatter", mode = "lines",
            name = "Sessions", yaxis = "y2",
            transforms = list(list(
              type = "aggregate",
              groups = bymonth$date,
              aggregations = list(list(target = "y", func = "sum",enabled = TRUE))))) %>%
  layout(title = "Conversion (mean) and Sessions in 2019 by month (all audience)",
         xaxis = list(title = "Time"),
         yaxis = list(showline = TRUE, side = "left",title = "Conversion Rate (mean)", ticksuffix = "%", range = c(0.3, 1.8)),
         yaxis2 = list(showline = TRUE,side = "right",overlaying = "y",
                       title = "Sessions (Millions)", range = c(1000000, 2700000))) #CR and Sessions per User Type


# CR per Audience and week

byweek_type <- df %>% dplyr::group_by(date=floor_date(date, "week"), userType) %>%
  dplyr::summarize_at(vars(sessions, transactions), funs(sum)) #grouping by week

byweek_type %>%
  ungroup() %>%
plot_ly( x = byweek_type$date, y = ~(transactions/sessions)*100, color = ~userType,
         mode = "lines", type = "scatter") %>%
  layout(yaxis = list(ticksuffix = "%")) %>%
  layout(title = "CR in 2019 per Month",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Conversion Rate")) #CR per Month

byweek_channel <- df %>% dplyr::group_by(date=floor_date(date, "week"), channelGrouping) %>%
  dplyr::summarize_at(vars(sessions, transactions), funs(sum)) #grouping by week

byweek_channel %>%
  ungroup() %>%
  plot_ly( x = byweek_channel$date, y = ~(transactions/sessions)*100, color = ~channelGrouping,
           mode = "lines", type = "scatter") %>%
  layout(yaxis = list(ticksuffix = "%")) %>%
  layout(title = "CR in 2019 per Month",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Conversion Rate")) #CR per Month

byweek_age <- df %>% dplyr::group_by(date=floor_date(date, "week"), userAgeBracket) %>%
  dplyr::summarize_at(vars(sessions, transactions), funs(sum)) #grouping by week

byweek_age %>%
  ungroup() %>%
  plot_ly( x = byweek_age$date, y = ~(transactions/sessions)*100, color = ~userAgeBracket,
           mode = "lines", type = "scatter") %>%
  layout(yaxis = list(ticksuffix = "%")) %>%
  layout(title = "CR in 2019 per Month",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Conversion Rate")) #CR per Month

# Understanding the relationship between sessions and conversion rate

ggplot(df, aes(x = df$sessions, y = df$conversionRate))+
  geom_point()+
  geom_smooth(method = "lm")

# Understanding our Customer Persona

df$customer <- apply( df[, 2:4 ] , 1 , paste , collapse = "-" ) #pasting our audience into one column

aggr_cr <- aggregate(list(conversionRate = df$conversionRate), list(customer = factor(df$customer)), mean)
aggr_cr <- aggr_cr[order(aggr_cr$conversionRate, decreasing = TRUE), ] %>%
  top_n(10)

ggplot(aggr_cr, aes(x = aggr_cr$customer, y = aggr_cr$conversionRate))+
  geom_bar(stat = "identity",  fill = "#f5973d")+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

aggr_sessions <- aggregate(list(conversionRate = df$sessions), list(customer = factor(df$customer)), sum)
aggr_sessions <- aggr_sessions[order(aggr_sessions$conversionRate, decreasing = TRUE), ] %>%
  top_n(10)

ggplot(aggr_sessions, aes(x = aggr_sessions$customer, y = aggr_sessions$conversionRate))+
  geom_bar(stat = "identity",  fill = "#5564eb")+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))



