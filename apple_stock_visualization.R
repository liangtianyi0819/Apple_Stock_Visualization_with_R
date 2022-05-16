library(tidyquant)
library(dplyr)
library(sampling)
library(ggplot2)
library(plotly)

#Read data
getSymbols("AAPL", from = '2015-01-01',
           to = "2019-12-31",warnings = FALSE,
           auto.assign = T)
df = as.data.frame(AAPL)
gf = tibble::rownames_to_column(df[6], 'Date')
gf$AAPL.Change = 0
for (i in 2:length(gf$AAPL.Change)) {
  gf$AAPL.Change[i] = df$AAPL.Adjusted[i] -  df$AAPL.Adjusted[i - 1]
}

# Calculate average for each year (Categorical Variable)
YEARS = c(2015, 2016, 2017, 2018, 2019)
ave = c()
for (i in c(1:5)) {
  ave[i] = mean(gf$AAPL.Adjusted[format(as.Date(gf$Date), '%Y')==YEARS[i]])
}


#=====================================
#Numerical variable (Sets of variables)
#=====================================
name_space = c(rep('increase', length(gf$Date)))
for (i in c(1:length(gf$Date))) {
  if (gf$AAPL.Change[i] <=0) {
    name_space[i] = 'decrease'
  }
}
plot_ly(gf, x=~Date, colors = c('#FF6347', '#90EE90')) %>%
        add_lines(x=~Date, y = ~AAPL.Adjusted, name = 'Price') %>%
        add_trace(x = ~Date, y = ~AAPL.Change*5, 
                  name = name_space,
                  type = 'bar', color = ~AAPL.Change > 0) %>% 
        layout(title='Daily Price',
              xaxis=list(title='Date', showgrid = T),
              yaxis=list(title='Adjusted Close Price', showgrid = T))

ggplot(gf, aes(as.Date(gf$Date), AAPL.Change, color=AAPL.Change)) +
  geom_point(shape = 16, size = 3) +
  theme_minimal() + 
  scale_color_gradient(low = '#f0650e', high = '#1E90FF') + 
  ggtitle('Distribution of Daily Change from Previous Day') + 
  xlab('Years') + ylab('Changes') + 
  labs(colour='Changes')


#=====================================
#Categorical variable
#=====================================
plot_ly(as.data.frame(ave), x=YEARS, y=ave, type = 'bar') %>%
  layout(title=c('Daily Price','Average Price by Years'),
         xaxis=list(title='Years'),
         yaxis=list(title='Average Price')) %>%
  add_annotations( x = YEARS,
                   y = ave,
                   text = round(ave, 2),
                   font = list(size = 15),
                   showarrow = T)

plot_list = vector("list", 5)
for (i in c(1:5)) {
  p <- plot_ly(gf, y = gf$AAPL.Change[format(as.Date(gf$Date), '%Y')==YEARS[i]], 
               x = YEARS[i], type='box', 
               name = YEARS[i])
  plot_list[[i]] = p
}
subplot(plot_list)

ggplot(gf, aes(AAPL.Adjusted)) + 
  geom_histogram(bins = 25, 
                 aes(fill=..count.., color =..count.., y =..density..)) + 
  geom_density(col=2, size = 1.5) + 
  ggtitle('Distribution of Close Price') + 
  labs( xlab('Adjusted Close Price'))

set.seed(8584)
par(mfrow = c(2,2))
samples <- 800
xbar <- numeric(samples)
for (size in c(25, 50, 75, 100)) {
  for (i in 1: samples) {
    xbar[i] <- mean(sample(df$AAPL.Adjusted, size, replace = FALSE))
  }
  hist(xbar, xlab = "AAPL Adjusted Price", 
       ylim = c(0, 0.50), prob = TRUE, 
       breaks = seq(29.5, 45.5, 0.5),  
       xaxp=c(29.5, 45.5, 32), las = 1, 
       main = paste("Sample Size = ",size))

  lines(density(xbar), # density plot
        lwd = 1, # thickness of line
        col = "chocolate3")
  
  abline(v = mean(xbar),
         col = "royalblue",
         lwd = 1)
  
  abline(v = median(xbar),
         col = "red",
         lwd = 1)
  
  legend(x = "topright", # location of legend within plot area
         c("Density plot", "Mean", "Median"),
         col = c("chocolate3", "royalblue", "red"),
         lwd = c(1, 1, 1), cex=0.45, text.font = 30,)
}

#Sampling
increment <- gf[gf$AAPL.Change > 0,]


#=====================================
#Random Sampling
#=====================================
par(mfrow = c(1,2))
set.seed(8584)
s <- srswor(60, nrow(increment))
srsworSample <- increment[s != 0, ]
year <- srsworSample$Date
table(substring(year,1,4))
barplot(table(substring(year,1,4)))


#=====================================
#Systematic Sampling
#=====================================
set.seed(8584)
k <- ceiling(nrow(increment) / 60)

r <- sample(k, 1)

t <- seq(r, by = k, length = 60)

systematicSample <- increment[t, ]
year <- systematicSample$Date
table(substring(year,1,4))
barplot(table(substring(year,1,4)))


#=====================================
#Stratified, unequal sized strata
#=====================================
set.seed(8584)
pf <- data.frame(Year = substring(increment$Date,1,4), 
Change = increment$AAPL.Change)
freq <- table(pf$Year)
st.sizes <- round(60 * freq / sum(freq))


stratasSample <- sampling::strata(pf, stratanames = c("Year"), 
size = st.sizes, method = "srswor", description = TRUE)

stratasSample <- getdata(pf, stratasSample)
stratasSample
table(stratasSample$Year)
barplot(table(stratasSample$Year),ylim = c(0, 14),
        col=c("RED","ORANGE","YELLOW","BLUE","GREEN"), 
        ylab = "Frequency", xlab = "Years", 
        main = "# of times adjusted price change throughout year")
abline(h=0)
