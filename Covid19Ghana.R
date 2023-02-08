install.packages("bookdown")
library(devtools)
devtools::install_github("thiyangt/sta3262")
library(sta3262)
get_individual_project_country("AS2018310")
install.packages("coronavirus")
library(coronavirus)
data(coronavirus)
head(coronavirus)
tail(coronavirus)
?coronavirus
unique(coronavirus$country)
library(tidyverse)
library(magrittr)
library(dplyr)
library(ggplot2)
library(maptools)
install.packages("ggrepel")
library(png)
library(grid)
install.packages("bslib")
install.packages("shiny")
install.packages("rmarkdown")
library(bslib)
library(shiny)
library(rmarkdown)
ghana_corona <- coronavirus %>% filter(country == "Ghana")



#RESHAPING DATA___________________________________________________________________

summary(ghana_corona)
#minimum of cases is a negative value. Usually they cant be zero. So we get absolute values of cases
ghana_corona$cases <- abs(ghana_corona$cases)
summary(ghana_corona)

#checking missing values in cases
sum(is.na(ghana_corona$cases)) 
#Changing NA values to zero
is.data.frame(ghana_corona) 
ghana_corona <-mutate_at(ghana_corona,c("cases"),~replace(.,is.na(.),0))
summary(ghana_corona)



unique(coronavirus$type)
seperate_types <- ghana_corona %>% 
  pivot_wider(names_from=type, 
              values_from=cases)

summary(seperate_types)
sum(is.na(ghana_corona$recovered)) 
sum(is.na(ghana_corona$death)) 
sum(is.na(ghana_corona$confirmed)) 


#boxplot to identify outliers
install.packages("breakDown")




#p1 <-ggplot(ghana_corona,aes(x=cases,y=type))+geom_boxplot(aes(frame=cases),color="blue")
#ggplotly(p1)

#ggplot(ghana_corona, aes(x=cases, y=type)) +geom_boxplot(fill="skyblue")


#dataframes for each type of cases

recovered_data <- ghana_corona[ghana_corona$type=="recovered", ]
colnames(recovered_data) <- c("date", "region", "country", "latitudtes","longitudes","type","recoverd.cases")
head(recovered_data)

death_data <- ghana_corona[ghana_corona$type=="death", ]
colnames(death_data) <- c("date", "region", "country", "latitudtes","longitudes","type","death.cases")
head(death_data)

confirmed_data <- ghana_corona[ghana_corona$type=="confirmed", ]
colnames(confirmed_data) <- c("date", "region", "country", "latitudtes","longitudes","type","confirmed.cases")
head(confirmed_data)


#Recoverd data has an outlier

install.packages("plotly")
library(plotly)
box1 <- ggplot(ghana_corona, aes(x=cases, y=type, fill=type))+ geom_jitter()+ geom_boxplot(alpha=0.5)
box2 <- ggplotly(box1)

#identifying outlier
#according to box plot outlier case has 5526 recorded cases

#Removing outlier from entire dataframe
ghana_corona <- ghana_corona %>% filter(!cases == 5526)

#Removing outlier from type seperated dataframe because 
seperate_types <- seperate_types %>% filter(!date == "2020-06-20")
summary(seperate_types)


#Preliminary Data Analysis___________________________________________________________________

cumsum_confirmed <-cumsum(seperate_types$confirmed)
cumsum_death <-cumsum(seperate_types$death)
cumsum_recovered <- cumsum(seperate_types$recovered)


new1 <-seperate_types %>% mutate(seperate_types,cum_sum_recov=cumsum(recovered),cum_sum_confirmed=cumsum(death),cum_sum_conf=cumsum(confirmed))
view(new1)

new2<-seperate_types %>% mutate(new1,active= cumsum_confirmed-(cumsum_recovered+cumsum_death))
view(new2)


# Library
install.packages("dygraphs")
library(dygraphs)
library(xts)          # To make the convertion data-frame / xts format
library(tidyverse)
library(lubridate)

# Read the data (hosted on the gallery website)

ts1 <- ts(ghana_corona$cases)
ts2 <- as.data.frame(ts1)
nrow(data)
# Check type of variable
str(ts2)

# Since my time is currently a factor, I have to convert it to a date-time format!


# Then you can create the xts necessary to use dygraph
don <- xts(x = ts1, order.by = ghana_corona$date)
?xts
# Finally the plot
p <- dygraph(don) %>%
  dyOptions(labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.1, drawGrid = FALSE, colors="#DAA520") %>%
  dyRangeSelector() %>%
  dyCrosshair(direction = "vertical") %>%
  dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
  dyRoller(rollPeriod = 1)
library(htmlwidgets)



# save the widget
# library(htmlwidgets)
# saveWidget(p, file=paste0( getwd(), "/HtmlWidget/dygraphs318.html"))

Recovered <- ts(seperate_types$recovered)
Confirmed <- tseperate_types$confirmed
Deaths <- seperate_types$death
Date <- seperate_types$date

ts1 <- ts(ghana_corona$cases)
ts2 <- as.data.frame(ts1)ts2

p1 <- cbind(Recovered,Confirmed,Deaths)
p2<- ts(p1,frequency=356,start=c(2020,1,22))
library(zoo)
p3 <- zoo(p1, seq(from = as.Date("2020-01-22"), to = as.Date("2021-09-18"), by = 1))


don <- xts(x =ghana_corona$cases , order.by = ghana_corona$date)

dygraph(p3, main = "Covid Cases in Ghana from 2020 onwards",ylab = "No. of cases") %>%
  dySeries("Recovered",pointSize = 0.5, stepPlot = TRUE,drawPoints = TRUE,color="red") %>% 
  dySeries("Confirmed",pointSize = 0.5, stepPlot = TRUE,drawPoints = TRUE,color="blue")%>%
  dySeries("Deaths",pointSize = 0.5, stepPlot = TRUE,drawPoints = TRUE,color="green")
  

data(UKLungDeaths)
lungDeaths <- cbind(mdeaths, fdeaths, ldeaths)
str(lungDeaths)
dygraph(lungDeaths, main = "Deaths from Lung Disease (UK)") %>%
  dySeries("fdeaths", stepPlot = TRUE, color = "red") %>% 
  dyGroup(c("mdeaths", "ldeaths"), drawPoints = TRUE, color = c("blue", "green"))

?coronavirus



# Libraries
library(ggplot2)
library(dplyr)
library(plotly)
install.packages("hrbrthemes")
library(hrbrthemes)

# Load dataset from github
data <- read.table("https://raw.githubusercontent.com/holtzy/data_to_viz/master/Example_dataset/3_TwoNumOrdered.csv", header=T)
data$date <- as.Date(data$date)

r <-seperate_types$recovered
c <-seperate_types$confirmed
d <-seperate_types$death



# death chart
p11 <- seperate_types %>%
  ggplot( aes(x=date, y=death)) +
  geom_area(fill="#69b3a2", alpha=0.5) +
  geom_line(color="#54b3a2") +
  ylab("No of death cases") + xlab("Date reported")
  theme_ipsum()
# Turn it interactive with ggplotly
p12 <- ggplotly(p11)
p12


# recovered chart
p11 <- seperate_types %>%
  ggplot( aes(x=date, y=recovered)) +
  geom_area(fill="#000080", alpha=0.8) +
  geom_line(color="#000000") +
  ylab("No of death cases") +
  theme_ipsum()
# Turn it interactive with ggplotly
p12 <- ggplotly(p11)
p12



# save the widget
# library(htmlwidgets)
# saveWidget(p, file=paste0( getwd(), "/HtmlWidget/ggplotlyAreachart.html"))


ggplot() +
  geom_point(data = df, aes(gp, y)) +
  geom_point(data = ds, aes(gp, mean), colour = 'red', size = 3) +
  geom_errorbar(
    data = ds,
    aes(gp, mean, ymin = mean - sd, ymax = mean + sd),
    colour = 'red',
    width = 0.4
  )



#2020 and 2021 comparison

gapminder2007 <- gapminder %>% filter(year == 2007)



