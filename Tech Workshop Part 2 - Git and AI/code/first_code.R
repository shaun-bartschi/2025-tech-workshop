library(tidyverse)

bean <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRkk9DSL8Cqn-RvCrutmo2pcvEu0deEH9NOkUzen8fFWdDTqS1V2vMZPVPDeoqUqUH2X1QFjxNFkRFH/pub?output=csv")

plot(bean$date,bean$shower)
plot(bean$date,bean$weight)
plot(bean$date,bean$shave)
plot(bean$date,bean$sugar)

ggplot(greatSL, aes(x = as.Date(Date,format = "%m/%d/%Y"), y = Depth)) + 
     geom_point() + 
     geom_smooth()
