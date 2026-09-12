canadian_lynx <- tibble(year = start(lynx)[1]: end(lynx)[1],  lynx_trapped = c(lynx))
glimpse(canadian_lynx)

ggplot(canadian_lynx, aes(x  = year, y = lynx_trapped))+
  geom_line()


ggplot(canadian_lynx, aes(x  = year, y = lynx_trapped))+
  geom_line()+
  ggtitle("Candian lynx trapped every year 1821 to 1934")
