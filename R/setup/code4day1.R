library(tidyverse)
library(knitr)
library(kableExtra)
#install.packages("gapminder")
library(hrbrthemes)
library(viridis)
library(kableExtra)
options(knitr.table.format = "html")
library(plotly)
library(gridExtra)
library(ggrepel)




#| label: setup
#| include: false
library(tidyverse)
library(kableExtra)
library(patchwork)
library(fontawesome)
library(gapminder)
library(scales)
 

## Data Science


## R


ggplot(data = gapminder, 
       aes(x = gdpPercap,
           y = lifeExp,
           color = continent))+
  geom_point(alpha=0.3)+
  geom_smooth(method = "lm")+
  scale_x_log10(breaks=c(1000,10000, 100000),
                label=scales::dollar)+
  labs(x = "GDP/Capita",
       y = "Life Expectancy (Years)")+
  facet_wrap(~continent)+
  guides(color = F)+
  theme_light()
## 

ggplot(data = gapminder, 
       aes(x = gdpPercap,
           y = lifeExp,
           color = continent))+
  geom_point(alpha=0.3)+
  geom_smooth(method = "lm")+
  scale_x_log10(breaks=c(1000,10000, 100000),
                label=scales::dollar)+
  labs(x = "GDP/Capita",
       y = "Life Expectancy (Years)")+
  facet_wrap(~continent)+
  guides(color = F)+
  theme_light()
 
  
  
  ## Or This
  
       
### Input

#| echo: true
#| fig-width: 18
#| fig-align: center
library(leaflet)
leaflet() %>%
  addTiles() %>%
  addMarkers(lng = 73.0874, lat =33.6604 ,
             popup = "Avari Xpress Hotel, Islamabad")
 

# Meet R and R Studio {.centered background-color="#314f4f"}

## R and R Studio


## Output

   
library(tidyverse)
library(gapminder)
library(gganimate)
gapminder %>%
  filter(continent != "Oceania") %>%
  ggplot(aes(x = gdpPercap,
             y = lifeExp,
             color = country,
             size = pop))+
  geom_point(alpha=0.3)+
  scale_x_log10(breaks=c(1000,10000, 100000),
                label=scales::dollar)+
  scale_size(range = c(0.5, 12)) +
  scale_color_manual(values = gapminder::country_colors) +
  labs(x = "GDP/Capita",
       y = "Life Expectancy (Years)",
       caption = "Source: Hans Rosling's gapminder.org",
       title = "Income & Life Expectancy - {frame_time}")+
  facet_wrap(~continent)+
  guides(color = F, size = F)+
  theme_bw(base_family = "Fira Sans Condensed")+
  transition_time(year)+
  ease_aes("linear")
 

## Code

   
library(gapminder)
library(gganimate)
gapminder %>%
  filter(continent != "Oceania") %>%
  ggplot(aes(x = gdpPercap,
             y = lifeExp,
             color = country,
             size = pop))+
  geom_point(alpha=0.3)+
  scale_x_log10(breaks=c(1000,10000, 100000),
                label=scales::dollar)+
  scale_size(range = c(0.5, 12)) +
  scale_color_manual(values = gapminder::country_colors) +
  labs(x = "GDP/Capita",
       y = "Life Expectancy (Years)",
       caption = "Source: Hans Rosling's gapminder.org",
       title = "Income & Life Expectancy - {frame_time}")+
  facet_wrap(~continent)+
  guides(color = F, size = F)+
  theme_minimal(base_family = "Fira Sans Condensed")+
  transition_time(year)+
  ease_aes("linear")
 
  
  
 

## The dataset is provided in the gapminder library

   
library(gapminder)

gapminder %>% filter(country=="Sweden")%>%
  mutate(gdpPercap=round(gdpPercap,0), lifeExp=round(lifeExp,2))%>%kable()%>%
  kable_styling(bootstrap_options = "striped", full_width = F)

 

## Information in **gapminder** data


## Summary

summary(gapminder) 

 

# dplyr features

### `filter()` to keep selected observations

### `select()` to keep selected variables

### `arrange()` to reorder observations by a value

### `mutate()` to create new variables

### `summarize()` to create summary statistics

### `group_by()` for performing operations by group

# `Select()`

## Column Selection


   
gapminder %>% select(country, pop, lifeExp)
 

## select most of the variables and drop one or two

   
gapminder %>% select(-gdpPercap)

 

## Data Filtering

### `filter()` funtion

   
gapminder_07<- gapminder %>% filter(year==2007)
kbl(gapminder_07[1:10,])%>%kable_styling(fixed_thead=T)
 

##\>`Have we accidently deleted all other rows? Answer is no.`

### Nope: If you don't believe me try entering gapminder at the console.

 {r tables}
gapminder %>% filter(year==2007)

 

## Filtering with respect to two variables

### One can apply multiple `filters`

   
gapminder %>% filter(year==2007,country=="Sri Lanka")

gapminder %>% filter(year==2007, country=="Pakistan")

 

### Now we are selecting multiple countries for year 2007.

   
gapminder %>% filter(year==2007, country %in% c("India", "Pakistan","Bangladesh", "Afghanistan", "Iran"))
 

## Filtering data for South Asia countries

   
gapminderSA<-gapminder %>% filter(country %in% c("Bangladesh","India","Pakistan","Sri Lanka","Nepal", "Afghanistan","Bhutan", "Maldives"))
gapminderSA
 

## Sort data with `arrange()`

   
gapminderSA %>% arrange(gdpPercap)

 

## `arrange()` sorts in ascending order. Sort in descending order, `desc()`.

   
gapminderSA %>% arrange(desc(gdpPercap))
 

## Life Expectancy in South Asia in 2007

What is the lowest and highest life expectancy among South Asian countries?
  
     
gapminderSA %>% filter(year==2007) %>%  arrange(lifeExp)
 

### What was it in 1952?

## `mutate()` to change existing or create new variable

   
gapminderSA %>% mutate(pop=pop/1000000)
 


gapminderSA %>% mutate(gdp = pop * gdpPercap)
 

## How to calculate new variables


gapminder %>% filter(year==2007) %>% 
  mutate(gdp=gdpPercap*pop) %>% 
  arrange(desc(gdp)) %>% 
  top_n(10)

 

gapminder %>% filter(year==2007) %>% 
  transmute(gdp=gdpPercap*pop) %>% 
  arrange(desc(gdp)) %>% 
  top_n(10)

 

## Ordering


   
gapminder %>% 
  select(country, year,lifeExp) %>% 
  filter(year==2007) %>% 
  arrange(lifeExp)

 

#top to bottom, then use `arrange(desc())` command as follows:

   
gapminder %>% 
  select(country, year,lifeExp) %>% 
  filter(year==2007) %>% 
  arrange(desc(lifeExp))

 

### Top 5

   
gapminder %>% 
  select(country, year,lifeExp) %>% 
  filter(year==2007) %>% 
  arrange(desc(lifeExp)) %>% 
  top_n(5)

 

# Summarising data

Another feature of dplyr is `summarise` data

   
gapminder %>% filter(year==2007) %>% group_by(continent) %>% summarise(mean=mean(lifeExp),min=min(lifeExp),max=max(lifeExp))
 

   
gapminder %>% 
  summarise(avglifeExp=mean(lifeExp))

 

## Summarising data by groups

   
gapminder %>%
  filter(year == 2007, continent == "Asia") %>%
  summarize(avgLifeExp = mean(lifeExp)) 

 

## 

   
gapminder %>% 
  group_by(continent) %>% 
  filter(year==2007) %>% 
  summarize(avglife=mean(lifeExp))

 

## mean, maximum, and mean life expectancy

   
gapminder %>%
  summarise(
    lifeExp_min = min(lifeExp),
    lifeExp_max = max(lifeExp),
    lifeExp_mean = mean(lifeExp)
  )
 

## LE for each year

   
gapminder %>%
  group_by(year) %>%
  summarise(
    lifeExp_min = min(lifeExp),
    lifeExp_max = max(lifeExp),
    lifeExp_mean = mean(lifeExp)
  )

 

##[dplyr tutorial0](https://anderfernandez.com/en/blog/dplyr-tutorial/)

## if_else command alongwith mutate

   
gapminder %>%
  filter(year == 2007) %>%
  group_by(continent) %>%
  summarize(avgLifeExp = mean(lifeExp)) %>%
  mutate(over75 = if_else(avgLifeExp > 70, "Y", "N"))

 

## Total Population by Continets in 2007

   
gapminder %>% 
  filter(year==2007) %>% 
  group_by(continent) %>% 
  summarize(tot_pop=sum(pop)) 

 

## Percentiles {.scroll}

   
gapminder %>% select(country,year, lifeExp, gdpPercap) %>% 
  filter(year == 2007) %>%
  mutate(percentile = ntile(lifeExp, 100)) %>%
  arrange(desc(gdpPercap))
 

## Bottom side

So it makes sense that higher the GDP, higher the lifeExp. This is not formal testing but exploratory data makes lot of sense here.

   
gapminder %>% select(country,year, lifeExp, gdpPercap) %>% 
  filter(year == 2007) %>%
  mutate(percentile = ntile(lifeExp, 100)) %>%
  arrange(gdpPercap)
 

# Advanced Analysis


gapminder %>% filter(year==2007) %>% 
  mutate(percentile=ntile(lifeExp,100)) %>% 
  filter(percentile>90) %>% 
  arrange(desc(percentile)) %>% 
  top_n(10,wt=percentile) %>% 
  select(country,continent,lifeExp,gdpPercap)

 

## In case you are interested in bottom 10 (worst lifeExp countries from the bottom), use `top_n` with `-10`.

   
gapminder %>% filter(year==2007) %>% 
  mutate(percentile=ntile(lifeExp,100)) %>% 
  filter(percentile<10) %>% 
  arrange(percentile) %>% 
  top_n(-10,wt=percentile) %>% 
  select(country,continent,lifeExp,gdpPercap)


 

## Visualizing data to get data insight

Visualizing data is one of the most important aspect of getting data insight and may provide a better data insight than a complicated model. Visualizing large data sets were not an easy task, so researchers relied on mathematical and core econometric/regression models. `ggplot2` which is a set of `tidyverse` package is probably one of the greatest tool for data visualization used in `R`. In the following sections we are going to visualize `gapminder` data.

Stat graphics is a mapping of variable to `aes`thetic attributes of `geom`etric objects.

## 3 Essential components of `ggplot2`

-   data: dataset containing the variables of interest
-   geom: geometric object in question line, point, bars
-   aes: aesthetic attributes of an object x/y position, colors, shape, size

## Scatter plot

   

gapminder2007<-gapminder %>% filter(year==2007)
p1<-ggplot(data=gapminder2007,mapping = aes(x=gdpPercap,y=lifeExp,color=continent,size=pop))+geom_point()
p1+facet_wrap(~continent)
p1+  labs(x = "GDP Per Capita", y = "Life Expectancy in Years",
          title = "Economic Growth and Life Expectancy",
          subtitle = "Data points are country-years",
          caption = "Source: Gapminder.")

 

## Bubbleplot

# Show a bubbleplot

data <- gapminder %>% filter(year=="2007") %>% select(-year)

data %>%
  mutate(pop=pop/1000000) %>%
  arrange(desc(pop)) %>%
  mutate(country = factor(country, country)) %>%
  ggplot( aes(x=gdpPercap, y=lifeExp, size = pop, color = continent)) +
  geom_point(alpha=0.7) +
  scale_size(range = c(1.4, 19), name="Population (M)") +
  scale_color_viridis(discrete=TRUE, guide=FALSE) +
  theme_ipsum() +
  theme(legend.position="bottom")
 

## 

# Prepare data
tmp <- data %>%
  mutate(
    annotation = case_when(
      gdpPercap > 5000 & lifeExp < 60 ~ "yes",
      lifeExp < 30 ~ "yes",
      gdpPercap > 40000 ~ "yes"
    )
  ) %>%
  mutate(pop=pop/1000000) %>%
  arrange(desc(pop)) %>%
  mutate(country = factor(country, country))

# Plot
ggplot( tmp, aes(x=gdpPercap, y=lifeExp, size = pop, color = continent)) +
  geom_point(alpha=0.7) +
  scale_size(range = c(1.4, 19), name="Population (M)") +
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum() +
  theme(legend.position="none") +
  geom_text_repel(data=tmp %>% filter(annotation=="yes"), aes(label=country), size=4 )
 

## 

   
##This is a table of data about a large number of countries, each observed over several years. Let's make a scatterplot with it.
P<-ggplot(data=gapminder,mapping = aes(x=gdpPercap,y=lifeExp))  

P+geom_point()+geom_smooth()

P+geom_point()+geom_smooth(method = "lm")

P+geom_point()+geom_smooth(method = "gam")+scale_x_log10()


P+geom_point()+geom_smooth(method = "gam")+scale_x_log10(labels=scales::dollar)


P<-ggplot(data=gapminder,mapping = aes(gdpPercap,y=lifeExp,color="purple"))
P+geom_point()+geom_smooth(method = "loess")+scale_x_log10()
 

## 

##aes() is for variables
P<-ggplot(data=gapminder,mapping = aes(gdpPercap,y=lifeExp))
P+geom_point(color="purple")+geom_smooth(method = "loess")+scale_x_log10()

P<-ggplot(data=gapminder,aes(x=gdpPercap,y=lifeExp))
P+geom_point(alpha=0.3)+
  geom_smooth(color="orange",se=FALSE,size=8,method = "lm")+
  scale_x_log10()
 

## With proper title

   
P<-ggplot(data=gapminder,mapping = aes(gdpPercap,y=lifeExp))
P+geom_point(alpha=0.3)+
  geom_smooth(method = "gam")+
  scale_x_log10(labels=scales::dollar)+
  labs(x = "GDP Per Capita", y = "Life Expectancy in Years",
       title = "Economic Growth and Life Expectancy",
       subtitle = "Data points are country-years",
       caption = "Source: Gapminder.")

 

## 

##Continent wise

p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap,
                          y = lifeExp,
                          color = continent))
p + geom_point() +
  geom_smooth(method = "loess") +
  scale_x_log10()
 

## 

p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap,
                          y = lifeExp,
                          color = continent,
                          fill = continent))
p + geom_point() +
  geom_smooth(method = "loess") +
  scale_x_log10()
 

## 

##Aesthetics can be mapped per geom
p <- ggplot(data = gapminder, mapping = aes(x = gdpPercap, y = lifeExp))
p + geom_point(mapping = aes(color = continent)) +
  geom_smooth(method = "loess") +
  scale_x_log10()
 

## 

   
p + geom_point(mapping = aes(color = log(pop))) +
  scale_x_log10()  


 

## Comparison of 2007 vs 1952 continentwise

gapminder2007<-gapminder %>% filter(year==2007)
p1<-ggplot(data=gapminder2007,mapping = aes(x=gdpPercap,y=lifeExp,color=continent,size=pop))+geom_point()
cont_2007<-p1+facet_wrap(~continent)+ylim(0,90)  

gapminder1952<-gapminder %>% filter(year==1952)
p11<-ggplot(data=gapminder1952,mapping = aes(x=gdpPercap,y=lifeExp,color=continent,size=pop))+geom_point()
cont_1952<-p11+facet_wrap(~continent)+ylim(0,90)
library(gridExtra)
grid.arrange(cont_1952,cont_2007,nrow=1)

 

## 

   
p1+labs(x = "GDP Per Capita", y = "Life Expectancy in Years",
        title = "Economic Growth and Life Expectancy",
        subtitle = "Data points are country-years",
        caption = "Source: Gapminder.")


 

## 

 {r roslings_plot_animation, eval=F, echo=F, comment = " "}
ggplot(gapminder) +
  aes(gdpPercap, lifeExp, size = pop, color = continent) +
  aes(group = country) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  facet_wrap(~ continent) +
  labs(x = 'GDP per capita', y = 'life expectancy') +
  labs(title = 'Year: {frame_time}') +
  gganimate::transition_time(year) +
  gganimate::ease_aes('linear')
 

## 

   
gapminder %>%  
  select(-pop, -gdpPercap) %>%  
  filter(year == 2007) %>%  
  group_by(continent) %>%  
  summarise(mean_life_exp =  
              mean(lifeExp),median_life_exp=median(lifeExp)) 

 

## 

   
gapminder %>%  
  select(-pop, -gdpPercap) %>%  
  filter(year == 2007) %>%  
  group_by(continent) %>%  
  summarise(median_life_exp =  
              median(lifeExp)) %>%  
  ggplot() +  
  aes(x = continent) +  
  aes(y = median_life_exp) +  
  geom_point(color = "blue", size = 3,  
             alpha = .4) +  
  scale_y_continuous(limits = c(0,85)) +  
  labs(title = "Median life expectency by continent in 2007") +  
  labs(subtitle = "Data Source: Gapminder package in R") +  
  labs(x = "") +  
  labs(y = "Median life expectancy") +  
  labs(caption = "Zahid Asghar for 'QM4SSH'") +  
  theme_minimal()
 

## 

   
gapminder %>%  
  filter(year == 1997) %>%  
  select(country, continent, lifeExp) %>%  
  mutate(  
    life_cats =  
      case_when(lifeExp >= 70 ~ "70+",  
                lifeExp < 70 ~ "<70")) %>%  
  ggplot() +  
  aes(x = continent, y = life_cats) +  
  geom_jitter(width = .25, height = .25)+  
  aes(col = continent) +  
  scale_color_discrete(guide = FALSE) +  
  theme_dark() +  
  labs(x = "", y = "") +  
  labs(title = "Life expectency beyond 70 in 1997")

 

## 

   
gapminder %>%  
  mutate(gdp_billions =  
           gdpPercap *  
           pop/1000000000) %>%  
  ggplot() +  
  aes(x = year) +  
  aes(y = gdp_billions) +  
  geom_line() +  
  aes(group = country) +  
  scale_y_log10() +  
  aes(col = continent) +  
  facet_wrap( ~ continent) +  
  scale_color_discrete(guide = F) +  
  theme_minimal()
 

## 

   
gapminder %>%  
  filter(year == 2007) %>%  
  ggplot() +  
  aes(x = continent, y = lifeExp) +  
  geom_boxplot() +  
  geom_jitter(height = 0, width = .2) +  
  stat_summary(fun.y = mean,
               geom = "point",
               col = "goldenrod3",
               size = 5)
 

## Pakistan

   
gapminder %>%  
  filter(country == "Pakistan") %>%  
  ggplot() +  
  aes(x = year, y = lifeExp) +  
  geom_point() +  
  geom_line() +  
  aes(alpha = year) +  
  aes(col = year) +  
  scale_color_viridis_c() +  
  theme_classic()
 

## 

   
gapminder %>%  
  filter(year == 2007) %>%  
  ggplot() +  
  aes(x = gdpPercap) +  
  aes(y = lifeExp) +  
  geom_point(alpha = .5) +  
  geom_rug(size = 1) +  
  aes(col = continent) +  
  aes(col = lifeExp) +  
  scale_x_log10() +  
  aes(size = gdpPercap) +  
  aes(size = pop) +  
  geom_point(col = "darkgreen", size = 1) +  
  facet_wrap(~ continent)
 
