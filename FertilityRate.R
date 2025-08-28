rm(list = ls())

library(utils)
library(tidyverse)


gender_data <- as_tibble(read.csv('Gender_StatsData.csv'))
teenage_fr <- gender_data %>% 
  filter(Indicator.Code == "SP.ADO.TFRT")

rm(gender_data)

summary(teenage_fr$X1963)
round(mean(teenage_fr$X2000, na.rm = TRUE),2)
round(sd(teenage_fr$X2000, na.rm =TRUE), 2)

#filtering by categories: low, mid, hid, world (in term of avg income)
filterbyincome <- filter(teenage_fr, Country.Code %in% c('WLD', 'LIC','MIC', 'HIC'))
view(teenage_fr)

#since dataset is wide, i'll make it long
teenage_fr_long <- gather(filterbyincome,
                          key="Year",
                          value = "FertilityRate", X1960:X2016
                          ) %>% 
  select(Year, Country.Name, Country.Code, FertilityRate)



#when spread is necessary:

teenage_fr_spread <- select(teenage_fr_long,
                          Year, Country.Code, FertilityRate) %>% 
  spread(key = Country.Code, 
         value = FertilityRate)

teenage_fr_long <- mutate(teenage_fr_long,
                          Year=as.numeric(str_replace(Year, "X","")))

#plotting to see the trend
ggplot(teenage_fr_long,
       aes(x=Year,
           y=FertilityRate,
           group = Country.Code,
           color = Country.Code)) +
  geom_line(size=2) +
  geom_smooth(aes(color = 'regression line'),
                  method='lm',
              linetype = 'dashed',
              size = 0.5,
              color = 'black') +
  labs(title = "Fertility Rate of Adolescent Aged 15 - 19 Trend Line per Country Income Categories (1960 - 2016)")
  theme_update()
  ggsave("fertilityratetrendline.png", plot = p, width = 10, height = 6, dpi = 300)


  
  histdata_twoyears <- select(teenage_fr, 
                              Country.Name, 
                              Country.Code, 
                              Indicator.Name, 
                              Indicator.Code, 
                              X1960,
                              X2000)
  
  histdata_twoyears <- gather(teenage_fr, Year, FertilityRate, X1960, X2000) %>%
    select(Year, Country.Name, Country.Code, FertilityRate)
  
  histdata_twoyears <- filter(histdata_twoyears,!is.na(FertilityRate))
  
  ggplot(histdata_twoyears, aes(x=FertilityRate)) + 
    geom_histogram(data=subset(histdata_twoyears, Year=="X1960"), 
                   color="darkred", fill="red", alpha=0.2) + 
    geom_histogram(data=subset(histdata_twoyears, Year=="X2000"), 
                   color="darkblue", fill="blue", alpha=0.2) 
  ggsave("hist2year.png")
  
  

  

  
  ggplot(histdata_twoyears, aes(x=FertilityRate, group=Year, color=Year,
                                alpha=0.2)) + geom_histogram(aes(y=..density..)) +
    geom_density(data=subset(histdata_twoyears, Year=="X1960"),
                 color="darkred", fill="red", alpha=0.2, bw=15)+
    geom_density(data=subset(histdata_twoyears, Year=="X2000"),
                 color="darkblue", fill="blue", alpha=0.2, bw=15) +
    labs(
      title = "Global Shift in Adolescent Fertility Rates (1960–2000))",
      x = "Adolescent Fertility Rate (births per 1,000 women ages 15–19)",
      y = "Density"
    )

  

  ggplot(histdata_twoyears,
         aes(x=FertilityRate,
             color=Year)) +
    stat_ecdf(size=1.2) +
    scale_color_manual(values = c("X1960" = "red", "X2000" = "blue")) +
    labs(
      title = 'Cumulative Distribution of Fertility Rates in 1960 and 2000'
    )
  
    
    
    
    
    
    
    