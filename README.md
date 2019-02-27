# Visualisation project

Alcohol is one of the most favourite beverages in the world, the indispensable part of every party. Each country has a different drinking behaviour and favourite alcoholic drink, which is affected heavily by geographical, economic and cultural factors. This project targets at alcoholic and travel lovers who are curious about culture, and specifically drinking pattern of different parts of the world.

The visualisation includes 3 main tabs: Favourite alcoholic drink by country, alcohol consumption level by country and data tab

## File description

1. Rscript.R contains the R program to run
2. merge.csv contains processed data from multiple datasource
3. abc.html  contains content and layout of the "favourite map" tab
4. datavis.css contains the code to create interactive effect on maps

## Data source

This project used multiple data sources as follows.

### Recorded alcohol per capita consumption from 2000
Description: Alcohol consumption is defined as annual sales of alcohol in litres per person aged 15 years and older. This index unit is in litres per capita. (World Health Organization, 2018)

- Time series: 2000 - 2015
- Attributes: Name of countries, type of alcohol, data by year
- Link: http://apps.who.int/gho/data/node.main.A1026?lang=en

### GDP per hour worked by nations (current US$)
Description: GDP per hour worked is a measure of labour productivity. It indicates the total GDP per hours worked of all person engaged in the economy production. The measurement unit is USD.  (The World Bank Group, 2017)

Time series: 2000 – 2016
Main attributes: Name of countries, country code, indicator name, indicator code, GDP per capita
URL: https://data.worldbank.org/indicator/sl.gdp.pcap.em.kd

### GDP per capita by nations (current US$)
Description: GDP per capita is calculated by dividing gross domestic product by midyear population of each nation. GDP is the sum of gross value added by all resident producers in the economy plus any product taxes and minus any subsidies not included in the value of the products. The currency unit is USD. (The World Bank Group, 2018)
Time series: 1960 – 2016
Attributes: Name of countries, country code, indicator name, indicator code, GDP per capita
Link: https://data.worldbank.org/indicator/NY.GDP.PCAP.CD
