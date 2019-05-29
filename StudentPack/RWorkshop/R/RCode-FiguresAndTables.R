# 0. Install the packages (To do ONLY the first time we use the package)
  # install.packages("ggplot2")   # To create beautiful graphs ("gg" means "Grammar of Graphics")
  # install.packages("stargazer") # To output LaTeX regression tables from R
  # install.packages("maptools")  # To combine spatial data
  # install.packages("rworldmap") # To map global data
  # install.packages("ggthemes")  # Cool premade ggplot themes


# 1. Load the packages 
  library(ggplot2)   # To create beautiful graphs ("gg" means "Grammar of Graphics")
  library(stargazer) # To output LaTeX regression tables from R
  library(maptools)  # To combine spatial data
  library(rworldmap) # To map global data
  library(ggthemes)  # Cool premade ggplot themes


# 2. Load some data
# About the data: https://fivethirtyeight.com/features/dear-mona-followup-where-do-people-drink-the-most-beer-wine-and-spirits/
  # DataWHO <- read.csv(".../AlcoholDataWHO.csv") # The "..." must be replaced by the path to your data
  # Luckily, these data are also available directly online
  DataWHO <- read.csv("https://raw.githubusercontent.com/fivethirtyeight/data/master/alcohol-consumption/drinks.csv")
  
  
# 3. Explore the Data dataframe
  names(DataWHO) # To see the variables in the dataframe
  head(DataWHO)  # To see the first couple of rows in the dataframe
  

# 4. Create new variables
  # An area variable
  DataWHO$geoArea <- 0
  DataWHO$geoArea[DataWHO$country == "Canada"] <- 1
  DataWHO$geoArea[DataWHO$country == "France" | 
                  DataWHO$country == "United Kingdom" |
                  DataWHO$country == "Spain" | 
                  DataWHO$country == "Italy" |
                  DataWHO$country == "Germany" |
                  DataWHO$country == "Belgium"] <- 2
  
  # Look at the new variable
  table(DataWHO$geoArea)
  

  # Build an additive scale
  DataWHO$alcoholTotal <- DataWHO$beer_servings + DataWHO$spirit_servings + DataWHO$wine_servings
  # Look at the new variable
  summary(DataWHO$alcoholTotal)


# 5. Make a histogram
  ggplot(DataWHO, aes(y=alcoholTotal, x=reorder(country, alcoholTotal), 
                      fill=as.factor(geoArea))) +
        geom_bar(stat = "identity", width=0.5) +
        scale_fill_manual(values=c("0"="grey", "1"="red", "2"="blue"), 
                          labels=c("0"="Others", "1"="Canada", "2"="Europe")) +
        theme_wsj() +
        theme(legend.title=element_blank(),
              axis.text.x=element_text(angle=45, vjust=1, hjust=1, size=4),
              axis.title=element_blank()) 
  



#****************#




# 6. Prepare the data for the map

  # Replace Badly coded countries in df
  DataWHO$country <- as.character(DataWHO$country) # Dealing with factors...
  DataWHO$country[DataWHO$country == "Russian Federation"] <- "Russia"
  DataWHO$country[DataWHO$country == "United Kingdom"] <- "UK"
  DataWHO$country[DataWHO$country == "Congo"] <- "Republic of Congo"
  DataWHO$country[DataWHO$country == "DR Congo"] <- "Democratic Republic of the Congo"

  # Merge the "map data" dataframe with the WHO dataframe
  DataWHO$region <- as.character(DataWHO$country) # Create a "matching" variable
  MapWorldData <- map_data(map='world') # Get the world map drawing data  
  MergedData <- merge(DataWHO, MapWorldData, by='region', all.y=TRUE)
  MergedData <- MergedData[order(MergedData$order), ] # <---

  # Fix the missing code in DataWHO
  #MergedData$region[MergedData$region == "Denmark"] <- "Greenland"

  
# 7. Make a map
  ggplot() + 
    geom_map(data = MergedData, map = MergedData, 
             aes(map_id = region, x=long, y=lat,
                 fill=alcoholTotal)) + 
    coord_quickmap() +
    scale_fill_gradient2("Alcohol Consumption", low ="#ff8630", mid="lightgrey", high = "black", 
                         midpoint = max(DataWHO$alcoholTotal)/2,
                         na.value = "lightgrey") +
    theme_wsj() +
    theme(legend.text = element_text(colour = "black"),
          legend.title = element_text(colour = "black"),
          panel.grid = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.line = element_blank())


# 8. Make a ridiculous non-sense regression
  myModel <- lm(beer_servings ~ wine_servings + spirit_servings, data=DataWHO)
  summary(myModel)
  
  stargazer(myModel)
  
  
  
# 9. An even more ridiculous non-sense regression
  myModel1 <- lm(beer_servings ~ wine_servings + spirit_servings, data=DataWHO)
  myModel2 <- lm(wine_servings ~ alcoholTotal + spirit_servings, data=DataWHO)
  myModel3 <- lm(spirit_servings ~ wine_servings + alcoholTotal, data=DataWHO)
  myModel4 <- lm(alcoholTotal ~ wine_servings + spirit_servings, data=DataWHO)

  stargazer(myModel1, myModel2, myModel3, myModel4,
            title = "My Super Regression Table",
            font.size = "scriptsize")
  
  
  
  
  

