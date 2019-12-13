library(tidyverse)
library(ggplot2)
library(ggthemes)
library(shiny)
library(shinythemes)

full <- read_rds("full_data.rds")
gdp <- read_rds("gdp_percap.rds")
dem_growth <- read_rds("growth_democracy.rds")
hdi_score <- read_rds("hdi.rds")
im <- read_rds("infant_mortality.rds")
happiness <- read_rds("happiness_country.rds")
expectancy <- read_rds("life_expectancy.rds")
regime <- read_rds("regime_type.rds")
dense <- read_rds("world_density.rds")
pop<- read_rds("world_pop.rds")

ui <- fluidPage(theme = shinytheme("superhero"),
                
                # Thi creates the aesthetic theme I put using the shinythemes package
                
                navbarPage("Effects of Democracy Around the World (Is Democracy the Best Form of Government?)",
                           
                           tabPanel("Democracy Growth",
                                    h2("Make-up of Governments in the World"),
                                    selectInput("plot1", label = h5("Select Plot"), 
                                                choices = list("Democracy" = "democracy_line",
                                                               "Autocracy" = "autocracy_line",
                                                               "Democracy + Autocracy" = "both_line"), 
                                                selected =  "Democracy + Autocracy",
                                                multiple = FALSE,
                                                selectize = FALSE,
                                                width = '450px',
                                                size = 1),
                                    plotOutput("demGrowth"),
                                    h3("Analysis"),
                                    p("The general trends of these lines suggest that more and more countries are opting in to switch to democratic regime types. The number of autocracies in the world is
                                      clearly diminishing and it appears as if those countries leaving the autocratic forms of government behind are the ones adopting democracy. ")
                           ),
                           
                           tabPanel("About",
                                    
                                    #here I simply describe what I aimed to do with my project and explain what it is, who I am and what the
                                    # data is
                                    
                                    h1("The Project"),
                                    p("The number of democracies in the world has skyrocketed within the past century. Within the United States
                          there is a general notion that anything done within the United States is the best and unimprovable.  
                          This feeling extends to the form of government running the country. So, is Democracy truly the best form
                          of government? How does one test that? In this project I aim to determine if data can help support this claim. 
                          As a result, I sought to analyze the effect that a change to a democratic government would have on a country in 
                          terms of factors that I could find data for. These came in variables such as 'Happiness', 'Life Expectancy', 
                          'Population Density', and 'Population' among others. Through viewing the relationships between these sets of 
                          data and the growth of democracy, I hoped to show whether democracy has proof or not to claim that it is a 
                          good form of government."),
                                    h2("The Data"),
                                    p("These visualizations are based on the data sets: 'Democracy' put together by Max
                          Roser in June 2019 'Happiness', put together by Esteban Ortiz and Max Roser, first published in 2013 
                          but revised in 2017 'Life Expectancy', put together by Max Roser in 2015 and 'World Population Growth',
                          put together by Max Roser, Hannah Ritchie and Esteban Ortiz-Ospina in 2019. These data sets can be found
                          in the data center https://ourworldindata.org. The World Bank was also a valuable asset in terms of finding
                          data on GDP and regime type which was used to draw further analysis."),
                                    h3("About Me"),
                                    p("My name is Diego Flores. I am a first-year from the Bronx, New York on track to concentrate in 
                          Government although that is subject to change."),
                                    p("Github: https://github.com/dieflores2123")
                                    ),
                           
                           #This creates title and main oage of my final project
                           
                           tabPanel("Democracy & GDP",
                                    p("Select from the box to see a country's GDP growth in relation with democracy."),
                                    selectInput("plot2", label = h5("Select Plot"), 
                                                choices = list("Afghanistan" = "afhganistan_gdp",
                                                               "Indonesia" = "indonesia_gdp",
                                                               "Spain" = "spain_gdp",
                                                               "Slovenia" = "slovenia_gdp",
                                                               "Germany" = "germany_gdp",
                                                               "Croatia" = "croatia_gdp",
                                                               "Albania"= "albania_gdp",
                                                               "Hungary" = "hungary_gdp",
                                                               "Serbia" = "serbia_gdp"), 
                                                selected =  "Afghanistan",
                                                multiple = FALSE,
                                                selectize = FALSE,
                                                width = '450px',
                                                size = 1),
                                    plotOutput("dem_gdp"),
                                    h3("Analysis"),
                                    p("There appears to be a slight relationship between GDP growth and the introduction of democracy into a
                                      country. While these graphics do not show anything concrete, they do seem to suggest that democracy 
                                      helps GDP growth."),
                                    p("A regression analysis will be required in order to fully analyze this relationship and judge if it is 
                                      causal or not.")
                           ),
                           
                           tabPanel("Life Expectancy",
                                    p("Select from the list to see a country's life expectancy in relation to the introduction of democracy."),
                                    selectInput("plot3", label = h5("Select Plot"), 
                                                choices = list("Afghanistan" = "afhgani_le",
                                                               "Sweden" = "sweden_le",
                                                               "Indonesia" = "indonesia_le",
                                                               "Spain" = "spain_le",
                                                               "Slovenia" = "slovenia_le",
                                                               "Germany" = "germany_le",
                                                               "Croatia" = "croatia_le",
                                                               "Albania"= "albania_le",
                                                               "Hungary" = "hungary_le",
                                                               "Serbia" = "serbia_le"), 
                                                selected =  "Afghanistan",
                                                multiple = FALSE,
                                                selectize = FALSE,
                                                width = '450px',
                                                size = 1),
                                    plotOutput("dem_le"),
                                    h3("Analysis"),
                                    p("The overall life expectancy for all of these countries appears to have already been on an upward trend
                                      to begin with before the introduction of democracy. A relationship here seems a bit farfetched as the data suggests
                                      that life expectancy is a factor that increases generally as time goes by.")
                           ), 
                           
                           tabPanel("Human Development Index (HDI)",
                                    p("Select from the list to see a country's HDI in relation to the introduction of democracy."),
                                    selectInput("plot4", label = h5("Select Plot"), 
                                                choices = list("Afghanistan" = "afhganistan_hdi",
                                                               "Indonesia" = "indonesia_hdi",
                                                               "Albania" = "albania_hdi",
                                                               "Serbia" = "serbia_hdi",
                                                               "Croatia" = "croatia_hdi",
                                                               "Tunisia" = "tunisia_hdi",
                                                               "Nepal" = "nepal_hdi"), 
                                                selected =  "Afghanistan",
                                                multiple = FALSE,
                                                selectize = FALSE,
                                                width = '450px',
                                                size = 1),
                                    plotOutput("dem_hdi"),
                                    h3("Analysis"),
                                    p("This is very similar to the life expectancy case. These countries seemed 
                                      to already have an upward trend to begin with with HDI and thus it seems as if the
                                      relationship may not truly be there with democracy.")
                           ),
                           
                           tabPanel("Happiness",
                                    p("Select from the list to see a country's happiness rating in relation to the introduction of democracy."),
                                    selectInput("plot5", label = h5("Select Plot"), 
                                                choices = list("Tunisia" = "tunisia_h",
                                                               "Nigeria" = "nigeria_h", 
                                                               "Pakistan" = "pakistan_h",
                                                               "Nigeria" = "nigeria_h",
                                                               "Malaysia" = "malaysia_h",
                                                               "Sierra Leone" = "sierra_h"), 
                                                selected =  "Tunisia",
                                                multiple = FALSE,
                                                selectize = FALSE,
                                                width = '450px',
                                                size = 1),
                                    plotOutput("dem_hap"),
                                    h3("Analysis"),
                                    p("The overall happiness rating for all of these countries appears to be dependant on more than the factor of 
                                      type of government. As a result, finding a direct relationship with the introduction of democracy and overall 
                                      happiness seems difficult and unlikely here.")
                           ),
                         tabPanel("Infant Mortality",
                                  p("Select from the list to see a country's child mortality rate in relation to the introduction of democracy."),
                                  p("The blue line marks the introduction of Democracy in the country"),
                                  selectInput("plot6", 
                                              label = h5("Select Plot"), 
                                              choices = list("Afghanistan" = "afghanistan_im",
                                                             "Indonesia" = "indonesia_im",
                                                             "Spain" = "spain_im",
                                                             "Slovenia" = "slovenia_im",
                                                             "Germany" = "germany_im",
                                                             "Croatia" = "croatia_im",
                                                             "Albania"= "albania_im",
                                                             "Hungary" = "hungary_im",
                                                             "Serbia" = "serbia_im"), 
                                              selected =  "Afghanistan",
                                              multiple = FALSE,
                                              selectize = FALSE,
                                              width = '450px',
                                              size = 1),
                                  plotOutput("dem_im"),
                                  
                                  
                                  h3("Analysis"),
                                  p("The overall happiness rating for all of these countries appears to be dependant on more than the factor of 
                                      type of government. As a result, finding a direct relationship with the introduction of democracy and overall 
                                      happiness seems difficult and unlikely within these specific graphs. It appears as if for specific countries that did experience 
                                    a direct transition to democracy there is no explicit effect from democratization.")
                         ),
                         
                         tabPanel("Analysis of Variables",
                                  titlePanel("Regression Analysis"),
                                  
                                  # Sidebar layout with input and output definitions ----
                                  sidebarLayout(
                                    
                                    # Sidebar panel for inputs ----
                                    sidebarPanel(
                                      
                                      # Input: Select the random distribution type ----
                                      
                                      
                                      # Input: Slider for the number of observations to generate ----
                                      sliderInput("year",
                                                  "Year for Observations:",
                                                  value = 2010,
                                                  min = 2000,
                                                  max = 2017,
                                                  sep = "")
                                      
                                    ),
                                    
                                    # Main panel for displaying outputs ----
                                    mainPanel(
                                      
                                      # Output: Tabset w/ plot, summary, and table ----
                                      tabsetPanel(type = "tabs",
                                                  tabPanel("GDP", 
                                                           plotOutput("plot7"),
                                                           p("From these graphs it can be seen from the regressions that countries which are democratic tend to have higher GDP per Capitas
                                                             than countries that are not democratic, this appears to be true throughout all years. This finding supports the suggestion from 
                                                             the GDP and Democracy tab where we were inclined to suggest that democracy may aide a country in bettering its economic situation")),
                                                  tabPanel("Life Expectancy", plotOutput("plot8"),
                                                           p("The regressions support the assumption I had made before doing any data analysis although the Life Expectancy tab appears to go against it.
                                                             It would be easy to suggest that democracy leads to higher life expectancy, but our Life Expectancy tab, which analyzes specidic countries
                                                             appears to suggests that these countries were already experiencing increases in Life Expectancy without the presence of democracy. These
                                                             regressions however, alllow us to take a step back and see that overall, in Democratic countries have higher life expectancies than countries
                                                             that are not democratic. This is a similar finding to the other three of our variables we analyzed.")),
                                                  tabPanel("HDI", plotOutput("plot9"),
                                                           p("These regressions appear to tell a similar story to the ones from Life Expectancy. While perhaps the introduction of democracy
                                                             may not have affected the already increasing rate of HDI in specific countries, by taking a broader scope through regressions which
                                                             compare Democratic Countries to those that are not, within specific years, it is easy to see that democratic countries have significantly
                                                             higher scored for HDI. This suggests that there may in fact be a relationship between the two: Democracies tend to have higher HDI")),
                                                  tabPanel("Happiness", plotOutput("plot10"),
                                                           p("Once again these regressions suggest that Democratic Countries appear to be happier than non-democratic countries. While
                                                             the other tab in this project anaylyzing happiness within specific countries before and after democracy was implemented, 
                                                             appeared to find no real relationship, this different apprach does.")),
                                                  tabPanel("Infant Mortality", plotOutput("plot11"),
                                                           p("It appears as if infant mortality rate, within this selected time window, will be low despite the political structure of
                                                             a country. It supports our assumption we made in the IM tab that the IM rate decreases by virtue of the advancement of human
                                                             society as a whole, and should not be attributed to any political system. This is great, to know that IM rate has always been on 
                                                             a general downward trend")),
                                                  tabPanel("Conclusions",
                                                           p("It is pretty clear from our regressions that in the larger picture, the presence of democracy does have an effect
                                                             on variables that we attribute to the prosperity of a society. However, our specific analyses of singular countries
                                                             before and after they became democratic lead me to believe that the adoption of democracy itself is not the sole
                                                             factor at play that affects these variables. Many of the countries that are not democratic simply never have been nor have they tried 
                                                             to be democratic. As a result it can be suggested that perhaps the countries we analysed already had systems in place that allowed
                                                             democracy to be implemented. Perhaps these bettered conditions reflected in the regressions were not because of democracy, but maybe they were
                                                             the conditions necessary for democracy to grow. This suggestsion echoes theories such as that of Lipset's theory of modernization where he
                                                             claims that economic development is the breeding ground for democracy along with improved conditions of survival. That being said however,
                                                             it must be acknowledged that democratic countries, based off of this analysis have overall better conditions of living, thus making them
                                                             better and easier places to live."))
                                      )
                                      
                                    )
                                  )
                         ) 
                                    
                                   
                                    
                                    ))
                

server <- function(input, output) {

  output$demGrowth <- renderPlot({  
    if(input$plot1 == "democracy_line") {
      ggplot(full) + 
        geom_line(data = dem_growth, aes(x = year, y = democracies), 
                  color = "blue") +
        ggtitle("Rise of Democracy Throughout the World") + 
        labs(x = "Year", y = "Number of Democracies") +
        theme_igray()
} else if(input$plot1 == "autocracy_line") { 
    ggplot(full) + 
      geom_line(data = dem_growth, aes(x = year, y = autocracies), 
                color = "red") + 
      ggtitle("Rise of Democracy Throughout the World") + 
      labs(x = "Year", y = "Number of Autocracies") + 
      theme_igray()
  
} else if(input$plot1 == "both_line") { 
    ggplot(full) + 
      geom_line(data = dem_growth, aes(x = year, y = democracies), 
                color = "blue") + 
      geom_line(data = dem_growth, aes(x = year, y = autocracies), 
                color = "red") + 
      ggtitle("Rise of Democracy Throughout the World") + 
      labs(x = "Year", y = "Number of Democracies/Autocracies") + 
      theme_igray()
}
  })
  
  output$dem_gdp <- renderPlot({
    if(input$plot2 == "afhganistan_gdp") {
      
     afg_gdp <- gdp %>%
        filter(entity == "Afghanistan")
     
        ggplot(afg_gdp, aes(x = year, y = value )) + 
        geom_line(color = "red") +
        ggtitle("Afghanistan GDP per Capita") +
        labs(x = "Year", y = "Value of GDP per Capita") +
        theme_igray() +
        geom_vline(xintercept = 1988, color = "blue")
  
} else if(input$plot2 == "indonesia_gdp") {
  
  ind_gdp <- gdp %>% 
    filter(entity == "Indonesia")
  ind_gdp %>% 
    ggplot(aes(x = year, y = value)) +
    geom_line(color = "red") +
    ggtitle("Indonesian GDP per Capita") +
    labs(x = "Year", y = "Value of GDP per Capita") +
    theme_igray() +
    geom_vline(xintercept = 1999, color = "blue")

} else if(input$plot2 == "spain_gdp") {
  
  spa_gdp <-  gdp %>% 
    filter(entity == "Spain")
  spa_gdp %>% 
    ggplot(aes(x = year, y = value)) +
    geom_line(color = "red") +
    ggtitle("Spanish GDP per Capita") +
    labs(x = "Year", y = "Value of GDP per Capita") +
    theme_igray() +
    geom_vline(xintercept = 1978, color = "blue")
  
} else if(input$plot2 == "slovenia_gdp") {
  
  slo_gdp <- gdp %>%
    filter(entity == "Slovenia") 
  slo_gdp %>% 
    ggplot(aes(x = year, y = value)) +
    geom_line(color = "red") +
    ggtitle("Slovene GDP per Capita") +
    labs(x = "Year", y = "Value of GDP per Capita") +
    theme_igray() +
    geom_vline(xintercept = 1991, color = "blue")
  
} else if(input$plot2 == "germany_gdp") {
  
  ger_gdp <- gdp %>% 
    filter(entity == "Germany")
  ger_gdp %>% 
    ggplot(aes(x = year, y = value)) +
    geom_line(color = "red") +
    ggtitle("German GDP per Capita") +
    labs(x = "Year", y = "Value of GDP per Capita") +
    theme_igray() +
    geom_vline(xintercept = 1990, color = "blue")

} else if(input$plot2 == "croatia_gdp") {
  
  cro_gdp <- gdp %>% 
    filter(entity == "Croatia")
  cro_gdp %>% 
    ggplot(aes(x = year, y = value)) +
    geom_line(color = "red") +
    ggtitle("Croatian GDP per Capita") +
    labs(x = "Year", y = "Value of GDP per Capita") +
    theme_igray() +
    geom_vline(xintercept = 2000, color = "blue")
  
} else if(input$plot2 == "albania_gdp") {
  
  alb_gdp <- gdp %>% 
    filter(entity == "Albania")
  alb_gdp %>% 
    ggplot(aes(x = year, y = value)) +
    geom_line(color = "red") +
    ggtitle("Albanian GDP per Capita") +
    labs(x = "Year", y = "Value of GDP per Capita") +
    theme_igray() +
    geom_vline(xintercept = 2002, color = "blue")
  
} else if(input$plot2 == "hungary_gdp") {
  
  hun_gdp <- gdp %>% 
    filter(entity == "Hungary")
  hun_gdp %>% 
    ggplot(aes(x = year, y = value)) +
    geom_line(color = "red") +
    ggtitle("Hungarian GDP per Capita") +
    labs(x = "Year", y = "Value of GDP per Capita") +
    theme_igray() +
    geom_vline(xintercept = 1990, color = "blue")
  
} else if(input$plot2 == "serbia_gdp") {
 
   serb_gdp <- gdp %>% 
     filter(entity == "Serbia")
   serb_gdp %>% 
     ggplot(aes(x = year, y = value)) +
     geom_line(color = "red") +
     ggtitle("Serbian GDP per Capita") +
     labs(x = "Year", y = "Value of GDP per Capita") +
     theme_igray() +
     geom_vline(xintercept = 2006, color = "blue")
}
  })
  
  
  output$dem_le <- renderPlot({
    if(input$plot3 == "afhgani_le") {
      afg_le <- expectancy %>%
        filter(entity == "Afghanistan")
      afg_le  %>% 
        ggplot(aes(x = year, y = expectancy)) + 
        geom_line(color = "red") + 
        ggtitle("Afgani Life Expectancy") + 
        labs(x = "Year", y = "Expected Age of Death") + 
        theme_igray() + 
        geom_vline(xintercept = 1988, color = "blue")

} else if(input$plot3 ==  "sweden_le") {
      swe_le <- expectancy %>% 
        filter(entity == "Sweden") %>% 
        filter(year > 1890)
      swe_le %>% ggplot(aes(x = year, y = expectancy)) + 
        geom_line(color = "red") + 
        ggtitle("Swedish Life Expectancy") +
        labs(x = "Year", y = "Expected Age of Death") + 
        theme_igray() + 
        geom_vline(xintercept = 1920, color = "blue")
      
} else if(input$plot3 ==  "indonesia_le") {
      ind_le <- expectancy %>% 
        filter(entity == "Indonesia") %>% 
        filter(year > 1960)
      
      ind_le %>% ggplot(aes(x = year, y = expectancy)) +
        geom_line(color = "red") + 
        ggtitle("Indonesian Life Expectancy") + 
        labs(x = "Year", y = "Expected Age of Death") + 
        theme_igray() +
        geom_vline(xintercept = 1999, color = "blue")

} else if(input$plot3 ==  "spain_le") {
      spa_le <- expectancy %>% 
        filter(entity == "Spain")
      
      spa_le %>% ggplot(aes(x = year, y = expectancy)) +
        geom_line(color = "red") + 
        ggtitle("Spanish Life Expectancy") + 
        labs(x = "Year", y = "Expected Age of Death") + 
        theme_igray() +
        geom_vline(xintercept = 1978, color = "blue")
      
} else if(input$plot3 ==  "slovenia_le") {  
      slo_le <- expectancy %>% 
        filter(entity == "Slovenia")
      slo_le %>% 
        ggplot(aes(x = year, y = expectancy)) +
        geom_line(color = "red") + 
        ggtitle("Slovene Life Expectancy") + 
        labs(x = "Year", y = "Expected Age of Death") + 
        theme_igray() +
        geom_vline(xintercept = 1991, color = "blue")
      
} else if(input$plot3 ==  "germany_le") {  
      ger_le <- expectancy %>% 
        filter(entity == "Germany")
      ger_le %>% 
        ggplot(aes(x = year, y = expectancy)) +
        geom_line(color = "red") + 
        ggtitle("German Life Expectancy") + 
        labs(x = "Year", y = "Expected Age of Death") + 
        theme_igray() +
        geom_vline(xintercept = 1990, color = "blue")
      
} else if(input$plot3 ==  "croatia_le") { 
      cro_le <- expectancy %>% 
        filter(entity == "Croatia")
      cro_le %>% 
        ggplot(aes(x = year, y = expectancy)) +
        geom_line(color = "red") + 
        ggtitle("Croatian Life Expectancy") + 
        labs(x = "Year", y = "Expected Age of Death") + 
        theme_igray() +
        geom_vline(xintercept = 2000, color = "blue")
    
} else if(input$plot3 ==  "albania_le") { 
      alb_le <- expectancy %>% 
        filter(entity == "Albania")
      alb_le %>%
        ggplot(aes(x = year, y = expectancy)) +
        geom_line(color = "red") + 
        ggtitle("Albanian Life Expectancy") + 
        labs(x = "Year", y = "Expected Age of Death") + 
        theme_igray() +
        geom_vline(xintercept = 2002, color = "blue")

} else if(input$plot3 ==  "hungary_le") { 
      hun_le <- expectancy %>% 
        filter(entity == "Hungary")
      hun_le %>% 
        ggplot(aes(x = year, y = expectancy)) +
        geom_line(color = "red") + 
        ggtitle("Albanian Life Expectancy") + 
        labs(x = "Year", y = "Expected Age of Death") + 
        theme_igray() +
        geom_vline(xintercept = 1990, color = "blue")
      
} else if(input$plot3 ==  "serbia_le") { 
      ser_le <- expectancy %>% 
        filter(entity == "Serbia")
      ser_le %>% 
        ggplot(aes(x = year, y = expectancy)) +
        geom_line(color = "red") + 
        ggtitle("Serbian Life Expectancy") + 
        labs(x = "Year", y = "Expected Age of Death") + 
        theme_igray() +
        geom_vline(xintercept = 1990, color = "blue")
      
}
  })

  
  output$dem_hdi <- renderPlot({
    if(input$plot4 == "afhganistan_hdi") {
      afg_hdi <- hdi_score %>%
        filter(entity == "Afghanistan")
      afg_hdi  %>% 
        ggplot(aes(x = year, y = hdi)) + 
        geom_line(color = "red") + 
        ggtitle("Afgani HDI Scores") + 
        labs(x = "Year", y = "HDI Score") + 
        theme_igray() + 
        geom_vline(xintercept = 1988, color = "blue")

} else if(input$plot4 ==  "indonesia_hdi") {
      ind_hdi <- hdi_score %>% 
        filter(entity == "Indonesia")
      ind_hdi %>% ggplot(aes(x = year, y = hdi)) +
        geom_line(color = "red") + 
        ggtitle("Indonesian HDI Score") + 
        labs(x = "Year", y = "HDI Score") + 
        theme_igray() +
        geom_vline(xintercept = 1999, color = "blue")

} else if(input$plot4 == "albania_hdi") {
      alb_hdi <- hdi_score %>% 
        filter(entity == "Albania")
      alb_hdi %>% 
        ggplot(aes(x = year, y = hdi)) +
        geom_line(color = "red") +
        ggtitle("Albanian HDI Score") +
        labs(x = "Year", y = "HDI Score") +
        theme_igray() +
        geom_vline(xintercept = 2002, color = "blue")
      
} else if(input$plot4 ==  "serbia_hdi") { 
      ser_hdi<- hdi_score %>% 
       filter(entity == "Serbia")
      ser_hdi %>% 
        ggplot(aes(x = year, y = hdi)) +
        geom_line(color = "red") + 
        ggtitle("Serbian HDI Score") + 
        labs(x = "Year", y = "HDI Score") + 
        theme_igray() +
        geom_vline(xintercept = 2006, color = "blue")
      
} else if(input$plot4 ==  "croatia_hdi") { 
      cro_hdi <- hdi_score %>% 
        filter(entity == "Croatia")
      cro_hdi %>% 
        ggplot(aes(x = year, y = hdi)) +
        geom_line(color = "red") + 
        ggtitle("Croatian HDI Score") + 
        labs(x = "Year", y = "HDI Score") + 
        theme_igray() +
        geom_vline(xintercept = 2000, color = "blue")
      
} else if(input$plot4 ==  "tunisia_hdi") { 
      tun_hdi <- hdi_score %>% 
        filter(entity == "Croatia")
      tun_hdi %>% 
        ggplot(aes(x = year, y = hdi)) +
        geom_line(color = "red") + 
        ggtitle("Tunisian HDI Score") + 
        labs(x = "Year", y = "HDI Score") + 
        theme_igray() +
        geom_vline(xintercept = 2013, color = "blue")
      
} else if(input$plot4 ==  "nepal_hdi") {
      nep_hdi <- hdi_score %>% 
        filter(entity == "Nepal")
      nep_hdi %>% 
        ggplot(aes(x = year, y = hdi))+
        geom_line(color = "red") + 
        ggtitle("Nepalese HDI Score") + 
        labs(x = "Year", y = "HDI Score") + 
        theme_igray() +
        geom_vline(xintercept = 2008, color = "blue")
      
}
  })
  
  
  output$dem_hap <- renderPlot({
    if(input$plot5 == "tunisia_h") {
      tun_h <- happiness %>%
        filter(entity == "Tunisia")
      tun_h  %>% 
        ggplot(aes(x = year, y = cantril_score)) + 
        geom_line(color = "red") + 
        ggtitle("Tunisia Cantril Score Over Time") + 
        labs(x = "Year", y = "Cantril Score") + 
        theme_igray() + 
        geom_vline(xintercept = 2013, color = "blue")
      
      
} else if(input$plot5 ==  "nigeria_h") { 
      nig_h <- happiness %>% 
        filter(entity == "Nigeria")
      nig_h %>% 
        ggplot(aes(x = year, y = cantril_score))  + 
        geom_line(color = "red") + 
        ggtitle("Nigeria Cantril Score Over Time") + 
        labs(x = "Year", y = "Cantril Score") + 
        theme_igray() + 
        geom_vline(xintercept = 2015, color = "blue")

} else if(input$plot5 ==  "malaysia_h") { 
      mal_h <- happiness %>% 
        filter(entity == "Malaysia")
      mal_h %>% 
        ggplot(aes(x = year, y = cantril_score))  + 
        geom_line(color = "red") + 
        ggtitle("Malaysia Cantril Score Over Time") + 
        labs(x = "Year", y = "Cantril Score") + 
        theme_igray() + 
        geom_vline(xintercept = 2008, color = "blue")
      
} else if(input$plot5 ==  "sierra_h") { 
      sie_h <- happiness %>% 
        filter(entity == "Sierra Leone")
      sie_h %>% 
        ggplot(aes(x = year, y = cantril_score))  + 
        geom_line(color = "red") + 
        ggtitle("Sierra Leone Cantril Score Over Time") + 
        labs(x = "Year", y = "Cantril Score") + 
        theme_igray() + 
        geom_vline(xintercept = 2007, color = "blue")
  
} else if(input$plot5 ==  "malaysia_h") { 
      mal_h <- happiness %>% 
        filter(entity == "Malaysia")
      mal_h %>% 
        ggplot(aes(x = year, y = cantril_score))  + 
        geom_line(color = "red") + 
        ggtitle("Malaysia Cantril Score Over Time") + 
        labs(x = "Year", y = "Cantril Score") + 
        theme_igray() + 
        geom_vline(xintercept = 2008, color = "blue")
      
} else if(input$plot5 ==  "pakistan_h") { 
      pak_h <- happiness %>% 
        filter(entity == "Pakistan")
      pak_h %>% 
        ggplot(aes(x = year, y = cantril_score))  + 
        geom_line(color = "red") + 
        ggtitle("Pakistan Cantril Score Over Time") + 
        labs(x = "Year", y = "Cantril Score") + 
        theme_igray() + 
        geom_vline(xintercept = 2010, color = "blue")
      
}
  })

  output$dem_im <- renderPlot({
    if(input$plot6 == "afghanistan_im") {
      afg_im <- im %>%
        filter(entity == "Afghanistan")
      afg_im  %>% 
        ggplot(aes(x = year, y = rate)) + 
        geom_line(color = "red") + 
        ggtitle("Afghanistan Infant Mortality Rate") + 
        labs(x = "Year", y = "Number of Infant Deaths per Year") + 
        theme_igray() + 
        geom_vline(xintercept = 1988, color = "blue")

} else if(input$plot6 == "indonesia_im") {
  
  ind_im <- im  %>% 
    filter(entity == "Indonesia")
  ind_im %>% 
    ggplot(aes(x = year, y = rate)) +
    geom_line(color = "red") +
    ggtitle("Indonesian Infant Mortality Rate") +
    labs(x = "Year", y = "Number of Infant Deaths per Year") +
    theme_igray() +
    geom_vline(xintercept = 1999, color = "blue")
  
} else if(input$plot6 == "spain_im") {
  
  spa_im <-  im  %>% 
    filter(entity == "Spain")
  spa_im %>% 
    ggplot(aes(x = year, y = rate)) +
    geom_line(color = "red") +
    ggtitle("Spanish Infant Mortality Rate") +
    labs(x = "Year", y = "Number of Infant Deaths per Year") +
    theme_igray() +
    geom_vline(xintercept = 1978, color = "blue")
  
} else if(input$plot6 == "slovenia_im") {
  
  slo_im <- im %>%
    filter(entity == "Slovenia") 
  slo_im %>% 
    ggplot(aes(x = year, y = rate)) +
    geom_line(color = "red") +
    ggtitle("Slovene Infant Mortality Rate") +
    labs(x = "Year", y = "Number of Infant Deaths per Year") +
    theme_igray() +
    geom_vline(xintercept = 1991, color = "blue")
  
} else if(input$plot6 == "germany_im") {
  
  ger_im <- im %>% 
    filter(entity == "Germany")
  ger_im %>% 
    ggplot(aes(x = year, y = rate)) +
    geom_line(color = "red") +
    ggtitle("German Infant Mortality Rate") +
    labs(x = "Year", y = "Number of Infant Deaths per Year") +
    theme_igray() +
    geom_vline(xintercept = 1990, color = "blue")
      
  
} else if(input$plot6 == "croatia_im") {
  
  cro_im <- im %>% 
    filter(entity == "Croatia")
  cro_im %>% 
    ggplot(aes(x = year, y = rate)) +
    geom_line(color = "red") +
    ggtitle("Croatian Infnat Mortality Rate") +
    labs(x = "Year", y = "Number of Infant Deaths per Year") +
    theme_igray() +
    geom_vline(xintercept = 2000, color = "blue")
  
} else if(input$plot6 == "albania_im") {
  
  alb_im <- im %>% 
    filter(entity == "Albania")
  alb_im %>% 
    ggplot(aes(x = year, y = rate)) +
    geom_line(color = "red") +
    ggtitle("Albanian Infant Mortality Rate") +
    labs(x = "Year", y = "Number of Infant Deaths per Year") +
    theme_igray() +
    geom_vline(xintercept = 2002, color = "blue")
  
} else if(input$plot6 == "hungary_im") {
  hun_im <- im %>% 
    filter(entity == "Hungary")
  hun_im %>% 
    ggplot(aes(x = year, y = rate)) +
    geom_line(color = "red") +
    ggtitle("Hungarian Infnat Mortality Rate") +
    labs(x = "Year", y = "Number of Infant Deaths per Year") +
    theme_igray() +
    geom_vline(xintercept = 1990, color = "blue")

} else if(input$plot6 == "serbia_im") {
  serb_im <- im %>% 
    filter(entity == "Serbia")
  serb_im %>% 
    ggplot(aes(x = year, y = rate)) +
    geom_line(color = "red") +
    ggtitle("Serbian Infant Mortality Rate") +
    labs(x = "Year", y = "Number of Infant Deaths per Year") +
    theme_igray() +
    geom_vline(xintercept = 2006, color = "blue")
  
}
  })
  
  output$plot7 <- renderPlot({
    year <- input$year
    
    full %>% filter(year == year) %>% 
      ggplot(aes(x = democracy, y = value)) + geom_point() + 
      geom_jitter(height = 0.2, width = 0.2) +
      geom_smooth(method = "lm", se = FALSE)  + labs(y = "Value", x = "Non-Democratic (0) or Democratic (1)") +
      ggtitle("GDP per Capita of Democratic vs Non-Democratic Countries")
    
    

  })
  
  # Generate a summary of the data ----
  output$plot8 <- renderPlot({
    year <- input$year
    full %>% filter(year == year) %>% 
      ggplot(aes(x = democracy, y = expectancy)) + geom_point() + 
      geom_jitter(height = 0.2, width = 0.2) +
      geom_smooth(method = "lm", se = FALSE)  + 
      labs(y = "Life Expectancy", x = "Non-Democratic (0) or Democratic (1)") +
      ggtitle("Life Expectancy of Democratic vs Non-Democratic Countries")
    
  })
  
  # Generate an HTML table view of the data ----
  output$plot9 <- renderPlot({
    year <- input$year
    full %>% filter(year == year) %>% 
      ggplot(aes(x = democracy, y = hdi)) + geom_point() + 
      geom_jitter(height = 0.2, width = 0.2) +
      geom_smooth(method = "lm", se = FALSE)  + labs(y = "Value", x = "Non-Democratic (0) or Democratic (1)") +
      ggtitle("HDI of Democratic vs Non-Democratic Countries")
  })
  
  output$plot10 <- renderPlot({
    year <- input$year
    full %>% filter(year == year) %>% 
      ggplot(aes(x = democracy, y = cantril_score)) + geom_point() + 
      geom_jitter(height = 0.2, width = 0.2) +
      geom_smooth(method = "lm", se = FALSE)  + labs(y = "Value", x = "Non-Democratic (0) or Democratic (1)")+
      ggtitle("Happiness of Democratic vs Non-Democratic Countries")
  })
  output$plot11 <- renderPlot({
    year <- input$year
    full %>% filter(year == year) %>% 
      ggplot(aes(x = democracy, y = rate)) + geom_point() + 
      geom_jitter(height = 0.2, width = 0.2) +
      geom_smooth(method = "lm", se = FALSE)  + labs(y = "Value", x = "Non-Democratic (0) or Democratic (1)") +
      ggtitle("Infant Mortality Rates of Democratic vs Non-Democratic Countries")

  })
}
  
shinyApp(ui = ui, server = server)
