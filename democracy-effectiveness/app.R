library(shiny)
library(shinythemes)
library(tidyverse)

full <- readRDS("full_data.rds")
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
                           
                           #This creates title and main oage of my final project
                           
                           tabPanel("About",
                                    
                                    #here I simply describe what I aimed to do with my project and explain what it is, who I am and what the
                                    # data is
                                    
                                    h1("The Project"),
                                    p("The number of democracies in the world has skyrocketed within the oast centuries. Within the United
                          there is a general notion that anything done within the United States is the best and unimprovable.  
                          This feeling extends to the form of government running the country. So, is Democracy truly the best form
                          of government? How does one test that? In this project I aimto determine if data can help support this claim 
                          As a result, I sought to analyze the effect that a change to a democratic government would have on a country in 
                          terms of factors that I could find data for. These came in variables such as 'Happiness', 'Life Expectancy', 
                          'Population Density', and 'Population' among others. Through viewing the relationships between these sets of 
                          data and the growth of democracy, I hoped to show that democracy has proof to claim that it is a 
                          good form of government."),
                                    h2("The Data"),
                                    p("These visualizations are based on the data sets: 'Democracy' put together by Max
                          Roser in June 2019 'Happiness', put together by Esteban Ortiz and Max Roser, first published in 2013 
                          but revised in 2017 'Life Expectancy', put together by Max Roser in 2015 and 'World Population Growth',
                          put together by Max Roser, Hannah Ritchie and Esteban Ortiz-Ospina in 2019. These data sets can be found
                          in the cata center https://ourworldindata.org"),
                                    h3("About Me"),
                                    p("My name is Diego Flores. I am a first-year from the Bronx, New York on track to concentrate in 
                          Government although that is subject to change."),
                                    p("Github: https://github.com/dieflores2123"))))

server <- function(input, output) {}

shinyApp(ui = ui, server = server)
