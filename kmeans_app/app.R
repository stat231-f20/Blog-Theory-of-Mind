# loads necessary libraries
library(shiny)
library(shinythemes)
library(tidyverse)
library(dplyr)
library(stringr)
library(magrittr)
library(ggrepel)
library(mdsr)

 # get datasets
 allgroups <- read_csv("allgroups.csv")

#create subsets of the data based on groupings and run kmeans clustering on each grouping
age <- allgroups %>%
  filter(Group == "By Age") %>%
#removing "years" from the age group categories
  separate(Subgroup, into = c("Subgroup", "remove"), sep = " years", convert = TRUE) %>%
  select(-remove) %>%
  mutate(Subgroup = ifelse(Subgroup == "80", "80+", Subgroup))
km_age <- kmeans(age[ , 4:5], centers = 4, nstart = 20)

race <- allgroups %>%
  filter(Group == c("By Race/Hispanic ethnicity"))
km_race <- kmeans(race[ , 4:5], centers = 4, nstart = 20)

gender <- allgroups %>%
  filter(Group == "By Gender")
km_gender <- kmeans(gender[ , 4:5], centers = 4, nstart = 20)

edu <- allgroups %>%
  filter(Group == "By Education")
km_edu <- kmeans(edu[ , 4:5], centers = 4, nstart = 20)

# for selectInput choices, needs to be named list
age_choices <- as.list(c("None", age$Subgroup))
names(age_choices) <- c("None", age$Subgroup)

race_choices <- as.list(c("None", race$Subgroup))
names(race_choices) <- c("None", race$Subgroup)

gender_choices <- as.list(c("None", gender$Subgroup))
names(gender_choices) <- c("None", gender$Subgroup)

edu_choices <- as.list(c("None", edu$Subgroup))
names(edu_choices) <- c("None", edu$Subgroup)

# for checkboxGroupInput
age_choice_scatter <- (age %>%
                           count(Subgroup))$Subgroup
race_choice_scatter <- (race %>%
                         count(Subgroup))$Subgroup
gender_choice_scatter <- (gender %>%
                         count(Subgroup))$Subgroup
edu_choice_scatter <- (edu %>%
                         count(Subgroup))$Subgroup
 
 
# ui 
ui <- fluidPage(theme = shinytheme("journal"),
  
  h1("Exploring with K-Means Clustering"),
  h3("Trends in Anxiety & Depression for Different Demographic Groups"),
  
  tabsetPanel(
    tabPanel(
      "Age",
      sidebarLayout(
        sidebarPanel(
          # creates check boxes for user to filter data by age group
          checkboxGroupInput(inputId = "age_scatter",
                             label = "Select an Age Group",
                             choices = age_choice_scatter,
                             selected = "18 - 29",
                             inline = TRUE)
        ),
        
        mainPanel(
          plotOutput(outputId = "age")
        )
      )
    ),
    tabPanel(
      "Race",
      sidebarLayout(
        sidebarPanel(
          #checkboxes that allow user to select different racial/ethnicity group
          checkboxGroupInput(inputId = "race_scatter",
                             label = "Select a Race/Ethnicity",
                             choices = race_choice_scatter,
                             selected = "Hispanic or Latino",
                             inline = TRUE)
        ),
        
        mainPanel(
          plotOutput(outputId = "race")
        )
      )
    ),
    tabPanel(
      "Gender",
      sidebarLayout(
        sidebarPanel(
          #checkboxes for gender
          checkboxGroupInput(inputId = "gender_scatter",
                             label = "Select a Gender",
                             choices = gender_choice_scatter,
                             selected = "Female",
                             inline = TRUE)
        ),
        
        mainPanel(
          plotOutput(outputId = "gender")
        )
      )
    ),
    tabPanel(
      "Education",
      sidebarLayout(
        sidebarPanel(
          #checkbox to select education level
          checkboxGroupInput(inputId = "edu_scatter",
                             label = "Select an Education Group",
                             choices = edu_choice_scatter,
                             selected = "Bachelor's degree or higher",
                             inline = TRUE)
        ),
        
        mainPanel(
          plotOutput(outputId = "edu")
        )
      )
    )
  )
)


# server
server <- function(input,output){
  #creating reactive data for the plots
  use_data1 <- reactive({
    data <- filter(age, Subgroup %in% input$age_scatter)
  })
  use_data2 <- reactive({
    data <- filter(race, Subgroup %in% input$race_scatter)
  })
  use_data3 <- reactive({
    data <- filter(gender, Subgroup %in% input$gender_scatter)
  })
  use_data4 <- reactive({
    data <- filter(edu, Subgroup %in% input$edu_scatter)
  })
  
  output$age <- renderPlot({
#plot 1: group by age
      ggplot(data = age, aes(x = Depression, y = Anxiety)) + 
        geom_point(aes(color = as.character(km_age$cluster)), alpha = 0.5) +
        # add centroids to plot
        geom_point(data = as.data.frame(km_age$centers), shape = "X", color = "#58616D", size = 3) +
        geom_text_repel(data = use_data1(), aes(label = Subgroup), size = 3) +
        theme_bw() +
        guides(color = FALSE) +
        labs(title = "Patterns of Anxiety and Depression by Age Group")
  })
  output$race <- renderPlot({
  #plots 2: group by race
    ggplot(data = race, aes(x = Depression, y = Anxiety)) + 
      geom_point(aes(color = as.character(km_race$cluster)), alpha = 0.5) +
      # add centroids to plot
      geom_point(data = as.data.frame(km_race$centers), shape = "X", color = "#58616D", size = 3) +
      geom_text_repel(data = use_data2(), aes(label = Subgroup), size = 3) +
      theme_bw() +
      guides(color = FALSE) +
      labs(title = "Patterns of Anxiety and Depression by Race/Ethnicity")
  })
  output$gender <- renderPlot({
  #plot 3: group by gender
    ggplot(data = gender, aes(x = Depression, y = Anxiety)) + 
      geom_point(aes(color = as.character(km_gender$cluster)), alpha = 0.5) +
      # add centroids to plot
      geom_point(data = as.data.frame(km_gender$centers), shape = "X", color = "#58616D", size = 3) +
      geom_text_repel(data = use_data3(), aes(label = Subgroup), size = 3) +
      theme_bw() +
      guides(color = FALSE) +
      labs(title = "Patterns of Anxiety and Depression by Gender")
  })
  output$edu <- renderPlot({
  #plot 4: group by education level  
    ggplot(data = edu, aes(x = Depression, y = Anxiety)) + 
      geom_point(aes(color = as.character(km_edu$cluster)), alpha = 0.5) +
      # add centroids to plot
      geom_point(data = as.data.frame(km_edu$centers), shape = "X", color = "#58616D", size = 3) +
      geom_text_repel(data = use_data4(), aes(label = Subgroup), size = 3) +
      theme_bw() +
      guides(color = FALSE) +
      labs(title = "Patterns of Anxiety and Depression by Education Level")
  })
}

# call to shinyApp
shinyApp(ui = ui, server = server)
