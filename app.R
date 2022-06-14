#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Link to data
# https://www.kaggle.com/datasets/brandao/diabetes

library(shiny)
library(shinyWidgets)
library(tidyverse)


diabetic_data <- read_csv("./diabetic_data.csv", na = "?")
id_mapping <- read_csv("./IDs_mapping.csv")

# admission_type_id 
admission_type_data <- id_mapping %>%
  filter(row(id_mapping) >= 1 & row(id_mapping) <= 8) %>% 
  mutate(admission_type_id = as.numeric(admission_type_id)) %>% 
  rename(admission_type = description)

## discharge_disposition_id,description
discharge_disposition_data <- id_mapping %>% 
  filter(row(id_mapping) >= 11 & row(id_mapping) <= 40) %>% 
  rename(discharge_disposition_id = admission_type_id) %>% 
  mutate(discharge_disposition_id = as.numeric(discharge_disposition_id)) %>% 
  rename(discharge_disposition = description)

## admission_source_id
admission_source_data <- id_mapping %>%  
  filter(row(id_mapping) >= 43) %>% 
  rename(admission_source_id = admission_type_id) %>% 
  mutate(admission_source_id = as.numeric(admission_source_id)) %>% 
  rename(admission_source = description)

cleaned_databetic_data <- diabetic_data %>% 
  left_join(admission_type_data, by = c("admission_type_id")) %>% 
  left_join(discharge_disposition_data, by = c("discharge_disposition_id")) %>% 
  left_join(admission_source_data, by = c("admission_source_id")) %>% 
  select(-c(admission_type_id, discharge_disposition_id, admission_source_id)) %>% 
  mutate(race = ifelse(race == "AfricanAmerican", "African American", race),
         medical_specialty = ifelse(is.na(medical_specialty), "Unknown", medical_specialty),
         readmitted = case_when(
           readmitted == "NO" ~ "No Readmission",
           readmitted == ">30" ~ "Readmission in greater than 30 days",
           readmitted == "<30" ~ "Readmission in less than 30 days")) 

# Define UI for application
ui <- fluidPage(
  # Application title
  titlePanel(h3("Diabetes in 130 US hospitals (1999-2008)")),
  titlePanel(h5("Information on Data: https://www.kaggle.com/datasets/brandao/diabetes")),
  tabsetPanel(
    tabPanel("Demographics",
             # Sidebar with a slider input for number of bins 
             sidebarLayout(position = "left",
                           sidebarPanel(
                             selectInput("selectDemoChart", label = "Select Chart Demographic", 
                                         choices = c("Race", "Gender", "Age")),
                             pickerInput("selectAdmissionType", label = "Select Admission Type", 
                                         choices = unique(cleaned_databetic_data$admission_type), 
                                         multiple = T, options = list(`actions-box` = TRUE),
                                         selected = unique(cleaned_databetic_data$admission_type)),
                             pickerInput("selectAdmissionSource", label = "Select Admission Source", 
                                         choices = unique(cleaned_databetic_data$admission_source), 
                                         multiple = T, options = list(`actions-box` = TRUE),
                                         selected = unique(cleaned_databetic_data$admission_source)),
                             pickerInput("selectDischargeDisposition", label = "Select Discharge Disposition", 
                                         choices = unique(cleaned_databetic_data$discharge_disposition), 
                                         multiple = T, options = list(`actions-box` = TRUE),
                                         selected = unique(cleaned_databetic_data$discharge_disposition))),
                           mainPanel(
                             plotOutput("demoChart")
                           )
             )
    ),
    tabPanel("Hospital Data",
             sidebarLayout(position = "left",
                           sidebarPanel(
                             sliderInput("selectTimeInHospital", label = "Select Days in Hospital",
                                         min = 1, max = 14, value = c(1, 14)),
                             pickerInput("selectReadmission", label = "Select Readmission Time", 
                                         choices = unique(cleaned_databetic_data$readmitted), 
                                         multiple = T, options = list(`actions-box` = TRUE),
                                         selected = unique(cleaned_databetic_data$readmitted)),
                             pickerInput("selectDiabetesMed", label = "Select Diabetes Medicaiton", 
                                         choices = unique(cleaned_databetic_data$diabetesMed), 
                                         multiple = T, options = list(`actions-box` = TRUE),
                                         selected = unique(cleaned_databetic_data$diabetesMed))),
                           mainPanel(
                             tableOutput("stayInHospital")
                           )
             )
    )
  )
)


# Define server logic
server <- function(input, output) {
  
  ## First Tab Panel
  demographic_diabetic_data <- reactive({cleaned_databetic_data %>% 
    filter(admission_type %in% input$selectAdmissionType & admission_source %in% input$selectAdmissionSource &
             discharge_disposition %in% input$selectDischargeDisposition)})
  
  output$demoChart <- renderPlot({
    if (input$selectDemoChart == "Race") {
      ggplot(demographic_diabetic_data() %>% 
               group_by(race) %>% 
               count() %>% 
               arrange(-n) %>% 
               rename(Race = race,
                      Frequency = n) %>% 
               mutate(Race = ifelse(is.na(Race), "Unknown", Race)), 
             aes(y = Frequency, x = fct_reorder(as.factor(Race), -Frequency)), color = "#1b94e4") +
        geom_col(fill = "#1b94e4") +
        theme_classic() +
        labs(x = "Race", y = "Number of Patients") +
        geom_text(aes(label = Frequency), vjust = -0.5)
    }
    else if (input$selectDemoChart == "Gender") {
      ggplot(demographic_diabetic_data() %>% 
               group_by(gender) %>% 
               count() %>% 
               arrange(-n) %>% 
               rename(Gender = gender,
                      Frequency = n), aes(y = Frequency, x = fct_reorder(as.factor(Gender), -Frequency))) +
        geom_col(fill = "#1b94e4") +
        theme_classic() +
        labs(x = "Gender", y = "Number of Patients") +
        geom_text(aes(label = Frequency), vjust = -0.5)
    }
    else {
      ggplot(demographic_diabetic_data() %>% 
               group_by(age) %>% 
               count() %>% 
               arrange(-n) %>% 
               rename(Age = age,
                      Frequency = n), aes(x = as.factor(Age), 
                                          order = c("[0-10)", "[10-20)", "[20-30)", "[30-40)",
                                                    "[40-50)", "[50-60)","[60-70)",
                                                    "[70-80)", "[80-90)", "[90-100)"), y = Frequency, fill = Frequency)) +
        geom_col() +
        theme_classic() +
        theme(legend.position = "none") +
        labs(x = "Age", y = "Number of Patients") +
        geom_text(aes(label = Frequency), vjust = -0.5)
    }
  }, res = 96)
  
  
  # Second Tab Panel
  hosipital_diabetic_data <- reactive({cleaned_databetic_data %>% 
      select(race, gender, age, time_in_hospital, num_lab_procedures, num_procedures,
             number_emergency, readmitted, medical_specialty, number_diagnoses, admission_type,
             discharge_disposition, admission_source, readmitted, diabetesMed) %>% 
      filter(time_in_hospital >= input$selectTimeInHospital[1] &
               time_in_hospital <= input$selectTimeInHospital[2] &
               readmitted %in% input$selectReadmission &
               diabetesMed %in% input$selectDiabetesMed
               )
  })
           
           
  output$stayInHospital <- renderTable({
    hosipital_diabetic_data() %>% 
      select(race, gender, age, time_in_hospital, num_lab_procedures, num_procedures,
             number_emergency, readmitted, medical_specialty, number_diagnoses) %>% 
      mutate(medical_specialty = ifelse(is.na(medical_specialty), "Unknown", medical_specialty))
  })
}

# Run the application 
shinyApp(ui = ui, server = server, options = list(height = 1300))
