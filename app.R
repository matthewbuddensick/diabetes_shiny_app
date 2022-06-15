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
           readmitted == ">30" ~ "Readmission >30 days",
           readmitted == "<30" ~ "Readmission <30 days")) 

# Define UI for application
ui <- fluidPage(
  # Application title
  titlePanel(h3("Diabetes in 130 US hospitals (1999-2008)")),
  tabsetPanel(
    tabPanel("Patient Demographics",
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
                             tableOutput("exampleData"), plotOutput("demoChart")
                           )
             )
    ),
    tabPanel("Patient Data",
             sidebarLayout(position = "left",
                           sidebarPanel(
                             sliderInput("selectTimeInHospital", label = "Select Days in Hospital",
                                         min = 1, max = 14, value = c(1, 14)),
                             selectInput("selectChartY", label = "Select Chart Axis",
                                         choices = c("Race" = "race", "Gender" = "gender", 
                                                     "Age" = "age", "Medical Specialty")),
                             pickerInput("selectReadmission", label = "Select Readmission Time", 
                                         choices = unique(cleaned_databetic_data$readmitted), 
                                         multiple = T, options = list(`actions-box` = TRUE),
                                         selected = unique(cleaned_databetic_data$readmitted)),
                             pickerInput("selectDiabetesMed", label = "Select Diabetes Medicaiton", 
                                         choices = unique(cleaned_databetic_data$diabetesMed), 
                                         multiple = T, options = list(`actions-box` = TRUE),
                                         selected = unique(cleaned_databetic_data$diabetesMed))),
                           mainPanel(
                             plotOutput("readmission"), plotOutput("diagnoses")
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
  
  output$exampleData <- renderTable({
    demographic_diabetic_data() %>% 
      distinct(patient_nbr) %>% 
      mutate(`Total Number of Patients` = nrow(demographic_diabetic_data() %>% 
                                                 distinct(patient_nbr))) %>% 
      slice_max(1) %>% 
      select(`Total Number of Patients`)
  })
  
  output$demoChart <- renderPlot({
    if (input$selectDemoChart == "Race") {
      ggplot(demographic_diabetic_data() %>% 
               select(race, patient_nbr) %>% 
               distinct() %>% 
               group_by(race) %>% 
               count() %>% 
               arrange(-n) %>% 
               rename(Race = race,
                      Frequency = n) %>% 
               mutate(Race = ifelse(is.na(Race), "Unknown", Race)), 
             aes(y = Frequency, x = fct_reorder(as.factor(Race), -Frequency)), color = "#1b94e4") +
        geom_col(fill = "#1b94e4") +
        theme_classic() +
        labs(x = "", y = "", titles = "Patients By Race") +
        geom_text(aes(label = Frequency), vjust = -0.2) +
        theme(axis.text.x = element_text(size = 8), axis.text.y = element_text(size = 8))
    }
    else if (input$selectDemoChart == "Gender") {
      ggplot(demographic_diabetic_data() %>% 
               select(gender, patient_nbr) %>% 
               distinct() %>% 
               group_by(gender) %>% 
               count() %>%  
               arrange(-n) %>% 
               rename(Gender = gender,
                      Frequency = n), aes(y = Frequency, x = fct_reorder(as.factor(Gender), -Frequency))) +
        geom_col(fill = "#1b94e4") +
        theme_classic() +
        labs(x = "", y = "", title = "Patients by Gender") +
        geom_text(aes(label = Frequency), vjust = -0.2) +
        theme(axis.text.x = element_text(size = 8), axis.text.y = element_text(size = 8))
    }
    else {
      ggplot(demographic_diabetic_data() %>% 
               select(age, patient_nbr) %>% 
               distinct() %>% 
               group_by(age) %>% 
               count() %>% 
               rename(Age = age,
                      Frequency = n), aes(x = as.factor(Age), 
                                          order = c("[0-10)", "[10-20)", "[20-30)", "[30-40)",
                                                    "[40-50)", "[50-60)","[60-70)",
                                                    "[70-80)", "[80-90)", "[90-100)"), y = Frequency, fill = Frequency)) +
        geom_col() +
        theme_classic() +
        theme(legend.position = "none") +
        labs(x = "", y = "", title = "Patients by Age") +
        geom_text(aes(label = Frequency), vjust = -0.2) +
        theme(axis.text.x = element_text(size = 8), axis.text.y = element_text(size = 8))
    }
  }, res = 96)
  
  
  # Second Tab Panel
  hosipital_diabetic_data <- reactive({cleaned_databetic_data %>% 
      select(race, gender, age, time_in_hospital, num_lab_procedures, num_procedures, patient_nbr,
             number_emergency, readmitted, medical_specialty, number_diagnoses, admission_type,
             discharge_disposition, admission_source, readmitted, diabetesMed) %>% 
      mutate(medical_specialty = fct_lump(medical_specialty, 10)) %>% 
      filter(time_in_hospital >= input$selectTimeInHospital[1] &
               time_in_hospital <= input$selectTimeInHospital[2] &
               readmitted %in% input$selectReadmission &
               diabetesMed %in% input$selectDiabetesMed
               )
  })
           
           
  output$readmission <- renderPlot({
    if (input$selectChartY == "Medical Specialty") {
      hosipital_diabetic_data() %>% 
        group_by(medical_specialty) %>% 
        count(readmitted) %>% 
        ggplot(aes(y = medical_specialty, x = n, fill = readmitted)) +
        geom_col(position = "fill") +
        scale_fill_brewer(palette = "Blues") +
        theme_classic() +
        labs(x = "", y = "", fill = "Readmission", title = "Patient Readmissions") +
        scale_x_continuous(labels = scales::percent_format()) +
        theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10))
    }
    else {
      hosipital_diabetic_data() %>% 
        group_by(!!as.symbol(input$selectChartY)) %>% 
        count(readmitted) %>%  
        ggplot(aes_string(y = input$selectChartY, x = "n", fill = "readmitted")) +
        geom_col(position = "fill") +
        scale_fill_brewer(palette = "Blues") +
        theme_classic() +
        labs(x = "", y = "", fill = "Readmission", title = "Patient Readmissions") +
        scale_x_continuous(labels = scales::percent_format()) +
        theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10))
    }
  }, res = 96)
  
  output$diagnoses <- renderPlot({
    if (input$selectChartY == "Medical Specialty") {
    hosipital_diabetic_data() %>% 
      group_by(patient_nbr, medical_specialty) %>% 
      summarise(total_diagnoses = sum(number_diagnoses)) %>% 
      group_by(medical_specialty) %>% 
      summarise(avg_diagnoses = mean(total_diagnoses)) %>% 
      ggplot(aes(y = fct_reorder(medical_specialty,  avg_diagnoses), x = avg_diagnoses)) +
      geom_col(fill = "#1b94e4") +
      geom_text(aes(label = round(avg_diagnoses, 2))) +
      labs(x = "", y = "", title = "Average Diagnoses") +
      theme_classic() +
      theme(legend.position = "none") +
      scale_color_distiller()
    }
    else if (input$selectChartY == "Age") {
      hosipital_diabetic_data() %>% 
        group_by(patient_nbr, age) %>% 
        summarise(total_diagnoses = sum(number_diagnoses)) %>% 
        group_by(age) %>% 
        summarise(avg_diagnoses = mean(total_diagnoses)) %>% 
        ggplot(aes(x = as.factor(age), 
                   order = c("[0-10)", "[10-20)", "[20-30)", "[30-40)",
                             "[40-50)", "[50-60)","[60-70)",
                             "[70-80)", "[80-90)", "[90-100)"), y = avg_diagnoses)) +
        geom_col(fill = "#1b94e4") +
        geom_text(aes(label = round(avg_diagnoses, 2), vjust = -0.2)) +
        labs(x = "", y = "", title = "Average Diagnoses") +
        theme_classic() +
        theme(legend.position = "none")
    }
    else {
      hosipital_diabetic_data() %>% 
        group_by(patient_nbr, !!as.symbol(input$selectChartY)) %>% 
        summarise(total_diagnoses = sum(number_diagnoses)) %>% 
        group_by(!!as.symbol(input$selectChartY)) %>% 
        summarise(avg_diagnoses = mean(total_diagnoses)) %>% 
        ggplot(aes(x = fct_reorder(!!as.symbol(input$selectChartY),  -avg_diagnoses), y = avg_diagnoses)) +
        geom_col(fill = "#1b94e4") +
        geom_text(aes(label = round(avg_diagnoses, 2), vjust = -0.2)) +
        labs(x = "", y = "", title = "Average Diagnoses") +
        theme_classic() +
        theme(legend.position = "none")
    }
  }, res = 96)
}

# Run the application 
shinyApp(ui = ui, server = server, options = list(height = 1300))
