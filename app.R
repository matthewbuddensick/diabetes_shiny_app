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
library(tidymodels)
library(tidyverse)
library(xgboost)


cleaned_diabetic_data <- read_csv("Data/cleaned_diabetic_data.csv")
readmission_model <- readRDS("Model/readmission_model.rds")

# Define UI for application
ui <- fluidPage(
  # Application title
  titlePanel(h3("Diabetes in 130 US hospitals (1999-2008)")),
  tabsetPanel(
    tabPanel("Patient Demographics",
             # Sidebar with a slider input for number of bins 
             sidebarLayout(position = "left",
                           sidebarPanel(
                             tableOutput("exampleData"),
                             selectInput("selectDemoChart", label = "Select Chart Demographic", 
                                         choices = c("Race" = "race", "Gender" = "gender", 
                                                     "Age" = "age")),
                             pickerInput("selectAdmissionType", label = "Select Admission Type", 
                                         choices = unique(cleaned_diabetic_data$admission_type), 
                                         multiple = T, options = list(`actions-box` = TRUE),
                                         selected = unique(cleaned_diabetic_data$admission_type)),
                             pickerInput("selectAdmissionSource", label = "Select Admission Source", 
                                         choices = unique(cleaned_diabetic_data$admission_source), 
                                         multiple = T, options = list(`actions-box` = TRUE),
                                         selected = unique(cleaned_diabetic_data$admission_source)),
                             pickerInput("selectDischargeDisposition", label = "Select Discharge Disposition", 
                                         choices = unique(cleaned_diabetic_data$discharge_disposition), 
                                         multiple = T, options = list(`actions-box` = TRUE),
                                         selected = unique(cleaned_diabetic_data$discharge_disposition))),
                           mainPanel(
                             plotOutput("demoChart"), dataTableOutput("sampleData")
                           )
             )
    ),
    tabPanel("Patient-Hospital Data",
             sidebarLayout(position = "left",
                           sidebarPanel(
                             sliderInput("selectTimeInHospital", label = "Select Days in Hospital",
                                         min = 1, max = 14, value = c(1, 14)),
                             selectInput("selectChartY", label = "Select Chart Axis",
                                         choices = c("Race" = "race", "Gender" = "gender", 
                                                     "Age" = "age", "Medical Specialty")),
                             selectInput("bottomChart", label = "Select Bottom Chart Dimension",
                                         choices = c("Diagnoses" = "diagnosis", "Encounters" = "encounters")),
                             pickerInput("selectReadmission", label = "Select Readmission Time", 
                                         choices = unique(cleaned_diabetic_data$readmitted), 
                                         multiple = T, options = list(`actions-box` = TRUE),
                                         selected = unique(cleaned_diabetic_data$readmitted)),
                             pickerInput("selectDiabetesMed", label = "Select Diabetes Medication", 
                                         choices = unique(cleaned_diabetic_data$diabetesMed), 
                                         multiple = T, options = list(`actions-box` = TRUE),
                                         selected = unique(cleaned_diabetic_data$diabetesMed)),
                             tableOutput("numberPatients")),
                           mainPanel(
                             plotOutput("readmission"), plotOutput("diagnoses")
                           )
             )
    ),
    tabPanel("Readmission Prediction",
             fluidRow(
               column(3, numericInputIcon("modelTimeInHospital",
                                     label = "Select Days in Hospital",
                                     min = 1, max = 14, value = 1, width = "100%",
                                     help_text = "Value must be between 1 and 14")),
               column(3, selectInput("modelRace", label = "Select Race", 
                                     choices = unique(cleaned_diabetic_data$race), width = "100%")),
               column(3, selectInput("modelGender", label = "Select Gender",
                                     choices = unique(cleaned_diabetic_data$gender), width = "100%")),
               column(3, selectInput("modelAge", label = "Select Age",
                                     choices = unique(cleaned_diabetic_data$age), width = "100%"))
             ),
             fluidRow(
               column(3, selectInput("modelMaxGluSerum",
                                     label = "Select Glucose Serum Test Result",
                                     choices = unique(cleaned_diabetic_data$max_glu_serum), width = "100%")),
               column(3, selectInput("modelA1Cresult", label = "Select A1C Test Result", 
                                     choices = unique(cleaned_diabetic_data$A1Cresult), width = "100%")),
               column(3, selectInput("modelMed", label = "Select Diabetes Medication",
                                     choices = unique(cleaned_diabetic_data$diabetesMed), width = "100%")),
               column(3, selectInput("modelChangeMed", label = "Select Change in Medication",
                                     choices = c("No Change" = "No", "Changed Medication" = "Ch"), width = "100%"))
             ),
             fluidRow(
               column(3, selectInput("modelMedicalSpeciality",
                                     label = "Select Medical Specialty",
                                     choices = unique(cleaned_diabetic_data$medical_specialty), width = "100%")),
               column(3, selectInput("modelAdmissionType",
                                     label = "Select Admission Type",
                                     choices = unique(cleaned_diabetic_data$admission_type), width = "100%")),
               column(3, selectInput("modelAdmissionSource",
                                     label = "Select AdmissionSource",
                                     choices = unique(cleaned_diabetic_data$admission_source), width = "100%")),
               column(3, selectInput("modelDischargeDisposition",
                                     label = "Select Discharge Disposition",
                                     choices = unique(cleaned_diabetic_data$discharge_disposition), width = "100%"))
             ),
             fluidRow(
               column(3, numericInputIcon("modelNumLabProcedures",
                                          label = "Select Number of Lab Procedures",
                                          min = 1, max = 132, value = 1, width = "100%",
                                          help_text = "Value must be between 1 and 132")),
               column(3, numericInputIcon("modelOutpatient",
                                          label = "Select Number of Outpatient Visits",
                                          min = 0, max = 42, value = 1, width = "100%",
                                          help_text = "Value must be between 0 and 42")),
               column(3, numericInputIcon("modelInpatient",
                                          label = "Select Number of Inpatient Visits",
                                          min = 0, max = 21, value = 1, width = "100%",
                                          help_text = "Value must be between 0 and 21")),
               column(3, numericInputIcon("modelEmergency",
                                          label = "Select Number of Emergency Visits",
                                          min = 0, max = 76, value = 1, width = "100%",
                                          help_text = "Value must be between 0 and 76"))
               
             ),
             fluidRow(
               tableOutput("prediction"), tableOutput("predictionProb")
             )
    )
  )
)


# Define server logic
server <- function(input, output) {
  
  ## First Tab Panel
  demographic_diabetic_data <- reactive({cleaned_diabetic_data %>% 
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
  
  output$sampleData <- renderDataTable({
    demographic_diabetic_data() %>% 
      select(patient_nbr, race, gender, age, admission_source, admission_type, discharge_disposition,
             time_in_hospital)
  }, options = list(pageLength = 5))
  
  output$demoChart <- renderPlot({
    if (input$selectDemoChart == "race") {
      ggplot(demographic_diabetic_data() %>% 
               select(race, patient_nbr) %>% 
               distinct() %>% 
               group_by(race) %>% 
               count() %>% 
               arrange(-n) %>% 
               rename(Frequency = n) %>% 
               mutate(race = ifelse(is.na(race), "Unknown", race)), 
             aes(y = Frequency, x = fct_reorder(as.factor(race), -Frequency)), color = "#1b94e4") +
        geom_col(fill = "#1b94e4") +
        theme_classic() +
        labs(x = "", y = "", titles = "Patients") +
        geom_text(aes(label = Frequency), vjust = -0.2) +
        theme(axis.text.x = element_text(size = 8), axis.text.y = element_text(size = 8))
    }
    else if (input$selectDemoChart == "gender") {
      ggplot(demographic_diabetic_data() %>% 
               select(gender, patient_nbr) %>% 
               distinct() %>% 
               group_by(gender) %>% 
               count() %>%  
               arrange(-n) %>% 
               rename(Frequency = n), 
             aes(y = Frequency, x = fct_reorder(as.factor(gender), -Frequency))) +
        geom_col(fill = "#1b94e4") +
        theme_classic() +
        labs(x = "", y = "", title = "Patients") +
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
                      Frequency = n), aes(x = fct_inorder(Age), y = Frequency, fill = Frequency)) +
        geom_col() +
        theme_classic() +
        theme(legend.position = "none") +
        labs(x = "", y = "", title = "Patients") +
        geom_text(aes(label = Frequency), vjust = -0.2) +
        theme(axis.text.x = element_text(size = 8), axis.text.y = element_text(size = 8))
    }
  }, res = 96)
  
  # Second Tab Panel
  hosipital_diabetic_data <- reactive({cleaned_diabetic_data %>% 
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
           
  output$numberPatients <- renderTable({
    if (input$selectChartY == "Medical Specialty") {
      hosipital_diabetic_data() %>% 
        group_by(medical_specialty) %>% 
        count() %>% 
        rename(Patients = n) %>% 
        rename_with(tools::toTitleCase) %>% 
        arrange(desc(Patients))
    }
    else {
    hosipital_diabetic_data() %>% 
      group_by(!!as.symbol(input$selectChartY)) %>% 
      count() %>% 
      rename(Patients = n) %>% 
      rename_with(tools::toTitleCase) %>% 
      arrange(desc(Patients))
    }
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
    if (input$bottomChart == "diagnosis") {
      if (input$selectChartY == "Medical Specialty") {
        hosipital_diabetic_data() %>% 
          group_by(patient_nbr, medical_specialty) %>% 
          summarise(total_diagnoses = sum(number_diagnoses)) %>% 
          group_by(medical_specialty) %>% 
          summarise(avg_diagnoses = mean(total_diagnoses)) %>% 
          ggplot(aes(y = fct_reorder(medical_specialty,  avg_diagnoses), x = avg_diagnoses)) +
          geom_col(fill = "#1b94e4") +
          geom_text(aes(label = round(avg_diagnoses, 2), hjust = 1)) +
          labs(x = "", y = "", title = "Average Patient Diagnoses") +
          theme_classic() +
          theme(legend.position = "none") +
          scale_color_distiller()
      }
      else if (input$selectChartY == "age") {
        hosipital_diabetic_data() %>% 
          group_by(patient_nbr, age) %>% 
          summarise(total_diagnoses = sum(number_diagnoses)) %>% 
          group_by(age) %>% 
          summarise(avg_diagnoses = mean(total_diagnoses)) %>% 
          ggplot(aes(x = fct_inorder(age), y = avg_diagnoses, fill = avg_diagnoses)) +
          geom_col() +
          geom_text(aes(label = round(avg_diagnoses, 2), vjust = -0.2)) +
          labs(x = "", y = "", title = "Average Patient Diagnoses") +
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
          labs(x = "", y = "", title = "Average Patient Diagnoses") +
          theme_classic() +
          theme(legend.position = "none")
      }
    }
    else {
      if (input$selectChartY == "Medical Specialty") {
        hosipital_diabetic_data() %>% 
          group_by(patient_nbr) %>% 
          count() %>% 
          rename(num_encounters = n) %>% 
          inner_join(hosipital_diabetic_data() %>% 
                       select(patient_nbr, medical_specialty), by = c("patient_nbr")) %>% 
          ungroup() %>% 
          group_by(medical_specialty) %>% 
          summarise(avg_encounters = mean(num_encounters)) %>% 
          distinct() %>% 
          ggplot(aes(y = fct_reorder(medical_specialty, avg_encounters), x = avg_encounters)) +
          geom_col(fill = "#1b94e4") +
          geom_text(aes(label = round(avg_encounters, 2)), hjust = 1) +
          theme_classic() +
          labs(x = "", y = "", title = "Average Patient Encounters")
        
      }
      else if (input$selectChartY == "age") {
        hosipital_diabetic_data() %>% 
          group_by(patient_nbr) %>% 
          count() %>% 
          rename(num_encounters = n) %>% 
          inner_join(hosipital_diabetic_data() %>% 
                       select(patient_nbr, age), by = c("patient_nbr")) %>% 
          ungroup() %>% 
          group_by(age) %>% 
          summarise(avg_encounters = mean(num_encounters)) %>% 
          distinct() %>% 
          ggplot(aes(x = fct_inorder(age), y = avg_encounters, fill = avg_encounters)) +
          geom_col() +
          geom_text(aes(label = round(avg_encounters, 2)), vjust = -0.2) +
          theme_classic() +
          labs(x = "", y = "", title = "Average Patient Encounters") +
          theme(legend.position = "none")
      }
      else {
        hosipital_diabetic_data() %>% 
          group_by(patient_nbr) %>% 
          count() %>% 
          rename(num_encounters = n) %>% 
          inner_join(demographic_diabetic_data() %>% 
                       select(patient_nbr, race, gender, age), by = c("patient_nbr")) %>% 
          ungroup() %>% 
          group_by(!!as.symbol(input$selectChartY)) %>% 
          summarise(avg_encounters = mean(num_encounters)) %>% 
          distinct() %>% 
          ggplot(aes(x = fct_reorder(!!as.symbol(input$selectChartY), -avg_encounters), y = avg_encounters)) +
          geom_col(fill = "#1b94e4") +
          geom_text(aes(label = round(avg_encounters, 2)), vjust = -0.2) +
          theme_classic() +
          labs(x = "", y = "", title = "Average Patient Encounters")
        
      }
    }
  }, res = 96)
  
  # Third Tab Panel
  output$prediction <- renderTable({
    predict(readmission_model, new_data = tibble(
      "race" = input$modelRace,
      "time_in_hospital" = input$modelTimeInHospital,
      "gender" = input$modelGender,
      "age" = input$modelAge,
      "max_glu_serum" = input$modelMaxGluSerum,
      "A1Cresult" = input$modelA1Cresult,
      "diabetesMed" = input$modelMed,
      "change" = input$modelChangeMed,
      "medical_specialty" = input$modelMedicalSpeciality,
      "admission_type" = input$modelAdmissionType,
      "admission_source" = input$modelAdmissionSource,
      "discharge_disposition" = input$modelDischargeDisposition,
      "num_lab_procedures" = input$modelNumLabProcedures,
      "number_outpatient" = input$modelOutpatient,
      "number_inpatient" = input$modelInpatient,
      "number_emergency" = input$modelEmergency
    )) %>% 
      rename(`Predicted Class` = .pred_class)
  })
  
  output$predictionProb <- renderTable({
    predict(readmission_model, new_data = tibble(
      "race" = input$modelRace,
      "time_in_hospital" = input$modelTimeInHospital,
      "gender" = input$modelGender,
      "age" = input$modelAge,
      "max_glu_serum" = input$modelMaxGluSerum,
      "A1Cresult" = input$modelA1Cresult,
      "diabetesMed" = input$modelMed,
      "change" = input$modelChangeMed,
      "medical_specialty" = input$modelMedicalSpeciality,
      "admission_type" = input$modelAdmissionType,
      "admission_source" = input$modelAdmissionSource,
      "discharge_disposition" = input$modelDischargeDisposition,
      "num_lab_procedures" = input$modelNumLabProcedures,
      "number_outpatient" = input$modelOutpatient,
      "number_inpatient" = input$modelInpatient,
      "number_emergency" = input$modelEmergency
    ), type = "prob") %>% 
      rename(`No Readmission Probability` = `.pred_No Readmission`,
             `Readmission in Less Than 30 Days Probability` = `.pred_Readmission <30 days`,
             `Readmission in More Than 30 Days Probability` = `.pred_Readmission >30 days`)
  })
}

# Run the application 
shinyApp(ui = ui, server = server, options = list(height = 1300))
