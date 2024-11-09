#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

install.packages(c("shinydashboard", "ggplot2", "DT", "dplyr", "RColorBrewer", "shinyWidgets", "shinyjs", "bslib", "lubridate"))


# Load necessary libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT)
library(dplyr)
library(shinyWidgets)
library(shinyjs)
library(bslib)
library(lubridate)
library(RColorBrewer)

# Sample data for admissions and resources
set.seed(123)
hospital_data <- data.frame(
  department = sample(c("Emergency", "ICU", "General", "Pediatrics", "Cardiology"), 500, replace = TRUE),
  admission_date = as.Date('2023-01-01') + sample(0:364, 500, replace = TRUE),
  discharge_date = as.Date('2023-01-01') + sample(1:30, 500, replace = TRUE),
  age = sample(18:90, 500, replace = TRUE),
  gender = sample(c("Male", "Female"), 500, replace = TRUE),
  satisfaction = sample(1:10, 500, replace = TRUE),
  insurance = sample(c("Public", "Private", "Self-Pay"), 500, replace = TRUE)
)

# Sample data for resources
resource_data <- data.frame(
  Resource = c("Beds", "Ventilators", "IV Pumps", "Monitors", "Defibrillators", "ECG Machines", "Wheelchairs", "Oxygen Tanks"),
  Department = c("ICU", "Emergency", "General", "ICU", "Emergency", "General", "Outpatient", "ICU"),
  Total = c(100, 50, 75, 40, 20, 30, 60, 80),
  Occupied = c(85, 30, 50, 20, 15, 10, 35, 60),
  Available = c(15, 20, 25, 20, 5, 20, 25, 20),
  UsageRate = c("85%", "60%", "67%", "50%", "75%", "33%", "58%", "75%")
)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Hospital Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("KPIs", tabName = "kpi", icon = icon("dashboard")),
      menuItem("Admissions", tabName = "admissions", icon = icon("hospital")),
      menuItem("Resource Utilization", tabName = "resources", icon = icon("bed")),
      menuItem("Heatmap", tabName = "heatmap", icon = icon("thermometer-half")),
      menuItem("Real-Time Data", tabName = "realtime", icon = icon("sync"))
    ),
    pickerInput("departmentFilter", "Select Department:", 
                choices = unique(hospital_data$department), 
                multiple = TRUE, options = pickerOptions(actionsBox = TRUE)),
    sliderInput("dateRange", "Select Date Range:", 
                min = as.Date('2023-01-01'), 
                max = as.Date('2023-12-31'), 
                value = c(as.Date('2023-01-01'), as.Date('2023-12-31')))
  ),
  
  dashboardBody(
    useShinyjs(),
    tabItems(
      
      # KPIs Tab
      tabItem(tabName = "kpi",
              fluidRow(
                valueBoxOutput("bed_occupancy"),
                valueBoxOutput("avg_length_stay"),
                valueBoxOutput("patient_satisfaction"),
                valueBoxOutput("readmission_rate")
              ),
              fluidRow(
                infoBoxOutput("emergency_utilization"),
                infoBoxOutput("icu_utilization"),
                infoBoxOutput("cardiology_utilization")
              )),
      
      # Admissions Tab
      tabItem(tabName = "admissions",
              fluidRow(
                box(title = "Admissions Over Time", status = "primary", solidHeader = TRUE, 
                    plotOutput("admissions_plot", click = "plot_click")),
                box(title = "Detailed Admissions Data", status = "primary", solidHeader = TRUE,
                    DTOutput("admissions_table"))
              )),
      
      # Resource Utilization Tab
      tabItem(tabName = "resources",
              fluidRow(
                box(title = "Bed Utilization by Department", status = "primary", solidHeader = TRUE, 
                    plotOutput("bed_utilization_plot")),
                box(title = "Resource Availability", status = "primary", solidHeader = TRUE,
                    DTOutput("resource_table"))
              )),
      
      # Heatmap Tab for Occupancy
      tabItem(tabName = "heatmap",
              fluidRow(
                box(title = "Occupancy Heatmap", status = "primary", solidHeader = TRUE,
                    plotOutput("heatmap_plot"))
              )),
      
      # Real-Time Data Tab
      tabItem(tabName = "realtime",
              fluidRow(
                valueBoxOutput("current_patients"),
                valueBoxOutput("icu_bed_occupancy"),
                valueBoxOutput("emergency_wait_time"),
                box(title = "Live Data Feed", status = "primary", solidHeader = TRUE,
                    plotOutput("live_admissions_plot"))
              ))
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive data filtered by department and date
  filtered_data <- reactive({
    hospital_data %>%
      filter(department %in% input$departmentFilter,
             admission_date >= input$dateRange[1],
             admission_date <= input$dateRange[2])
  })
  
  # KPIs
  output$bed_occupancy <- renderValueBox({
    valueBox("85%", "Bed Occupancy", icon = icon("bed"), color = "green")
  })
  
  output$avg_length_stay <- renderValueBox({
    avg_stay <- mean(as.numeric(difftime(filtered_data()$discharge_date, filtered_data()$admission_date)))
    valueBox(round(avg_stay, 1), "Avg Length of Stay (days)", icon = icon("clock"), color = "blue")
  })
  
  output$patient_satisfaction <- renderValueBox({
    satisfaction <- mean(filtered_data()$satisfaction)
    valueBox(round(satisfaction, 1), "Patient Satisfaction", icon = icon("smile"), color = "yellow")
  })
  
  output$readmission_rate <- renderValueBox({
    readmission_rate <- 5 # Placeholder
    valueBox(paste(readmission_rate, "%"), "Readmission Rate", icon = icon("sync"), color = "red")
  })
  
  # InfoBoxes for department utilization rates
  output$emergency_utilization <- renderInfoBox({
    infoBox("Emergency Utilization", "60%", icon = icon("exclamation-circle"), color = "red")
  })
  output$icu_utilization <- renderInfoBox({
    infoBox("ICU Utilization", "85%", icon = icon("heartbeat"), color = "purple")
  })
  output$cardiology_utilization <- renderInfoBox({
    infoBox("Cardiology Utilization", "90%", icon = icon("heart"), color = "blue")
  })
  
  # Admissions plot with click interaction
  output$admissions_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = admission_date)) +
      geom_histogram(binwidth = 7, fill = "blue", color = "white") +
      labs(title = "Admissions Over Time", x = "Date", y = "Admissions") +
      theme_minimal()
  })
  
  # Admissions data table
  output$admissions_table <- renderDT({
    datatable(filtered_data())
  })
  
  # Bed utilization plot with distinct colors for each department
  output$bed_utilization_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = department, fill = department)) +
      geom_bar() +
      scale_fill_brewer(palette = "Set3") + # Use a color palette to distinguish departments
      labs(title = "Bed Utilization by Department", x = "Department", y = "Count") +
      theme_minimal() +
      theme(legend.position = "none") # Remove legend if it's not necessary
  })
  
  # Resource data table
  output$resource_table <- renderDT({
    datatable(resource_data)
  })
  
  # Heatmap for occupancy
  output$heatmap_plot <- renderPlot({
    occupancy_data <- expand.grid(department = unique(hospital_data$department),
                                  month = month.abb) %>%
      mutate(occupancy = runif(nrow(.), 0.5, 1.0))
    ggplot(occupancy_data, aes(x = department, y = month, fill = occupancy)) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "lightblue", high = "red") +
      labs(title = "Occupancy Heatmap by Department", x = "Department", y = "Month") +
      theme_minimal()
  })
  
  # Real-time data simulation
  output$current_patients <- renderValueBox({
    valueBox(sample(80:120, 1), "Current Patients", icon = icon("users"), color = "blue")
  })
  output$icu_bed_occupancy <- renderValueBox({
    valueBox("85%", "ICU Bed Occupancy", icon = icon("bed"), color = "purple")
  })
  output$emergency_wait_time <- renderValueBox({
    wait_time <- sample(10:60, 1)
    valueBox(paste(wait_time, "min"), "Emergency Wait Time", icon = icon("clock"), color = "red")
  })
  
  output$live_admissions_plot <- renderPlot({
    admissions_over_time <- data.frame(
      time = seq.POSIXt(from = Sys.time() - 60*60, to = Sys.time(), by = "min"),
      count = cumsum(sample(c(-1, 1), 61, replace = TRUE))
    )
    ggplot(admissions_over_time, aes(x = time, y = count)) +
      geom_line(color = "darkgreen") +
      labs(title = "Live Admissions Data", x = "Time", y = "Admissions Count") +
      theme_minimal()
  })
}

# Run the app
shinyApp(ui, server)
