# ================================================================
# SMART EYE CAMERA vs CONVENTIONAL SLIT LAMP
# CLINICAL COMPARATIVE RESEARCH DASHBOARD
#
# Complete R Shiny Application - CORRECTED VERSION
# ================================================================
#
# INSTALL REQUIRED PACKAGES FIRST:
#
install.packages(c(
  "shiny",
  "shinydashboard",
  "DT",
  "dplyr",
  "tidyr",
  "ggplot2",
  "readr",
  "writexl",
  "scales",
  "broom",
  "irr"
))
shiny::runApp()
#
# ================================================================


# ================================================================
# 1. LOAD PACKAGES
# ================================================================

library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(writexl)
library(scales)
library(broom)
library(irr)


# ================================================================
# 2. GLOBAL SETTINGS
# ================================================================

options(
  stringsAsFactors = FALSE,
  shiny.maxRequestSize = 50 * 1024^2
)


# ================================================================
# 3. RESEARCH DATA STRUCTURE
# ================================================================

research_columns <- c(
  
  # -------------------------------
  # DEMOGRAPHICS
  # -------------------------------
  
  "Study_ID",
  "Patient_Name",
  "Age",
  "Gender",
  "Address",
  "Date_Entered",
  
  # -------------------------------
  # DISTANCE VISION - SLIT LAMP
  # -------------------------------
  
  "VA_Slit",
  "PH_Slit",
  "BVCA_Slit",
  
  # -------------------------------
  # DISTANCE VISION - CAMERA
  # -------------------------------
  
  "VA_Camera",
  "PH_Camera",
  "BVCA_Camera",
  
  # -------------------------------
  # IOP
  # -------------------------------
  
  "IOP_Slit",
  "IOP_Camera",
  
  # -------------------------------
  # REFRACTION - SLIT
  # -------------------------------
  
  "SPH_Slit",
  "CYL_Slit",
  "Axis_Slit",
  
  # -------------------------------
  # REFRACTION - CAMERA
  # -------------------------------
  
  "SPH_Camera",
  "CYL_Camera",
  "Axis_Camera",
  
  # -------------------------------
  # OTHER CLINICAL VARIABLES
  # -------------------------------
  
  "Auto_Refraction_Right",
  "Near_Vision",
  "Extra_Ocular_Movement",
  "Convergence",
  "Cover_Uncover_Test",
  "Hirschberg_Straight",
  "Hirschberg_Deviation",
  
  # -------------------------------
  # DIAGNOSIS
  # -------------------------------
  
  "Diagnosis",
  "Treatment",
  
  # -------------------------------
  # EXAMINATION OUTCOME
  # -------------------------------
  
  "Slit_Examination",
  "Camera_Examination",
  
  # ==============================================================
  # TECHNICAL COMPARISON
  # ==============================================================
  
  # 1 Convenience
  "Slit_Convenience",
  "Camera_Convenience",
  
  # 2 Accuracy
  "Slit_Accuracy",
  "Camera_Accuracy",
  
  # 3 Time Efficiency
  "Slit_Time_Efficiency",
  "Camera_Time_Efficiency",
  
  # 4 Training
  "Slit_Training",
  "Camera_Training",
  
  # 5 External Light
  "Slit_External_Light",
  "Camera_External_Light",
  
  # 6 Portability
  "Slit_Portability",
  "Camera_Portability",
  
  # 7 Recording
  "Slit_Recording",
  "Camera_Recording",
  
  # 8 Clinical Examination
  "Slit_Clinical_Examination",
  "Camera_Clinical_Examination",
  
  # 9 Resolution
  "Slit_Resolution",
  "Camera_Resolution",
  
  # 10 Field of View
  "Slit_Field_of_View",
  "Camera_Field_of_View"
)


# ================================================================
# 4. EMPTY DATASET (CORRECTED)
# ================================================================

empty_dataset <- function() {
  d <- as.data.frame(
    setNames(
      lapply(research_columns, function(x) character(0)),
      research_columns
    ),
    stringsAsFactors = FALSE
  )
  
  d$Age <- numeric(0)
  
  d
}


# ================================================================
# 5. HELPER FUNCTIONS
# ================================================================


# ------------------------------------------------
# Safe numeric conversion
# ------------------------------------------------

safe_numeric <- function(x) {
  
  suppressWarnings(
    as.numeric(
      gsub(
        "[^0-9.\\-]",
        "",
        as.character(x)
      )
    )
  )
}


# ------------------------------------------------
# Percentage
# ------------------------------------------------

percentage <- function(x, total) {
  
  if (total == 0) {
    return(0)
  }
  
  round(
    100 * x / total,
    2
  )
}


# ------------------------------------------------
# Clean text
# ------------------------------------------------

clean_text <- function(x) {
  
  x <- as.character(x)
  
  x[
    is.na(x) |
      trimws(x) == ""
  ] <- NA
  
  x
}


# ------------------------------------------------
# Safe statistics (for empty data handling)
# ------------------------------------------------

safe_mean <- function(x) {
  x <- safe_numeric(x)
  
  if (all(is.na(x))) {
    return(NA_real_)
  }
  
  mean(x, na.rm = TRUE)
}

safe_sd <- function(x) {
  x <- safe_numeric(x)
  
  if (sum(!is.na(x)) < 2) {
    return(NA_real_)
  }
  
  sd(x, na.rm = TRUE)
}

safe_median <- function(x) {
  x <- safe_numeric(x)
  
  if (all(is.na(x))) {
    return(NA_real_)
  }
  
  median(x, na.rm = TRUE)
}

safe_min <- function(x) {
  x <- safe_numeric(x)
  
  if (all(is.na(x))) {
    return(NA_real_)
  }
  
  min(x, na.rm = TRUE)
}

safe_max <- function(x) {
  x <- safe_numeric(x)
  
  if (all(is.na(x))) {
    return(NA_real_)
  }
  
  max(x, na.rm = TRUE)
}


# ================================================================
# 6. USER INTERFACE
# ================================================================

ui <- dashboardPage(
  
  # ==============================================================
  # HEADER
  # ==============================================================
  
  dashboardHeader(
    
    title = "Smart Eye Research",
    
    titleWidth = 300
    
  ),
  
  
  # ==============================================================
  # SIDEBAR
  # ==============================================================
  
  dashboardSidebar(
    
    width = 300,
    
    sidebarMenu(
      
      id = "tabs",
      
      menuItem(
        "Dashboard",
        tabName = "dashboard",
        icon = icon("dashboard")
      ),
      
      menuItem(
        "Patient Data Entry",
        tabName = "entry",
        icon = icon("user-plus")
      ),
      
      menuItem(
        "Research Data",
        tabName = "data",
        icon = icon("table")
      ),
      
      menuItem(
        "Clinical Analysis",
        tabName = "clinical",
        icon = icon("eye")
      ),
      
      menuItem(
        "Technical Comparison",
        tabName = "technical",
        icon = icon("balance-scale")
      ),
      
      menuItem(
        "Statistical Analysis",
        tabName = "statistics",
        icon = icon("calculator")
      ),
      
      menuItem(
        "Research Tables",
        tabName = "tables",
        icon = icon("file-alt")
      ),
      
      menuItem(
        "Data Quality",
        tabName = "quality",
        icon = icon("check-circle")
      ),
      
      menuItem(
        "Export",
        tabName = "export",
        icon = icon("download")
      )
      
    )
    
  ),
  
  
  # ==============================================================
  # BODY
  # ==============================================================
  
  dashboardBody(
    
    tags$head(
      
      tags$style(
        
        HTML("

        .content-wrapper,
        .right-side {

          background-color: #f4f6f9;

        }

        .box {

          border-radius: 10px;

        }

        .small-box {

          border-radius: 10px;

        }

        .form-control {

          border-radius: 5px;

        }

        .btn {

          border-radius: 5px;

        }

        h4 {

          font-weight: 600;

        }

        ")
        
      )
      
    ),
    
    
    tabItems(
      
      # ==========================================================
      # DASHBOARD
      # ==========================================================
      
      tabItem(
        
        tabName = "dashboard",
        
        fluidRow(
          
          valueBoxOutput(
            "total_patients",
            width = 3
          ),
          
          valueBoxOutput(
            "mean_age",
            width = 3
          ),
          
          valueBoxOutput(
            "slit_satisfactory",
            width = 3
          ),
          
          valueBoxOutput(
            "camera_satisfactory",
            width = 3
          )
          
        ),
        
        
        fluidRow(
          
          box(
            width = 6,
            title = "Examination Outcome",
            status = "primary",
            solidHeader = TRUE,
            
            plotOutput(
              "dashboard_outcome",
              height = 350
            )
            
          ),
          
          box(
            width = 6,
            title = "Gender Distribution",
            status = "info",
            solidHeader = TRUE,
            
            plotOutput(
              "dashboard_gender",
              height = 350
            )
            
          )
          
        ),
        
        
        fluidRow(
          
          box(
            width = 6,
            title = "Age Distribution",
            status = "success",
            solidHeader = TRUE,
            
            plotOutput(
              "dashboard_age",
              height = 350
            )
            
          ),
          
          box(
            width = 6,
            title = "Diagnosis Distribution",
            status = "warning",
            solidHeader = TRUE,
            
            plotOutput(
              "dashboard_diagnosis",
              height = 350
            )
            
          )
          
        )
        
      ),
      
      
      # ==========================================================
      # PATIENT ENTRY
      # ==========================================================
      
      tabItem(
        
        tabName = "entry",
        
        fluidRow(
          
          box(
            
            width = 12,
            
            title = "Patient / Study Information",
            
            status = "primary",
            
            solidHeader = TRUE,
            
            fluidRow(
              
              column(
                4,
                
                textInput(
                  "Study_ID",
                  "Study ID *",
                  placeholder = "EYE-001"
                )
                
              ),
              
              column(
                4,
                
                textInput(
                  "Patient_Name",
                  "Patient Name"
                )
                
              ),
              
              column(
                4,
                
                numericInput(
                  "Age",
                  "Age",
                  value = NA,
                  min = 0,
                  max = 120
                )
                
              )
              
            ),
            
            fluidRow(
              
              column(
                
                4,
                
                selectInput(
                  
                  "Gender",
                  
                  "Gender",
                  
                  choices = c(
                    "",
                    "Male",
                    "Female",
                    "Other",
                    "Not recorded"
                  )
                  
                )
                
              ),
              
              column(
                
                4,
                
                textInput(
                  "Address",
                  "Address"
                )
                
              ),
              
              column(
                
                4,
                
                dateInput(
                  "Date_Entered",
                  "Date",
                  value = Sys.Date()
                )
                
              )
              
            )
            
          )
          
        ),
        
        
        # --------------------------------------------------------
        # VISION
        # --------------------------------------------------------
        
        fluidRow(
          
          box(
            
            width = 6,
            
            title = "Conventional Slit Lamp",
            
            status = "primary",
            
            solidHeader = TRUE,
            
            textInput(
              "VA_Slit",
              "Distance Vision - VA"
            ),
            
            textInput(
              "PH_Slit",
              "Distance Vision - PH"
            ),
            
            textInput(
              "BVCA_Slit",
              "Distance Vision - BVCA"
            ),
            
            textInput(
              "IOP_Slit",
              "IOP AT"
            ),
            
            textInput(
              "SPH_Slit",
              "SPH"
            ),
            
            textInput(
              "CYL_Slit",
              "CYL"
            ),
            
            textInput(
              "Axis_Slit",
              "Axis"
            )
            
          ),
          
          
          box(
            
            width = 6,
            
            title = "Smart Eye Camera",
            
            status = "info",
            
            solidHeader = TRUE,
            
            textInput(
              "VA_Camera",
              "Distance Vision - VA"
            ),
            
            textInput(
              "PH_Camera",
              "Distance Vision - PH"
            ),
            
            textInput(
              "BVCA_Camera",
              "Distance Vision - BVCA"
            ),
            
            textInput(
              "IOP_Camera",
              "IOP AT"
            ),
            
            textInput(
              "SPH_Camera",
              "SPH"
            ),
            
            textInput(
              "CYL_Camera",
              "CYL"
            ),
            
            textInput(
              "Axis_Camera",
              "Axis"
            )
            
          )
          
        ),
        
        
        # --------------------------------------------------------
        # CLINICAL INFORMATION
        # --------------------------------------------------------
        
        fluidRow(
          
          box(
            
            width = 6,
            
            title = "Clinical Examination",
            
            status = "warning",
            
            solidHeader = TRUE,
            
            textInput(
              "Auto_Refraction_Right",
              "Auto R"
            ),
            
            textInput(
              "Near_Vision",
              "Near Vision"
            ),
            
            textInput(
              "Extra_Ocular_Movement",
              "Extra Ocular Movement"
            ),
            
            textInput(
              "Convergence",
              "Convergence"
            ),
            
            textInput(
              "Cover_Uncover_Test",
              "Cover / Uncover Test"
            ),
            
            textInput(
              "Hirschberg_Straight",
              "Hirschberg Test - Straight"
            ),
            
            textInput(
              "Hirschberg_Deviation",
              "Hirschberg - Deviation"
            )
            
          ),
          
          
          box(
            
            width = 6,
            
            title = "Diagnosis & Treatment",
            
            status = "warning",
            
            solidHeader = TRUE,
            
            textInput(
              "Diagnosis",
              "Diagnosis"
            ),
            
            textAreaInput(
              "Treatment",
              "Treatment",
              rows = 8
            )
            
          )
          
        ),
        
        
        # --------------------------------------------------------
        # EXAMINATION OUTCOME
        # --------------------------------------------------------
        
        fluidRow(
          
          box(
            
            width = 12,
            
            title = "Examination Outcome",
            
            status = "success",
            
            solidHeader = TRUE,
            
            fluidRow(
              
              column(
                
                6,
                
                selectInput(
                  
                  "Slit_Examination",
                  
                  "Conventional Slit Lamp Examination",
                  
                  choices = c(
                    "Satisfactory",
                    "Unsatisfactory",
                    "Not recorded"
                  )
                  
                )
                
              ),
              
              column(
                
                6,
                
                selectInput(
                  
                  "Camera_Examination",
                  
                  "Smart Eye Camera Examination",
                  
                  choices = c(
                    "Satisfactory",
                    "Unsatisfactory",
                    "Not recorded"
                  )
                  
                )
                
              )
              
            )
            
          )
          
        ),
        
        
        # --------------------------------------------------------
        # TECHNICAL COMPARISON
        # --------------------------------------------------------
        
        fluidRow(
          
          box(
            
            width = 12,
            
            title = "Technical / Usability Comparison",
            
            status = "primary",
            
            solidHeader = TRUE,
            
            h4(
              "1. Convenience / Ease"
            ),
            
            fluidRow(
              
              column(
                
                6,
                
                selectInput(
                  
                  "Slit_Convenience",
                  
                  "Slit Lamp",
                  
                  choices = c(
                    "Very Easy",
                    "Easy",
                    "Difficult",
                    "Not recorded"
                  )
                  
                )
                
              ),
              
              column(
                
                6,
                
                selectInput(
                  
                  "Camera_Convenience",
                  
                  "Smart Eye Camera",
                  
                  choices = c(
                    "Very Easy",
                    "Easy",
                    "Difficult",
                    "Not recorded"
                  )
                  
                )
                
              )
              
            ),
            
            
            h4(
              "2. Accuracy & Clarity"
            ),
            
            fluidRow(
              
              column(
                
                6,
                
                selectInput(
                  
                  "Slit_Accuracy",
                  
                  "Slit Lamp",
                  
                  choices = c(
                    "Satisfied",
                    "Unsatisfied",
                    "Not recorded"
                  )
                  
                )
                
              ),
              
              column(
                
                6,
                
                selectInput(
                  
                  "Camera_Accuracy",
                  
                  "Smart Eye Camera",
                  
                  choices = c(
                    "Satisfied",
                    "Unsatisfied",
                    "Not recorded"
                  )
                  
                )
                
              )
              
            ),
            
            
            h4(
              "3. Time Efficiency"
            ),
            
            fluidRow(
              
              column(
                
                6,
                
                selectInput(
                  
                  "Slit_Time_Efficiency",
                  
                  "Slit Lamp",
                  
                  choices = c(
                    "Yes",
                    "No",
                    "Not recorded"
                  )
                  
                )
                
              ),
              
              column(
                
                6,
                
                selectInput(
                  
                  "Camera_Time_Efficiency",
                  
                  "Smart Eye Camera",
                  
                  choices = c(
                    "Yes",
                    "No",
                    "Not recorded"
                  )
                  
                )
                
              )
              
            ),
            
            
            h4(
              "4. Training & Learning Curve"
            ),
            
            fluidRow(
              
              column(
                
                6,
                
                selectInput(
                  
                  "Slit_Training",
                  
                  "Slit Lamp",
                  
                  choices = c(
                    "Yes",
                    "No",
                    "Not recorded"
                  )
                  
                )
                
              ),
              
              column(
                
                6,
                
                selectInput(
                  
                  "Camera_Training",
                  
                  "Smart Eye Camera",
                  
                  choices = c(
                    "Yes",
                    "No",
                    "Not recorded"
                  )
                  
                )
                
              )
              
            ),
            
            
            h4(
              "5. External Light Source"
            ),
            
            fluidRow(
              
              column(
                
                6,
                
                selectInput(
                  
                  "Slit_External_Light",
                  
                  "Slit Lamp",
                  
                  choices = c(
                    "Yes",
                    "No",
                    "Not recorded"
                  )
                  
                )
                
              ),
              
              column(
                
                6,
                
                selectInput(
                  
                  "Camera_External_Light",
                  
                  "Smart Eye Camera",
                  
                  choices = c(
                    "Yes",
                    "No",
                    "Not recorded"
                  )
                  
                )
                
              )
              
            ),
            
            
            h4(
              "6. Portability"
            ),
            
            fluidRow(
              
              column(
                
                6,
                
                selectInput(
                  
                  "Slit_Portability",
                  
                  "Slit Lamp",
                  
                  choices = c(
                    "Yes",
                    "No",
                    "Not recorded"
                  )
                  
                )
                
              ),
              
              column(
                
                6,
                
                selectInput(
                  
                  "Camera_Portability",
                  
                  "Smart Eye Camera",
                  
                  choices = c(
                    "Yes",
                    "No",
                    "Not recorded"
                  )
                  
                )
                
              )
              
            ),
            
            
            h4(
              "7. Recording & Storage"
            ),
            
            fluidRow(
              
              column(
                
                6,
                
                selectInput(
                  
                  "Slit_Recording",
                  
                  "Slit Lamp",
                  
                  choices = c(
                    "Yes",
                    "No",
                    "Not recorded"
                  )
                  
                )
                
              ),
              
              column(
                
                6,
                
                selectInput(
                  
                  "Camera_Recording",
                  
                  "Smart Eye Camera",
                  
                  choices = c(
                    "Yes",
                    "No",
                    "Not recorded"
                  )
                  
                )
                
              )
              
            ),
            
            
            h4(
              "8. Clinical Examination"
            ),
            
            fluidRow(
              
              column(
                
                6,
                
                selectInput(
                  
                  "Slit_Clinical_Examination",
                  
                  "Slit Lamp",
                  
                  choices = c(
                    "Yes",
                    "No",
                    "Not recorded"
                  )
                  
                )
                
              ),
              
              column(
                
                6,
                
                selectInput(
                  
                  "Camera_Clinical_Examination",
                  
                  "Smart Eye Camera",
                  
                  choices = c(
                    "Yes",
                    "No",
                    "Not recorded"
                  )
                  
                )
                
              )
              
            ),
            
            
            h4(
              "9. Resolution"
            ),
            
            fluidRow(
              
              column(
                
                6,
                
                selectInput(
                  
                  "Slit_Resolution",
                  
                  "Slit Lamp",
                  
                  choices = c(
                    "Yes",
                    "No",
                    "Not recorded"
                  )
                  
                )
                
              ),
              
              column(
                
                6,
                
                selectInput(
                  
                  "Camera_Resolution",
                  
                  "Smart Eye Camera",
                  
                  choices = c(
                    "Yes",
                    "No",
                    "Not recorded"
                  )
                  
                )
                
              )
              
            ),
            
            
            h4(
              "10. Field of View"
            ),
            
            fluidRow(
              
              column(
                
                6,
                
                selectInput(
                  
                  "Slit_Field_of_View",
                  
                  "Slit Lamp",
                  
                  choices = c(
                    "Yes",
                    "No",
                    "Not recorded"
                  )
                  
                )
                
              ),
              
              column(
                
                6,
                
                selectInput(
                  
                  "Camera_Field_of_View",
                  
                  "Smart Eye Camera",
                  
                  choices = c(
                    "Yes",
                    "No",
                    "Not recorded"
                  )
                  
                )
                
              )
              
            ),
            
            
            br(),
            
            actionButton(
              
              "save_patient",
              
              "SAVE PATIENT / OBSERVATION",
              
              icon = icon("save"),
              
              class = "btn-success btn-lg"
              
            ),
            
            actionButton(
              
              "clear_form",
              
              "CLEAR FORM",
              
              icon = icon("eraser"),
              
              class = "btn-warning btn-lg"
              
            )
            
          )
          
        )
        
      ),
      
      
      # ==========================================================
      # RESEARCH DATA
      # ==========================================================
      
      tabItem(
        
        tabName = "data",
        
        fluidRow(
          
          box(
            
            width = 12,
            
            title = "Research Dataset",
            
            status = "primary",
            
            solidHeader = TRUE,
            
            DTOutput(
              "research_data"
            )
            
          )
          
        )
        
      ),
      
      
      # ==========================================================
      # CLINICAL ANALYSIS
      # ==========================================================
      
      tabItem(
        
        tabName = "clinical",
        
        fluidRow(
          
          box(
            
            width = 4,
            
            title = "Clinical Variable",
            
            status = "primary",
            
            solidHeader = TRUE,
            
            selectInput(
              
              "clinical_variable",
              
              "Select variable",
              
              choices = c(
                "IOP",
                "SPH",
                "CYL",
                "Axis"
              )
              
            )
            
          ),
          
          box(
            
            width = 8,
            
            title = "Clinical Comparison",
            
            status = "info",
            
            solidHeader = TRUE,
            
            plotOutput(
              "clinical_plot",
              height = 450
            )
            
          )
          
        ),
        
        fluidRow(
          
          box(
            
            width = 6,
            
            title = "Paired Statistical Test",
            
            status = "success",
            
            solidHeader = TRUE,
            
            verbatimTextOutput(
              "clinical_test"
            )
            
          ),
          
          box(
            
            width = 6,
            
            title = "Clinical Summary",
            
            status = "warning",
            
            DTOutput(
              "clinical_summary"
            )
            
          )
          
        )
        
      ),
      
      
      # ==========================================================
      # TECHNICAL COMPARISON
      # ==========================================================
      
      tabItem(
        
        tabName = "technical",
        
        fluidRow(
          
          box(
            
            width = 4,
            
            title = "Technical Parameter",
            
            status = "primary",
            
            solidHeader = TRUE,
            
            selectInput(
              
              "technical_variable",
              
              "Select parameter",
              
              choices = c(
                
                "Convenience / Ease",
                "Accuracy & Clarity",
                "Time Efficiency",
                "Training & Learning Curve",
                "External Light Source",
                "Portability",
                "Recording & Storage",
                "Clinical Examination",
                "Resolution",
                "Field of View"
                
              )
              
            ),
            
            selectInput(
              
              "technical_chart",
              
              "Chart",
              
              choices = c(
                "Count",
                "Percentage"
              )
              
            )
            
          ),
          
          
          box(
            
            width = 8,
            
            title = "Device Comparison",
            
            status = "info",
            
            solidHeader = TRUE,
            
            plotOutput(
              
              "technical_plot",
              
              height = 450
              
            )
            
          )
          
        ),
        
        fluidRow(
          
          box(
            
            width = 12,
            
            title = "Technical Comparison Table",
            
            status = "success",
            
            solidHeader = TRUE,
            
            DTOutput(
              "technical_table"
            )
            
          )
          
        )
        
      ),
      
      
      # ==========================================================
      # STATISTICS
      # ==========================================================
      
      tabItem(
        
        tabName = "statistics",
        
        fluidRow(
          
          box(
            
            width = 12,
            
            title = "Statistical Analysis",
            
            status = "primary",
            
            solidHeader = TRUE,
            
            HTML(
              
              "

              <h4>Recommended paired-study analyses</h4>

              <ul>

              <li>
              <b>McNemar test:</b>
              paired binary examination outcomes
              </li>

              <li>
              <b>Paired t-test:</b>
              paired continuous measurements
              </li>

              <li>
              <b>Wilcoxon signed-rank:</b>
              non-parametric paired comparison
              </li>

              <li>
              <b>Cohen's kappa:</b>
              categorical agreement
              </li>

              <li>
              <b>Bland-Altman:</b>
              agreement between continuous measurement methods
              </li>

              </ul>

              "
              
            )
            
          )
          
        ),
        
        
        fluidRow(
          
          box(
            
            width = 6,
            
            title = "McNemar Test - Examination Outcome",
            
            status = "info",
            
            solidHeader = TRUE,
            
            verbatimTextOutput(
              "mcnemar_result"
            )
            
          ),
          
          
          box(
            
            width = 6,
            
            title = "Cohen's Kappa",
            
            status = "success",
            
            solidHeader = TRUE,
            
            verbatimTextOutput(
              "kappa_result"
            )
            
          )
          
        ),
        
        
        fluidRow(
          
          box(
            
            width = 6,
            
            title = "Paired t-test - IOP",
            
            status = "warning",
            
            verbatimTextOutput(
              "ttest_result"
            )
            
          ),
          
          
          box(
            
            width = 6,
            
            title = "Wilcoxon Signed-Rank - IOP",
            
            status = "danger",
            
            verbatimTextOutput(
              "wilcox_result"
            )
            
          )
          
        )
        
      ),
      
      
      # ==========================================================
      # RESEARCH TABLES
      # ==========================================================
      
      tabItem(
        
        tabName = "tables",
        
        fluidRow(
          
          box(
            
            width = 12,
            
            title = "Table 1 - Demographic Characteristics",
            
            status = "primary",
            
            solidHeader = TRUE,
            
            DTOutput(
              "table_demographics"
            )
            
          )
          
        ),
        
        fluidRow(
          
          box(
            
            width = 12,
            
            title = "Table 2 - Examination Outcome",
            
            status = "info",
            
            solidHeader = TRUE,
            
            DTOutput(
              "table_examination"
            )
            
          )
          
        ),
        
        fluidRow(
          
          box(
            
            width = 12,
            
            title = "Table 3 - Technical Comparison",
            
            status = "success",
            
            solidHeader = TRUE,
            
            DTOutput(
              "table_technical"
            )
            
          )
          
        )
        
      ),
      
      
      # ==========================================================
      # DATA QUALITY
      # ==========================================================
      
      tabItem(
        
        tabName = "quality",
        
        fluidRow(
          
          box(
            
            width = 12,
            
            title = "Data Quality Report",
            
            status = "warning",
            
            solidHeader = TRUE,
            
            verbatimTextOutput(
              "quality_report"
            )
            
          )
          
        )
        
      ),
      
      
      # ==========================================================
      # EXPORT
      # ==========================================================
      
      tabItem(
        
        tabName = "export",
        
        fluidRow(
          
          box(
            
            width = 6,
            
            title = "Research Data Export",
            
            status = "primary",
            
            solidHeader = TRUE,
            
            downloadButton(
              
              "download_csv",
              
              "Download CSV",
              
              class = "btn-primary"
              
            ),
            
            br(),
            br(),
            
            downloadButton(
              
              "download_excel",
              
              "Download Excel",
              
              class = "btn-success"
              
            )
            
          ),
          
          
          box(
            
            width = 6,
            
            title = "Analysis Export",
            
            status = "info",
            
            solidHeader = TRUE,
            
            downloadButton(
              
              "download_summary",
              
              "Download Research Summary",
              
              class = "btn-info"
              
            )
            
          )
          
        )
        
      )
      
    )
    
  )
  
)


# ================================================================
# 7. SERVER
# ================================================================

server <- function(input, output, session) {
  
  
  # ==============================================================
  # REACTIVE DATABASE
  # ==============================================================
  
  database <- reactiveVal(
    empty_dataset()
  )
  
  
  # ==============================================================
  # SAVE PATIENT
  # ==============================================================
  
  observeEvent(
    
    input$save_patient,
    
    {
      
      # ----------------------------------------------------------
      # REQUIRE STUDY ID
      # ----------------------------------------------------------
      
      if (
        is.null(input$Study_ID) ||
        trimws(input$Study_ID) == ""
      ) {
        
        showNotification(
          
          "Study ID is required.",
          
          type = "error",
          
          duration = 5
          
        )
        
        return()
        
      }
      
      
      # ----------------------------------------------------------
      # CHECK DUPLICATE STUDY ID
      # ----------------------------------------------------------
      
      existing <- database()
      
      if (
        nrow(existing) > 0 &&
        input$Study_ID %in% existing$Study_ID
      ) {
        
        showNotification(
          
          "This Study ID already exists.",
          
          type = "error",
          
          duration = 5
          
        )
        
        return()
        
      }
      
      
      # ----------------------------------------------------------
      # CREATE NEW RECORD
      # ----------------------------------------------------------
      
      new_record <- data.frame(
        
        Study_ID = input$Study_ID,
        
        Patient_Name = input$Patient_Name,
        
        Age = input$Age,
        
        Gender = input$Gender,
        
        Address = input$Address,
        
        Date_Entered =
          as.character(input$Date_Entered),
        
        VA_Slit = input$VA_Slit,
        
        PH_Slit = input$PH_Slit,
        
        BVCA_Slit = input$BVCA_Slit,
        
        VA_Camera = input$VA_Camera,
        
        PH_Camera = input$PH_Camera,
        
        BVCA_Camera = input$BVCA_Camera,
        
        IOP_Slit = input$IOP_Slit,
        
        IOP_Camera = input$IOP_Camera,
        
        SPH_Slit = input$SPH_Slit,
        
        CYL_Slit = input$CYL_Slit,
        
        Axis_Slit = input$Axis_Slit,
        
        SPH_Camera = input$SPH_Camera,
        
        CYL_Camera = input$CYL_Camera,
        
        Axis_Camera = input$Axis_Camera,
        
        Auto_Refraction_Right =
          input$Auto_Refraction_Right,
        
        Near_Vision =
          input$Near_Vision,
        
        Extra_Ocular_Movement =
          input$Extra_Ocular_Movement,
        
        Convergence =
          input$Convergence,
        
        Cover_Uncover_Test =
          input$Cover_Uncover_Test,
        
        Hirschberg_Straight =
          input$Hirschberg_Straight,
        
        Hirschberg_Deviation =
          input$Hirschberg_Deviation,
        
        Diagnosis =
          input$Diagnosis,
        
        Treatment =
          input$Treatment,
        
        Slit_Examination =
          input$Slit_Examination,
        
        Camera_Examination =
          input$Camera_Examination,
        
        Slit_Convenience =
          input$Slit_Convenience,
        
        Camera_Convenience =
          input$Camera_Convenience,
        
        Slit_Accuracy =
          input$Slit_Accuracy,
        
        Camera_Accuracy =
          input$Camera_Accuracy,
        
        Slit_Time_Efficiency =
          input$Slit_Time_Efficiency,
        
        Camera_Time_Efficiency =
          input$Camera_Time_Efficiency,
        
        Slit_Training =
          input$Slit_Training,
        
        Camera_Training =
          input$Camera_Training,
        
        Slit_External_Light =
          input$Slit_External_Light,
        
        Camera_External_Light =
          input$Camera_External_Light,
        
        Slit_Portability =
          input$Slit_Portability,
        
        Camera_Portability =
          input$Camera_Portability,
        
        Slit_Recording =
          input$Slit_Recording,
        
        Camera_Recording =
          input$Camera_Recording,
        
        Slit_Clinical_Examination =
          input$Slit_Clinical_Examination,
        
        Camera_Clinical_Examination =
          input$Camera_Clinical_Examination,
        
        Slit_Resolution =
          input$Slit_Resolution,
        
        Camera_Resolution =
          input$Camera_Resolution,
        
        Slit_Field_of_View =
          input$Slit_Field_of_View,
        
        Camera_Field_of_View =
          input$Camera_Field_of_View,
        
        stringsAsFactors = FALSE
        
      )
      
      
      # ----------------------------------------------------------
      # APPEND
      # ----------------------------------------------------------
      
      database(
        bind_rows(
          existing,
          new_record
        )
      )
      
      
      # ----------------------------------------------------------
      # MESSAGE
      # ----------------------------------------------------------
      
      showNotification(
        
        paste(
          "Patient",
          input$Study_ID,
          "saved successfully."
        ),
        
        type = "message",
        
        duration = 5
        
      )
      
    }
    
  )
  
  
  # ==============================================================
  # CLEAR FORM
  # ==============================================================
  
  observeEvent(
    
    input$clear_form,
    
    {
      
      updateTextInput(
        session,
        "Study_ID",
        value = ""
      )
      
      updateTextInput(
        session,
        "Patient_Name",
        value = ""
      )
      
      updateNumericInput(
        session,
        "Age",
        value = NA
      )
      
      updateSelectInput(
        session,
        "Gender",
        selected = ""
      )
      
      updateTextInput(
        session,
        "Address",
        value = ""
      )
      
      updateTextInput(
        session,
        "Diagnosis",
        value = ""
      )
      
      updateTextAreaInput(
        session,
        "Treatment",
        value = ""
      )
      
    }
    
  )
  
  
  # ==============================================================
  # DASHBOARD - TOTAL PATIENTS
  # ==============================================================
  
  output$total_patients <- renderValueBox({
    
    n <- nrow(
      database()
    )
    
    valueBox(
      
      value = n,
      
      subtitle = "Total Study Participants",
      
      icon = icon("users"),
      
      color = "aqua"
      
    )
    
  })
  
  
  # ==============================================================
  # DASHBOARD - MEAN AGE
  # ==============================================================
  
  output$mean_age <- renderValueBox({
    
    d <- database()
    
    age <- safe_numeric(
      d$Age
    )
    
    if (
      all(is.na(age))
    ) {
      
      result <- "NA"
      
    } else {
      
      result <- round(
        mean(
          age,
          na.rm = TRUE
        ),
        1
      )
      
    }
    
    valueBox(
      
      value = result,
      
      subtitle = "Mean Age",
      
      icon = icon("birthday-cake"),
      
      color = "green"
      
    )
    
  })
  
  
  # ==============================================================
  # SLIT SATISFACTORY
  # ==============================================================
  
  output$slit_satisfactory <- renderValueBox({
    
    d <- database()
    
    total <- sum(
      d$Slit_Examination != "Not recorded",
      na.rm = TRUE
    )
    
    satisfactory <- sum(
      d$Slit_Examination ==
        "Satisfactory",
      na.rm = TRUE
    )
    
    pct <- percentage(
      satisfactory,
      total
    )
    
    valueBox(
      
      value = paste0(
        pct,
        "%"
      ),
      
      subtitle = "Slit Lamp Satisfactory",
      
      icon = icon("eye"),
      
      color = "yellow"
      
    )
    
  })
  
  
  # ==============================================================
  # CAMERA SATISFACTORY
  # ==============================================================
  
  output$camera_satisfactory <- renderValueBox({
    
    d <- database()
    
    total <- sum(
      d$Camera_Examination != "Not recorded",
      na.rm = TRUE
    )
    
    satisfactory <- sum(
      d$Camera_Examination ==
        "Satisfactory",
      na.rm = TRUE
    )
    
    pct <- percentage(
      satisfactory,
      total
    )
    
    valueBox(
      
      value = paste0(
        pct,
        "%"
      ),
      
      subtitle = "Smart Camera Satisfactory",
      
      icon = icon("camera"),
      
      color = "purple"
      
    )
    
  })
  
  
  # ==============================================================
  # DASHBOARD OUTCOME
  # ==============================================================
  
  output$dashboard_outcome <- renderPlot({
    
    d <- database()
    
    if (
      nrow(d) == 0
    ) {
      
      ggplot() +
        theme_void() +
        labs(
          title =
            "No data available"
        )
      
    } else {
      
      plot_data <- bind_rows(
        
        data.frame(
          Device =
            "Conventional Slit Lamp",
          Outcome =
            d$Slit_Examination
        ),
        
        data.frame(
          Device =
            "Smart Eye Camera",
          Outcome =
            d$Camera_Examination
        )
        
      ) %>%
        
        filter(
          !is.na(Outcome),
          Outcome != "Not recorded"
        )
      
      ggplot(
        
        plot_data,
        
        aes(
          x = Device,
          fill = Outcome
        )
        
      ) +
        
        geom_bar(
          position = "dodge"
        ) +
        
        labs(
          
          x = NULL,
          
          y = "Number of observations",
          
          fill = "Outcome"
          
        ) +
        
        theme_minimal(
          base_size = 14
        )
      
    }
    
  })
  
  
  # ==============================================================
  # DASHBOARD GENDER
  # ==============================================================
  
  output$dashboard_gender <- renderPlot({
    
    d <- database()
    
    d <- d %>%
      filter(
        !is.na(Gender),
        Gender != "",
        Gender != "Not recorded"
      )
    
    if (
      nrow(d) == 0
    ) {
      
      ggplot() +
        theme_void() +
        labs(
          title =
            "No gender data"
        )
      
    } else {
      
      ggplot(
        d,
        aes(
          x = Gender
        )
      ) +
        
        geom_bar() +
        
        labs(
          x = "Gender",
          y = "Number of participants"
        ) +
        
        theme_minimal(
          base_size = 14
        )
      
    }
    
  })
  
  
  # ==============================================================
  # DASHBOARD AGE
  # ==============================================================
  
  output$dashboard_age <- renderPlot({
    
    d <- database()
    
    d$Age_numeric <-
      safe_numeric(
        d$Age
      )
    
    ggplot(
      d,
      aes(
        x = Age_numeric
      )
    ) +
      
      geom_histogram(
        bins = 10,
        na.rm = TRUE
      ) +
      
      labs(
        x = "Age",
        y = "Number of participants"
      ) +
      
      theme_minimal(
        base_size = 14
      )
    
  })
  
  
  # ==============================================================
  # DASHBOARD DIAGNOSIS
  # ==============================================================
  
  output$dashboard_diagnosis <- renderPlot({
    
    d <- database() %>%
      
      filter(
        !is.na(Diagnosis),
        Diagnosis != ""
      )
    
    if (
      nrow(d) == 0
    ) {
      
      ggplot() +
        theme_void() +
        labs(
          title =
            "No diagnosis data"
        )
      
    } else {
      
      diagnosis_count <-
        d %>%
        
        count(
          Diagnosis
        ) %>%
        
        arrange(
          desc(n)
        )
      
      ggplot(
        
        diagnosis_count,
        
        aes(
          x = reorder(
            Diagnosis,
            n
          ),
          y = n
        )
        
      ) +
        
        geom_col() +
        
        coord_flip() +
        
        labs(
          x = "Diagnosis",
          y = "Number of patients"
        ) +
        
        theme_minimal(
          base_size = 13
        )
      
    }
    
  })
  
  
  # ==============================================================
  # RESEARCH DATA TABLE
  # ==============================================================
  
  output$research_data <- renderDT({
    
    datatable(
      
      database(),
      
      rownames = FALSE,
      
      filter = "top",
      
      editable = TRUE,
      
      extensions = "Buttons",
      
      options = list(
        
        scrollX = TRUE,
        
        pageLength = 10,
        
        dom =
          "Bfrtip",
        
        buttons =
          c(
            "copy",
            "csv",
            "excel"
          )
        
      )
      
    )
    
  })
  
  
  # ==============================================================
  # EDIT DATA TABLE (CORRECTED)
  # ==============================================================
  
  observeEvent(
    
    input$research_data_cell_edit,
    
    {
      
      info <-
        input$research_data_cell_edit
      
      d <- database()
      
      row <- info$row
      
      col <- info$col
      
      value <- info$value
      
      if (col == which(names(d) == "Age")) {
        d[row, col] <- safe_numeric(value)
      } else {
        d[row, col] <- as.character(value)
      }
      
      database(d)
      
    }
    
  )
  
  
  # ==============================================================
  # CLINICAL PLOT
  # ==============================================================
  
  output$clinical_plot <- renderPlot({
    
    d <- database()
    
    variable <-
      input$clinical_variable
    
    
    if (
      variable == "IOP"
    ) {
      
      slit <-
        safe_numeric(
          d$IOP_Slit
        )
      
      camera <-
        safe_numeric(
          d$IOP_Camera
        )
      
      label <-
        "IOP"
      
    }
    
    
    if (
      variable == "SPH"
    ) {
      
      slit <-
        safe_numeric(
          d$SPH_Slit
        )
      
      camera <-
        safe_numeric(
          d$SPH_Camera
        )
      
      label <-
        "SPH"
      
    }
    
    
    if (
      variable == "CYL"
    ) {
      
      slit <-
        safe_numeric(
          d$CYL_Slit
        )
      
      camera <-
        safe_numeric(
          d$CYL_Camera
        )
      
      label <-
        "CYL"
      
    }
    
    
    if (
      variable == "Axis"
    ) {
      
      slit <-
        safe_numeric(
          d$Axis_Slit
        )
      
      camera <-
        safe_numeric(
          d$Axis_Camera
        )
      
      label <-
        "Axis"
      
    }
    
    
    plot_data <- data.frame(
      
      Patient =
        seq_along(slit),
      
      Slit_Lamp =
        slit,
      
      Smart_Camera =
        camera
      
    ) %>%
      
      pivot_longer(
        
        cols =
          c(
            Slit_Lamp,
            Smart_Camera
          ),
        
        names_to =
          "Device",
        
        values_to =
          "Value"
        
      )
    
    ggplot(
      
      plot_data,
      
      aes(
        x = Device,
        y = Value
      )
      
    ) +
      
      geom_boxplot(
        na.rm = TRUE
      ) +
      
      geom_jitter(
        width = 0.15,
        alpha = 0.5,
        na.rm = TRUE
      ) +
      
      labs(
        
        x = NULL,
        
        y = label
        
      ) +
      
      theme_minimal(
        base_size = 14
      )
    
  })
  
  
  # ==============================================================
  # CLINICAL SUMMARY
  # ==============================================================
  
  output$clinical_summary <- renderDT({
    
    d <- database()
    
    variable <-
      input$clinical_variable
    
    
    if (
      variable == "IOP"
    ) {
      
      slit <-
        safe_numeric(
          d$IOP_Slit
        )
      
      camera <-
        safe_numeric(
          d$IOP_Camera
        )
      
    } else if (
      variable == "SPH"
    ) {
      
      slit <-
        safe_numeric(
          d$SPH_Slit
        )
      
      camera <-
        safe_numeric(
          d$SPH_Camera
        )
      
    } else if (
      variable == "CYL"
    ) {
      
      slit <-
        safe_numeric(
          d$CYL_Slit
        )
      
      camera <-
        safe_numeric(
          d$CYL_Camera
        )
      
    } else {
      
      slit <-
        safe_numeric(
          d$Axis_Slit
        )
      
      camera <-
        safe_numeric(
          d$Axis_Camera
        )
      
    }
    
    
    summary_table <- data.frame(
      
      Device = c(
        "Conventional Slit Lamp",
        "Smart Eye Camera"
      ),
      
      N = c(
        sum(!is.na(slit)),
        sum(!is.na(camera))
      ),
      
      Mean = c(
        mean(
          slit,
          na.rm = TRUE
        ),
        mean(
          camera,
          na.rm = TRUE
        )
      ),
      
      SD = c(
        sd(
          slit,
          na.rm = TRUE
        ),
        sd(
          camera,
          na.rm = TRUE
        )
      ),
      
      Median = c(
        median(
          slit,
          na.rm = TRUE
        ),
        median(
          camera,
          na.rm = TRUE
        )
      )
      
    )
    
    summary_table[, -1] <-
      round(
        summary_table[, -1],
        3
      )
    
    datatable(
      summary_table,
      rownames = FALSE
    )
    
  })
  
  
  # ==============================================================
  # CLINICAL TEST
  # ==============================================================
  
  output$clinical_test <- renderPrint({
    
    d <- database()
    
    variable <-
      input$clinical_variable
    
    
    if (
      variable == "IOP"
    ) {
      
      x <-
        safe_numeric(
          d$IOP_Slit
        )
      
      y <-
        safe_numeric(
          d$IOP_Camera
        )
      
    } else if (
      variable == "SPH"
    ) {
      
      x <-
        safe_numeric(
          d$SPH_Slit
        )
      
      y <-
        safe_numeric(
          d$SPH_Camera
        )
      
    } else if (
      variable == "CYL"
    ) {
      
      x <-
        safe_numeric(
          d$CYL_Slit
        )
      
      y <-
        safe_numeric(
          d$CYL_Camera
        )
      
    } else {
      
      x <-
        safe_numeric(
          d$Axis_Slit
        )
      
      y <-
        safe_numeric(
          d$Axis_Camera
        )
      
    }
    
    
    complete <-
      complete.cases(
        x,
        y
      )
    
    x <- x[complete]
    y <- y[complete]
    
    
    cat(
      "\nPaired observations:",
      length(x),
      "\n\n"
    )
    
    
    if (
      length(x) < 2
    ) {
      
      cat(
        "At least 2 paired observations are required."
      )
      
      return()
      
    }
    
    
    cat(
      "Paired t-test\n"
    )
    
    print(
      t.test(
        x,
        y,
        paired = TRUE
      )
    )
    
    
    cat(
      "\n\nWilcoxon signed-rank test\n"
    )
    
    print(
      wilcox.test(
        x,
        y,
        paired = TRUE,
        exact = FALSE
      )
    )
    
  })
  
  
  # ==============================================================
  # TECHNICAL VARIABLE MAP
  # ==============================================================
  
  technical_map <- reactive({
    
    list(
      
      "Convenience / Ease" =
        c(
          "Slit_Convenience",
          "Camera_Convenience"
        ),
      
      "Accuracy & Clarity" =
        c(
          "Slit_Accuracy",
          "Camera_Accuracy"
        ),
      
      "Time Efficiency" =
        c(
          "Slit_Time_Efficiency",
          "Camera_Time_Efficiency"
        ),
      
      "Training & Learning Curve" =
        c(
          "Slit_Training",
          "Camera_Training"
        ),
      
      "External Light Source" =
        c(
          "Slit_External_Light",
          "Camera_External_Light"
        ),
      
      "Portability" =
        c(
          "Slit_Portability",
          "Camera_Portability"
        ),
      
      "Recording & Storage" =
        c(
          "Slit_Recording",
          "Camera_Recording"
        ),
      
      "Clinical Examination" =
        c(
          "Slit_Clinical_Examination",
          "Camera_Clinical_Examination"
        ),
      
      "Resolution" =
        c(
          "Slit_Resolution",
          "Camera_Resolution"
        ),
      
      "Field of View" =
        c(
          "Slit_Field_of_View",
          "Camera_Field_of_View"
        )
      
    )
    
  })
  
  
  # ==============================================================
  # TECHNICAL PLOT
  # ==============================================================
  
  output$technical_plot <- renderPlot({
    
    d <- database()
    
    # Get selected variable mapping
    selected <- technical_map()[[input$technical_variable]]
    
    # Get corresponding database columns
    slit <- d[[selected[1]]]
    camera <- d[[selected[2]]]
    
    # Create plotting data
    plot_data <- bind_rows(
      
      data.frame(
        Device = "Conventional Slit Lamp",
        Response = slit
      ),
      
      data.frame(
        Device = "Smart Eye Camera",
        Response = camera
      )
      
    ) %>%
      
      filter(
        !is.na(Response),
        Response != "",
        Response != "Not recorded"
      )
    
    # If no data is available
    if (nrow(plot_data) == 0) {
      
      ggplot() +
        theme_void() +
        labs(
          title = "No data available"
        )
      
    } else {
      
      # Percentage chart
      if (input$technical_chart == "Percentage") {
        
        ggplot(
          plot_data,
          aes(
            x = Device,
            fill = Response
          )
        ) +
          
          geom_bar(
            position = "fill"
          ) +
          
          scale_y_continuous(
            labels = percent_format()
          ) +
          
          labs(
            x = NULL,
            y = "Percentage",
            fill = "Response"
          ) +
          
          theme_minimal(
            base_size = 14
          )
        
      } else {
        
        # Count chart
        ggplot(
          plot_data,
          aes(
            x = Device,
            fill = Response
          )
        ) +
          
          geom_bar(
            position = "dodge"
          ) +
          
          labs(
            x = NULL,
            y = "Count",
            fill = "Response"
          ) +
          
          theme_minimal(
            base_size = 14
          )
      }
    }
  })
  
  
  # ==============================================================
  # TECHNICAL TABLE
  # ==============================================================
  
  output$technical_table <- renderDT({
    
    d <- database()
    
    selected <-
      technical_map()[[input$technical_variable]]
    
    slit <-
      d[[selected[1]]]
    
    camera <-
      d[[selected[2]]]
    
    
    responses <-
      sort(
        unique(
          c(
            slit,
            camera
          )
        )
      )
    
    responses <-
      responses[
        !is.na(responses) &
          responses != "" &
          responses != "Not recorded"
      ]
    
    
    result <- data.frame()
    
    
    for (
      response in responses
    ) {
      
      slit_n <-
        sum(
          slit == response,
          na.rm = TRUE
        )
      
      camera_n <-
        sum(
          camera == response,
          na.rm = TRUE
        )
      
      slit_total <-
        sum(
          !is.na(slit) &
            slit != "" &
            slit != "Not recorded"
        )
      
      camera_total <-
        sum(
          !is.na(camera) &
            camera != "" &
            camera != "Not recorded"
        )
      
      
      result <-
        bind_rows(
          
          result,
          
          data.frame(
            
            Response =
              response,
            
            Slit_Lamp_n =
              slit_n,
            
            Slit_Lamp_percent =
              percentage(
                slit_n,
                slit_total
              ),
            
            Smart_Camera_n =
              camera_n,
            
            Smart_Camera_percent =
              percentage(
                camera_n,
                camera_total
              )
            
          )
          
        )
      
    }
    
    
    datatable(
      result,
      rownames = FALSE
    )
    
  })
  
  
  # ==============================================================
  # McNEMAR TEST
  # ==============================================================
  
  output$mcnemar_result <- renderPrint({
    
    d <- database()
    
    paired <-
      d %>%
      
      filter(
        
        Slit_Examination %in%
          c(
            "Satisfactory",
            "Unsatisfactory"
          ),
        
        Camera_Examination %in%
          c(
            "Satisfactory",
            "Unsatisfactory"
          )
        
      )
    
    
    if (
      nrow(paired) < 2
    ) {
      
      cat(
        "Not enough paired observations."
      )
      
      return()
      
    }
    
    
    tbl <-
      table(
        
        Slit =
          paired$Slit_Examination,
        
        Camera =
          paired$Camera_Examination
        
      )
    
    
    cat(
      "Paired contingency table:\n\n"
    )
    
    print(tbl)
    
    
    cat(
      "\n\nMcNemar test:\n\n"
    )
    
    
    if (
      all(
        dim(tbl) == c(2,2)
      )
    ) {
      
      print(
        mcnemar.test(
          tbl
        )
      )
      
    } else {
      
      cat(
        "2 x 2 paired table required."
      )
      
    }
    
  })
  
  
  # ==============================================================
  # COHEN KAPPA
  # ==============================================================
  
  output$kappa_result <- renderPrint({
    
    d <- database()
    
    paired <-
      d %>%
      
      filter(
        
        Slit_Examination %in%
          c(
            "Satisfactory",
            "Unsatisfactory"
          ),
        
        Camera_Examination %in%
          c(
            "Satisfactory",
            "Unsatisfactory"
          )
        
      )
    
    
    if (
      nrow(paired) < 2
    ) {
      
      cat(
        "Not enough paired observations."
      )
      
      return()
      
    }
    
    
    kappa_data <-
      data.frame(
        
        Slit =
          factor(
            paired$Slit_Examination
          ),
        
        Camera =
          factor(
            paired$Camera_Examination
          )
        
      )
    
    
    cat(
      "Cohen's Kappa:\n\n"
    )
    
    
    print(
      kappa2(
        kappa_data,
        weight = "unweighted"
      )
    )
    
  })
  
  
  # ==============================================================
  # PAIRED T-TEST IOP
  # ==============================================================
  
  output$ttest_result <- renderPrint({
    
    d <- database()
    
    x <-
      safe_numeric(
        d$IOP_Slit
      )
    
    y <-
      safe_numeric(
        d$IOP_Camera
      )
    
    
    complete <-
      complete.cases(
        x,
        y
      )
    
    
    x <- x[complete]
    y <- y[complete]
    
    
    if (
      length(x) < 2
    ) {
      
      cat(
        "At least 2 paired IOP observations are required."
      )
      
      return()
      
    }
    
    
    print(
      
      t.test(
        
        x,
        y,
        
        paired = TRUE
        
      )
      
    )
    
  })
  
  
  # ==============================================================
  # WILCOXON IOP
  # ==============================================================
  
  output$wilcox_result <- renderPrint({
    
    d <- database()
    
    x <-
      safe_numeric(
        d$IOP_Slit
      )
    
    y <-
      safe_numeric(
        d$IOP_Camera
      )
    
    
    complete <-
      complete.cases(
        x,
        y
      )
    
    x <- x[complete]
    y <- y[complete]
    
    
    if (
      length(x) < 2
    ) {
      
      cat(
        "At least 2 paired IOP observations are required."
      )
      
      return()
      
    }
    
    
    print(
      
      wilcox.test(
        
        x,
        y,
        
        paired = TRUE,
        
        exact = FALSE
        
      )
      
    )
    
  })
  
  
  # ==============================================================
  # TABLE 1 - DEMOGRAPHICS (CORRECTED WITH SAFE STATS)
  # ==============================================================
  
  output$table_demographics <- renderDT({
    
    d <- database()
    
    
    total <-
      nrow(d)
    
    age <-
      safe_numeric(
        d$Age
      )
    
    
    result <- data.frame(
      
      Variable = c(
        
        "Sample size",
        
        "Mean age",
        
        "SD age",
        
        "Median age",
        
        "Minimum age",
        
        "Maximum age",
        
        "Male",
        
        "Female"
        
      ),
      
      Value = c(
        
        total,
        
        round(
          safe_mean(age),
          2
        ),
        
        round(
          safe_sd(age),
          2
        ),
        
        round(
          safe_median(age),
          2
        ),
        
        safe_min(age),
        
        safe_max(age),
        
        sum(
          d$Gender == "Male",
          na.rm = TRUE
        ),
        
        sum(
          d$Gender == "Female",
          na.rm = TRUE
        )
        
      )
      
    )
    
    
    datatable(
      result,
      rownames = FALSE
    )
    
  })
  
  
  # ==============================================================
  # TABLE 2 - EXAMINATION
  # ==============================================================
  
  output$table_examination <- renderDT({
    
    d <- database()
    
    
    result <- data.frame(
      
      Outcome = c(
        
        "Satisfactory",
        
        "Unsatisfactory"
        
      ),
      
      Slit_Lamp = c(
        
        sum(
          d$Slit_Examination ==
            "Satisfactory",
          na.rm = TRUE
        ),
        
        sum(
          d$Slit_Examination ==
            "Unsatisfactory",
          na.rm = TRUE
        )
        
      ),
      
      Smart_Eye_Camera = c(
        
        sum(
          d$Camera_Examination ==
            "Satisfactory",
          na.rm = TRUE
        ),
        
        sum(
          d$Camera_Examination ==
            "Unsatisfactory",
          na.rm = TRUE
        )
        
      )
      
    )
    
    
    datatable(
      result,
      rownames = FALSE
    )
    
  })
  
  
  # ==============================================================
  # TABLE 3 - TECHNICAL
  # ==============================================================
  
  output$table_technical <- renderDT({
    
    d <- database()
    
    
    parameters <- c(
      
      "Convenience / Ease",
      
      "Accuracy & Clarity",
      
      "Time Efficiency",
      
      "Training & Learning Curve",
      
      "External Light Source",
      
      "Portability",
      
      "Recording & Storage",
      
      "Clinical Examination",
      
      "Resolution",
      
      "Field of View"
      
    )
    
    
    result <- data.frame(
      
      Parameter =
        parameters,
      
      Slit_Lamp_Positive = NA,
      
      Smart_Camera_Positive = NA
      
    )
    
    
    # Convenience
    
    result$Slit_Lamp_Positive[1] <-
      sum(
        d$Slit_Convenience ==
          "Very Easy",
        na.rm = TRUE
      )
    
    result$Smart_Camera_Positive[1] <-
      sum(
        d$Camera_Convenience ==
          "Very Easy",
        na.rm = TRUE
      )
    
    
    # Accuracy
    
    result$Slit_Lamp_Positive[2] <-
      sum(
        d$Slit_Accuracy ==
          "Satisfied",
        na.rm = TRUE
      )
    
    result$Smart_Camera_Positive[2] <-
      sum(
        d$Camera_Accuracy ==
          "Satisfied",
        na.rm = TRUE
      )
    
    
    # Time
    
    result$Slit_Lamp_Positive[3] <-
      sum(
        d$Slit_Time_Efficiency ==
          "Yes",
        na.rm = TRUE
      )
    
    result$Smart_Camera_Positive[3] <-
      sum(
        d$Camera_Time_Efficiency ==
          "Yes",
        na.rm = TRUE
      )
    
    
    # Training
    
    result$Slit_Lamp_Positive[4] <-
      sum(
        d$Slit_Training ==
          "Yes",
        na.rm = TRUE
      )
    
    result$Smart_Camera_Positive[4] <-
      sum(
        d$Camera_Training ==
          "Yes",
        na.rm = TRUE
      )
    
    
    # External light
    
    result$Slit_Lamp_Positive[5] <-
      sum(
        d$Slit_External_Light ==
          "Yes",
        na.rm = TRUE
      )
    
    result$Smart_Camera_Positive[5] <-
      sum(
        d$Camera_External_Light ==
          "Yes",
        na.rm = TRUE
      )
    
    
    # Portability
    
    result$Slit_Lamp_Positive[6] <-
      sum(
        d$Slit_Portability ==
          "Yes",
        na.rm = TRUE
      )
    
    result$Smart_Camera_Positive[6] <-
      sum(
        d$Camera_Portability ==
          "Yes",
        na.rm = TRUE
      )
    
    
    # Recording
    
    result$Slit_Lamp_Positive[7] <-
      sum(
        d$Slit_Recording ==
          "Yes",
        na.rm = TRUE
      )
    
    result$Smart_Camera_Positive[7] <-
      sum(
        d$Camera_Recording ==
          "Yes",
        na.rm = TRUE
      )
    
    
    # Clinical examination
    
    result$Slit_Lamp_Positive[8] <-
      sum(
        d$Slit_Clinical_Examination ==
          "Yes",
        na.rm = TRUE
      )
    
    result$Smart_Camera_Positive[8] <-
      sum(
        d$Camera_Clinical_Examination ==
          "Yes",
        na.rm = TRUE
      )
    
    
    # Resolution
    
    result$Slit_Lamp_Positive[9] <-
      sum(
        d$Slit_Resolution ==
          "Yes",
        na.rm = TRUE
      )
    
    result$Smart_Camera_Positive[9] <-
      sum(
        d$Camera_Resolution ==
          "Yes",
        na.rm = TRUE
      )
    
    
    # Field of view
    
    result$Slit_Lamp_Positive[10] <-
      sum(
        d$Slit_Field_of_View ==
          "Yes",
        na.rm = TRUE
      )
    
    result$Smart_Camera_Positive[10] <-
      sum(
        d$Camera_Field_of_View ==
          "Yes",
        na.rm = TRUE
      )
    
    
    datatable(
      
      result,
      
      rownames = FALSE,
      
      options = list(
        scrollX = TRUE
      )
      
    )
    
  })
  
  
  # ==============================================================
  # DATA QUALITY REPORT
  # ==============================================================
  
  output$quality_report <- renderPrint({
    
    d <- database()
    
    
    if (
      nrow(d) == 0
    ) {
      
      cat(
        "No records available."
      )
      
      return()
      
    }
    
    
    # ------------------------------------------------------------
    # DUPLICATE IDs
    # ------------------------------------------------------------
    
    duplicate_ids <-
      sum(
        duplicated(
          d$Study_ID
        )
      )
    
    
    # ------------------------------------------------------------
    # MISSING IDs
    # ------------------------------------------------------------
    
    missing_ids <-
      sum(
        is.na(
          d$Study_ID
        ) |
          trimws(
            d$Study_ID
          ) == ""
      )
    
    
    # ------------------------------------------------------------
    # MISSING AGE
    # ------------------------------------------------------------
    
    missing_age <-
      sum(
        is.na(
          d$Age
        ) |
          d$Age == ""
      )
    
    
    # ------------------------------------------------------------
    # MISSING DIAGNOSIS
    # ------------------------------------------------------------
    
    missing_diagnosis <-
      sum(
        is.na(
          d$Diagnosis
        ) |
          trimws(
            d$Diagnosis
          ) == ""
      )
    
    
    # ------------------------------------------------------------
    # MISSING EXAM
    # ------------------------------------------------------------
    
    missing_slit <-
      sum(
        is.na(
          d$Slit_Examination
        ) |
          d$Slit_Examination ==
          "Not recorded"
      )
    
    
    missing_camera <-
      sum(
        is.na(
          d$Camera_Examination
        ) |
          d$Camera_Examination ==
          "Not recorded"
      )
    
    
    # ------------------------------------------------------------
    # OUTPUT
    # ------------------------------------------------------------
    
    cat(
      "============================================\n"
    )
    
    cat(
      "DATA QUALITY REPORT\n"
    )
    
    cat(
      "============================================\n\n"
    )
    
    cat(
      "Total records:",
      nrow(d),
      "\n"
    )
    
    cat(
      "Duplicate Study IDs:",
      duplicate_ids,
      "\n"
    )
    
    cat(
      "Missing Study IDs:",
      missing_ids,
      "\n"
    )
    
    cat(
      "Missing Age:",
      missing_age,
      "\n"
    )
    
    cat(
      "Missing Diagnosis:",
      missing_diagnosis,
      "\n"
    )
    
    cat(
      "Missing Slit Lamp examination:",
      missing_slit,
      "\n"
    )
    
    cat(
      "Missing Smart Camera examination:",
      missing_camera,
      "\n\n"
    )
    
    
    cat(
      "RECOMMENDATION:\n"
    )
    
    cat(
      "Resolve duplicate IDs and important missing values before final statistical analysis.\n"
    )
    
  })
  
  
  # ==============================================================
  # RESEARCH EXPORT DATA
  # ==============================================================
  #
  # IMPORTANT:
  # Patient Name and Address are removed from research export.
  #
  # ==============================================================
  
  research_export <- reactive({
    
    d <- database()
    
    
    d %>%
      
      select(
        -Patient_Name,
        -Address
      )
    
  })
  
  
  # ==============================================================
  # CSV DOWNLOAD
  # ==============================================================
  
  output$download_csv <-
    downloadHandler(
      
      filename = function() {
        
        paste0(
          "Smart_Eye_Research_Data_",
          Sys.Date(),
          ".csv"
        )
        
      },
      
      content = function(file) {
        
        write_csv(
          
          research_export(),
          
          file,
          
          na = ""
          
        )
        
      }
      
    )
  
  
  # ==============================================================
  # EXCEL DOWNLOAD
  # ==============================================================
  
  output$download_excel <-
    downloadHandler(
      
      filename = function() {
        
        paste0(
          "Smart_Eye_Research_Data_",
          Sys.Date(),
          ".xlsx"
        )
        
      },
      
      content = function(file) {
        
        write_xlsx(
          
          list(
            
            Research_Data =
              research_export(),
            
            Demographics =
              database(),
            
            Examination =
              data.frame(
                
                Slit_Lamp =
                  database()$Slit_Examination,
                
                Smart_Eye_Camera =
                  database()$Camera_Examination
                
              )
            
          ),
          
          path = file
          
        )
        
      }
      
    )
  
  
  # ==============================================================
  # SUMMARY DOWNLOAD
  # ==============================================================
  
  output$download_summary <-
    downloadHandler(
      
      filename = function() {
        
        paste0(
          "Research_Summary_",
          Sys.Date(),
          ".xlsx"
        )
        
      },
      
      content = function(file) {
        
        d <- database()
        
        
        # --------------------------------------------------------
        # DEMOGRAPHIC SUMMARY
        # --------------------------------------------------------
        
        age <-
          safe_numeric(
            d$Age
          )
        
        
        demographic_table <-
          data.frame(
            
            Variable = c(
              
              "Sample size",
              
              "Mean age",
              
              "SD age",
              
              "Median age",
              
              "Male",
              
              "Female"
              
            ),
            
            Value = c(
              
              nrow(d),
              
              round(
                safe_mean(age),
                2
              ),
              
              round(
                safe_sd(age),
                2
              ),
              
              round(
                safe_median(age),
                2
              ),
              
              sum(
                d$Gender ==
                  "Male",
                na.rm = TRUE
              ),
              
              sum(
                d$Gender ==
                  "Female",
                na.rm = TRUE
              )
              
            )
            
          )
        
        
        # --------------------------------------------------------
        # EXAMINATION SUMMARY
        # --------------------------------------------------------
        
        examination_table <-
          data.frame(
            
            Outcome = c(
              
              "Satisfactory",
              
              "Unsatisfactory"
              
            ),
            
            Slit_Lamp = c(
              
              sum(
                d$Slit_Examination ==
                  "Satisfactory",
                na.rm = TRUE
              ),
              
              sum(
                d$Slit_Examination ==
                  "Unsatisfactory",
                na.rm = TRUE
              )
              
            ),
            
            Smart_Eye_Camera = c(
              
              sum(
                d$Camera_Examination ==
                  "Satisfactory",
                na.rm = TRUE
              ),
              
              sum(
                d$Camera_Examination ==
                  "Unsatisfactory",
                na.rm = TRUE
              )
              
            )
            
          )
        
        
        # --------------------------------------------------------
        # EXPORT
        # --------------------------------------------------------
        
        write_xlsx(
          
          list(
            
            Demographics =
              demographic_table,
            
            Examination =
              examination_table,
            
            Full_Research_Data =
              research_export()
            
          ),
          
          path = file
          
        )
        
      }
      
    )
  
}


# ================================================================
# 8. RUN APPLICATION
# ================================================================

shinyApp(
  
  ui = ui,
  
  server = server
  
)