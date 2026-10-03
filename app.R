library(shiny)
library(ggplot2)
library(dplyr)

# ============================================================
# 1. PLANT DATA - 30 UNIQUE RECORDS
# ============================================================

plants <- data.frame(
  
  Plant_ID = sprintf("PL%03d", 1:30),
  
  Plant = c(
    "Tomato", "Rice", "Wheat", "Maize", "Potato",
    "Cotton", "Chilli", "Brinjal", "Groundnut", "Soybean",
    "Sugarcane", "Onion", "Cabbage", "Carrot", "Spinach",
    "Banana", "Mango", "Papaya", "Guava", "Grapes",
    "Tea", "Coffee", "Turmeric", "Ginger", "Pepper",
    "Coconut", "Sunflower", "Mustard", "Peas", "Okra"
  ),
  
  Temperature = c(
    29, 31, 24, 28, 22,
    30, 27, 29, 26, 25,
    32, 24, 20, 19, 23,
    28, 30, 27, 29, 26,
    21, 23, 28, 25, 24,
    30, 27, 22, 18, 28
  ),
  
  Humidity = c(
    78, 84, 65, 72, 82,
    70, 76, 80, 68, 74,
    81, 67, 86, 62, 79,
    85, 73, 77, 71, 69,
    88, 82, 75, 83, 80,
    74, 66, 64, 72, 78
  ),
  
  Soil_Moisture = c(
    52, 71, 48, 55, 68,
    44, 58, 62, 47, 53,
    76, 49, 73, 41, 65,
    79, 57, 61, 54, 46,
    82, 69, 63, 72, 67,
    75, 43, 51, 60, 56
  ),
  
  Rainfall = c(
    110, 180, 65, 95, 140,
    72, 105, 125, 58, 88,
    210, 55, 160, 45, 130,
    190, 115, 135, 90, 82,
    240, 205, 145, 175, 155,
    220, 70, 62, 100, 118
  ),
  
  Disease = c(
    "Early Blight",
    "Leaf Blast",
    "Leaf Rust",
    "Healthy",
    "Late Blight",
    "Healthy",
    "Leaf Spot",
    "Bacterial Wilt",
    "Healthy",
    "Downy Mildew",
    "Red Rot",
    "Purple Blotch",
    "Black Rot",
    "Root Rot",
    "Leaf Spot",
    "Panama Disease",
    "Anthracnose",
    "Mosaic Virus",
    "Wilt",
    "Powdery Mildew",
    "Blister Blight",
    "Coffee Leaf Rust",
    "Rhizome Rot",
    "Soft Rot",
    "Quick Wilt",
    "Bud Rot",
    "Downy Mildew",
    "White Rust",
    "Powdery Mildew",
    "Yellow Vein Mosaic"
  )
)

# ============================================================
# 2. AI-STYLE HEALTH RISK CLASSIFICATION
# ============================================================

plants$Risk_Score <-
  (plants$Humidity * 0.30) +
  (plants$Soil_Moisture * 0.30) +
  (plants$Rainfall / 4 * 0.20) +
  (plants$Temperature * 0.20)

plants$Risk_Level <- ifelse(
  plants$Risk_Score >= 55,
  "High Risk",
  ifelse(
    plants$Risk_Score >= 45,
    "Medium Risk",
    "Low Risk"
  )
)

# ============================================================
# 3. USER INTERFACE
# ============================================================

ui <- fluidPage(
  
  titlePanel("🌱 Plant Disease & Health Analyzer"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput(
        "plant",
        "Select Plant:",
        choices = plants$Plant
      ),
      
      br(),
      
      h4("Analyzer"),
      p("The system evaluates environmental conditions and estimates plant health risk.")
      
    ),
    
    mainPanel(
      
      h3("Plant Information"),
      
      textOutput("plant_id"),
      textOutput("plant_name"),
      textOutput("temperature"),
      textOutput("humidity"),
      textOutput("soil"),
      textOutput("rainfall"),
      textOutput("disease"),
      textOutput("risk_score"),
      textOutput("risk_level"),
      
      br(),
      
      plotOutput("environment_plot"),
      
      br(),
      
      plotOutput("disease_plot"),
      
      br(),
      
      plotOutput("relationship_plot")
    )
  )
)

# ============================================================
# 4. SERVER
# ============================================================

server <- function(input, output) {
  
  selected_plant <- reactive({
    
    plants %>%
      filter(Plant == input$plant)
    
  })
  
  # Plant ID
  output$plant_id <- renderText({
    
    paste(
      "Plant ID:",
      selected_plant()$Plant_ID
    )
    
  })
  
  # Plant name
  output$plant_name <- renderText({
    
    paste(
      "Plant:",
      selected_plant()$Plant
    )
    
  })
  
  # Temperature
  output$temperature <- renderText({
    
    paste(
      "Temperature:",
      selected_plant()$Temperature,
      "°C"
    )
    
  })
  
  # Humidity
  output$humidity <- renderText({
    
    paste(
      "Humidity:",
      selected_plant()$Humidity,
      "%"
    )
    
  })
  
  # Soil moisture
  output$soil <- renderText({
    
    paste(
      "Soil Moisture:",
      selected_plant()$Soil_Moisture,
      "%"
    )
    
  })
  
  # Rainfall
  output$rainfall <- renderText({
    
    paste(
      "Rainfall:",
      selected_plant()$Rainfall,
      "mm"
    )
    
  })
  
  # Disease
  output$disease <- renderText({
    
    paste(
      "Detected Condition:",
      selected_plant()$Disease
    )
    
  })
  
  # Risk score
  output$risk_score <- renderText({
    
    paste(
      "Health Risk Score:",
      round(selected_plant()$Risk_Score, 2)
    )
    
  })
  
  # Risk level
  output$risk_level <- renderText({
    
    paste(
      "Health Risk:",
      selected_plant()$Risk_Level
    )
    
  })
  
  # ==========================================================
  # GRAPH 1 - ENVIRONMENTAL CONDITIONS
  # ==========================================================
  
  output$environment_plot <- renderPlot({
    
    plant <- selected_plant()
    
    environment_data <- data.frame(
      
      Measurement = c(
        "Temperature",
        "Humidity",
        "Soil Moisture",
        "Rainfall"
      ),
      
      Value = c(
        plant$Temperature,
        plant$Humidity,
        plant$Soil_Moisture,
        plant$Rainfall / 3
      )
    )
    
    ggplot(
      environment_data,
      aes(
        x = Measurement,
        y = Value
      )
    ) +
      
      geom_col() +
      
      labs(
        title = "Plant Environmental Conditions",
        x = "Measurement",
        y = "Value"
      )
    
  })
  
  # ==========================================================
  # GRAPH 2 - DISEASE DISTRIBUTION
  # ==========================================================
  
  output$disease_plot <- renderPlot({
    
    disease_data <- plants %>%
      count(Disease) %>%
      arrange(desc(n))
    
    ggplot(
      disease_data,
      aes(
        x = reorder(Disease, n),
        y = n
      )
    ) +
      
      geom_col() +
      
      coord_flip() +
      
      labs(
        title = "Disease Distribution",
        x = "Disease",
        y = "Number of Plants"
      )
    
  })
  
  # ==========================================================
  # GRAPH 3 - TEMPERATURE VS SOIL MOISTURE
  # ==========================================================
  
  output$relationship_plot <- renderPlot({
    
    ggplot(
      plants,
      aes(
        x = Temperature,
        y = Soil_Moisture
      )
    ) +
      
      geom_point(size = 4) +
      
      labs(
        title = "Temperature vs Soil Moisture",
        x = "Temperature (°C)",
        y = "Soil Moisture (%)"
      )
    
  })
  
}

# ============================================================
# 5. RUN APPLICATION
# ============================================================

shinyApp(
  ui = ui,
  server = server
)