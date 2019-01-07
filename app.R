#
library(shiny)
library(data.table)
library(tidyverse)
library(plotly)

#  geom_quad

# Define UI for application that draws a histogram
ui <- fluidPage(
   # Application title
   titlePanel("Dovetail joints. Layout for splayed pin board"),
   tags$h5("Assuming:"),
   tags$ol(
     tags$li("the board with the pins is splayed"), 
     tags$li("the widths of the dovetails and pins are uniform")
     ),
     tags$hr(),
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        wellPanel(
          numericInput("sideLength", "Length of the dovetail side (in mm) before adjustment for the splay in the side with pins", value = "200", min = 0),
          numericInput("splay", "Splay angle (in degrees)", value = "10", min = -89, max = 89),
          numericInput("boardThickness", "Thickness of the board (in mm)", value = "20", min = 0),
          numericInput("numberTails", "number of tails", value = "5", min = 1),  # pins = number of tails + 1
          numericInput("tailWidth2pinWidth", "pin : tail ratio of widths", value = "3", min = 0),
          numericInput("cutAngle", 
                       tags$div(HTML(paste0("Inner angle for tails (1:6 = 80", tags$sup("o"), ", 1:8 = 83", tags$sup("o"), ")"))), 
                      value = 8, min = 1, max = 10),
          # slope 1 in 6 = 100/80 degrees 
          radioButtons("RelativeWidthEndPins","Width of the end pins relative to the others", 
                       choices = c(
                         "50% (half width of other pins)" = 0.5, 
                         "100%  (same width as other pins)" = 1),
                       selected = 0.5)
          ),
        tags$hr(),
          tags$h6("Developed by HMichaelPower at gmail etc",
                  style = "background: seashell; color: seagreen"),
          tags$hr()
        ),
      mainPanel(
        fluidRow(
          tags$h3("For board with tails: cutting layout on the edge of the board"),
          # tags$h5("(For board with pins: cutting guidelines on board face)"),
          plotOutput("edgePlot"),
          tags$hr(),
          tags$h3("For board with tails: cutting layout on both faces of the board"),
          # tags$h5("(For board with pins: layout for cutting board edge)"),
          plotOutput("quadrilateralPlot"),
          tags$hr(),
          tags$h3("Layout for cutting dovetails"),
          dataTableOutput("edgeLayoutTbl")
          
          # uncomment for debugging
          # , tags$hr(),
          # tags$h3("Layouts for pins and tails"),
          # dataTableOutput("quadrilateralTbl"),
          # tags$hr(),
          # dataTableOutput("edgeLayoutPlotTbl"),
          # tags$hr()

        ))
      ))

# Define server logic 
server <- function(input, output) {
 
##%######################################################%##
#                                                          #
####     build the layout for dovetail edge marking     ####
#        use geom_polygon                                  #
##%######################################################%##
  
  
  edgeLayout <- reactive({
    # number of units in the joint =
    #      4 (for full/half pins at the ends)
    #    + 2 * (number of pins less 1)
    #    + 2 * (number of tails) * "ratio of width of tails to width of pins"
    nLengthUnits  <- ifelse(input$RelativeWidthEndPins == 1, 4, 2) + 2*(input$numberTails - 1)  +  2*input$numberTails*input$tailWidth2pinWidth
    unitLength   <-input$sideLength/nLengthUnits*ifelse(input$RelativeWidthEndPins == 1, 2, 1)
    relativeWidthEndPins <- as.numeric(input$RelativeWidthEndPins)
    cutAngleDeltaX <- input$boardThickness/input$cutAngle
    pinWidth      <- unitLength*ifelse(relativeWidthEndPins == 1, 1, 2)
    pinWidthEnds  <- relativeWidthEndPins*pinWidth
    tailWidth     <- pinWidth*input$tailWidth2pinWidth
    tailWidthInner <- tailWidth + 2*cutAngleDeltaX
 
    # to delete   
    # innerShrinkFactor <- (1 - 1/input$cutAngle)
    # innerStretchFactor <- (1 + 1/input$cutAngle)
    nTailsnPins     <- 1 + 2*input$numberTails
    
    jointLayout <- data.table(
      joint = c("pin-1", rep(c("tail", "pin"), times = input$numberTails)),
      left = c(rep(0, times = 1 + 2*input$numberTails)),
      right = c(rep(unitLength, times = 1 + 2*input$numberTails))    )
    
    for (cutN in 1:input$numberTails) {
      N <- 2* cutN
      # layout for tail N
      jointLayout$joint[N] <- paste0("tail-", cutN)
      jointLayout$left[N] <- jointLayout$right[N - 1] # left for joint = right for preceeding joint
      jointLayout$right[N] <- jointLayout$left[N] + tailWidth # right for tail
      
      # layout for the following pin
      jointLayout$joint[N + 1] <- paste0("pin-", cutN + 1)
      jointLayout$left[N + 1] <- jointLayout$right[N] # left for joint = right for preceeding joint
      jointLayout$right[N + 1] <- jointLayout$left[N + 1] + pinWidth # right for pin
    }
    # If end pins are 50% of other pins, correct the width of the last pin
      jointLayout$right[nTailsnPins] <- input$sideLength # last pin can be half-width
    jointLayout
  })
  
  output$edgeLayoutTbl <- renderDataTable(edgeLayout())
  
  edgeLayout4Plot <- reactive({
    
    edgeLayout4Plot <- edgeLayout() %>% 
      transmute(.,
             joint  = joint, # select 4th point of rectangle
             x = right,
             y = 0)
    
    edgeLayout4Plot <- edgeLayout() %>% 
      transmute(.,
             joint  = joint, # select 3rd point of rectangle
             x = right,
             y = input$boardThickness) %>% 
      bind_rows(., edgeLayout4Plot)
    
    edgeLayout4Plot <- edgeLayout() %>% 
      transmute(.,
             joint  = joint, # select 2nd point of rectangle
             x = left,
             y = input$boardThickness) %>% 
      bind_rows(., edgeLayout4Plot)
    
    edgeLayout4Plot <- edgeLayout() %>% 
      transmute(.,
             joint  = joint, # select 1st point of rectangle
             x = left,
             y = 0) %>% 
      bind_rows(., edgeLayout4Plot) %>% 
      arrange(., joint) %>% 
      mutate(., 
             y = -y, 
             jointComponent = ifelse("pin" == substr(joint, 1, 3), "pin", "tail")
      )
    
  edgeLayout4Plot
  })
  
  output$edgeLayoutPlotTbl <- renderDataTable(edgeLayout4Plot())
  
  output$edgePlot <- renderPlot({
    ggplot(edgeLayout4Plot(), aes(x = x, y = y)) +
      geom_polygon(aes(fill = jointComponent, group = joint)) +
      annotate("text",
               x = edgeLayout()$left[-1],
               y = 0.2*input$boardThickness,
               label = paste0(" ", round(edgeLayout()$left[-1], 1)),
               colour = "black", 
               hjust = c(rep(c("left", "right"), length(edgeLayout()$left[-1])/2))
      ) +
      annotate("text",
               x = input$sideLength/2,
               y = -1.50* input$boardThickness,
               label = "If you can't read the numbers, scroll down to see the table with the cutting layout.",
               colour = "darkblue",
               size = 5
      ) +
      scale_x_continuous(limits=c(0, input$sideLength)) +
      scale_y_continuous(
        breaks = NULL,
        limits = c(-input$sideLength, 0.2*input$boardThickness)
        ) +
      xlab("length along the joint edge (mm)") +
      ylab(NULL) +
      theme(axis.text.y = element_blank())
  })
  
##%######################################################%##
#                                                          #
####      build the layout for dovetail face marking       ####
#         use geom_polygon                                 #
##%######################################################%##
 
  quadLayout <- reactive({
 
    nTailsnPins  <- 1 + 2*input$numberTails
    nLengthUnits  <- ifelse(input$RelativeWidthEndPins == 1, 4, 2) + 2*(input$numberTails - 1)  +  2*input$numberTails*input$tailWidth2pinWidth
    unitLength   <-input$sideLength/nLengthUnits*ifelse(input$RelativeWidthEndPins == 1, 2, 1)
    relativeWidthEndPins <- as.numeric(input$RelativeWidthEndPins)
    cutAngleDeltaX <- input$boardThickness/input$cutAngle
    pinWidth      <- unitLength*ifelse(relativeWidthEndPins == 1, 1, 2)
    pinWidthEnds  <- pinWidth*relativeWidthEndPins
    pinWidthInner <- pinWidth - 2*cutAngleDeltaX
    tailWidth     <- pinWidth*input$tailWidth2pinWidth
    tailWidthInner <- tailWidth + 2*cutAngleDeltaX

    # initialise the table of quadrilaterals
    quadLayout <- data.table(
      jointComponent = c(
        rep( 
          c(rep("pin", times = 4), rep("tail", times = 4)), 
          times = input$numberTails), rep("pin", times = 4)),
      jointComponentNumber = rep("1", times = nTailsnPins), 
      jointName = rep("pin-1", times = nTailsnPins),
      quadX = rep(0, times = nTailsnPins),
      quadY = c(rep(c(0, 0, -input$boardThickness, -input$boardThickness), times = 2*input$numberTails  + 1))
    )
  
    # complete the definition of the quadrilateral for the first pin
    quadLayout$quadX[1:4] <- c(
      0, # outer left of pin
      pinWidthEnds, # outer right of pin
      pinWidthEnds - cutAngleDeltaX, # innner right of pin
      0) # inner left of pin

    # define quadrilaterals for the subsequent tails and pins
    for (setN in 1:input$numberTails)  { # 1 tail and 1 pin per set
      rowN <- setN * 8 - 3 # skip first 4 rows for first pin, which was defined above
      
      # define tail
      quadLayout$jointComponentNumber[rowN:(rowN + 3)] <- setN # 4 points per tail; 1 row per point
      quadLayout$jointName[rowN:(rowN + 3)] <- paste0("tail-", setN)
      quadLayout$quadX[rowN + 0] <- quadLayout$quadX[rowN - 3] # outer left of tail = outer right of previous pin
      quadLayout$quadX[rowN + 1] <- quadLayout$quadX[rowN] + tailWidth # outer right of tail
      quadLayout$quadX[rowN + 2] <- quadLayout$quadX[rowN + 1] + cutAngleDeltaX # inner right of tail
      quadLayout$quadX[rowN + 3] <- quadLayout$quadX[rowN] - cutAngleDeltaX # inner left of tail
      
      # define pin
      rowN <-  rowN + 4
      quadLayout$jointComponentNumber[rowN:(rowN + 3)] <- setN + 1 # 4 points per pin; 1 row per point
      quadLayout$jointName[rowN:(rowN + 3)] <- paste0("pin-", (setN + 1))
      quadLayout$quadX[rowN + 0] <- quadLayout$quadX[rowN - 3] # outer left of pin = outer right of previous tail
      quadLayout$quadX[rowN + 1] <- quadLayout$quadX[rowN] + pinWidth # outer right of pin
      quadLayout$quadX[rowN + 2] <- quadLayout$quadX[rowN + 1] + cutAngleDeltaX # inner right of pin
      quadLayout$quadX[rowN + 3] <- quadLayout$quadX[rowN] - cutAngleDeltaX # inner left of tail
  }
  
  # convert last pin's definition to one for a half pin

  quadLayout$quadX[rowN + 1] <- input$sideLength # outer right of pin - length of that side
  quadLayout$quadX[rowN + 2] <- quadLayout$quadX[rowN + 1] # inner right of pin - length of that side
  quadLayout
    })
    
output$quadrilateralTbl <- renderDataTable(quadLayout())

output$quadrilateralPlot <- renderPlot({
  ggplot(quadLayout(), aes(x = quadX, y = quadY)) +
    geom_polygon(aes(fill = jointComponent, group = jointName)) +
    annotate("text",
             x = input$sideLength/15,
             y = -1.25*input$boardThickness,
             label = paste0("width of inner pin = ", round(
               quadLayout()$quadX[2] - quadLayout()$quadX[1], digits = 2)),
             colour = "darkblue", hjust = "left",
             size = 5
    ) +
    annotate("text",
             x = input$sideLength/15,
             y = -1.25*input$boardThickness - 12,
             label = paste0("width of outer pin = ", round(
               quadLayout()$quadX[3] - quadLayout()$quadX[4], digits = 2)),
             colour = "darkblue", hjust = "left",
             size = 5
    ) +
    annotate("text",
             x = input$sideLength/15,
             y = -1.25*input$boardThickness - 36,
             label = paste0("width of inner dovetail = ", round(
               quadLayout()$quadX[6] - quadLayout()$quadX[5], digits = 2)),
             colour = "darkblue", hjust = "left",
             size = 5
    ) +
    annotate("text",
             x = input$sideLength/15,
             y = -1.25*input$boardThickness - 48,
             label = paste0("width of outer dovetail = ", round(
               quadLayout()$quadX[7] - quadLayout()$quadX[8], digits = 2)),
             colour = "darkblue", hjust = "left",
             size = 5
    ) +
    scale_x_continuous(limits=c(0, input$sideLength)) +
    scale_y_continuous(
      breaks = NULL,
      limits = c(-input$sideLength, 0.2*input$boardThickness)
    ) +
    xlab("length along the joint edge (mm)") +
    ylab(NULL) 
})

}

# Run the application 
shinyApp(ui = ui, server = server)

