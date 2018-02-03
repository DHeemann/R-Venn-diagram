library(shiny)
source("venn_diagram.R")

ui <- fluidPage(
   
   # Application title
   titlePanel("Venn Diagram"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("Ryx1",
                     "Squared correlation of y with x1:",
                     min = 0,
                     max = 1,
                     value = 0.3),
         sliderInput("Ryx2",
                     "Squared correlation of y with x2:",
                     min = 0,
                     max = 1,
                     value = 0.3),
         sliderInput("Rx1x2",
                     "Squared correlation of x1 with x2:",
                     min = 0,
                     max = 1,
                     value = 0.3)
      ),
      
      # Show the venn diagram
      mainPanel(
         plotOutput("vennplot")
      )
   )
)

# server logic required to draw the venn diagram
server <- function(input, output) {
   
   output$vennplot <- renderPlot({
      venn_diagram(input$Ryx1, input$Ryx2, input$Rx1x2)}, height = 800, width = 800
   )
}
# Run the application 
shinyApp(ui = ui, server = server)

