#' Server for shiny Cardiovascular Calculator

library(shiny)

######### Server
shinyServer <- function(input, output) {
  output$guess <- renderTable({
    guess.sb(simple.purify(input$user_txt))
  },digits=5)
}
