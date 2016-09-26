#' User interface for shiny nextWord! App

library(shiny)

########## UI
shinyUI <-
  fluidPage(
    tabsetPanel(
      tabPanel("nextWord! App",
               tags$head(tags$style("body {background-color:#b8e0dd}")),
               # titlePanel("nextWord! App"),
               h1("Word Guessing with 4-gram model and Stupid Backoff",align="center"),
               hr(),
               h2("Type a phrase!"),
               h2("The system will try to guess your next word once you pause."),
               h2("Default guesses are the top 3 unigrams."),
               hr(),
               textInput("user_txt","","Enter text!"),
               h3("Suggestion:"),
               tableOutput('guess'),
               h4("If no better guess than the one one displayed, this will not change!"),
               hr()
      ),
      tabPanel("What's all this about!",
               h1("Background - Coursera Data Science Capstone Project"),
               hr(),
               tags$ul(
                 tags$li(strong("Mobile devices are small platforms where typing input is cumbersome.")),
                 tags$li(
                   strong("Voice input is one solution, but has drawbacks:"),
                   tags$ul(
                     tags$li(em("Require large computer resources")),
                     tags$li(em("May fail to recognize the user's voice:"),
                             tags$ul(
                               tags$li("Due to accents"),
                               tags$li("Temporary voice changes due to sickness, like colds")
                             )
                     )
                   )
                 ),
                 tags$li(strong("Assisted typing is another solution.")),
                 tags$li(
                   strong("'NextWord!' is a prototype assisted typing program suitable for mobile devices."),
                   tags$ul(em("It is fast and has as a small foot print."))
                 )
               ),
               hr(),
               h1("Algorithm"),
               hr(),
               tags$ul(
                 tags$li(
                   strong("Prediction engine:"),
                   tags$ul(
                     tags$li(
                       em("Statistical 4-gram language model with modified stupid back off algorithm"),
                       tags$ul(tags$li("Optimized for the small foot print of 'NextWord!'"))
                     )
                   )
                 ),
                 tags$li(
                   strong("The N-gram database:"),
                   tags$ul(
                     tags$li(
                       em("Source corpus is web text."),
                       tags$ul(tags$li("A good simulation for mobile device input."))
                     )
                   )
                 ),
                 tags$li(
                   strong("See additional details of this project in"),
                   a(href="https://api.rpubs.com/gamercier/204989","RPubs"),
                   strong("and"),
                   a(href="https://api.rpubs.com/gamercier/212721","Slides")
                 )
               ),
               hr()
      ),
      tabPanel("What's the novelty!!",
               h1("Performance"),
               hr(),
               tags$ul(
                 tags$li(
                   strong("The strength is a small foot print. Database is 4Mb"),
                   tags$ul(
                     tags$li(
                       em("Database constructed with well defined reproducible steps.")
                     )
                   )
                 ),
                 tags$li(
                   strong("It is fast with real time results."),
                   tags$ul(
                     tags$li(
                       em("Extensive use of look up tables with minimal increase in size.")
                     )
                   )
                 ),
                 tags$li(
                   strong("Extrinsic test sets provide a more realistic measure of performance."),
                   tags$ul(
                     tags$li(em("Avoids optimizing perplexity.")),
                     tags$li(em("Suitable for non-strictly probabilitic model, like stupid backoff."))
                   )
                 )
               ),
               hr(),
               h1("Brief performance statistics"),
               hr(),
               tags$ul(
                 tags$li(strong("App size (Mb): 30.9")),
                 tags$li(strong("Database size (Mb): 3.6")),
                 tags$li(strong("User time (sec): 1.3 for 1000 guesses.")),
                 tags$li(strong("Speed (msec):  1.3 per guess")),
                 tags$li(strong("Hits (within top 3 guesses):  244  out of  1000")),
                 tags$li(strong("Accuracy: 24.4 %"))
               ),
               hr()
      )
    )
  )
