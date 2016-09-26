## nextWord! prototype using shiny.
#

library(shiny)

## nextWord! App
### GLOBALS
load("nextWordApp/nextWordDB")

# helper functions
library(stringr)

toWords <- function(ngram) str_split(ngram,pattern=boundary("word"))
toStr <- function(words) lapply(words,str_c,collapse=" ")
dropFirstWord <- function(ngram) {
  words <- toWords(ngram)
  n <- length(words[[1]]) # no checks! Assumes all ngrams have same n
  toStr(lapply(words,function(s) s[2:n]))
}
getLastWord <- function(ngram){
  words <- toWords(ngram)
  toStr(lapply(words,function(s) { tail(s,1) }))
}
countWords <- function(ngram) lapply(toWords(ngram),length)

toSpace <- function(x, pattern, ...){
  return(gsub(pattern," ", x, ...))
}
toNone <- function(x, pattern, ...){
  return(gsub(pattern,"", x, ...))
}

simple.purify <- function(ngram){
  # takes ngram and return a "cleaned version"
  ## regexs
  ## contraction pattern
  CNT.PAT <-"(?<=[a-zA-Z0-9])'(?=[a-zA-Z0-9])|(?<=[a-zA-Z0-9])\"(?=[a-zA-Z0-9])"
  ## hyphens (-), en-dash(--), em-dash(---)
  HYPHEN.PAT <- "(?<=[a-zA-Z0-9 ])-(?=[a-zA-Z0-9 ])"
  ENDASH.PAT <- "(?<=[a-zA-Z0-9 ])--(?=[a-zA-Z0-9 ])" # true endash is outside ASCII code
  EMDASH.PAT <- "(?<=[a-zA-Z0-9 ])---(?=[a-zA-Z0-9 ])" # true emdash is outside ASCII code
  # bad characters.
  BAD.PAT <- "[^[:alnum:][:punct:][:space:]]"
  PUNC.PAT <- "[[:punct:]]"
  NOTALPHA <- "[^[:alpha:][:space:]]"
  MULTISPACE <- "[[:space:]]+"
  
  ngram <- toNone(ngram,pattern=NOTALPHA) # gets rid of punctuation and numbers
  ngram <- toNone(ngram,pattern=CNT.PAT,perl=TRUE) # collapse contractions
  ngram <- toNone(ngram,pattern=HYPHEN.PAT,perl=TRUE) # collapse hyphens
  ngram <- toSpace(ngram,pattern=ENDASH.PAT,perl=TRUE)
  ngram <- toSpace(ngram,pattern=EMDASH.PAT,perl=TRUE)
  ngram <- toSpace(ngram,pattern=MULTISPACE) #replaces multi space with on space
  
  ngram <- str_trim(tolower(ngram))
  while(countWords(ngram) > 3){
    ngram <- unlist(dropFirstWord(ngram))
  }
  return(ngram)
}

top3 <- function(v){
  n <- length(v)
  hits <- sort(v,partial=c(n,n-1,n-2))[n:(n-2)]
  idx <- unlist(sapply(hits, function(x) which(v == x)))
  return(v[idx])
}

# simple_guess_sb was modified 2016-09-18 stringsAsFactors = FALSE
# only needs scoresDB, basesDB, ALPHA, and TOP.UNI.SCORES
simple_guess.sb <- function(base_ngram){
  words <- unlist(toWords(base_ngram))
  n <- length(words)   # size of base_ngram. ngram size is n+1
  nMax <- n
  scores <- (ALPHA^nMax)*TOP.UNI.SCORES # ngram is unigram (base_ngram is ""), n=0
  while(n>0){
    base <- paste(words[(nMax-n+1):nMax],collapse=" ")
    hits <- base == basesDB[[n]]  # basesDB[[n]] has base for ngramsDB[[(n+1)]]
    if(sum(hits)){
      # scores <- c(scores,(ALPHA^(nMax-n))*scoresDB[[(n+1)]][ ngramsDB[[(n+1)]][hits] ])
      scores <- c(scores,(ALPHA^(nMax-n))*scoresDB[[(n+1)]][ hits ])
    }
    n <- n-1
  }
  guesses <- top3(scores)
  data.frame(guess=unlist(getLastWord(names(guesses)))[1:3],
             score=guesses[1:3], source=names(guesses),
             row.names=c("1st","2nd","3rd"),
             stringsAsFactors = FALSE)
}

guess.sb <- simple_guess.sb

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
                  a(href="https://api.rpubs.com/gamercier/204989","RPubs")
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


######### Server
shinyServer <- function(input, output) {
  output$guess <- renderTable({
    guess.sb(simple.purify(input$user_txt))
  },digits=5)
}

### Run it
shinyApp(ui=shinyUI, server=shinyServer)

