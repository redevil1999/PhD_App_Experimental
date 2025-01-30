# The server from which the app runs

# add the libraries
library(shiny)
library(tidyverse)
library(gganimate)
library(ggforce)
library(gifski)
library(lubridate)
library(rsconnect)
library(googlesheets4)
library(googledrive)
library(viridis)



# data for the breathing guidance
breathing_circle_data <- data.frame(
  x0 = c(5, each = 5),
  y0 = c(5, each = 5),
  r = c(0.2, 0.5, 1.25, 2.5, 3.75, 5, 7.5, 8.75, 10, 10, 10, 10, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0.5, 0.2),
  speed_maybe = c(1:24)
)


server <- function(input, output, session) {
  
  
  # Default breathing time
  default_breathingtime <- 10
  
  # UI for logged-in users
  output$sidebarpanel <- renderUI({
    #req(credentials()$user_auth)
    #column(width = 4, p(paste("You have", credentials()$info[["permissions"]], "permission")))
    
    navbarPage('NTNU',
               tabPanel('Guided Breathing',
                        paste("Hello!", "Welcome to the guided breathing!"),
                        p("The pace of this breathing is set to what was measured in the lab."),
                          p("Remember to inhale as the circle expands and exhale as it contracts."),
                          mainPanel(plotOutput("distPlot"),
                                    hr(),
                                    actionButton('start', 'Start'),
                                    actionButton('stop', 'Stop'),
                                    actionButton('reset', 'Reset'),
                                    numericInput('minutes', 'Minutes:', value = default_breathingtime, min = 0, max = 99999, step = 1),
                                    textOutput('timeleft'),
                                    paste("If you would like to change the duration, remember to press reset after entering a different number."),
                                    tags$a(href = "https://nettskjema.no/a/481084", "Please fill out this short questionnaire to confirm that you have completed a session", target = "_blank"))
               )
    )
  })
  
  # Create Timer
  timer <- reactiveVal(default_breathingtime * 60)
  active <- reactiveVal(FALSE)
  session_start_time <- reactiveVal(0)  # Store the initial time when user starts
  session_started <- reactiveVal(FALSE) # Initialize session_started variable
  
  # Time left output
  output$timeleft <- renderText({
    paste("Time left: ", seconds_to_period(timer()))
  })
  
  # Observe start button click
  observeEvent(input$start, {
    session_start_time(timer())  # Store initial time
    active(TRUE)  # Start timer
    session_started(TRUE) #add a start variable for the fact that they started a session

  })
  
  # Observer for countdown and session tracking
  observe({
    invalidateLater(1000, session)
    isolate({
      if (active()) {
        timer(timer() - 1)
        
        
        if (timer() < 1) {
          active(FALSE)
          showModal(modalDialog(
            title = "Important message",
            "Session completed! The session tracker will update the next time you log in."
          ))
          
        }
      }
    })
  })
  

  
  # Action button observers
  observeEvent(input$stop, { active(FALSE) })
  observeEvent(input$reset, { timer(input$minutes * 60) })
  
  # Plot
  output$distPlot <- renderImage({
    
    
    # create a temporary file
    outfile <- tempfile(fileext = '.gif')
    
    # make the animation
    
    Breathing_Circle <- ggplot() +
      geom_circle(data = breathing_circle_data, mapping = aes(x0 = x0, y0 = y0, r = r, col = r, fill = r), show.legend = FALSE) +
      scale_fill_viridis(option = "mako") +
      theme(axis.line=element_blank(),axis.text.x=element_blank(),
            axis.text.y=element_blank(),axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),legend.position="none",
            panel.border=element_blank(),panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),panel.background = element_rect(fill = 'black', color = 'black')) +
      coord_fixed () +
      transition_states(speed_maybe,
                        transition_length = 1,
                        state_length = 0) +
      enter_fade() +
      exit_shrink() +
      ease_aes('linear')
    

    # save the animation
    animate(Breathing_Circle, nframes = 200, renderer = gifski_renderer('outfile.gif'), duration = 12)
    
    # Return a list containing the filename
    list(src = "outfile.gif",
         contentType = 'image/gif'
         # width = 400,
         # height = 300,
         # alt = "This is alternate text"
    )}, deleteFile = TRUE)
  
}