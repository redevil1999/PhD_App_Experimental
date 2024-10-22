# ui.R
# this is the part that defines the UI of the app

library(shiny)
library(shinythemes)
library(rsconnect)


ui <- fluidPage(
  titlePanel('Stress Management Tool - Experimental Group'),
  theme = shinytheme('cyborg'),
  
  # logout button
  div(class = "pull-right", shinyauthr::logoutUI(id = "logout")),
  
  # login section
  shinyauthr::loginUI(id = "login"),
  
  # Sidebar to show user info after login
  uiOutput("sidebarpanel")
  
)