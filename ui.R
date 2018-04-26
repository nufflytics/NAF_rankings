
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinycssloaders)

title = a(href = "http://www.nufflytics.com", img(src = "http://www.nufflytics.com/img/main/blockdice_logo.png", height = "20px"), "Nufflytics")

tagList(
  HTML('<header id="header">
    
       <h2><a href="http://www.nufflytics.com/">Nufflytics</a></h2>
       
       
       <nav class="links">
       <ul>
       
       <li>
       <a href="http://www.nufflytics.com/post">
       
       <i class="fa fa-newspaper-o">&nbsp;</i>Posts
       </a>
       </li>
       
       <li>
       <a href="http://www.nufflytics.com/apps">
       
       <i class="fa fa-bar-chart">&nbsp;</i>Apps
       </a>
       </li>
       
       <li>
       <a href="http://www.nufflytics.com/categories">
       
       <i class="fa fa-tag">&nbsp;</i>Categories
       </a>
       </li>
       
       <li>
       <a href="http://www.nufflytics.com/contact">
       
       <i class="fa fa-envelope">&nbsp;</i>Contact
       </a>
       </li>
       
       <li>
       <a href="http://www.nufflytics.com/donate">
       
       <i class="fa fa-paypal">&nbsp;</i>Donate
       </a>
       </li>
       
       <li>
       <a href="http://www.nufflytics.com/about">
       
       <i class="fa fa-info-circle">&nbsp;</i>About
       </a>
       </li>
       
       </ul>
       </nav>
       </header>'),
  
  shiny::fluidPage(
    h1("NAF Rankings", style="margin-top: 16px;"),
    sidebarLayout(
      sidebarPanel(width = 3,
        selectizeInput('player',"Coach:",  choices = NULL, selected = NULL, multiple = FALSE),
        conditionalPanel("input.player != ''", 
                         selectizeInput("excl_races", div("Remove races:",HTML('<i class="fa fa-question-circle-o" data-toggle="tooltip" data-placement="top" title="Select races to exclude from the ranking figure"></i>')), choices = NULL, selected = NULL, multiple = T),
                         sliderInput("bins", div("Phi distribution resolution:", HTML('<i class="fa fa-question-circle-o" data-toggle="tooltip" data-placement="top" title="Some combinations of size/screen resolution can create odd banding patterns in the phi distributions.<br/> Adjusting this value up or down may help remove them." data-html="true"></i>')),value =  10, min = 0, max = 100, step = 1)
                         )
        ),
      mainPanel(width = 9,
                 withSpinner(plotOutput("plot"), type = 3, color = "#3c3b3b",  color.background = "#f4f4f4")
        )
    )
  ),
  
  tags$head(
    tags$link(href = "https://fonts.googleapis.com/css?family=Raleway:400,800", rel = "stylesheet"),
    tags$link(href = "http://www.nufflytics.com/css/main.css", rel="stylesheet"),
    tags$link(href = "http://www.nufflytics.com/css/add-on.css", rel="stylesheet"),
    #tags$link(href = "http://www.nufflytics.com/css/extra.css", rel="stylesheet"),
    tags$script(src = "https://use.fontawesome.com/987156438c.js"),
    tags$script("$(function () { $(\"[data-toggle='tooltip']\").tooltip(); });")
  )
)

