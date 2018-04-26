
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(rhdf5) # to load the .h5 data file
library(tidyverse)
library(nufflytics)

theme_nufflytics <- function(...) {
  ggplot2::theme_linedraw(base_family = "Open Sans Condensed Bold", ...) +
    ggplot2::theme(plot.subtitle = ggplot2::element_text(family = "Open Sans Condensed Light"),
                   axis.text = ggplot2::element_text(family = "Open Sans Condensed Light"),
                   legend.text = ggplot2::element_text(family="Open Sans Condensed Light"),
                   legend.position = "bottom" )
}

# data in the .h5 file are stored as a matrix  per coach 
# with races as rows and dates as columns
# we need them in a tidy format for plotting
tidy_data <- function(data, r_name, c_name) {
  data <- data %>% 
    as_data_frame() %>% 
    magrittr::set_colnames(c_name) %>% 
    mutate(race = r_name) %>% 
    gather(date, ranking, -race) %>% 
    filter(!is.nan(ranking)) %>% 
    mutate(date = lubridate::ymd(date))
}

density_data <- function(d, bins) {
  dens_data <- data_frame(
    date      = rep(d$date, each = bins), 
    last_date = rep(d$last_date, each = bins), 
    race      = rep(d$race, each = bins), 
    mu        = rep(d$mu, each=bins), 
    phi       = rep(d$phi, each = bins), 
    y         = map2(d$mu, d$phi, ~seq(.x-.y*3, .x+.y*3, length.out = bins)) %>% unlist # $bins points evenly spaced mu +/- 3 phi  
  )
  
  dens_data %>% 
    group_by(date,race) %>% 
    mutate(dens = dnorm(y, mean=mu, sd=phi)) # normal density at each bin
}

shinyServer(function(input, output, session) {
    
  all_coaches <- names(h5dump("data/rankings.h5", recursive = 2, datasetinfo = T, load = F)$coaches)
  
  observeEvent(all_coaches, updateSelectizeInput(session, "player", choices = c("Select Player" = "", all_coaches), server = T))
  
  excl = reactive({
   if(is.null(input$excl_races)) {
     ""
   } else {
    input$excl_races
   }
    }) %>% debounce(1000)
  
  bins <- reactive({input$bins}) %>% debounce(500)
  
    player_data <- reactive({
      validate(need(input$player, message = F))
      
      player_accession = paste0("coaches/", input$player)
      
      ranking_data <- h5read("data/rankings.h5", player_accession) %>% 
        map(tidy_data, h5read("data/rankings.h5", "race_ids"), h5read("data/rankings.h5", "date")) %>% 
        cbind() %>% 
        bind_cols() %>% 
        select(race, date, mu = "ranking", phi = "ranking1") %>% 
        group_by(race) %>% 
        arrange(date) %>% 
        mutate(last_date = lag(date))
      
      if(!is.null(excl())) {
        ranking_data <- ranking_data %>% 
          filter(!race %in% excl())
      }
      
      ranking_data
    })
  
    output$debug <- renderText(excl())
    output$debug2 <- renderText(input$excl_races)
    
    player_races <- reactive({
      unique(player_data()$race) %>% sort
    })
    
  observeEvent(input$player, updateSelectizeInput(session, "excl_races", choices = player_races(), server = F))
    
  output$plot <- renderPlot({
    req(nchar(input$player)>0)
    
      player_data() %>% 
        ggplot(aes(x = date, y = mu - (2.5*phi))) +
        facet_wrap(~race, ncol = 5) + 
        labs(title = "Glicko rating", subtitle = input$player, x = "", y = "Rating") +
        geom_rect( # Hidden skill probability distribution
          data = function(d) density_data(d, bins = 40+(bins()*20)), 
          aes(xmin = last_date, xmax = date, ymin = y, ymax = lead(y), alpha = dens, group=race), 
          colour=NA,
          fill="black"
        ) +
        scale_alpha_continuous(range = c(0,1)) +
        geom_line(aes(y = mu), colour = "white", size = 0.5) +
        geom_line(size = 0.8) +
        theme_nufflytics(base_size = 16) +
        theme(legend.position = "none") 
      
 
  }, height = function(x) {return(ceiling(length(player_races())/5)*250) + 30}, width = function(x) { if(length(player_races()) >=5 ) {return("auto")}; return(length(player_races()) * 200) }
) 
    
})
