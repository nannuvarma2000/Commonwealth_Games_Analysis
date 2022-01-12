#Importing necessary libraries
library(shiny)
library(shinydashboard)
library(bslib)
library(ggplot2)
library(leaflet)
library(reshape)
library(ggthemes)
library(hrbrthemes)
library(ggthemr)
library(bbplot)
library(gganimate)
#devtools::install_github('bbc/bbplot')

#UI Component
ui <- shinyUI(
      navbarPage("Commonwealth Games Analysis",theme = bs_theme(version = 4, bootswatch = "cosmo"),
                 tabPanel("Home",
                          fluidPage(column(6),
                                    column(12,
                                           div(id="intro-head",h1("The Commonwealth Games"),
                                               h2("An Indepth Analysis and Visualization of Commonwealth Games"),
                                               hr(),
                                               img(id="games-logo", src="cgf-logo.png")
                                               ),
                                           br(),
                                           hr(),
                                           br(),
                                           h3("Overview"),
                                           p("This application provides visualisations on data on the Commonwealth Games, ",
                                              "the international sporting event held every four years between members of the ", 
                                           a("Commonwealth of Nations", href="https://en.wikipedia.org/wiki/Commonwealth_of_Nations"), "."),
                                           p("The project has the following panels:"),
                                           tags$ul(
                                           tags$li(strong("Growth of the Games"), "- shows how the games has grown in numbers since its inception"),
                                           tags$li(strong("Results: Medal Tally"), "- shows the medal results for a selected year or for all years"),
                                           tags$li(strong("Results: Map View"), "- shows the participant countries and medal winnings on a map"),
                                           tags$li(strong("Country Profiles"), "- shows details about a participating country, including medals won and in which sports"),
                                           tags$li(strong("Sports and Events"), "- shows the countries that have won medals in order for any selected sport and event")
                                           ),
                                           hr(),
                                           br(),
                                           h3("About Commonwealth Games"),
                                           p(id="p","The Commonwealth Games is a world-class multi-sport meet of athletes from around the world which is governed by Commonwealth Games Federation (CGF). The Commmonwealth Games are a International multisport event.
                                           The Commonwealth is a group of countries, most of which were once associated with the British Empire.
                                           The first Commonwealth Games were held in 1930 in Hamilton, Canada, where 11 countries sent 400 athletes to take part in six sports and 59 events. Since then, the Games have been conducted every four years. 
                                           The games have gone through many changes since that time. The name has changed several times, and many sports have been added.
                                           There are currently 54 members of the Commonwealth of Nations and around 71 countries participate in the Games every fourth year.
                                           The last Commonwealth held in 2018, XXI Commonwealth Games (CWG) were held on the Gold Coast, Queensland, Australia, between 4 and 15 April 2018 with Motto of Share the Dream and 4,426 athletes participated in 275 events in 19 sports."),
                                           p("The 2022 Commonwealth Games, officially known as the XXII Commonwealth Games and commonly known as Birmingham 2022, will be an international multi-sport event for members of the Commonwealth and CWG 2022 is scheduled to be held in Birmingham, England from 27 Jul 2022 - 7 Aug 2022."),
                                       ),column(6),
                                    )
                          ),
                 tabPanel("Growth of Games", 
                          fluidPage(
                            column(3, 
                                   wellPanel(
                                     radioButtons("growthProp", label="Select number to view",
                                                  choices=c('Countries', 'Sports', 'Events', 'Athletes'), 
                                                  selected='Countries'))
                            ),
                            
                            column(9,
                                   plotOutput("growthPlot", hover=hoverOpts(id="growth.hover")),
                                   hr(),
                                   htmlOutput("yearInfo")
                            )
                          )
                 ),
                 tags$head(
                   tags$link(rel="stylesheet", type="text/css", href="style.css")
                   )
                 )
      )


# read data
growth.df <- read.csv(file="data/growth_of_the_games.csv",
                      head=TRUE, fileEncoding='UTF-8-BOM')

results.df <- read.csv(file="data/results_all_per_year_with_code.csv",
                       head=TRUE, fileEncoding='UTF-8-BOM', stringsAsFactors=FALSE)

sports.df <- read.csv(file="data/results_all_per_sport.csv",
                      head=TRUE, fileEncoding='UTF-8-BOM')
sports.df$total <- (sports.df$gold + sports.df$silver + sports.df$bronze)

# years
uniqueYears <- as.list(sort(unique(results.df$year), decreasing=T)) # decreasing

# sports
uniqueSports <- as.list(sort(unique(sports.df$sport)))

# regions/continents
uniqueRegions <- as.list(sort(unique(results.df$continent)))
uniqueRegions <- c('All', uniqueRegions)

# region/continent colors
region.colors <- rainbow(6, alpha=NULL)

# color for each region/continent
getRegionColor <- function(df) {
  sapply(df$continent, function(continent) {
    #cat(file=stderr(), paste0(continent, ' '))
    if (continent=='Asia') { region.colors[1] }
    else if (continent=='Africa') { region.colors[2] }
    else if (continent=='Americas') { region.colors[3] }
    else if (continent=='Caribbean') { region.colors[4] }
    else if (continent=='Europe') { region.colors[5] }
    else if (continent=='Oceania') { region.colors[6] }
    else { 'gray' }
  })
}

# base url for flag images
#flagBaseUrl <- 'http://www.thecgf.com/media/flags/'
# changed to local subdir under www
flagBaseUrl <- 'flags/'


#Server Component
server <- function(input, output) {
  ###################################
  # Growth data
  ###################################
  
  growth.misc.data <- reactive({
    if (input$growthProp=='Countries') {
      y <- growth.df$participating.countries
      y.name <- 'participating.countries'
      subtitle <- 'Number of Participating Countries'
      color <- 'red'
      styles <- list(countries=paste0('color:', color, '; font-style:italic'))
    } else if (input$growthProp=='Sports') {
      y <- growth.df$sports
      y.name <- 'sports'
      subtitle <- 'Number of Sports Played'
      color <- 'blue'
      styles <- list(sports=paste0('color:', color, '; font-style:italic'))
    } else if (input$growthProp=='Events') {
      y <- growth.df$events
      y.name <- 'events'
      subtitle <- 'Number of Events Contested'
      color <- 'green'
      styles <- list(events=paste0('color:', color, '; font-style:italic'))
    } else {
      y <- growth.df$athletes
      y.name <- 'athletes'
      subtitle <- 'Number of Participating Athletes'
      color <- 'orange'
      styles <- list(athletes=paste0('color:', color, '; font-style:italic'))
    }
    
    # save to a list
    list(y=y, y.name=y.name, subtitle=subtitle, color=color, styles=styles)
  })
  
  output$growthPlot <- renderPlot({
    ggthemr('dust')
    ggplot(growth.df, aes(x=year, y=growth.misc.data()[['y']], 
                          )) + 
      geom_point(size=4) + geom_line() +
      labs(x='Year', y=input$growthProp,
           title='Growth of the Commonwealth Games', subtitle=growth.misc.data()[['subtitle']]) +
      theme(plot.title=element_text(size=20, hjust=0.5, face="bold")) +
      theme(plot.subtitle=element_text(size=18, hjust=0.5, color=growth.misc.data()[['color']])) +
      scale_colour_ggthemr_d() +
      scale_x_continuous(breaks=seq(1930, 2018, by=4)) +
      scale_y_continuous(labels=function(x) { floor(x) })
  })
  
  output$yearInfo <- renderUI({
    if (!is.null(input$growth.hover)) {
      res <- nearPoints(growth.df, input$growth.hover, 
                        'year', growth.misc.data()[['y.name']])
      if (nrow(res) > 0) {
        div(id="growth-plot-info",
            div(id="growth-year-logo",
                img(src=paste0(res$year, ".gif"))
            ),
            div(id="growth-year-info",
                tags$table(
                  tags$tr(tags$td(class="cell-name", "Official Name"), 
                          tags$td(class="cell-value", res$official.name)),
                  tags$tr(tags$td(class="cell-name", "Year"), 
                          tags$td(class="cell-value", res$year)),
                  tags$tr(tags$td(class="cell-name", "Host City"), 
                          tags$td(class="cell-value", paste0(res$city, ', ', res$country))),
                  tags$tr(tags$td(class="cell-name", "Countries"), 
                          tags$td(class="cell-value", res$participating.countries), style=growth.misc.data()[['styles']][['countries']]),
                  tags$tr(tags$td(class="cell-name", "Sports"), 
                          tags$td(class="cell-value", res$sports), style=growth.misc.data()[['styles']][['sports']]),
                  tags$tr(tags$td(class="cell-name", "Events"), 
                          tags$td(class="cell-value", res$events), style=growth.misc.data()[['styles']][['events']]),
                  tags$tr(tags$td(class="cell-name", "Athletes"), 
                          tags$td(class="cell-value", res$athletes), style=growth.misc.data()[['styles']][['athletes']])
                )
            )          
        )
      } else {
        p(class="plot-note", "Hover on a point on the line graph to get more information.")
      }
    } else {
      p(class="plot-note", "Hover on a point on the line graph to get more information.") 
    }
  })
}

shinyApp(ui, server)