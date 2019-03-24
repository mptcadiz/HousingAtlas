# Library -----------------------------------------------------------------

library(shiny)
library(tidyverse)
library(sf)
library(scales)
library(plotly)
library(Hmisc)
library(extrafont)
library(gridExtra)
#library(lettercase)
library(ggiraph)
loadfonts()


# Prepare ggplot themes ---------------------------------------------------

## These change the ggplot themes to more directly match CRPD's style

theme_bar <- theme_bw() +
  theme(text = element_text(family = "Helvetica Neue,Helvetica,Arial,sans-serif", size = 16),
        panel.grid.major = element_line(color = "grey70", size = 0.1),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_text(face = "bold"),
        panel.border = element_blank(),
        legend.background = element_rect(fill = "transparent", color = "transparent"),
        legend.key = element_rect(fill = "transparent"),
        legend.title = element_blank())

theme_line <- theme_bw() +
  theme(legend.background = element_rect(fill = "transparent", color = "transparent"),
        legend.key = element_rect(fill = "transparent"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "grey70", size = 0.1),
        axis.ticks = element_blank(),
        axis.text = element_text(face = "bold"),
        panel.border = element_blank(),
        text = element_text(family = "Helvetica Neue,Helvetica,Arial,sans-serif", size = 16),
        plot.caption = element_text(family = "Helvetica Neue,Helvetica,Arial,sans-serif", size = 13, hjust = 0.5, face = "italic"))

theme_sf <- theme_bw() +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "white"),
        panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 16, family = "Helvetica Neue,Helvetica,Arial,sans-serif"),
        legend.title = element_blank())

# Objects - Visualization of home value data by county ---------------------------------------------------------
med.home.val.1990_2010

med.year.built.1990_2017

county.list <- med.home.val.1990_2010 %>%
  select(countyName) %>%
  distinct(countyName)

vis.list <- c("Trend lines","Data points", "Both visualizations") %>% 
  as.list()

# UI - Title Panel --------------------------------------------------------

##Setting up the theme, logo, etc....

ui <- fluidPage(

#for visualizing home values by county over time
  
  selectizeInput(inputId = "home.val.county",
                 label = "Choose a county",
                 choices = county.list,
                 multiple = TRUE),
  
  radioButtons(inputId="home.val.vis.list",
               label="How would you like to visualize the data?",
               choices = vis.list,
               inline=TRUE),
  
  ggiraphOutput("homevalcountygraph"),
  
  
  #for visualizing median year built by county over time
  
  selectizeInput(inputId = "year.built.county",
                 label = "Choose a county",
                 choices = county.list,
                 multiple = TRUE),
  
  radioButtons(inputId="year.built.vis.list",
               label="How would you like to visualize the data?",
               choices = vis.list,
               inline=TRUE),
  
  ggiraphOutput("yearbuiltcountygraph")
)

# Server - homeValue: cities and townships from counties -----------------------------------
server <- function(input, output, session) {
  
  #Median Home Value visualization
  output$homevalcountygraph <- renderggiraph({
    
    home.val.county.plot <- ggplot(filter(med.home.val.1990_2010, countyName %in% input$home.val.county), aes(color=countyName, x=as.numeric(year), y=as.numeric(homeValue))) +
      scale_x_continuous(breaks=c(1990,2000,2010,2017))+
      labs(x="Year", y="Home Value")+
      theme_bar+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    if (input$home.val.vis.list == "Data points"){
      home.val.county.plot <- home.val.county.plot + geom_point_interactive(size=3,aes(tooltip=paste(countyName, year,"\n Home Value: ",homeValue)))
    }
    else if (input$home.val.vis.list == "Trend lines"){
      home.val.county.plot <- home.val.county.plot + geom_line()
    }
    else if (input$home.val.vis.list == "Both visualizations"){
      home.val.county.plot <- home.val.county.plot + geom_point_interactive(size=3,aes(tooltip=paste(countyName, year,"\n Home Value: ",homeValue))) +
        geom_line()
    }
    ggiraph(code=print(home.val.county.plot), selection_type="none",hover_css = "r:7;",width_svg=10)
  })

  
  #Median Year Built Visualization
  output$yearbuiltcountygraph <- renderggiraph({
    
    year.built.county.plot <- ggplot(filter(med.year.built.1990_2017, countyName %in% input$year.built.county), aes(color=countyName, x=as.numeric(year), y=as.numeric(yearBuilt))) +
      scale_x_continuous(breaks=c(2000,2010,2017))+
      labs(x="Year", y="Year Built")+
      theme_bar+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    if (input$year.built.vis.list == "Data points"){
      year.built.county.plot <- year.built.county.plot + geom_point_interactive(size=3,aes(tooltip=paste(countyName, year,"\n Year Built: ",yearBuilt)))
    }
    else if (input$year.built.vis.list == "Trend lines"){
      year.built.county.plot <- year.built.county.plot + geom_line()
    }
    else if (input$year.built.vis.list == "Both visualizations"){
      year.built.county.plot <- year.built.county.plot + geom_point_interactive(size=3,aes(tooltip=paste(countyName, year,"\n Year Built: ",yearBuilt))) +
        geom_line()
    }
    ggiraph(code=print(year.built.county.plot), selection_type="none",hover_css = "r:7;",width_svg=10)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)