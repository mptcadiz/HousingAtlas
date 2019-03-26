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
options(scipen=999)


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

#Housing --------------------------------------------------------
med.home.val.1990_2010

med.year.built.1990_2017

tidymortgage.status.1990_2010

county.list <- med.home.val.1990_2010 %>%
  select(countyName) %>%
  distinct(countyName)

#Education --------------------------------------------------------
enrolled.ethnicity.2000_2018.tidy

ethnicity.district.list <- enrolled.ethnicity.2000_2018.tidy %>%
  select(districtName) %>%
  distinct(districtName) %>%
  mutate(districtName=toupper(districtName))

enrolled.2018_2000.tidy

enrollment.district.list <- enrolled.2018_2000.tidy %>%
  select(districtName) %>%
  distinct(districtName)

tidy.free.red.lunch.2006_2018

grad.math.2009_2013_total

grad.math.list <- grad.math.2009_2013_total %>%
  select(districtName) %>%
  distinct(districtName)

#Visualization types --------------------------------------------------------
vis.list <- c("Trend lines","Data points", "Both visualizations") %>% 
  as.list()


# UI - Title Panel --------------------------------------------------------

##Setting up the theme, logo, etc....

ui <- fluidPage(

#For visualizing home values by county over time --------------------------------------------------------
  
  selectizeInput(inputId = "home.val.county",
                 label = "Choose a county",
                 choices = county.list,
                 multiple = TRUE),
  
  radioButtons(inputId="home.val.vis.list",
               label="How would you like to visualize the data?",
               choices = vis.list,
               inline=TRUE),
  
  ggiraphOutput("homevalcountygraph"),
  
#For visualizing median year built by county over time --------------------------------------------------------
  
  selectizeInput(inputId = "year.built.county",
                 label = "Choose a county",
                 choices = county.list,
                 multiple = TRUE),
  
  radioButtons(inputId="year.built.vis.list",
               label="How would you like to visualize the data?",
               choices = vis.list,
               inline=TRUE),

  ggiraphOutput("yearbuiltcountygraph"),

#For visualizing percent of homeowners with mortgages --------------------------------------------------------
  selectizeInput(inputId = "mortgage.status.county",
               label = "Choose a county",
               choices = county.list,
               multiple = TRUE),

  radioButtons(inputId="mortgage.status.vis.list",
             label="How would you like to visualize the data?",
             choices = vis.list,
             inline=TRUE),

  ggiraphOutput("mortgagestatuscountygraph"),

#For visualizing percent of students of color enrolled --------------------------------------------------------
selectizeInput(inputId = "student.ethnicity.district",
               label = "Choose a district",
               choices = ethnicity.district.list,
               multiple = TRUE),

radioButtons(inputId="student.ethnicity.vis.list",
             label="How would you like to visualize the data?",
             choices = vis.list,
             inline=TRUE),

ggiraphOutput("studentethnicitygraph"),

#For visualizing total enrollment --------------------------------------------------------
selectizeInput(inputId = "student.enrollment.district",
               label = "Choose a district",
               choices = ethnicity.district.list,
               multiple = TRUE),

radioButtons(inputId="student.enrollment.vis.list",
             label="How would you like to visualize the data?",
             choices = vis.list,
             inline=TRUE),

ggiraphOutput("studentenrollmentgraph"),

#For visualizing number of students with free/reduced lunch --------------------------------------------------------
selectizeInput(inputId = "lunch.district",
               label = "Choose a district",
               choices = ethnicity.district.list,
               multiple = TRUE),

radioButtons(inputId="lunch.vis.list",
             label="How would you like to visualize the data?",
             choices = vis.list,
             inline=TRUE),

ggiraphOutput("lunchgraph"),

#For visualizing percent of GRAD: Math passers --------------------------------------------------------
selectizeInput(inputId = "grad.math.pass",
               label = "Choose a district",
               choices = grad.math.list,
               multiple = TRUE),

radioButtons(inputId="grad.math.pass.vis.list",
             label="How would you like to visualize the data?",
             choices = vis.list,
             inline=TRUE),

ggiraphOutput("gradmathpassgraph"),

#For visualizing average GRAD: Math scores  --------------------------------------------------------
selectizeInput(inputId = "grad.math.score",
               label = "Choose a district",
               choices = grad.math.list,
               multiple = TRUE),

radioButtons(inputId="grad.math.score.vis.list",
             label="How would you like to visualize the data?",
             choices = vis.list,
             inline=TRUE),

ggiraphOutput("gradmathscoregraph")


)

# Server - homeValue: cities and townships from counties -----------------------------------
server <- function(input, output, session) {
  
#Median Home Value visualization --------------------------------------------------------
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

  
#Median Year Built Visualization --------------------------------------------------------
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
  
#    output$yearbuiltcountygraph <- renderggiraph({
    
#      year.built.county.plot <- ggplot(med.year.built.1990_2017, aes(yearBuilt)) +
#        #geom_bar(aes(fill=countyName)) +
#        geom_histogram(aes(fill=countyName)) +
#        #scale_x_continuous(breaks=c(2000,2010,2017))+
#        labs(x="Year", y="Year Built")+
#        theme_bar+
#        theme(axis.text.x = element_text(angle = 45, hjust = 1))
#      
#      ggiraph(code=print(year.built.county.plot), selection_type="none",hover_css = "r:7;",width_svg=10)
#    })
  
#Mortgage Status Visualization --------------------------------------------------------
  output$mortgagestatuscountygraph <- renderggiraph({
    
    mortgage.status.county.plot <- ggplot(filter(tidymortgage.status.1990_2010, countyName %in% input$mortgage.status.county), aes(color=countyName, x=as.numeric(year), y=as.numeric(percentFree))) +
      scale_x_continuous(breaks=c(1990,2000,2010))+
      scale_y_continuous(labels=scales::percent)+
      labs(x="Year", y="Percent of houses without mortgages")+
      theme_bar+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    if (input$mortgage.status.vis.list == "Data points"){
      mortgage.status.county.plot <- mortgage.status.county.plot +
        geom_point_interactive(size=3,aes(tooltip=paste(countyName, year,"\n Percent of homes without mortgages: ",percentFree)))
    }
    else if (input$mortgage.status.vis.list == "Trend lines"){
      mortgage.status.county.plot <- mortgage.status.county.plot +
        geom_line()
    }
    else if (input$mortgage.status.vis.list == "Both visualizations"){
      mortgage.status.county.plot <- mortgage.status.county.plot +
        geom_point_interactive(size=3,aes(tooltip=paste(countyName, year,"\n Percent of homes without mortgages: ",percentFree))) +
        geom_line()
    }
    ggiraph(code=print(mortgage.status.county.plot), selection_type="none",hover_css = "r:7;",width_svg=10)
  })

#Ethnicity of Students Enrolled Visualization --------------------------------------------------------
  output$studentethnicitygraph <- renderggiraph({
    
    student.ethnicity.district.plot <- ggplot(filter(enrolled.ethnicity.2000_2018.tidy, districtName %in% input$student.ethnicity.district), aes(color=districtName, x=as.numeric(year), y=as.numeric(percentMinority))) +
      scale_x_continuous(breaks=c(2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018))+
      scale_y_continuous(labels=scales::percent)+
      labs(x="Year", y="Percent of students of color enrolled")+
      theme_bar+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    if (input$student.ethnicity.vis.list == "Data points"){
      student.ethnicity.district.plot <- student.ethnicity.district.plot +
        geom_point_interactive(size=3,aes(tooltip=paste(districtName, year,"\n Percent of students of color enrolled: ",percentMinority)))
    }
    else if (input$student.ethnicity.vis.list == "Trend lines"){
      student.ethnicity.district.plot <- student.ethnicity.district.plot +
        geom_line()
    }
    else if (input$student.ethnicity.vis.list == "Both visualizations"){
      student.ethnicity.district.plot <- student.ethnicity.district.plot +
        geom_point_interactive(size=3,aes(tooltip=paste(districtName, year,"\n Percent of students of color enrolled: ",percentMinority))) +
        geom_line()
    }
    ggiraph(code=print(student.ethnicity.district.plot), selection_type="none",hover_css = "r:7;",width_svg=10)
  })
  
#Students Enrolled Visualization --------------------------------------------------------
  output$studentenrollmentgraph <- renderggiraph({
    
    student.enrollment.district.plot <- ggplot(filter(enrolled.2018_2000.tidy, districtName %in% input$student.enrollment.district), aes(color=districtName, x=as.numeric(year), y=as.numeric(totalStudents))) +
      scale_x_continuous(breaks=c(2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018))+
      labs(x="Year", y="Number of students enrolled")+
      theme_bar+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    if (input$student.enrollment.vis.list == "Data points"){
      student.enrollment.district.plot <- student.enrollment.district.plot +
        geom_point_interactive(size=3,aes(tooltip=paste(districtName, year,"\n Number of students enrolled: ",totalStudents)))
    }
    else if (input$student.enrollment.vis.list == "Trend lines"){
      student.enrollment.district.plot <- student.enrollment.district.plot +
        geom_line()
    }
    else if (input$student.enrollment.vis.list == "Both visualizations"){
      student.enrollment.district.plot <- student.enrollment.district.plot +
        geom_point_interactive(size=3,aes(tooltip=paste(districtName, year,"\n Number of students enrolled: ",totalStudents))) +
        geom_line()
    }
    ggiraph(code=print(student.enrollment.district.plot), selection_type="none",hover_css = "r:7;",width_svg=10)
  })

#Students With Free/Reduced Lunch Visualization --------------------------------------------------------
  output$lunchgraph <- renderggiraph({
    
    lunch.district.plot <- ggplot(filter(tidy.free.red.lunch.2006_2018, districtName %in% input$lunch.district), aes(color=districtName, x=as.numeric(year), y=as.numeric(percentLunch))) +
      scale_x_continuous(breaks=c(2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018))+
      scale_y_continuous(labels=scales::percent)+
      labs(x="Year", y="Percent of students with free/reduced lunch")+
      theme_bar+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    if (input$lunch.vis.list == "Data points"){
      lunch.district.plot <- lunch.district.plot +
        geom_point_interactive(size=3,aes(tooltip=paste(districtName, year,"\n Percent of students with free/reduced lunch: ",percentLunch)))
    }
    else if (input$lunch.vis.list == "Trend lines"){
      lunch.district.plot <- lunch.district.plot +
        geom_line()
    }
    else if (input$lunch.vis.list == "Both visualizations"){
      lunch.district.plot <- lunch.district.plot +
        geom_point_interactive(size=3,aes(tooltip=paste(districtName, year,"\n Percent of students with free/reduced lunch: ",percentLunch))) +
        geom_line()
    }
    ggiraph(code=print(lunch.district.plot), selection_type="none",hover_css = "r:7;",width_svg=10)
  })
  
#GRAD: Math Score Visualization --------------------------------------------------------
  output$gradmathscoregraph <- renderggiraph({
    
    grad.math.score.plot <- ggplot(filter(grad.math.2009_2013_total, districtName %in% input$grad.math.score), aes(color=districtName, x=as.numeric(year), y=as.numeric(averageScore))) +
      scale_x_continuous(breaks=c(2009,2010,2011,2012,2013))+
      #scale_y_continuous(labels=scales::percent)+
      labs(x="Year", y="Average GRAD: Math Score")+
      theme_bar+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    if (input$grad.math.score.vis.list == "Data points"){
      grad.math.score.plot <- grad.math.score.plot +
        geom_point_interactive(size=3,aes(tooltip=paste(districtName, year,"\n Average GRAD: Math Score: ",averageScore)))
    }
    else if (input$grad.math.score.vis.list == "Trend lines"){
      grad.math.score.plot <- grad.math.score.plot +
        geom_line()
    }
    else if (input$grad.math.score.vis.list == "Both visualizations"){
      grad.math.score.plot <- grad.math.score.plot +
        geom_point_interactive(size=3,aes(tooltip=paste(districtName, year,"\n Average GRAD: Math Score: ",averageScore))) +
        geom_line()
    }
    ggiraph(code=print(grad.math.score.plot), selection_type="none",hover_css = "r:7;",width_svg=10)
  })

#GRAD: Math Passing Visualization --------------------------------------------------------
  output$gradmathpassgraph <- renderggiraph({
    
    grad.math.pass.plot <- ggplot(filter(grad.math.2009_2013_total, districtName %in% input$grad.math.pass), aes(color=districtName, x=as.numeric(year), y=as.numeric(percentPassed))) +
      scale_x_continuous(breaks=c(2009,2010,2011,2012,2013))+
      #scale_y_continuous(labels=scales::percent)+
      labs(x="Year", y="GRAD: Math Percent Passed")+
      theme_bar+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    if (input$grad.math.pass.vis.list == "Data points"){
      grad.math.pass.plot <- grad.math.pass.plot +
        geom_point_interactive(size=3,aes(tooltip=paste(districtName, year,"\n GRAD: Math Percent Passed: ",percentPassed)))
    }
    else if (input$grad.math.pass.vis.list == "Trend lines"){
      grad.math.pass.plot <- grad.math.pass.plot +
        geom_line()
    }
    else if (input$grad.math.pass.vis.list == "Both visualizations"){
      grad.math.pass.plot <- grad.math.pass.plot +
        geom_point_interactive(size=3,aes(tooltip=paste(districtName, year,"\n GRAD: Math Percent Passed: ",percentPassed))) +
        geom_line()
    }
    ggiraph(code=print(grad.math.pass.plot), selection_type="none",hover_css = "r:7;",width_svg=10)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)