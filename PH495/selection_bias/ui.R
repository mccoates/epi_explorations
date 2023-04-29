## UI code for selection bias illustration
rm(list=ls())
library(data.table)
library(shiny)
library(ggplot2)
library(DT)
library(openxlsx)
library(highcharter)
library(gtools)



# consideredDigits <- 3
# stepWidth <- 1/10^(consideredDigits+1)

# Define UI for application that draws a histogram
shinyUI(
  fluidPage(
    ## some formatting 
    tags$head(
      #tags$link(rel = "shortcut icon", type = "image/png", href = "NCDI_poverty_network_logo.png"),
      tags$title("Selection Bias Illustration")
    ),
    ## Padding to compensate for fixed-top
    tags$style(type="text/css", "body {padding-top: 70px}"),
    tags$style(HTML(".navbar{background-color: #7dbfc7}
                   .navbar-default .navbar-nav > .active > a,
                   .navbar-default .navbar-nav > .active > a:focus,
                   .navbar-default .navbar-nav > .active > a:hover {color: black;background-color: #7dbfc7}")), 
    navbarPage(
      #title = tags$div(img(src="NCDI_poverty_network_logo.png", height = '40px', width = '40px'), textOutput("cy",inline = T)), 
      theme="sandstone.css", 
      position = "fixed-top",
      
      ## ADD INTRODUCTION TAB
      tabPanel("Introduction",
               fluidRow(column(12,htmlOutput("Introduction"))),
               style = "background-color: #7dbfc7"
      ),
      
      ## ADD SETTINGS TAB
      tabPanel("Settings",
               tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"),
               # row introducing settings
               fluidRow(column(12,htmlOutput("FirstRowSettings"))),
               # row with some settings toggles
               fluidRow(column(6, align="center", selectInput("loc", "Location", c(sort(clist$location_name)), multiple=F, selectize=F, selected = "Afghanistan")),
                        #column(3, align="center", sliderInput("draws","Number of Simulations",min = 1,max = 5,value = 1)), #not doing multiple draws in tool, just for separate analysis
                        ## too much computation for the interactive tool to handle (at least the online version)
                        #column(3, align="center", sliderInput("base_year","First Year",min = 2022,max = 2025,value = 2022,sep="")), # will add back in if we want this feature
                        column(6, align="center", sliderInput("proj_yrs","Number of Years Projected",min = 1,max = 10,value = 10))
               ),
               # row with some toggles related to case mix
               fluidRow(column(12,htmlOutput("SecondRowSettings"))
               ),
               fluidRow(
                 column(3,numericInput(inputId=dlist$name_short[1],label=dlist$cause_name[1],value=20,min = 0,max = 99999, step = 1)),
                 column(3,numericInput(inputId=dlist$name_short[2],label=dlist$cause_name[2],value=20,min = 0,max = 99999, step = 1)),
                 column(3,numericInput(inputId=dlist$name_short[3],label=dlist$cause_name[3],value=20,min = 0,max = 99999, step = 1)),
                 column(3,numericInput(inputId=dlist$name_short[4],label=dlist$cause_name[4],value=20,min = 0,max = 99999, step = 1))
               ),
               fluidRow(
                 column(3,numericInput(inputId=dlist$name_short[5],label=dlist$cause_name[5],value=20,min = 0,max = 99999, step = 1)),
                 column(3,numericInput(inputId=dlist$name_short[6],label=dlist$cause_name[6],value=20,min = 0,max = 99999, step = 1)),
                 column(3,numericInput(inputId=dlist$name_short[7],label=dlist$cause_name[7],value=20,min = 0,max = 99999, step = 1)),
                 column(3,numericInput(inputId=dlist$name_short[8],label=dlist$cause_name[8],value=20,min = 0,max = 99999, step = 1))
               ),
               fluidRow(
                 column(3,numericInput(inputId=dlist$name_short[9],label=dlist$cause_name[9],value=20,min = 0,max = 99999, step = 1)),
                 column(3, textOutput("numSum"))
               ),
               fluidRow(
                 highchartOutput("caseMixPlot")
               )
               
      ),
      
      ## ADD OUTPUT TAB
      tabPanel("Output",
               
               # Show a plot of the generated distribution
               fluidRow(
                 #plotOutput("distPlot")
                 #DT::dataTableOutput("testout")
                 highchartOutput("projected_deaths_averted"),
                 highchartOutput("projected_pys_gained")
                 
               )
               
      ),
      
      ## ADD PARAMETERS TAB
      tabPanel("Input Parameters",
               # Sidebar with a slider input for number of bins
               fluidRow(
                 column(3, align="center", selectInput("params_dis", "Health Condition", c(dlist$cause_name), multiple=F, selectize=F, selected = "Sickle cell disorders")),
                 column(3, align="center", selectInput("params_in_yr", "Year", c(as.character(2022:2031)), multiple=F, selectize=F, selected = "2022"))
               ),
               fluidRow(
                 highchartOutput("mortplot_female"),
                 highchartOutput("mortplot_male")
                 # DT::dataTableOutput("mortplot_male_test")
               )
               
      )
      
      
      
    )
  )
)