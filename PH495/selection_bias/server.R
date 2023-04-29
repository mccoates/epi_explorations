## server code for selection bias illustration

rm(list=ls())
library(data.table)
library(shiny)
library(ggplot2)
library(DT)
library(openxlsx)
library(highcharter)
library(gtools)

source("model/model_input_function.R")
source("model/model_function.R")

clist <- read.xlsx(paste0("inputs/ncdi_pov_network_countries.xlsx"))
dlist <- read.xlsx(paste0("inputs/placeholder_case_mix.xlsx"))

# consideredDigits <- 3
# stepWidth <- 1/10^(consideredDigits+1)

##############################################################################################
## SHINY SERVER
##############################################################################################
shinyServer(function(input, output, session) {
  
  ########################################
  ## INTRODUCTION TAB HTML
  ########################################
  output$Introduction <- renderUI({
    HTML(paste0(h1("Selection Bias Illustration"),
                br(),br(),
                p(paste0("This tool provides illustrations of selection bias."), style = "font-size:16px"),
                br(),br(),
                p("Text instructions.
                      Text instructions.",style = "font-size:16px"),br(),br()#,
                
    )
    )
  })
  
  
  ########################################
  ## SETTINGS TAB HTML
  ########################################
  # output$FirstRowSettings <- renderUI({
  #   HTML(paste0(p(paste0("Model Settings"), style = "font-size:26px"),
  #               p("Select settings using menus and sliders. Model will rerun with new settings if you move to the output or input parameters tab.",style = "font-size:18px"),br(),br(),
  #               p("Select Country and Projection Period",style = "font-size:18px")
  #               
  #   )
  #   )
  # })
  
  # output$SecondRowSettings <- renderUI({
  #   HTML(paste0(p("Select Case Mix Assumptions",style = "font-size:18px"),
  #               p("Note: If new location selected, values will change to the default for that location (as percentages)",style = "font-size:16px"),
  #               p("Input values need not add to 100%. Proportions will be calculated from the values input (see figure below)",style = "font-size:16px"),br(),br()#,
  #               
  #   )
  #   )
  # })
  
  ## print sum of the values input
  observe({
    #numSum <- sum(unlist(lapply(dlist$name_short,FUN=function(x){input[[x]]})))
    output$numSum <- renderText(paste("Sum:", sum(unlist(lapply(dlist$name_short,FUN=function(x){input[[x]]})))))
  })
  
  caseMixReact<-reactive({
    dat <- rbindlist(lapply(dlist$name_short,FUN=function(x){
      df <- data.table(data.frame(cause_name=dlist$cause_name[dlist$name_short==x],val=input[[x]]))
    }))
    dat[,percent:=round(val/sum(val)*100,1)]
    dat[,year:=""]
  })
  
  observe({
    x <- input$loc
    # y <- dataReact()
    # y <- y$model_inputs$cm
    # y <- y[age==0 & sex=="male",c("")] ## NOTE IF WE CHANGE HOW WE INPUT, THIS WON'T WORK RIGHT
    # y <- merge(y,dlist,by=c("cause_name"),all.x=T)
    dlist$prop_cause <- round(dlist$prop_cause*100) ## will need to update when we make country-specific (and sex-specific?)
    
    for (i in 1:nrow(dlist)) {
      updateNumericInput(session,dlist$name_short[i],value=dlist$prop_cause[i])
    }      
    
    
    # output$testout <- DT::renderDataTable({
    #     y
    # })
    
  })
  
  output$caseMixPlot <-renderHighchart({
    d <- caseMixReact()
    
    hc <- d %>%
      hchart(
        'column', hcaes(x = 'year', y = 'percent', group = 'cause_name'),
        stacking = "normal"
      )%>%
      #hc_xAxis( categories = unique(d$year) ) %>%
      hc_xAxis( title=list(text="") ) %>%
      hc_yAxis( title = list(text = "Percent of New Patients"),
                labels = list( format = "{value:,.0f} %")
      )%>%
      hc_plotOptions(column = list(
        dataLabels = list(enabled = F),
        #stacking = "normal",
        enableMouseTracking = T )
      )%>%
      hc_yAxis(max = 100
      )%>% 
      hc_tooltip(table = TRUE,
                 sort = TRUE,
                 pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                       " {series.name}: {point.y} % of New Patients"),
                 headerFormat = '<span style="font-size: 13px">  {point.key}</span>'
      ) %>%
      # hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = 000
      # ) %>%
      hc_title(
        text = "Case Mix",
        margin = 20,
        align = "left",
        style = list(color = "#000000", useHTML = TRUE)
      )
    
  })
  
  
  
  
  #########################################################
  ## MAIN REACTIVE DATA (INPUT DATA AND MODEL OUTPUT)
  #########################################################
  ## main dataset--runs function to load inputs to run model and run model based on the user selections
  dataReact<-reactive({
    model_inputs <- load_inputs(loc=input$loc,draws=1,base_year=2022,proj_yrs=input$proj_yrs,sim=F)
    out <- run_model(base_year=2022, proj_yrs=input$proj_yrs, d=copy(model_inputs$d), mortrate=copy(model_inputs$mortrate),memsave=T,sim=F)
    ret <- list(model_inputs,out)
    names(ret) <- c("model_inputs","out")
    return(ret)
  })
  
  
  ########################################################
  ## FIGURES/TABLES IN OUTPUT TAB
  ########################################################
  
  ## data from the output of the model that is then formatted for use in the output plots
  outPlotDataReact<-reactive({
    d <- dataReact()
    d <- d[["out"]]
    d <- d[,list(death=sum(death),pys=sum(pys)),by=c("cause_name","year","int","draw")]
    d <- d[,list(death=mean(death),pys=mean(pys)),by=c("cause_name","year","int")]
    d[,cause_name:=factor(cause_name,levels=c("Diabetes mellitus type 1","Diabetes mellitus type 2, ID","Diabetes mellitus type 2, NID",
                                              "Chronic respiratory disease","Hypertension (stage 3-4)","Heart failure","Post-surgical RHD",
                                              "Sickle cell disorders","Advanced malignancies"))]
    d[,int:=factor(as.character(int),levels=c("FALSE","TRUE"),labels=c("No Scale-up","Scale-up"))]
    d <- dcast.data.table(d,cause_name+year~int,value.var=c("death","pys"))
    d[,deaths_averted:=`death_No Scale-up`-`death_Scale-up`]
    d[,pys_gained:=`pys_Scale-up`-`pys_No Scale-up`]
  })
  
  ## plot of projected deaths averted
  output$projected_deaths_averted <-renderHighchart({
    d <- outPlotDataReact()
    
    hc <- d %>% 
      hchart(
        'column', hcaes(x = 'year', y = 'deaths_averted', group = 'cause_name'),
        stacking = "normal"
      )%>%
      hc_xAxis( categories = unique(d$year) ) %>%
      hc_yAxis( title = list(text = "Deaths Averted"),
                labels = list( format = "{value:,.0f} ") 
      )%>%
      hc_plotOptions(column = list(
        dataLabels = list(enabled = F),
        #stacking = "normal",
        enableMouseTracking = T )
      )%>%
      hc_tooltip(table = TRUE,
                 sort = TRUE,
                 pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                       " {series.name}: {point.y} Deaths Averted"),
                 headerFormat = '<span style="font-size: 13px">Year {point.key}</span>'
      ) %>%
      # hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = 000
      # ) %>%
      hc_title(
        text = "Deaths Averted",
        margin = 20,
        align = "left",
        style = list(color = "#000000", useHTML = TRUE)
      )
    
  })
  
  ## plot of person-years gained
  output$projected_pys_gained <-renderHighchart({
    d <- outPlotDataReact()
    
    hc <- d %>% 
      hchart(
        'column', hcaes(x = 'year', y = 'pys_gained', group = 'cause_name'),
        stacking = "normal"
      )%>%
      hc_xAxis( categories = unique(d$year) ) %>%
      hc_yAxis( title = list(text = "Person-Years Gained"),
                labels = list( format = "{value:,.0f} ") 
      )%>%
      hc_plotOptions(column = list(
        dataLabels = list(enabled = F),
        #stacking = "normal",
        enableMouseTracking = T )
      )%>%
      hc_tooltip(table = TRUE,
                 sort = TRUE,
                 pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                       " {series.name}: {point.y} Person-Years Gained"),
                 headerFormat = '<span style="font-size: 13px">Year {point.key}</span>'
      ) %>%
      # hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = 000
      # ) %>%
      hc_title(
        text = "Person-Years Gained",
        margin = 20,
        align = "left",
        style = list(color = "#000000", useHTML = TRUE)
      )
    
  })
  
  ## previously used table output for testing
  # output$testout <- DT::renderDataTable({
  #     d <- dataReact()
  #     #observe(print(d))
  #     d$out
  # })
  
  ########################################################
  ## FIGURES/TABLES IN PARAMETERS TAB (that shows input assumptions)
  ########################################################
  
  ## dataset to make plot with mortality risks and effect sizes
  mortReact <- reactive({
    d <- dataReact()
    d <- d$out[cause_name==input$params_dis & year == as.numeric(input$params_in_yr)]
    # d <- d$model_inputs$mortrate[cause_name==input$params_dis] since calculation had to be more dynamic, moved to output code instead of input
    # d <- d[,c("cause_name","sex","age","year","effect_size","duration","qx_wc_nt","qx_wc_cov","qx_wc_ref"),with=F]
    d <- d[,c("cause_name","int","sex","age","year","qx_wc_nt","qx_wc_t","qx_wc_ref"),with=F]
    d <- d[int==T]
    d <- unique(d)
    #d <- melt(d,id.vars=c("cause_name","sex","age","year","effect_size","duration"))
    d <- melt(d,id.vars=c("cause_name","sex","age","year"))
    d[variable=="qx_wc_t",treated:="Covered with PEN-Plus"]
    d[variable=="qx_wc_ref",treated:="Average under Country-Specific Baseline Coverage"]
    d[variable=="qx_wc_nt",treated:="No Disease Management"]
    d[,treated:=factor(treated,levels=c("No Disease Management","Average under Country-Specific Baseline Coverage","Covered with PEN-Plus"))]
    d[,sex:=factor(as.character(sex),levels=c("female","male"))]
    d[,cause_name:=factor(cause_name,levels=c("Diabetes mellitus type 1","Diabetes mellitus type 2, ID","Diabetes mellitus type 2, NID",
                                              "Chronic respiratory disease","Hypertension (stage 3-4)","Heart failure","Post-surgical RHD",
                                              "Sickle cell disorders","Advanced malignancies"))]
    d[,treatedsex:=paste0(sex,treated)]
    return(d)
  })
  
  ## plot mortality risks/effect sizes
  output$mortplot_male <-renderHighchart({
    d <- mortReact()
    d <- d[sex=="male"]
    d[,value:=round(value*100,1)]
    
    highchart() %>%
      hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
      hc_chart(type = 'line') %>%
      hc_series( list(name = 'No Disease Management', data =d[treated=='No Disease Management']$value, color='brown' , marker = list(enabled = F), lineWidth = 3 ),
                 list(name = 'Average under Country-Specific Baseline Coverage', data =d[treated=='Average under Country-Specific Baseline Coverage']$value, color = 'darkgreen', dashStyle = 'shortDot', marker = list(enabled = F) ),
                 list(name = 'Covered with PEN-Plus', data =d[treated=='Covered with PEN-Plus']$value, color = 'darkblue', dashStyle = 'shortDot',  marker = list(enabled = F) )
      )%>%
      hc_xAxis( categories = unique(d$age) ) %>%
      hc_yAxis( title = list(text = "Annual Mortality Risk (%)"),
                labels = list( format = "{value:,.0f} %"),
                plotLines = list(
                  list(#label = list(text = "This is a plotLine"),
                    color = "#ff0000",
                    #dashStyle = 'shortDot',
                    width = 2,
                    value = 0 ) )
      ) %>%
      hc_plotOptions(column = list(
        dataLabels = list(enabled = F),
        #stacking = "normal",
        enableMouseTracking = T )
      )%>%
      hc_tooltip(table = TRUE,
                 sort = TRUE,
                 pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                       " {series.name}: {point.y} %"),
                 headerFormat = '<span style="font-size: 13px">Age {point.key}</span>'
      ) %>%
      hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = 000 
      )  %>%
      hc_title(
        text = "Male",
        margin = 20,
        align = "left",
        style = list(color = "#000000", useHTML = TRUE)
      )
  })
  
  ## for females
  output$mortplot_female <-renderHighchart({
    d <- mortReact()
    d <- d[sex=="female"]
    d[,value:=round(value*100,1)]
    
    highchart() %>%
      hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
      hc_chart(type = 'line') %>%
      hc_series( list(name = 'No Disease Management', data =d[treated=='No Disease Management']$value, color='brown' , marker = list(enabled = F), lineWidth = 3 ),
                 list(name = 'Average under Country-Specific Baseline Coverage', data =d[treated=='Average under Country-Specific Baseline Coverage']$value, color = 'darkgreen', dashStyle = 'shortDot', marker = list(enabled = F) ),
                 list(name = 'Covered with PEN-Plus', data =d[treated=='Covered with PEN-Plus']$value, color = 'darkblue', dashStyle = 'shortDot',  marker = list(enabled = F) )
      )%>%
      hc_xAxis( categories = unique(d$age) ) %>%
      hc_yAxis( title = list(text = "Annual Mortality Risk (%)"),
                labels = list( format = "{value:,.0f} %"),
                plotLines = list(
                  list(#label = list(text = "This is a plotLine"),
                    color = "#ff0000",
                    #dashStyle = 'shortDot',
                    width = 2,
                    value = 0 ) )
      ) %>%
      hc_plotOptions(column = list(
        dataLabels = list(enabled = F),
        #stacking = "normal",
        enableMouseTracking = T )
      )%>%
      hc_tooltip(table = TRUE,
                 sort = TRUE,
                 pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                       " {series.name}: {point.y} %"),
                 headerFormat = '<span style="font-size: 13px">Age {point.key}</span>'
      ) %>%
      hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = 000 
      ) %>%
      hc_title(
        text = "Female",
        margin = 20,
        align = "left",
        style = list(color = "#000000", useHTML = TRUE)
      )
    
  })
  
  
  
  
})
