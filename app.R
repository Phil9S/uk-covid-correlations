#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)

# vars
nation_per_million_ratio <- 0.0150037509 # UK
nation_population_adult <- 52654348 # UK
source_data <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=overview&metric=cumPeopleVaccinatedFirstDoseByPublishDate&metric=newAdmissions&metric=newCasesByPublishDate&metric=newDeaths28DaysByPublishDate&metric=newTestsByPublishDate&format=csv"
# funcs
cor_window <- function(dat,window,step,group){
    win.data <- data.frame(date=c(),corr.kendall=c(),p.value=c())
    start <- 1
    while(nrow(dat) - start > 10){
        if(window > nrow(dat)){
          sub <- dat[c(start:nrow(dat)),]
          incomp <- TRUE
        } else {
          sub <- dat[c(start:window),]
          incomp <- FALSE
        }
        if(group == "deaths"){
            if(all(is.na(sub$newDeaths28DaysByPublishDate))){
                
            } else {
                cr <- as.numeric(cor.test(sub$newCasesByPublishDate,
                                          sub$newDeaths28DaysByPublishDate,
                                          method = "kendall",
                                          exact = F)$estimate)
                pr <- as.numeric(cor.test(sub$newCasesByPublishDate,
                                          sub$newDeaths28DaysByPublishDate,
                                          method = "kendall",
                                          exact = F)$p.value)
                win.data <- rbind(win.data,data.frame(date=mean.Date(sub$date),corr.kendall=cr,p.value=pr,incomp=incomp))
            }
        } else if(group == "admissions"){
            if(all(is.na(sub$newAdmissions))){
                
            } else {
                cr <- as.numeric(cor.test(sub$newCasesByPublishDate,
                                          sub$newAdmissions,
                                          method = "kendall",
                                          exact = F)$estimate)
                pr <- as.numeric(cor.test(sub$newCasesByPublishDate,
                                          sub$newAdmissions,
                                          method = "kendall",
                                          exact = F)$p.value)
                win.data <- rbind(win.data,data.frame(date=mean.Date(sub$date),corr.kendall=cr,p.value=pr,incomp=incomp))
            }
        } else {
            stop("unknown group")
        }
        
        start <- step + start
        window <- window + step
    }
    
    win.data$date2 <- lead(win.data$date)
    win.data$date2[nrow(win.data)] <- win.data$date[nrow(win.data)] + step
    win.data$fill <- ifelse(win.data$p.value < 0.05 & win.data$corr.kendall > 0,"positive",ifelse(win.data$p.value < 0.05 & win.data$corr.kendall < 0,"negative","non-significant"))
    win.data$sig <- ifelse(win.data$p.value < 0.05,"significant","non-significant")
    #win.data <- win.data[!is.na(win.data$p.value),]
    return(win.data)
}
# data
dat <- read.csv(source_data,header = T) %>%
    mutate(date = as.Date(date)) %>%
    arrange(date) %>% slice(62:n()) %>% # First days (march 2020) of reporting is highly inconsistent and contains a lot of missing data for admissions and deaths
    mutate(newAdmissions = newAdmissions * nation_per_million_ratio) %>%
    mutate(newCasesByPublishDate = newCasesByPublishDate * nation_per_million_ratio) %>%
    mutate(newDeaths28DaysByPublishDate = newDeaths28DaysByPublishDate * nation_per_million_ratio) %>%
    mutate(newTestsByPublishDate = newTestsByPublishDate * nation_per_million_ratio) %>%
    mutate(cumPeopleVaccinatedFirstDoseByPublishDate = cumPeopleVaccinatedFirstDoseByPublishDate / nation_population_adult)


int_dat_h <- dat[which(apply(dat[,c("newAdmissions","newCasesByPublishDate")],MARGIN = 1,function(x)  !any(is.na(x)))),] %>%
              arrange(desc(date)) %>%
              slice(1:30)

int_dat_d <- dat[which(apply(dat[,c("newDeaths28DaysByPublishDate","newCasesByPublishDate")],MARGIN = 1,function(x)  !any(is.na(x)))),] %>%
              arrange(desc(date)) %>%
              slice(1:30)

pivot_dat <- dat %>%
    pivot_longer(cols = 5:9,names_to = "stat",values_to = "count")

stat_selection <- unique(pivot_dat$stat)[c(2,4)]
names(stat_selection) <- c("hospital admissions","deaths")

# Define UI
ui <- fluidPage(theme = shinytheme("flatly"),
    navbarPage(title = "COIVD-19: UK patient correlations", collapsible = TRUE,position = "fixed-top",    
    # Application title
    tabPanel(title = "Summary", icon = icon("signal"),
    fluidRow(
     column(12,h3(tags$b("COVID-19: UK correlation between cases and clinical events")),style='margin-top: 62.5px; padding-left: 30px'),
    ),
    hr(),
    column(5,
    fluidRow(
      column(12,h3(tags$b("Correlation (last 30 days)"))),
    ),
    fluidRow(
        column(6,
               valueBoxOutput(outputId = "hospitalLate")
        ),
        column(6,
            valueBoxOutput(outputId = "deathsLate")
        )
    ),
    fluidRow(
        column(6,
            textOutput(outputId = "hospitalLate_D")
       ),
       column(6,
            textOutput(outputId = "deathsLate_D")
       )
    ),
    hr(),
    fluidRow(
        sidebarPanel(title = "Browser", icon = icon("flask"),width = 12,
             dateRangeInput(inputId = "date_range",
                            label = "Date range:",
                            min = min(dat$date),
                            max = max(dat$date),
                            start = as.Date("2021-01-01"),
                            end = max(dat$date)),
             selectInput(inputId = "type",
                         label = "Statistic:",
                         choices = stat_selection,
                         selected = "newDeaths28DaysByPublishDate",
                         multiple = F),
             sliderInput(inputId = "window",
                         label = "Window (days):",
                         min = 5,
                         max = 100,
                         value = 30,
                         step = 1),
             sliderInput(inputId = "step",
                         label = "step (days):",
                         min = 1,
                         max = 30,
                         value = 1,
                         step = 1)
        )
      )
    ),
    column(7,
    mainPanel(width = 12,
           plotOutput("corrPlot",height = "800px")
        )
    )
  ),
  tabPanel(title = "Data", icon = icon("table"),
    fluidRow(
      column(12,h3(tags$b("Data table")),style='margin-top: 62.5px;'),
    ),
    fluidRow(
      column(12,"Data is downloaded from the",tags$a(target="_blank",href="https://coronavirus.data.gov.uk/","UK government COVID-19 dashboard"),"."),
    ),
    hr(),
    fluidRow(
      column(12,
             dataTableOutput(outputId = "table"))
    )
  )
 )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    plotData <- reactive({
        pData <- pivot_dat[pivot_dat$date >= input$date_range & pivot_dat$date <= input$date_range[2],]
        pData <- pData[pData$stat %in% c("newCasesByPublishDate",input$type),]
        
        if(input$type == "newDeaths28DaysByPublishDate"){
            cData <- cor_window(dat = dat,window = input$window,step = input$step,group = "deaths")
        } else if(input$type == "newAdmissions"){
            cData <- cor_window(dat = dat,window = input$window,step = input$step,group = "admissions")
        }
        cData <- cData[cData$date >= input$date_range[1] & cData$date <= input$date_range[2],]
        lData <- list(pData,cData)
        lData
    })
    output$corrPlot <- renderPlot({
        plotD <- plotData()
        pivot_dat <- plotD[[1]]
        cor_data <- plotD[[2]]
        
        p1 <- ggplot() +
            geom_rect(data = cor_data[cor_data$incomp == TRUE,],
                    aes(xmin = min(cor_data$date[cor_data$incomp == TRUE]),
                               xmax=max(pivot_dat$date),ymin = -Inf,ymax = Inf),
                    fill=ifelse(cor_data$incomp[cor_data$incomp == TRUE] == TRUE,"grey95",NA),
                    alpha=0.3) +
            geom_rect(data = cor_data,
                      aes(xmin = date, xmax=date2,ymin = -Inf,ymax = Inf,fill=fill),alpha=0.3) +
            geom_line(data = pivot_dat,aes(date,count,color=stat)) +
            #labs(title = "COVID-19: cases against hospital admissions correlation") +
            ylab(label = "patients per million") +
            scale_fill_manual(name="correlation",
                              values = c("negative"="green","non-significant"=NA,"positive"="pink"),
                              guide = guide_legend(override.aes = list(color="grey25"))) +
            scale_color_manual(name="statistics",values = c("newCasesByPublishDate"="blue","newDeaths28DaysByPublishDate"="black","newAdmissions"="orange"),
                               labels = c("newAdmissions"="admissions","newCasesByPublishDate"="cases","newDeaths28DaysByPublishDate"="deaths")) +
            scale_x_date(limits = c(min(pivot_dat$date),max(pivot_dat$date)),expand = c(0,0)) +
            theme_bw() +
            theme(text=element_text(size=18)) +
            theme(axis.text.x = element_blank(),
                  axis.ticks.x = element_blank(),
                  axis.title.x = element_blank())
        
        p2 <- ggplot(data = cor_data) +
          geom_rect(data = cor_data[cor_data$incomp == TRUE,],
                    aes(xmin = min(cor_data$date[cor_data$incomp == TRUE]),
                        xmax=max(pivot_dat$date),ymin = -Inf,ymax = Inf),
                    fill=ifelse(cor_data$incomp[cor_data$incomp == TRUE] == TRUE,"grey95",NA),
                    alpha=0.3) +
            geom_hline(yintercept = 0) +
            geom_line(aes(date,corr.kendall)) +
            geom_rect(data = cor_data,
                      aes(xmin = date, xmax=date2,ymin = -Inf,ymax = Inf,fill=fill),alpha=0.3) +
            ylab(label = "correlation (kendall)") +
            scale_fill_manual(name="correlation",
                              values = c("negative"="green","non-significant"=NA,"positive"="pink"),
                              guide = guide_legend(override.aes = list(color="grey25"))) +
            scale_y_continuous(limits = c(-1,1)) +
            scale_x_date(limits = c(min(pivot_dat$date),max(pivot_dat$date)),expand = c(0,0)) +
            theme_bw() +
            theme(text=element_text(size=18)) +
            theme(axis.text.x = element_blank(),
                  axis.ticks.x = element_blank(),
                  axis.title.x = element_blank())
        
        p3 <- ggplot(data = cor_data) +
            geom_rect(data = cor_data[cor_data$incomp == TRUE,],
                    aes(xmin = min(cor_data$date[cor_data$incomp == TRUE]),
                        xmax=max(pivot_dat$date),ymin = -Inf,ymax = Inf),
                    fill=ifelse(cor_data$incomp[cor_data$incomp == TRUE] == TRUE,"grey95",NA),
                    alpha=0.3) +
            geom_hline(yintercept = 1.30103) +
            geom_line(aes(date,-log10(p.value))) +
            geom_rect(data = cor_data,
                      aes(xmin = date, xmax=date2,ymin = -Inf,ymax = Inf,fill=fill),alpha=0.3) +
            ylab(label = "-log10(p.value)") +
            scale_fill_manual(name="correlation",
                              values = c("negative"="green","non-significant"=NA,"positive"="pink"),
                              guide = guide_legend(override.aes = list(color="grey25"))) +
            scale_y_continuous(limits = c(0,NA)) +
            scale_x_date(limits = c(min(pivot_dat$date),max(pivot_dat$date)),expand = c(0,0)) +
            theme_bw() +
            theme(text=element_text(size=18))
        ggarrange(p1,p2,p3,align = "v",ncol = 1,common.legend = T,legend = "right",heights = c(2,1,1))
    })
    
    output$hospitalLate <- renderValueBox({
        cr.T <- cor.test(int_dat_h$newCasesByPublishDate,int_dat_h$newAdmissions,method = "kendall",exact = F,)
        string <- paste0(round(cr.T$estimate,2)," (p=",round(cr.T$p.value,3),")")
        valueBox(subtitle = "cases to admissions",value = string)
    })
    
    output$hospitalLate_D <- renderText({
      paste("(last reported: ",max(int_dat_h$date),")",sep = "")
    })
    
    
    output$deathsLate <- renderValueBox({
        cr.T <- cor.test(int_dat_d$newCasesByPublishDate,int_dat_d$newDeaths28DaysByPublishDate,method = "kendall",exact = F)
        string <- paste0(round(cr.T$estimate,2)," (p=",round(cr.T$p.value,3),")")
        valueBox(subtitle = "cases to deaths",value = string,color = "blue")
    })
    
    output$deathsLate_D <- renderText({
      paste("(last reported: ",max(int_dat_d$date),")",sep = "")
    })
    
    output$table <- renderDataTable({
      dat %>%
        arrange(desc(date))
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
