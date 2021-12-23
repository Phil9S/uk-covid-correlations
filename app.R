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
library(zoo)

# vars
#nation_per_million_ratio <- 0.01598 # UK
#nation_population_adult <- 62578778 # UK
source_data <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=overview&metric=cumVaccinationFirstDoseUptakeByPublishDatePercentage&metric=newAdmissions&metric=newCasesByPublishDate&metric=newDeaths28DaysByPublishDate&metric=cumVaccinationSecondDoseUptakeByPublishDatePercentage&format=csv"
source_data2 <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=overview&metric=newTestsByPublishDate&format=csv&format=csv"
step <- 1

# funcs
cor_window <- function(dat,window,step,group){
    win.data <- data.frame(date=c(),corr.kendall=c(),p.value=c())
    start <- 1
    while(nrow(dat) - start > 8){
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

pct_plot_data <- function(dat){
  at_first <- floor(max(dat$cumVaccinationFirstDoseUptakeByPublishDatePercentage,na.rm = T))
  remain <- 100 - at_first
  first_mat <- matrix(c(rep(1,times=at_first),rep(0,times=remain)),nrow = 10,ncol = 10)
  at_second <- floor(max(dat$cumVaccinationSecondDoseUptakeByPublishDatePercentage,na.rm = T))
  remain2 <- 100 - at_second
  second_mat <- matrix(c(rep(1,times=at_second),rep(0,times=remain2)),nrow = 10,ncol = 10)
  plot_mat <- first_mat + second_mat
  rownames(plot_mat) <- 1:10
  colnames(plot_mat) <- 1:10
  plot_mat_df <- as.data.frame.table(plot_mat)
  ggplot(plot_mat_df) +
    geom_tile(aes(x = Var1,y = Var2,fill=as.factor(Freq)),color="white") +
    scale_fill_manual(values = c("grey90","dodgerblue","dodgerblue4")) +
    labs(title = "Vaccine coverage") +
    theme_bw() +
    theme(legend.position = "none",axis.title = element_blank(),axis.text = element_blank(),axis.ticks = element_blank())
}

# data
dat2 <- read.csv(source_data2,header = T) %>%
    select(date,newTestsByPublishDate)

dat <- read.csv(source_data,header = T) %>%
    left_join(y = dat2,by = "date") %>%
    mutate(date = as.Date(date)) %>%
    arrange(date) %>% slice(62:n()) %>% # First days (march 2020) of reporting is highly inconsistent and contains a lot of missing data for admissions and deaths
    mutate(admissionsperk = newAdmissions / (newCasesByPublishDate/1000)) %>%
    mutate(deathsperk = newDeaths28DaysByPublishDate / (newCasesByPublishDate / 1000)) %>%
    mutate(posivitity_rate = newCasesByPublishDate / newTestsByPublishDate * 100)
    # mutate(newCasesByPublishDate = newCasesByPublishDate * nation_per_million_ratio) %>%
    # mutate(newDeaths28DaysByPublishDate = newDeaths28DaysByPublishDate * nation_per_million_ratio)

int_dat_h <- dat[which(apply(dat[,c("newAdmissions","newCasesByPublishDate")],MARGIN = 1,function(x)  !any(is.na(x)))),] %>%
              arrange(desc(date)) %>%
              slice(1:30)

int_dat_d <- dat[which(apply(dat[,c("newDeaths28DaysByPublishDate","newCasesByPublishDate")],MARGIN = 1,function(x)  !any(is.na(x)))),] %>%
              arrange(desc(date)) %>%
              slice(1:30)

pivot_dat <- dat %>%
    pivot_longer(cols = 5:13,names_to = "stat",values_to = "count")

stat_selection <- unique(pivot_dat$stat)[c(3,5)]
names(stat_selection) <- c("hospital admissions","deaths")

# Define UI
ui <- fluidPage(theme = shinytheme("flatly"),
    navbarPage(title = "COIVD-19: UK patient correlations", collapsible = TRUE,position = "fixed-top",    
    # Application title
    tabPanel(title = "Summary", icon = icon("chart-bar"),
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
        sidebarPanel(title = "Browser", icon = icon("chart-bar"),width = 12,
             dateRangeInput(inputId = "date_range",
                            label = "Date range:",
                            min = min(dat$date),
                            max = max(dat$date),
                            start = as.Date("2020-12-01"),
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
             checkboxInput(inputId = "smooth",
                           label = "Moving average (7 days)",
                           value = TRUE)
        )
      ),
    fluidRow(
      plotOutput("perk")
      )
    ),
    column(7,
    mainPanel(width = 12,
           plotOutput("corrPlot",height = "1045px")
        )
    )
  ),
  # tabPanel(title = "Vaccination", icon = icon("grip-horizontal"),
  #          fluidRow(
  #            column(12,h3(tags$b("Vaccination")),style='margin-top: 62.5px;'),
  #          ),
  #          hr(),
  #          fluidRow(
  #            column(4,
  #                   plotOutput("vacc_prog",height = "500px")
  #            ),
  #            column(4,
  #                   plotOutput("first_vacc",height = "500px")
  #                   ),
  #            column(4,
  #                   plotOutput("second_vacc",height = "500px")
  #            ),
  #          )
  #          
  # ),
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
      if(input$smooth){
        pivot_dat <- pivot_dat %>% group_by(stat) %>% mutate(count = rollmean(count,k = 7,fill = c(NA,NA,NA)))
      }
      
      pData <- pivot_dat[pivot_dat$date >= input$date_range[1] & pivot_dat$date <= input$date_range[2],]
      pData <- pData[pData$stat %in% c("newCasesByPublishDate",input$type),]
        
      if(input$type == "newDeaths28DaysByPublishDate"){
        cData <- cor_window(dat = dat,window = input$window,step = step,group = "deaths")
      } else if(input$type == "newAdmissions"){
        cData <- cor_window(dat = dat,window = input$window,step = step,group = "admissions")
      }
      cData <- cData[cData$date >= input$date_range[1] & cData$date <= input$date_range[2],]
      lData <- list(pData,cData)
      lData
    })
    
    plotData2 <- reactive({
      if(input$smooth){
        pivot_dat <- pivot_dat %>% group_by(stat) %>% mutate(count = rollmean(count,k = 7,fill = c(NA,NA,NA)))
      }
      pData <- pivot_dat[pivot_dat$date >= input$date_range[1] & pivot_dat$date <= input$date_range[2],]
      pData <- pData[pData$stat %in% c("admissionsperk","deathsperk","posivitity_rate"),]
      pData
    })
    
    output$corrPlot <- renderPlot({
        plotD <- plotData()
        pivot_dat <- plotD[[1]]
        cor_data <- plotD[[2]]
        
        p1 <- ggplot() +
            geom_rect(data = cor_data,
                      aes(xmin = date, xmax=date2,ymin = -Inf,ymax = Inf,fill=fill),alpha=0.3) +
            geom_line(data = pivot_dat,aes(date,count,color=stat)) +
            geom_vline(xintercept = min(cor_data$date[cor_data$incomp == TRUE]),linetype=2) +
            ylab(label = "patients") +
            scale_fill_manual(name="correlation",
                              values = c("negative"="green","non-significant"=NA,"positive"="pink"),
                              guide = guide_legend(override.aes = list(color="grey25"))) +
            scale_color_manual(name="statistics",values = c("newCasesByPublishDate"="blue","newDeaths28DaysByPublishDate"="black","newAdmissions"="orange"),
                               labels = c("newCasesByPublishDate"="cases","newAdmissions"="admissions","newDeaths28DaysByPublishDate"="deaths")) +
            scale_x_date(limits = c(min(pivot_dat$date),max(pivot_dat$date)),expand = c(0,0)) +
            scale_y_continuous(limits = c(0,NA)) +
            facet_wrap(. ~ stat,nrow = 2,scales = "free") +
            theme_bw() +
            theme(text=element_text(size=18)) +
            theme(axis.text.x = element_blank(),
                  axis.ticks.x = element_blank(),
                  axis.title.x = element_blank())
        
        p2 <- ggplot(data = cor_data) +
            geom_hline(yintercept = 0) +
            geom_line(aes(date,corr.kendall)) +
            geom_rect(data = cor_data,
                      aes(xmin = date, xmax=date2,ymin = -Inf,ymax = Inf,fill=fill),alpha=0.3) +
            geom_vline(xintercept = min(cor_data$date[cor_data$incomp == TRUE]),linetype=2) +
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
            geom_hline(yintercept = 1.30103) +
            geom_line(aes(date,-log10(p.value))) +
            geom_rect(data = cor_data,
                      aes(xmin = date, xmax=date2,ymin = -Inf,ymax = Inf,fill=fill),alpha=0.3) +
            geom_vline(xintercept = min(cor_data$date[cor_data$incomp == TRUE]),linetype=2) +
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
    
    output$perk <- renderPlot({
      
      ggplot(plotData2()) +
        geom_line(aes(date,count,color=stat)) +
        scale_x_date(limits = c(min(plotData2()$date),max(plotData2()$date)),expand = c(0,0)) +
        scale_y_continuous(limits = c(0,NA)) +
        facet_wrap(. ~ stat,nrow = 1,scales = "free_y") +
        theme_bw() +
        theme(text=element_text(size=17)) +
        theme(axis.ticks.x = element_blank(),
              axis.title.x = element_blank(),
              legend.position = "bottom")
    })
    
    output$hospitalLate <- renderValueBox({
        cr.T <- cor.test(int_dat_h$newCasesByPublishDate,int_dat_h$newAdmissions,method = "kendall",exact = F,)
        string <- paste0(round(cr.T$estimate,2)," (p=",round(cr.T$p.value,5),")")
        valueBox(subtitle = "cases to admissions",value = string)
    })
    
    output$hospitalLate_D <- renderText({
      paste("(last reported: ",max(int_dat_h$date),")",sep = "")
    })
    
    
    output$deathsLate <- renderValueBox({
        cr.T <- cor.test(int_dat_d$newCasesByPublishDate,int_dat_d$newDeaths28DaysByPublishDate,method = "kendall",exact = F)
        string <- paste0(round(cr.T$estimate,2)," (p=",round(cr.T$p.value,5),")")
        valueBox(subtitle = "cases to deaths",value = string,color = "blue")
    })
    
    output$deathsLate_D <- renderText({
      paste("(last reported: ",max(int_dat_d$date),")",sep = "")
    })
    
    output$table <- renderDataTable({
      dat %>%
        arrange(desc(date))
    })
    
    # output$vacc_prog <- renderPlot({
    #   vacc_prog <- pct_plot_data(dat = dat)
    #   vacc_prog
    # })
    # 
    # output$first_vacc <- renderPlot({
    #   first_vacc <- pivot_dat[pivot_dat$stat == "cumVaccinationFirstDoseUptakeByPublishDatePercentage" & !is.na(pivot_dat$count),]
    #   first_vacc$group <- rep("actual",times=nrow(first_vacc))
    #   average_uptake <- mean(tail(first_vacc$count,n = 14) - lag(tail(first_vacc$count,n = 14)),na.rm = T)
    #   max_diff <- 100 - max(first_vacc$count)
    #   days_to_max <- round(max_diff / average_uptake)
    #   
    #   extrapv1 <- data.frame()
    #   for(x in 1:days_to_max){
    #       d <- max(first_vacc$date) + x
    #       c <- max(first_vacc$count) + x * average_uptake
    #       extrapv1 <- rbind(extrapv1,data.frame(areaCode = "K02000001",
    #                                         areaName = "United Kingdom",
    #                                         areaType = "overview",
    #                                         date = d,
    #                                         stat = "cumVaccinationFirstDoseUptakeByPublishDatePercentage",
    #                                         count = c,
    #                                         group = "predicted"
    #                                         ))
    #   }
    #   
    #   v1 <- ggplot() +
    #     geom_line(data = first_vacc,
    #               aes(date,count),color = "black",linetype=1) +
    #     geom_line(data = extrapv1,
    #               aes(date,count),color = "red",linetype=2) +
    #     geom_vline(xintercept = max(extrapv1$date)) +
    #     geom_text(data = extrapv1,aes(x = max(date) - 25,y = 2,label=paste0("100% on ",max(date)))) +
    #     xlab(NULL) + 
    #     ylab(label = "first dose (cummulative)") +
    #     scale_y_continuous(limits = c(0,100),expand = c(0,0)) +
    #     theme_bw()
    #   v1
    # })
    # 
    # output$second_vacc <- renderPlot({
    #   second_vacc <- pivot_dat[pivot_dat$stat == "cumVaccinationSecondDoseUptakeByPublishDatePercentage" & !is.na(pivot_dat$count),]
    #   second_vacc$group <- rep("actual",times=nrow(second_vacc))
    #   average_uptake <- mean(tail(second_vacc$count,n = 14) - lag(tail(second_vacc$count,n = 14)),na.rm = T)
    #   max_diff <- 100 - max(second_vacc$count)
    #   days_to_max <- round(max_diff / average_uptake)
    #   
    #   extrapv2 <- data.frame()
    #   for(x in 1:days_to_max){
    #     d <- max(second_vacc$date) + x
    #     c <- max(second_vacc$count) + x * average_uptake
    #     extrapv2 <- rbind(extrapv2,data.frame(areaCode = "K02000001",
    #                                       areaName = "United Kingdom",
    #                                       areaType = "overview",
    #                                       date = d,
    #                                       stat = "cumVaccinationSecondDoseUptakeByPublishDatePercentage",
    #                                       count = c,
    #                                       group = "predicted"
    #     ))
    #   }
    #   
    #   v2 <- ggplot() +
    #     geom_line(data = second_vacc,
    #               aes(date,count),color = "black",linetype=1) +
    #     geom_line(data=extrapv2,
    #               aes(date,count),color = "red",linetype=2) +
    #     geom_vline(xintercept = max(extrapv2$date)) +
    #     geom_text(data = extrapv2,aes(x = max(date) - 30,y = 2,label=paste0("100% on ",max(date)))) +
    #     xlab(NULL) + 
    #     ylab(label = "Second dose (cummulative)") +
    #     scale_y_continuous(limits = c(0,100),expand = c(0,0)) +
    #     theme_bw()
    #   v2
    # })
    
}

# Run the application 
shinyApp(ui = ui, server = server)