#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(httr)
library(tidyverse)
#library(devtools)
library(DT)
library(jsonlite)

source("toConnectToKobo.R")
# Define UI for application that draws a histogram

kobohr_getdata_csv<-function(url,u,pw){
    #supply url for the data
    rawdata<-GET(url,authenticate(u,pw),progress())
    d_content <- read_csv(content(rawdata,"raw",encoding = "UTF-8"), na = "n/a")
}

url_endline <-"https://kc.humanitarianresponse.info/api/v1/data/870395.csv"
url_backcheck <-"https://kc.humanitarianresponse.info/api/v1/data/871206.csv"


backcheck_keep <- c("index", "survey_agency",
                    "district_bl", "name_call_back","displacement_status")

backcheck_var <- c("Telephone_Number",
                   "head_hh_yn", 
                   "move_plan_12mths", "MealsEatPerDay",
                   "shelter_type", "electric_hh_yn", "HH_size", 
                   "males_gte_18", "females_gte_18", "edu_level_hh",
                   "meet_basic_needs", 
                   "main_source_income", "land_owned_by","csi_score")
backcheck_keep <- paste0(backcheck_keep,".s")
backcheck_var <- paste0(backcheck_var,".s")

endline_keep <- c("index", "survey_agency", "State",
                  "District", "username", "phonenumber_enumerator", "deviceid",
                  "gps_location", "_gps_location_latitude", "_gps_location_longitude",
                  "_gps_location_altitude", "_gps_location_precision", "Respondent_name", "date","start", "CompletionDateTime", "interviewDuration",
                  "interviewDuringDay", "interviewDuration","veryshort", "short", "reasonableDuration", "nbDontknow",  "DisplacementStatus_Full_EL")
endline_var <- c( "PhoneNumber",
                  "QuHeadOfHH", 
                  "MovePlan", "MealsEatPerDay",
                  "ShelterType", "ElectricityAccess", "hh_size", 
                  "Male_18_plus_years_old", "Female_18_plus_years_old", "Highest_Head_Secular_Education",
                  "SelfReliance", 
                  "MainIncomeSource", "LandOwnership",
                  "csi_score")
endline_keep <- paste0(endline_keep,".e")
endline_var <- paste0(endline_var,".e")
varshown <- c("percentMatch", "index.e", "survey_agency.e","username.e",  "phonenumber_enumerator.e", "deviceid.e", "date.e", "start.e", "CompletionDateTime.e","interviewDuration.e", "nbDontknow.e",
              "State.e","District.e", "DisplacementStatus_Full_EL.e","displacement_status.s", c(rbind(endline_var, backcheck_var)))
allvars <- c("percentMatch",endline_keep, endline_var, backcheck_keep, backcheck_var)

listData <- function(){
    d <- data.frame(matrix(ncol=length(varshown), nrow = 0))
    colnames(d)<-varshown
    return(d)
}


prepareData <- function(endline, backcheck){
    
    backcheck$csi_score <- as.numeric(backcheck$csiLess)+ as.numeric(backcheck$csiBorrow)+ as.numeric(backcheck$csiReduce)+ as.numeric(backcheck$csiAdult)+ as.numeric(backcheck$csiFewer)
    endline$csi_score <- as.numeric(endline$csiLess)+ as.numeric(endline$csiBorrow)+ as.numeric(endline$csiReduce)+ as.numeric(endline$csiAdult)+ as.numeric(endline$csiFewer)
    endline$interviewDuration <- difftime(endline$CompletionDateTime, endline$start, units='mins')
    endline$interviewDuringDay <- between(format(endline$start, format="%H%M%S"),40000, 160000)
    endline$reasonableDuration <- between(endline$interviewDuration, 30, 90)
    endline$short <- between(endline$interviewDuration, 25, 30)
    endline$veryshort <- endline$interviewDuration<25
    endline$nbDontknow <- apply(endline,1,function(x) sum(x=="dontknow"|x==-88, na.rm=T))
    endline$date <- format(endline$start, "%m-%d")
    
    
    colnames(endline) <- paste0(colnames(endline),".e")
    colnames(backcheck) <- paste0(colnames(backcheck),".s")
    
    data_check <- left_join(endline[,c(endline_keep,endline_var)], backcheck[,c(backcheck_keep,backcheck_var)], by=c("index.e"="index.s"), keep=TRUE)
    
    data_check$qualScore <-0
    
    for(i in 1:length(backcheck_var)){
        isItDifferent <- ifelse(data_check[,backcheck_var[i]]!=data_check[,endline_var[i]], 1, 0)
        if(backcheck_var[i] %in% c("age_years.s", "HH_size.s")){
            isItDifferent <- (abs(data_check[,backcheck_var[i]]-data_check[,endline_var[i]])) > 1
        }else if(backcheck_var[i]=="csi_score.s"){
            isItDifferent <- (abs(data_check[,backcheck_var[i]]-data_check[,endline_var[i]])) > 3
        }else if(backcheck_var[i]=="Telephone_Number.s"){
            isItDifferent <- (str_sub(data_check[,backcheck_var[i]], start= -8)!=str_sub(data_check[,endline_var[i]], start=-8))
        }
        data_check$qualScore <- data_check$qualScore + ifelse(is.na(isItDifferent), .5, isItDifferent)
    }
    
    data_check$percentMatch <- ifelse(is.na(data_check$index.s), NA, 100-data_check$qualScore/length(backcheck_var)*100)
    
    
    return(data_check)
}



get_data <- function(login, password){
    d_endline <- tryCatch(kobohr_getdata_csv(url_endline,login,password), error=function(e){message("can't access data")})
    d_backcheck <- tryCatch(kobohr_getdata_csv(url_backcheck,login,password), error=function(e){message("can't access data")})
    if(length(d_endline)>1 & length(d_endline)>1){
        endline <- as.data.frame(d_endline)
        colnames(endline) <-gsub(".*/","",colnames(endline))
        endline$survey_agency[is.na(endline$survey_agency)] <- "-"
        endline$username[is.na(endline$username)] <- "-"
        endline$District[is.na(endline$District)] <- "-"
        backcheck <- as.data.frame(d_backcheck)
        colnames(backcheck) <-gsub(".*/","",colnames(backcheck))
        return(prepareData(endline, backcheck))
    }
}

library(shiny)

ui <- fluidPage(
    
    titlePanel("LORA data collection monitoring dashboard"),
    
    sidebarLayout(
        sidebarPanel(
            actionButton("load_data", "Load data"),
            #selectInput("summary_by", "Choose focus of quality summary table", c("survey_agency.e", "username.e"), multiple = TRUE),
            pickerInput("summary_by", "Summary by (top table)", c("survey_agency.e", "District.e", "username.e", "date.e"), selected = c("survey_agency.e"), multiple = TRUE),
            pickerInput("filter_date", "Filter date",sort(unique(listData()$date.e), na.last=TRUE),selected = unique(listData()$date.e),options = list(`actions-box` = TRUE), multiple = T),
            pickerInput("filter_agency", "Filter agency partner",sort(unique(listData()$survey_agency.e), na.last=TRUE),selected = unique(listData()$survey_agency.e),options = list(`actions-box` = TRUE), multiple = T),
            pickerInput("filter_district", "Filter district",sort(unique(listData()$District.e), na.last=TRUE),selected = unique(listData()$District.e),options = list(`actions-box` = TRUE), multiple = T),
            pickerInput("filter_username", "Filter username",sort(unique(listData()$username.e), na.last=TRUE),selected=unique(listData()$username.e),options = list(`actions-box` = TRUE), multiple = T),
            h5("For bottom table:"),
            sliderInput("dontknow_threshold",
                        "Show when nomber of dont know is greater than... ",
                        min=0,
                        max=50,
                        value=0),
            #sliderInput("duration_threshold",
            #            "Show when interview durations is less than... ",
            #            min=0,
            #            max=999900,
            #            value=200),
            sliderInput("data_quality_threshold",
                        "Show when back-check percent match is less than... ",
                        min=0,
                        max=100,
                        value=100),
            downloadButton("downloadtable1", "Download top table"),
            downloadButton("downloadtable2", "Download bottom table")
        ),
        
        mainPanel(
            dataTableOutput("summary_table"),
            br(),br(),
            dataTableOutput("data")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    data <- reactiveValues()
    data$check <- data.frame(matrix(ncol=length(allvars), nrow = 0, dimnames=list(NULL, allvars)) )
    # observe({showModal(modalDialog(
    #   title = "start",
    #   paste(colnames(data$check)[1:5], collapse=","),
    #   easyClose = TRUE,
    #   footer = NULL
    # ))
    # })
    observeEvent(input$load_data, {
        showModal(modalDialog(
            textInput('login', 'Please enter login to access data'),
            textInput('password', 'Please enter password to access data'),
            footer=tagList(
                actionButton('submit', 'Submit'),
                modalButton('cancel')
            )
        ))
    })
    observeEvent(input$submit, {
        data$check <- get_data(isolate(input$login), isolate(input$password))
        removeModal()
        updatePickerInput(session, "filter_agency", choices = sort(unique((data$check)$survey_agency.e), na.last=TRUE),selected = unique((data$check)$survey_agency.e))
        updatePickerInput(session, "filter_date", choices = sort(unique((data$check)$date.e), na.last=TRUE),selected = unique((data$check)$date.e))
        updatePickerInput(session, "filter_district", choices = sort(unique((data$check)$District.e), na.last=TRUE),selected = unique((data$check)$District.e))
    })
    
    
    # update list of usernames
    updatedChoices = reactive({
        filtered_data <- isolate(data$check) %>%
            filter(survey_agency.e %in% input$filter_agency,
                   District.e %in% input$filter_district,
                   date.e %in% input$filter_date)
        enum_choices <- filtered_data %>%
            pull(username.e) %>% unique() %>% sort(na.last=TRUE)
        tmp<-list(enum_choices)
        return(tmp)
    })
    observe({
        updatePickerInput(session, "filter_username", choices = updatedChoices()[[1]], selected=updatedChoices()[[1]])
    })
    
    
    
    
    # Prepare top table (summary)
    summaryTable <- reactive({
        # showModal(modalDialog(
        #      title = "start",
        #      paste(colnames(data$check)[1:5], collapse=","),
        #      easyClose = TRUE,
        #      footer = NULL
        #    ))
        isolate(data$check) %>%
            filter(survey_agency.e%in%input$filter_agency,
                   username.e %in% input$filter_username,
                   date.e %in% input$filter_date,
                   District.e%in% input$filter_district)%>%
            group_by_at(vars(input$summary_by))%>%
            summarise(N=sum(!is.na(index.e), na.rm=T),
                      time_ok=mean(interviewDuringDay.e, na.rm=TRUE),
                      avg_duration = mean(interviewDuration.e, na.rm=TRUE),
                      `<25min`=mean(veryshort.e, na.rm=TRUE),
                      `25-30min`=mean(short.e, na.rm=TRUE),
                      `30-90min` = mean(reasonableDuration.e, na.rm=TRUE),
                      avg_dontknow = mean(nbDontknow.e),
                      N_backchecked=sum(!is.na(index.s), na.rm=T),
                      #prop_bc=sum(!is.na(index.s), na.rm=T)/sum(!is.na(index.e), na.rm=T),
                      avg_match_perc=mean(percentMatch, na.rm=T)/100,
                      min_match_perc=min(percentMatch, na.rm=T)/100,
                      #`25percMatch`=quantile(percentMatch, probs = .25, na.rm=T)/100
                      
            )
    })
    
    # Prepare bottom table
    filteredRawData <- reactive({
        data$check %>%
            
            # Apply filter
            #filter(!is.na(index.s))%>%
            filter(percentMatch<=input$data_quality_threshold | input$data_quality_threshold==100)%>%
            filter(nbDontknow.e>=input$dontknow_threshold)%>%
            #filter(reasonableDuration.e<=input$duration_threshold | is.na(input$duration_threshold))%>%
            
            filter(username.e %in% input$filter_username,
                   survey_agency.e%in%input$filter_agency,
                   date.e %in% input$filter_date,
                   District.e%in% input$filter_district)%>%
            .[,varshown]
    })
    
    
    # show the top table
    output$summary_table <- renderDataTable({
        datatable(summaryTable()) %>%
            formatPercentage(c("avg_match_perc", "min_match_perc", "<25min", "25-30min", "30-90min", "time_ok"), 0)%>%
            #formatRound(c("prop"), 3)%>%
            formatRound(c("avg_dontknow", "avg_duration"), 1)
    })
    
    # show the bottom table
    output$data <- renderDataTable({
        datatable(filteredRawData()) %>%
            formatRound(c("percentMatch"), 2)%>%
            formatRound(c("interviewDuration.e"), 0)
    })
    
    # make the download top table button
    output$downloadtable1 <- downloadHandler(
        filename = function() "summaryTable.csv",
        content = function(file) {
            write.csv(summaryTable(), file, row.names = FALSE)
        }
    )
    
    # make the download bottom table button
    output$downloadtable2 <- downloadHandler(
        filename = function() "filteredRawData.csv",
        content = function(file) {
            write.csv(filteredRawData(), file, row.names = FALSE)
        }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
