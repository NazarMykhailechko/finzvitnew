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
library(jsonlite)
library(reshape2)
library(dplyr)
library(tidyr)
library(openxlsx)
library(shinyauthr)
library(shinyjs)
library(data.table)
library(RMySQL)
library(rvest)
library(polite)
#library(lgr)


user_base <- data.frame(
  user = c("finzvit", "admin"),
  password = c("accord", "109205761"), 
  permissions = c("admin", "standard"),
  name = c("User One", "User Two"),
  stringsAsFactors = FALSE,
  row.names = NULL
)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # must turn shinyjs on
  shinyjs::useShinyjs(),
  # add logout button UI 
  div(class = "pull-right", logoutUI(id = "logout", label="Вийти", 
                                     icon = icon("angle-right",
                                     class = "normal"))),
  # add login panel UI function
  loginUI(id = "login", title = "Вхід до системи", 
                        user_title = "Логін",
                        pass_title = "Пароль",
                        error_message = "Невірний логін або пароль!",
                        login_title = "Увійти"),
  
  # setup table output to show user info after login
  tableOutput("user_table"),
  
  
  tags$head(
    HTML(
      "
          <script>
          var socket_timeout_interval
          var n = 0
          $(document).on('shiny:connected', function(event) {
          socket_timeout_interval = setInterval(function(){
          Shiny.onInputChange('count', n++)
          }, 15000)
          });
          $(document).on('shiny:disconnected', function(event) {
          clearInterval(socket_timeout_interval)
          });
          </script>
          "
    )
  ),
  
  div(id = "kp", textOutput("keepAlive")) %>% shinyjs::hidden(),
  
     theme = shinytheme("superhero"),

    # Application title
  div( id = "display_content",
       
       titlePanel(title="Фінансова звітність підприємств"),
  

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          
          textInput("okpo", "Введіть єдрпоу підприємства:",),
          actionButton("act","Знайти",icon = icon("search", class = "normal")),
          downloadButton('downloadData', 'Зберегти в .xlsx')
        ),

        # Show a plot of the generated distribution
        mainPanel(
          
          
          textOutput("company"),
          tags$head(tags$style('#company {color:red;font:strong;font-weight:bold;font-size:18px;}')),
          tags$body(tags$style('#companyinfo {color:lightgrey;background-color:black;font-size:12px;}')),
          tags$body(tags$style('#founders {color:lightgrey;background-color:black;font-size:12px;}')),
          
          tabsetPanel(type = "tab", id = "mytabs",
          
                      tabPanel("Баланс", tableOutput("balance")),  
                      tabPanel("Звіт про фінансові результати", tableOutput("finrez")),
                      tabPanel("Інфо", verbatimTextOutput("companyinfo")),
                      tabPanel("Власники", verbatimTextOutput("founders")),
                      tabPanel("Secret", id="Secret", tableOutput("secret")))

        )
    )
)  %>% shinyjs::hidden()

)

# Define server logic required to draw a histogram
server <- function(input, output, session)  {
  
  output$keepAlive <- renderText({
    req(input$count)
    paste("keep alive ", input$count)
  })
  
  
  # call the logout module with reactive trigger to hide/show
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )
  

  
  # call login module supplying data frame, user and password cols
  # and reactive trigger
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = user,
    pwd_col = password,
    #sodium_hashed = TRUE,
    log_out = reactive(logout_init())
  )
  
  # pulls out the user information returned from login module
  user_data <- reactive({credentials()$info})
  
  shiny::observe({
    req(credentials()$user_auth)
    shinyjs::show(id = "display_content")
  })
  
  shiny::observe({
    req(!credentials()$user_auth)
    shinyjs::hide(id = "display_content")
  })
  
  
  
  shiny::observe({
    req(credentials()$user_auth)
    if (user_data()$user == "admin"){
      showTab(inputId = "mytabs" ,target = "Secret")
      
      options(mysql = list(
        "host" = "nuepp3ddzwtnggom.chr7pe7iynqr.eu-west-1.rds.amazonaws.com",
        "port" = 3306,
        "user" = "ftgqrah1iipvc2dl",
        "password" = "honjdx189jnetqyi"
      ))
      databaseName <- "e2y37zog3x0r8b3l"
      table <- "logger"
      
      db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host, 
                      port = options()$mysql$port, user = options()$mysql$user, 
                      password = options()$mysql$password)
    
      
      # Construct the fetching query
      query <- sprintf("SELECT * FROM %s order by ts desc", "logger")
      # Submit the fetch query and disconnect
      data <- dbGetQuery(db, query)
      dbDisconnect(db)
      #print(data)
      
      output$secret <- renderTable({

        #logging <- read_json_lines("logging.json")
        #logging$timestamp <- strftime(logging$timestamp, format="%d.%m.%Y %H:%M:%S")
        #logging[,c(2,5)]
        
        #log <- read.table("logger.txt",header = T,sep = ";")
        #log[order(log$date, decreasing = TRUE), ]
        
        data

      })
      
      }
    else{
      hideTab(inputId = "mytabs" ,target = "Secret")
    }

  })

  #output$user_table <- renderText({
    # use req to only render results when credentials()$user_auth is TRUE
    #req(credentials()$user_auth)
    #paste("Ви увійшли як: ", user_data()$user)
  #})
  
  
  

  observeEvent(input$act,{
    
    print(input$okpo)
    if (input$okpo == "") {
      output$company <- renderText("Ви не ввели єдрпоу!")
      return()
    }

    
    if (is.na(as.numeric(as.character(input$okpo)))) {
      output$company <- renderText("Неправильний формат єдрпоу!")
      return()
    }

    
    if (nchar(as.character(input$okpo)) < 8) {
      output$company <- renderText("Довжина єдрпоу менше 8 символів!")
      return()
    }
    
    if (nchar(as.character(input$okpo)) > 9) {
      output$company <- renderText("Неправильний формат єдрпоу!")
      return()
    }
    

    #lgr$add_appender(AppenderJson$new("logging.json"), name = "json")
    
    files <- read.table("files.txt",sep = ";", header = T)

    jsonpath <- as.character(subset(files,as.numeric(minokpo) <= as.numeric(input$okpo) & 
                                          as.numeric(maxokpo) >= as.numeric(input$okpo), select=c("filepath")))

    print(jsonpath)

    jsondata <- jsonlite::fromJSON(jsonpath)
  
    
    companyName <- as.character(subset(jsondata,TIN == input$okpo, select=c("FN")))
    output$company <- renderText(companyName)
    
    
    
    #lgr$info(paste(input$okpo, " - ", companyName))
    #read_json_lines("logging.json")
    #lgr$remove_appender("json")
    #write(paste0(Sys.time(), ";",paste(input$okpo, " - ", companyName, "\n")),file="logger.txt",append=TRUE)
    
    
    #DATABASE
    #----------------------------------------------------------
    
    options(mysql = list(
      "host" = "nuepp3ddzwtnggom.chr7pe7iynqr.eu-west-1.rds.amazonaws.com",
      "port" = 3306,
      "user" = "ftgqrah1iipvc2dl",
      "password" = "honjdx189jnetqyi"
    ))
    databaseName <- "e2y37zog3x0r8b3l"
    table <- "logger"
    
    db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host, 
                    port = options()$mysql$port, user = options()$mysql$user, 
                    password = options()$mysql$password)
    
    # Connect to the database
    #db <- dbConnect(MySQL(), dbname = "clientsdb", host = "127.0.0.1", 
    #               port = 3306, user = "root", 
    #              password = "WIN72007@NAZAr")
    
    
    # Construct the update query by looping over the data fields
    query <- sprintf(
      "INSERT INTO %s (%s, %s) VALUES ('%s', '%s')",
      table, 
      paste("okpo", collapse = ", "),
      paste("name", collapse = "', '"),
      paste(input$okpo, collapse = "', '"),
      paste(companyName, collapse = "', '")
      
    )
    
    #print(query)
    # Submit the update query and disconnect
    dbGetQuery(db, query)
    dbDisconnect(db)
    
    #----------------------------------------------------------
    
    
    
    if (companyName == "character(0)") {
      output$company <- renderText("Про дану компанію Інформація відсутня :(")
      
      output$balance <- renderTable({
        return("Дані відсутні")
      },bordered = F,striped = F,rownames = F, na = "", hover = T, spacing = c("xs")) 
      
      output$finrez <- renderTable({
        return("Дані відсутні")
      },bordered = F,striped = F,rownames = F, na = "", hover = T) 
      
      output$companyinfo <- renderPrint({
        return("Дані відсутні")
      }) 
      
      return()
    }
    
    print(companyName)
    
    companyInfo <- subset(jsondata,TIN == input$okpo, select=c("A","K","R","T","S"))
  
    
    names(companyInfo)[1] <- "Адреса"
    names(companyInfo)[2] <- "КВЕД"
    names(companyInfo)[3] <- "Керівник"
    names(companyInfo)[4] <- "Телефон"
    names(companyInfo)[5] <- "Середня к-ть праціників"
    
    companyInfo <- as.data.frame(t(companyInfo))
    
    companyInfo <- format(companyInfo, justify = "left")
    
    names(companyInfo)[0] <- ""
    names(companyInfo)[1] <- ""
    
    
    res <- jsondata[jsondata$TIN == input$okpo,]
    res<-res[[8]][[1]]
    res <- cbind(999,res)
   
    res[res == ''] <- 0
    res[is.na(res)] <- 0

    res_melted <- melt(res, id=c("999"))
    res_melted[is.na(res_melted)] <- 0
    
    
    mutate(res_melted, value = as.numeric(gsub(",", ".", gsub("\\.", "", value)))) -> res_melted
    
    res_melted %>% separate(variable, c("YEAR", "ROW"), "_") -> res_melted
  
    
    
    #44776595

    
    
    finzvit <- dcast(res_melted, 999 + ROW ~ YEAR, value.var = "value",fun.aggregate = sum)
    
    
    balance_aricles <- read.csv("BALANCE_ARTICLES.txt", sep = ";", header = T, encoding = '1251')
    #30525175

    finrez_aricles <- read.csv("FINREZ_ARTICLES.txt", sep = ";", header = T, encoding = '1251')
    
    colcount <- length(finzvit)-2
    
    if (colcount == 1){
      finzvit[, c('2', '1', '0')] = 0
      }
    if (colcount == 2){
      finzvit[, c('1', '0')] = 0
  
        }
    if (colcount == 3){
      finzvit[, c('0')] = 0
      }
    
    finzvit <- subset(finzvit, select=c("1","ROW","0","1","2","3"))
    
    
    names(finzvit)[3] <- "01.01.2020"
    names(finzvit)[4] <- "01.01.2021"
    names(finzvit)[5] <- "01.01.2022"
    names(finzvit)[6] <- "01.01.2023"
    
    
    balance <- subset(merge(balance_aricles, finzvit[,c(2:6)], all.x=TRUE), select=c(1,7,6,5,4))
    balance[is.na(balance)] <- 0
    balance <- balance[balance$ROW > 0,]
    balance <- subset(merge(balance_aricles, balance[,c(1:5)], all.x=TRUE), select=c(2,3,1,4,5,6,7))
    names(balance)[2] <- "Стаття"
    names(balance)[3] <- "Код рядка"
    balance <- balance[order(balance$id, decreasing = FALSE), ]
    


    finrez <- subset(merge(finrez_aricles, finzvit[,c(2:6)], all.x=TRUE), select=c(1,7,6,5,4))
    finrez[is.na(finrez)] <- 0
    finrez <- finrez[finrez$ROW > 0,]
    finrez <- subset(merge(finrez_aricles, finrez[,c(1:5)], all.x=TRUE), select=c(2,3,1,4,5,6,7))
    names(finrez)[2] <- "Стаття"
    names(finrez)[3] <- "Код рядка"
    finrez <- finrez[order(finrez$id, decreasing = FALSE), ]

    
    
    
    #------------------------------------------------------------
    #Дата оновлення
    url <- paste0("https://youcontrol.com.ua/catalog/company_details/",input$okpo)
    #starwars <- read_html(url)
    starwars <- url %>% bow() %>% scrape()
    
    
    xmlset <- html_elements(starwars, css = "div #catalog-company-beneficiary div .seo-table-row") %>% html_children()
    set <- length(html_elements(starwars, css = "div #catalog-company-beneficiary div .seo-table-row") %>% html_children())/2
    
    actual_date <- as.character(strsplit(html_elements(starwars, css = "div #catalog-company-beneficiary .seo-table-date.date-actual-table span") %>% html_text2(),"\n"))
    
    #Засновники
    list <- strsplit(html_elements(starwars, css = "div #catalog-company-beneficiary div .seo-table-row") %>% html_text2(),"\n")
    
    indxf <- 0
    for (i in 1:length(xmlset)) {if(grepl("Перелік засновників", xmlset[i])){ indxf <- i}}
    
    if(indxf != 0){
      melted_list <- as.data.frame(melt(list[(indxf+1)/2])[,1])
      
      founders <- as.data.frame(melted_list[!apply(melted_list == "", 1, all), ])
      colnames(founders)[1] ="founders"
      founders <- filter(founders, founders!= "Всі Засновники Приховати")
      if (length(founders) == 0){
        founders <- "не має даних"
      }else{
        founders <- founders[-1,]
      }
    }else{
      founders <- "не має даних"
    }
    
    
    #Бенефіціари
    
    indxb <- 0
    for (i in 1:length(xmlset)) {if(grepl("бенефіціарн", xmlset[i])){ indxb <- i}}
    if(indxb != 0){
      list <- strsplit(html_elements(starwars, css = "div #catalog-company-beneficiary div .seo-table-row") %>% html_text2(),"\n")
      melted_list <- as.data.frame(melt(list[(indxb+1)/2])[,1])
      beneficiaries <- as.data.frame(melted_list[!apply(melted_list == "", 1, all), ])
      colnames(beneficiaries)[1] ="beneficiaries"
      if (length(beneficiaries[-1,]) == 0){
        beneficiaries <- "не має даних"
      }else{
        beneficiaries <- beneficiaries[-1,]
      }
    }else{
      beneficiaries <- "не має даних"
    }
    
    #------------------------------------------------------------
    
    
    
    #finrez[is.na(finrez)] <- 0
    #38324809
    
    output$balance <- renderTable({
      balance
    },bordered = F,striped = F,rownames = F, na = "", hover = T, spacing = c("xs")) 
    
    output$finrez <- renderTable({
      finrez
    },bordered = F,striped = F,rownames = F, na = "", hover = T) 
    
    output$companyinfo <- renderPrint({
      companyInfo
    })
    
    output$founders <- renderPrint({
      print(actual_date)
      cat("\n")
      print("-----------------------Засновники--------------------------")
      print(founders)
      cat("\n")
      print("-----------------------Бенефіціари-------------------------")
      print(beneficiaries)
      cat("\n")
    }) 
    
    if (user_data()$user == "admin"){
      
      
      options(mysql = list(
        "host" = "nuepp3ddzwtnggom.chr7pe7iynqr.eu-west-1.rds.amazonaws.com",
        "port" = 3306,
        "user" = "ftgqrah1iipvc2dl",
        "password" = "honjdx189jnetqyi"
      ))
      databaseName <- "e2y37zog3x0r8b3l"
      table <- "logger"
      
      db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host, 
                      port = options()$mysql$port, user = options()$mysql$user, 
                      password = options()$mysql$password)
      
      
      # Construct the fetching query
      query <- sprintf("SELECT * FROM %s order by ts desc", "logger")
      # Submit the fetch query and disconnect
      data <- dbGetQuery(db, query)
      dbDisconnect(db)
      #print(data)
      
      
    output$secret <- renderTable({
      
      #logging <- read_json_lines("logging.json")
      #logging$timestamp <- strftime(logging$timestamp, format="%d.%m.%Y %H:%M:%S")
      #logging[,c(2,5)]
      
      #log <- read.table("logger.txt",header = T,sep = ";")
      #log[order(log$date, decreasing = TRUE), ]
      
      data
      
    })}
    
    
    #output$downloadData <- downloadHandler(
      
     # filename = function() {paste0(companyName, ".xlsx")},
    #  content = function(file) {write_xlsx(balance, path = file)}
      
    #)
    
    output$downloadData <- downloadHandler(
      
      filename = function() {paste0(input$okpo,"-",companyName, ".xlsx")},
      content = function(file) {

         
         wb <- createWorkbook()
         addWorksheet(wb, "Balance")
         writeData(wb, "Balance", balance, rowNames = FALSE)
         setColWidths(wb, "Balance", cols = c(1, 2, 3, 4, 5, 6, 7), widths = c("auto", 70, 10, "auto", "auto", "auto", "auto"))
         addWorksheet(wb, "Finrez")
         writeData(wb, "Finrez", finrez, rowNames = FALSE)
         setColWidths(wb, "Finrez", cols = c(1, 2, 3, 4, 5, 6, 7), widths = c("auto", 70, 10, "auto", "auto", "auto", "auto"))
         addWorksheet(wb, "CompanyInfo")
         writeData(wb, "CompanyInfo", companyInfo, rowNames = TRUE)
         setColWidths(wb, "CompanyInfo", cols = c(1, 2), widths = c("auto", "auto"))
         addWorksheet(wb, "Founders")
         writeData(wb, "Founders", founders, rowNames = TRUE)
         setColWidths(wb, "Founders", cols = c(1, 2), widths = c("auto", "auto"))
         addWorksheet(wb, "Beneficiaries")
         writeData(wb, "Beneficiaries", beneficiaries, rowNames = TRUE)
         setColWidths(wb, "Beneficiaries", cols = c(1, 2), widths = c("auto", "auto"))
         saveWorkbook(wb, file, overwrite = TRUE)
        
        
        }
      
    )
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
