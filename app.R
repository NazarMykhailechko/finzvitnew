#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyBS)
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
library(XML)
library(xml2)
library(shinyWidgets)
#library(tableHTML)
#library(rvest)
#library(httr)
#library(downloader)
#library(curl)
#library(RSelenium)
#library(polite)
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
  div(id = "loginpage", loginUI(id = "login", title = "Вхід до системи", 
                        user_title = "Логін",
                        pass_title = "Пароль",
                        error_message = "Невірний логін або пароль!",
                        login_title = "Увійти")),
  
  # setup table output to show user info after login
  tableOutput("user_table"),
  
  tags$head(
    # Note the wrapping of the string in HTML()
    tags$style(HTML("@import url('https://fonts.googleapis.com/css?family=Source+Code+Pro:200');

body  {
    background-image: url('https://w0.peakpx.com/wallpaper/738/166/HD-wallpaper-blue-digital-background-technology-digital-technology-coding-concepts-blue-technology-background.jpg');
  background-size:cover;
        -webkit-animation: slidein 100s;
        animation: slidein 100s;

        -webkit-animation-fill-mode: forwards;
        animation-fill-mode: forwards;

        -webkit-animation-iteration-count: infinite;
        animation-iteration-count: infinite;

        -webkit-animation-direction: alternate;
        animation-direction: alternate;              
}

@-webkit-keyframes slidein {
from {background-position: top; background-size:1300px; }
to {background-position: -100px 0px;background-size:2750px;}
}

@keyframes slidein {
from {background-position: top;background-size:1300px; }
to {background-position: -100px 0px;background-size:2750px;}

}



.center
{
  display: flex;
  align-items: center;
  justify-content: center;
  position: absolute;
  margin: auto;
  top: 0;
  right: 0;
  bottom: 0;
  left: 0;
  background: rgba(75, 75, 250, 0.3);
  border-radius: 3px;
}
.center h1{
  text-align:center;
  color:white;
  font-family: 'Source Code Pro', monospace;
  text-transform:uppercase;
}"))
  ),
  

tags$head(
  # Note the wrapping of the string in HTML()
  tags$style(HTML("body {
  font-family: system-ui, sans-serif;
  padding: 1rem;
}


table {
  width: 100%;
  height: 70vh;
  margin: 0 auto;
  display: block;
  overflow-x: auto;
}

tbody {
  
  white-space: nowrap;
}

th,
td {
  padding: 1rem;
}

thead {
  position: sticky;
  top: 0;
  background: #4e5d6c;
  text-align: left;
}
"))
),
  
  


  
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
  div(id = "ip", textOutput("chekipadress")) %>% shinyjs::hidden(),
  
     theme = shinytheme("superhero"),

    # Application title
  div( id = "display_content",
       
       titlePanel(title="Фінансова звітність підприємств"),
  

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          
          textInput("okpo", "Введіть єдрпоу підприємства:",),
          actionButton("act","Знайти",icon = icon("search", class = "normal")),
          downloadButton('downloadData', 'Зберегти в .xlsx'),
          actionButton("search","Smida",icon = icon("business-time", class = "normal")),
          #sliderTextInput("slide1", "Показати приріст між датами", choices = c("01.01.2023", "01.01.2022","01.01.2021","01.01.2020"), selected=c("01.01.2023","01.01.2022"), grid = T),
          
        ),

        # Show a plot of the generated distribution
        mainPanel(
          
          
          textOutput("company"),
          
          
          
          #sliderTextInput("slide", "Показати приріст між датами", choices = c("01.01.2023", "01.01.2022","01.01.2021","01.01.2020"), selected=c("01.01.2023","01.01.2022"), grid = T,width = '100%'),
        
          tags$body(tags$style('.sw-dropdown-content.animated.sw-show {color:lightgrey;background-color:black;font-size:12px;}')),
          tags$body(tags$style('.sw-dropdown-in {color:lightgrey;background-color:black;font-size:12px;}')),
          tags$style('#slide-label {text-align:center;}'),
          tags$body(tags$style('{height: 100%;}')),
          tags$head(tags$style('#company {color:magenta;font:strong;font-weight:bold;font-size:18px;}')),
          tags$body(tags$style('#companyinfo {color:lightgrey;background-color:black;font-size:12px;}')),
          tags$body(tags$style('#distTable {color:lightgrey;background-color:black;font-size:12px;}')),
          tags$body(tags$style('#companyinfo1 {color:lime;background-color:black;font-size:12px;}')),
          tags$style(type="text/css", "#companyinfo2 {color:lightgrey;background-color:black;font-size:12px;white-space: pre-wrap;}"),
          tags$style(type="text/css", "#manrep {color:lightgrey;background-color:black;font-size:12px;white-space: pre-wrap;}"),
          tags$tbody(tags$style('.table.shiny-table {background-color:#4e5d6c;opacity:0.80;}')),
          tags$tbody(tags$style('.table.shiny-table td {white-space: wrap;}')),
    

          
          #tags$tbody(tags$style('.table.shiny-table thead {white-space: nowrap;}')),
          tags$body(tags$style('.col-sm-4 {opacity:0.80;}')),
          #tags$body(tags$style('.tab-content {height:500px;}')),
          
          bsModal("modalExample", title = div("Smida", icon("search",class = "fa-beat")), "search", size = "large", verbatimTextOutput("companyinfo1"),
                  
                  tabsetPanel(type = "tab", id = "mytabs2", 
                              
                              tabPanel("Баланс", tableOutput("balance2")),  
                              tabPanel("Звіт про фінансові результати", tableOutput("finrez2")),
                              tabPanel("Інфо", verbatimTextOutput("companyinfo2")),
                              tabPanel("Упр.звіт", verbatimTextOutput("manrep")),
                              tabPanel("Власники", tableOutput("founders2"))),
                  
                  tableOutput("distTable")),
          
          dropdown(
            
            sliderTextInput("slide", "Приріст між датами", choices = c("01.01.2023", "01.01.2022","01.01.2021","01.01.2020"), selected=c("01.01.2023","01.01.2022"), grid = T),
            
            style = "minimal", icon = icon("gear"),size = "sm",
            status = "default", width = "300px",
            animate = animateOptions(
              enter = animations$fading_entrances$fadeInLeftBig,
              #exit = animations$fading_exits$fadeOutRightBig
            ),label = "змінити прирости"
          ),
          
          tabsetPanel(type = "tab", id = "mytabs",
          
                      tabPanel("Баланс", tableOutput("balance")),  
                      tabPanel("Звіт про фінансові результати", tableOutput("finrez")),
                      tabPanel("Інфо", verbatimTextOutput("companyinfo")),
                      tabPanel("Власники", tableOutput("founders")),
                      tabPanel("Secret", id="Secret", tableOutput("secret")))

        )
    )
)  %>% shinyjs::hidden()

)

# Define server logic required to draw a histogram
server <- function(input, output, session)  {
  
  session$allowReconnect(TRUE)

  #shinyjs::hide(id = "loginpage")
  shinyjs::show(id = "loginpage")

  
  runjs('$.getJSON("https://api.ipify.org/?format=json", function(data) {
  var1 = JSON.stringify(data, null, 2);Shiny.setInputValue("var1", var1);
});')
  
  ipaddress <- reactive({
    if(length(input$var1) > 0){
      x <- fromJSON(input$var1)$ip
    }
  })
  
##################### IP ADDRESS#####################  
  # observeEvent(input$var1, {res <- ipaddress()})
  # 
  # #shinyjs::show(id = "loginpage")
  # shinyjs::show(id = "ip")
  # 
  #     
  #     output$chekipadress <- renderText({
  # 
  #       #shinyjs::show(id = "ip")
  #       #fromJSON(input$var1)$ipAddress
  #       if(length(ipaddress())>0){
  #           if(ipaddress() == "213.110.152.168" ){
  #             print(ipaddress())
  #             shinyjs::show(id = "loginpage")
  #   
  #           }else{
  #             shinyjs::hide(id = "loginpage")
  #             print("ERROR 403 FORBIDDEN!")
  #           }
  #       }
  #     })
#####################################################       
      
      

  
  
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
    UOfiles <- read.table("UOfiles.txt",sep = ";", header = T)

    jsonpath <- as.character(subset(files,as.numeric(minokpo) <= as.numeric(input$okpo) & 
                                          as.numeric(maxokpo) >= as.numeric(input$okpo), select=c("filepath")))

    UOjsonpath <- as.character(subset(UOfiles,as.numeric(minokpo) <= as.numeric(input$okpo) & 
                                      as.numeric(maxokpo) >= as.numeric(input$okpo), select=c("filepath")))    
    
    print(jsonpath)
    print(UOjsonpath)

    jsondata <- jsonlite::fromJSON(jsonpath)
    UOjsondata <- jsonlite::fromJSON(UOjsonpath)

    
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
    
    
    founders <- UOjsondata[UOjsondata$TIN == input$okpo,]

    if (nrow(founders) == 0){
      founders <- "Не має даних"
    }else{
      founders<-founders[[2]][[1]]
      
      names(founders)[1] <- ""
      names(founders)[2] <- ""
      names(founders)[3] <- ""
      founders <- format(founders, justify = "left")
    }
    

    
    res <- jsondata[jsondata$TIN == input$okpo,]
    res<-res[[8]][[1]]
    res <- cbind(999,res)
   
    res[res == ''] <- 0
    res[is.na(res)] <- 0

    res_melted <- reshape2::melt(res, id=c("999"))
    res_melted[is.na(res_melted)] <- 0
    
    
    mutate(res_melted, value = as.numeric(gsub(",", ".", gsub("\\.", "", value)))) -> res_melted
    
    res_melted %>% separate(variable, c("YEAR", "ROW"), "_") -> res_melted
  
    
    
    #44776595

    
    
    finzvit <- reshape2::dcast(res_melted, 999 + ROW ~ YEAR, value.var = "value",fun.aggregate = sum)
    
    
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
    
    #change number format
      # for (c in 3:ncol(finzvit)){
      #     finzvit[,c] <- formatC(finzvit[,c], format="f", big.mark=" ", digits=0)
      # }
    
    
    balance <- subset(merge(balance_aricles, finzvit[,c(2:6)], all.x=TRUE), select=c(1,7,6,5,4))
    balance[is.na(balance)] <- 0
    balance <- balance[balance$ROW > 0,]
    balance <- subset(merge(balance_aricles, balance[,c(1:5)], all.x=TRUE), select=c(2,3,1,4,5,6,7))
    names(balance)[2] <- "Стаття"
    names(balance)[3] <- "Код рядка"
    balance <- balance[order(balance$id, decreasing = FALSE), ]
    balance[is.na(balance)] = ""



    finrez <- subset(merge(finrez_aricles, finzvit[,c(2:6)], all.x=TRUE), select=c(1,7,6,5,4))
    finrez[is.na(finrez)] <- 0
    finrez <- finrez[finrez$ROW > 0,]
    finrez <- subset(merge(finrez_aricles, finrez[,c(1:5)], all.x=TRUE), select=c(2,3,1,4,5,6,7))
    names(finrez)[2] <- "Стаття"
    names(finrez)[3] <- "Код рядка"
    finrez <- finrez[order(finrez$id, decreasing = FALSE), ]
    finrez[is.na(finrez)] = ""
    
    
    #------------------------------------------------------------
    #Дата оновлення
    #url <- paste0("https://youcontrol.com.ua/catalog/company_details/",input$okpo)
    #starwars <- read_html(url)
    #starwars <- url %>% bow() %>% scrape()
    #con <- curl(url)
    #rrrr<-readLines(con)
    #print(rrrr)
    
    #headers = c(
     # `user-agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/102.0.5005.61 Safari/537.36'
    #)
    
    #headers = c(
    #  `user-agent` = 'Mozilla/5.0 (Windows NT 10.0; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/51.0.2704.103 Safari/537.36'
    #)
    #uastring <- "mozilla/5.0 (macintosh; intel mac os x 10_11_6) applewebkit/537.36 (khtml, like gecko) chrome/54.0.2840.71 safari/537.36"
    #pgsession <- session("https://youcontrol.com.ua/catalog/company_details/22467327/", user_agent(uastring))
    #print(pgsession)
    
    #res1 <- httr::GET(url = "https://clarity-project.info/edr/39443735", httr::add_headers(headers))
    #print(content(res1))
    
    #res <- httr::GET(url = url, httr::add_headers(headers))
    #starwars <- content(res)

    
    #xmlset <- html_elements(starwars, css = "div #catalog-company-beneficiary div .seo-table-row") %>% html_children()
    #set <- length(html_elements(starwars, css = "div #catalog-company-beneficiary div .seo-table-row") %>% html_children())/2
    #actual_date <- as.character(strsplit(html_elements(starwars, css = "div #catalog-company-beneficiary .seo-table-date.date-actual-table span") %>% html_text2(),"\n"))
    #Засновники
    #list <- strsplit(html_elements(starwars, css = "div #catalog-company-beneficiary div .seo-table-row") %>% html_text2(),"\n")
    
    #indxf <- 0
    #for (i in 1:length(xmlset)) {if(grepl("Перелік засновників", xmlset[i])){ indxf <- i}}
    
    #if(indxf != 0){
      #melted_list <- as.data.frame(reshape2::melt(list[(indxf+1)/2])[,1])
      
      #founders <- as.data.frame(melted_list[!apply(melted_list == "", 1, all), ])
      #colnames(founders)[1] ="founders"
      #founders <- filter(founders, founders!= "Всі Засновники Приховати")
      #if (length(founders) == 0){
       # founders <- "не має даних"
      #}else{
       # founders <- founders[-1,]
      #}
    #}else{
    #  founders <- "не має даних"
    #}
    
    
    #Бенефіціари
    
    #indxb <- 0
    #for (i in 1:length(xmlset)) {if(grepl("бенефіціарн", xmlset[i])){ indxb <- i}}
    #if(indxb != 0){
     # list <- strsplit(html_elements(starwars, css = "div #catalog-company-beneficiary div .seo-table-row") %>% html_text2(),"\n")
    #  melted_list <- as.data.frame(reshape2::melt(list[(indxb+1)/2])[,1])
    #  beneficiaries <- as.data.frame(melted_list[!apply(melted_list == "", 1, all), ])
    #  colnames(beneficiaries)[1] ="beneficiaries"
    #  if (length(beneficiaries[-1,]) == 0){
    #    beneficiaries <- "не має даних"
    #  }else{
    #    beneficiaries <- beneficiaries[-1,]
    #  }
    #}else{
    #    beneficiaries <- "не має даних"
    #}
    
    #------------------------------------------------------------
    
    
    
    #finrez[is.na(finrez)] <- 0
    #38324809
 
    output$balance <- renderTable({
      
      balance <- cbind(balance, Приріст = as.numeric(unlist(balance[input$slide[1]])) - as.numeric(unlist(balance[input$slide[2]])))
      
      balance$Приріст[is.na(balance$Приріст)] = ""

      
      balance <- balance %>% relocate(Приріст, .after = "Код рядка")
      
      for (r in 1:nrow(balance)){
        for (c in 5:ncol(balance)){
          
         if(balance[r,c] != ""){
          if(balance[r,c] < 0){
            balance[r,c] <- paste0('<div style="background-color:#FF0800;text-align:right"><span>', formatC(as.numeric(balance[r,c]), format="f", big.mark=" ", digits=0), '</span></div>')
          }else{
            balance[r,c] <- paste0('<div style="text-align:right;"><span>', formatC(as.numeric(balance[r,c]), format="f", big.mark=" ", digits=0), '</span></div>')
          }
         }
          
        }
      }
      
      for (r in 1:nrow(balance)){
        for (c in 4:4){
          
          if(balance[r,c] != ""){
            if(balance[r,c] < 0){
              balance[r,c] <- paste0('<div style="color:#FF0800;text-align:right"><span>', formatC(as.numeric(balance[r,c]), format="f", big.mark=" ", digits=0), '</span></div>')
            }else{
              balance[r,c] <- paste0('<div style="color:#3FFF00;text-align:right;"><span>', formatC(as.numeric(balance[r,c]), format="f", big.mark=" ", digits=0), '</span></div>')
            }
          }
          
        }
      }

      for (iterator in colnames(balance)){
        if (iterator == input$slide[1] || iterator == input$slide[2]){
          names(balance)[names(balance) == iterator] <-  paste0('<div style="color:magenta;"><span>', iterator, '</span></div>')
        }
      }
      
      
      balance
      

      
    },bordered = F,striped = F,rownames = F, na = "", hover = T, spacing = c("xs"),  sanitize.text.function = function(x) x)
    #},bordered = F,striped = F,rownames = F, na = "", hover = T, spacing = c("xs"))
    
    
    output$finrez <- renderTable({
      
      finrez <- cbind(finrez, Приріст = as.numeric(unlist(finrez[input$slide[1]])) - as.numeric(unlist(finrez[input$slide[2]])))
      
      finrez$Приріст[is.na(finrez$Приріст)] = ""
      
      
      finrez <- finrez %>% relocate(Приріст, .after = "Код рядка")
      
      for (r in 1:nrow(finrez)){
        for (c in 5:ncol(finrez)){
          
          if(finrez[r,c] != ""){
            if(finrez[r,c] < 0){
              finrez[r,c] <- paste0('<div style="background-color:#FF0800;text-align:right"><span>', formatC(as.numeric(finrez[r,c]), format="f", big.mark=" ", digits=0), '</span></div>')
            }else{
              finrez[r,c] <- paste0('<div style="text-align:right;"><span>', formatC(as.numeric(finrez[r,c]), format="f", big.mark=" ", digits=0), '</span></div>')
            }
          }
          
        }
      }
      
      for (r in 1:nrow(finrez)){
        for (c in 4:4){
          
          if(finrez[r,c] != ""){
            if(finrez[r,c] < 0){
              finrez[r,c] <- paste0('<div style="color:#FF0800;text-align:right"><span>', formatC(as.numeric(finrez[r,c]), format="f", big.mark=" ", digits=0), '</span></div>')
            }else{
              finrez[r,c] <- paste0('<div style="color:#3FFF00;text-align:right;"><span>', formatC(as.numeric(finrez[r,c]), format="f", big.mark=" ", digits=0), '</span></div>')
            }
          }
          
        }
      }
      
      for (iterator in colnames(finrez)){
        if (iterator == input$slide[1] || iterator == input$slide[2]){
          names(finrez)[names(finrez) == iterator] <-  paste0('<div style="color:magenta;"><span>', iterator, '</span></div>')
        }
      }
    
      finrez
    },bordered = F,striped = F,rownames = F, na = "", hover = T, spacing = c("xs"),  sanitize.text.function = function(x) x)
    #},bordered = F,striped = F,rownames = F, na = "", hover = T) 
    
    output$companyinfo <- renderPrint({
      companyInfo
    })
    
    output$founders <- renderTable({
      #print(founders, row.names=FALSE)
      founders
      #print(actual_date)
      #actual_date
      #cat("\n")
      #print(content(res1))
      #cat("\n")
      #print("-----------------------Засновники--------------------------")
      #print(founders)
      #cat("\n")
      #print("-----------------------Бенефіціари-------------------------")
      #print(beneficiaries)
      #cat("\n")
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
         #addWorksheet(wb, "Beneficiaries")
         #writeData(wb, "Beneficiaries", beneficiaries, rowNames = TRUE)
         #setColWidths(wb, "Beneficiaries", cols = c(1, 2), widths = c("auto", "auto"))
         saveWorkbook(wb, file, overwrite = TRUE)
        
        
        }
      
    )
  
    
    
  })
  
  #
  #
  #
  #
  #
  #
  #
  #
  #
  #
  #
  
  
  hrefV <- reactiveValues(href = "")
  doneV <- reactiveValues(done = "")
  finzvitV<- reactiveValues(data = data.frame())
  balanceV<- reactiveValues(data = data.frame())
  finrezV<- reactiveValues(data = data.frame())
  smida_ownersV<- reactiveValues(data = data.frame())
  smida_infoV <- reactiveValues(data = data.frame())
  smida_manrepV <- reactiveValues(data = data.frame())
  
  output$companyinfo1 <- renderPrint({
    doneV$done
  })
  
  observeEvent(input$search,{
    
    finzvit.final = data.frame()
    smida_owners <- data.frame()
    smida_ownersV$data <- data.frame()
    smida_info <- data.frame()
    smida_manrep <- data.frame()
    
    smida_infoV$data <- data.frame()
    smida_manrepV$data <- data.frame()
    finzvitV$data <- data.frame()
    balanceV$data <- data.frame()
    finrezV$data <- data.frame()
    doneV$data <- ""
    
    curryear <- format(Sys.Date(), "%Y")
    prevyear <- as.numeric(format(Sys.Date(), "%Y")) -1
    
    dateMax <- paste0(curryear, "-12-31")
    dateMin <- paste0(prevyear, "-12-31")
    
    #smidaurl <- paste0("https://smida.gov.ua/db/api/v1/feed-index.xml?edrpou=", input$okpo, "&date=2022-12-31,2023-12-31")
    smidaurl <- paste0("https://smida.gov.ua/db/api/v1/feed-index.xml?edrpou=", input$okpo, "&date=", dateMin, ",",dateMax)
    #smidaurl <- paste0("https://smida.gov.ua/db/api/v1/feed-index.xml?edrpou=", input$okpo)
    print(smidaurl)
    
    shinyjs::html(id = "companyinfo1", "Йде пошук звітності")
    
    if(as.character(class(try(read_xml(smidaurl))))[1] != "try-error"){
      
      web <- read_xml(smidaurl)
      
      num <- length(xml_children(web))
      num <- num - 1
      for(i in 1:num){
        {
          #href <- hrefV$href
          hrefV$href <- xml_attrs(xml_children(web)[[i+1]])[["href"]]
          #hrefV$href <- href
          
          if (grepl("report.xml", hrefV$href)){
            
            doc <- read_xml(hrefV$href)
          
            
            if (length(xml_find_all(doc, ".//*[name()='z:Fin-general']"))!=0){
              print(hrefV$href)
              shinyjs::html(id = "companyinfo1", paste("Йде пошук звітності", substring(hrefV$href,21)))
              
              report_date <- substring(xml_attrs(xml_children(xml_parent(doc)))[[1]][["FID"]],1,10)
              report_date <- as.Date(report_date, "%Y-%m-%d")
              report_date <- format(report_date, "%d.%m.%Y")
              
              ###Інфо################
              
              if(length(xml_find_all(doc, ".//*[name()='z:DTSBUS_TEXT']")) > 0){
              info <- as.data.frame(xml_attrs(xml_children(xml_find_all(doc, ".//*[name()='z:DTSBUS_TEXT']"))))
              info <- cbind(report_date,info)
              names(info)[1] <- "DATE"
              names(info)[2] <- "INFO"
              #names(info)[1] <- paste("Станом на", report_date)
              #info[,1] <- gsub('ризик','<mark>ризик</mark>',info[,1])
              #rownames(info) <- NULL
              #info <- format(info, justify = "left")
              smida_info <- rbind(info, smida_info)
              
              }
              
              ###########################
              
              ###Упр.звіт################
              
              if(length(xml_find_all(doc, ".//*[name()='z:DTSMANREPA']")) > 0){
                manrep <- as.data.frame(xml_attrs(xml_children(xml_find_all(doc, ".//*[name()='z:DTSMANREPA']"))))
                manrep <- cbind(report_date,manrep)
                names(manrep)[1] <- "DATE"
                names(manrep)[2] <- "INFO"
                #names(info)[1] <- paste("Станом на", report_date)
                #info[,1] <- gsub('ризик','<mark>ризик</mark>',info[,1])
                #rownames(info) <- NULL
                #info <- format(info, justify = "left")
                smida_manrep <- rbind(manrep, smida_manrep)
                
              }
              
              ###########################
              
              ###Власники################
              
              if(length(xml_find_all(doc, ".//*[name()='z:DTSCORP_SPO']")) > 0){
                
                report_date <- substring(xml_attrs(xml_children(xml_parent(doc)))[[1]][["FID"]],1,10)
                report_date <- as.Date(report_date, "%Y-%m-%d")
                report_date <- format(report_date, "%d.%m.%Y")
                
              df_name <- data.frame()
              
              
              for (i in 1:length(xml_attrs(xml_children(xml_find_all(doc, ".//*[name()='z:DTSCORP_SPO']"))))){
                df_name <- rbind(df_name,xml_attrs(xml_children(xml_find_all(doc, ".//*[name()='z:DTSCORP_SPO']")))[[i]][["O_NAME"]])
              }
              df_name <- format(df_name, justify = "left")
              
              df_vl_stat <- data.frame()
              for (i in 1:length(xml_attrs(xml_children(xml_find_all(doc, ".//*[name()='z:DTSCORP_SPO']"))))){
                df_vl_stat <- rbind(df_vl_stat,xml_attrs(xml_children(xml_find_all(doc, ".//*[name()='z:DTSCORP_SPO']")))[[i]][["VL_STAT"]])
              }
              df_vl_stat <- format(df_vl_stat, justify = "right")
              
              owners <- cbind(report_date,df_name,df_vl_stat)
              
              names(owners)[1] <- "Дата"
              names(owners)[2] <- "Власник"
              names(owners)[3] <- "%"
              
              smida_owners <- rbind(owners, smida_owners)
              smida_owners <- smida_owners[order(smida_owners$Дата,smida_owners$'%', decreasing = TRUE), ]

              }
              
              ###########################
              
              
              if (nchar(as.character(xml_find_all(xml_find_all(doc, ".//*[name()='z:Fin-general']"), ".//*[name()='z:DTSBP73_A']"))) > 100){
                balact <- as.data.frame(xml_attrs(xml_child(xml_find_all(xml_find_all(doc, ".//*[name()='z:Fin-general']"), ".//*[name()='z:DTSBP73_A']"))))
                balpass <- as.data.frame(xml_attrs(xml_child(xml_find_all(xml_find_all(doc, ".//*[name()='z:Fin-general']"), ".//*[name()='z:DTSBP73_P']"))))
                finrez <- as.data.frame(xml_attrs(xml_child(xml_find_all(xml_find_all(doc, ".//*[name()='z:Fin-general']"), ".//*[name()='z:DTSFP73']"))))
                
                
                balact <- cbind(rownames(balact), data.frame(balact, row.names=NULL))  
                balpass <- cbind(rownames(balpass), data.frame(balpass, row.names=NULL))  
                finrez <- cbind(rownames(finrez), data.frame(finrez, row.names=NULL))  
                
                names(balact)[1] <- "ROW"
                names(balact)[2] <- "SUMM"
                names(balpass)[1] <- "ROW"
                names(balpass)[2] <- "SUMM"
                names(finrez)[1] <- "ROW"
                names(finrez)[2] <- "SUMM"
                
                bal <- rbind(balact,balpass)
                bal %>% filter(ROW %like% '_04') -> bal
                finrez %>% filter(ROW %like% '_03') -> finrez
                
                bal <- rbind(bal,finrez)
                date <- substr(xml_attrs(xml_child(xml_find_all(xml_find_all(doc, ".//*[name()='z:Fin-general']"), ".//*[name()='z:DTSBP73_A']")))[["DATE"]],1,10)
                #date <- as.Date(date, "%Y-%m-%d")
                #date <- format(date, "%d.%m.%Y") 
                
                finzvit <- cbind(bal,date)
                finzvit.final <- rbind(finzvit.final, finzvit)
                finzvit
                
              }}}
          
          
        }}}
    
    
    if (length(finzvit.final)!=0){
      mutate(finzvit.final, SUMM = as.numeric(gsub(",", ".", gsub("\\.", "", SUMM)))) -> finzvit.final
      finzvit <- reshape2::dcast(finzvit.final, ROW ~ date, value.var = "SUMM",fun.aggregate = sum)
      mutate(finzvit, ROW = substring(ROW,3,6)) -> finzvit
      finzvit<-finzvit[,order(colnames(finzvit),decreasing=TRUE)]
      
      #change number format
      for (c in 2:ncol(finzvit)){
        finzvit[,c] <- formatC(finzvit[,c], format="f", big.mark=" ", digits=0)
      }
      
      
      balance_aricles <- read.csv("BALANCE_ARTICLES.txt", sep = ";", header = T, encoding = '1251')
      finrez_aricles <- read.csv("FINREZ_ARTICLES.txt", sep = ";", header = T, encoding = '1251')
      
      balance <-merge(balance_aricles, finzvit, all.x=TRUE)
      balance <- balance[order(balance$id, decreasing = FALSE), ]
      balance <- balance %>% relocate(ROW, .after = ARTICLE)
      
      names(balance)[2] <- "Стаття"
      names(balance)[3] <- "Код рядка"
      
      finrez <-merge(finrez_aricles, finzvit, all.x=TRUE)
      finrez <- finrez[order(finrez$id, decreasing = FALSE), ]
      finrez <- finrez %>% relocate(ROW, .after = ARTICLE)
      
      names(finrez)[2] <- "Стаття"
      names(finrez)[3] <- "Код рядка"
      
      #View(balance)
      #View(finrez)
      
      #finzvitV$data <- rbind(balance, finrez)
      balanceV$data <- balance
      finrezV$data <- finrez
      
      if (length(smida_owners) > 0){
      smida_ownersV$data <- smida_owners
      }else{
        smida_ownersV$data <- "Не має даних"
      }
      
      if (length(smida_info) > 0){
      
      smida_info <- subset(smida_info, smida_info$DATE == max(smida_info$DATE))
      names(smida_info)[2] <- paste("Станом на", smida_info$DATE[1])
      smida_info <- smida_info[-1]
      rownames(smida_info) <- NULL
      smida_info <- format(smida_info, justify = "left")
      smida_infoV$data <- smida_info
      
      }else{
        smida_infoV$data <- "Не має даних"
      }
      
      if (length(smida_manrep) > 0){
      smida_manrep <- subset(smida_manrep, smida_manrep$DATE == max(smida_manrep$DATE))
      names(smida_manrep)[2] <- paste("Станом на", smida_manrep$DATE[1])
      smida_manrep <- smida_manrep[-1]
      rownames(smida_manrep) <- NULL
      smida_manrep <- format(smida_manrep, justify = "left")
      smida_manrepV$data <- smida_manrep
      }else{
        smida_manrepV$data  <- "Не має даних"
      }
      
      done <- doneV$done
      done <- "Пошук завершено!"
      doneV$done <- done
      shinyjs::html(id = "companyinfo1", doneV$done)
      
    }else{
      print("не має даних")
      doneV$done <- "не має даних"
      shinyjs::html(id = "companyinfo1", doneV$done)
    }
    
  })
  
  
  output$balance2 <- renderTable({
    
    balanceV$data

  })
#},bordered = F,striped = F,rownames = F, na = "", hover = T, spacing = c("xs"),  sanitize.text.function = function(x) x)
  
  output$finrez2 <- renderTable({
    # finrezV$data[is.na(finrezV$data)] = ""
    # 
    # for (r in 1:nrow(finrezV$data)){
    #   for (c in 4:ncol(finrezV$data)){
    #     
    #     if(finrezV$data[r,c] < 0){
    #       finrezV$data[r,c] <- paste0('<div style="background-color:red;text-align:right"><span>', finrezV$data[r,c], '</span></div>')
    #     }else{
    #       finrezV$data[r,c] <- paste0('<div style="text-align:right;"><span>', finrezV$data[r,c], '</span></div>')
    #     }
    #   }z
    # }
    
    finrezV$data
  })
  
  output$founders2 <- renderTable({
    smida_ownersV$data
  })
  
  output$companyinfo2<- renderPrint({
    smida_infoV$data
  })
  
  output$manrep<- renderPrint({
    smida_manrepV$data
  })
  
  output$distTable <- renderTable({
    finzvitV$data
  })
  
  #
  #
  #
  #
  #
  #
  #
  #
  #
  #
  #
  
}

# Run the application 
shinyApp(ui = ui, server = server)
