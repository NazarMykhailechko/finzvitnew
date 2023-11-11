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


user_base <- data.frame(
  user = c("finzvit", "user2"),
  password = c("accord", "pass2"), 
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
          }, 5000)
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
          
          tabsetPanel(type = "tab",   
          
                      tabPanel("Баланс", tableOutput("balance")),  
                      tabPanel("Звіт про фінансові результати", tableOutput("finrez")),
                      tabPanel("Інфо", verbatimTextOutput("companyinfo")))

        )
    )
)  %>% shinyjs::hidden()

)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
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
    

    
    
    files <- read.table("files.txt",sep = ";", header = T)
    
    jsonpath <- as.character(subset(files,as.numeric(minokpo) <= as.numeric(input$okpo) & 
                                          as.numeric(maxokpo) >= as.numeric(input$okpo), select=c("filepath")))

    jsondata <- jsonlite::fromJSON(jsonpath)
    
    companyName <- as.character(subset(jsondata,TIN == input$okpo, select=c("FN")))
    output$company <- renderText(companyName)
    
    
    if (companyName == "character(0)") {
      output$company <- renderText("Про дану компанію Інформація відсутня :(")
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
    
    
    #output$downloadData <- downloadHandler(
      
     # filename = function() {paste0(companyName, ".xlsx")},
    #  content = function(file) {write_xlsx(balance, path = file)}
      
    #)
    
    output$downloadData <- downloadHandler(
      
      filename = function() {paste0(companyName, ".xlsx")},
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
         saveWorkbook(wb, file, overwrite = TRUE)
        
        
        }
      
    )
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
