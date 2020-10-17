library(shiny)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(h2o)
library(forecast)

# read file
setwd("/home/tao103")
getwd()
d<-read.table(file="Border_Crossing_Entry_Data.csv",header=T,sep=",")
origin<-read.table(file="Border_Crossing_Entry_Data.csv",header=T,sep=",")
# set type
names(d)<-c("Index","PortName","State","PortCode","Border","Date","Measure","Value")
d$PortName<-as.factor(d$PortName)
d$State<-as.factor(d$State)
d$PortCode<-as.factor(d$PortCode)
d$Border<-as.factor(d$Border)
d$Measure<-as.factor(d$Measure)
d$Value<-as.numeric(d$Value)
d$Date<-as.Date(d$Date,format="%m/%d/%Y")
d$Year<-as.factor(substring(d$Date,1,4))
d$Month<-as.factor(substring(d$Date,6,7))
d1<-d
d1$Date<-as.factor(d1$Date)
d2<-d
names(d2)<-c("Indexs","PortNames","States","PortCodes","Borders","Dates","Measures","Values","Years","Months")

h2o.init(nthreads=12, max_mem_size="64g")
df<-as.h2o(d2)
y <- "Values"                                # target variable to learn
x <- setdiff(names(df), y)                # feature variables are all other columns
parts <- h2o.splitFrame(df, 0.8, seed=99) # randomly partition data into 80/20
train <- parts[[1]]                         # random set of training obs
test <- parts[[2]]    

aml <- h2o.automl(x, y, train
                  , max_runtime_secs = 300     # max time to run in seconds
                  , max_models = 10            # max num of models
                  , seed = 123                # for reproducibility.
)


pred <- as.data.frame(h2o.predict(aml,df))  # predict(aml, test) also works
d2<- data.frame(cbind(d2,pred))


# Define UI for application that draws a histogram
ui <- fluidPage(
    headerPanel('Border Crossing'),
    navbarPage(
        'NavBar',
        tabPanel('Historical Data',
                 sidebarPanel(
                     selectInput('PortName', 'Choose a Port Name', levels(d$PortName)),
                     selectInput('Measure', 'Choose a Measure', levels(d$Measure))
                 ),  
                 actionButton(inputId = 'clicks', 
                              label = "GO!"),
                 mainPanel(
                     plotOutput('plot1')
                 )),
        
        tabPanel('Measures',
                 sidebarPanel(
                     selectInput('Year', 'Choose a Year', levels(d1$Year))
                 ),  
                 actionButton(inputId = "clicks2", 
                              label = "GO!"),
                 mainPanel(
                     plotOutput('plot2')
                 ) ),
        
        tabPanel('Predict',
                 sidebarPanel(
                     selectInput('PortNames', 'Choose a Port Name', levels(d2$PortNames)),
                     selectInput('Measures', 'Choose a Measure', levels(d2$Measures))
                 ),  
                 actionButton(inputId = "clicks3", 
                              label = "Predict"),
                 mainPanel(
                     plotOutput('plot3')
                 )
                 
        ))
)


server <- function(input, output) {
    selectedData <- eventReactive(input$clicks,{
        d%>%filter(PortName==input$PortName&Measure==input$Measure)
    })
    output$plot1 <- renderPlot({
        p1<-ggplot(selectedData(),aes(x=Date,y=Value))+
            geom_line()+scale_x_date(date_labels="%Y",date_breaks = "1 year")
        p2<-ggplot(selectedData(),aes(x=Date,y=Value,col=Month))+
            geom_line()
        grid.arrange(p1,p2,nrow=2,ncol=1)
    })
    selectedData2 <- eventReactive(input$clicks2,{
        d1%>%filter(Year==input$Year)
    })
    output$plot2 <- renderPlot({
        ggplot(selectedData2(),aes(x = reorder(Measure, Value, sum), y = Value,fill=Border))+
            geom_bar(stat = "identity",position='dodge')+coord_flip()+labs(x="Measures")
    })
    selectedData3 <- eventReactive(input$clicks3,{
        d2%>%filter(PortNames==input$PortNames&Measures==input$Measures)
    })
    output$plot3 <- renderPlot({
        ggplot(selectedData3(),aes(x=Dates,y=Values,col='Red'))+
            geom_line()+
            geom_line(aes(x=Dates,y=predict,col='Blue'))+
            scale_x_date(date_labels="%Y",date_breaks = "1 year")+
            labs(color="Difference")+scale_color_manual(values=c("Blue","Red"),labels=c("Predictions","Actual"))
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)