library(shiny)

shinyUI
(
  fluidPage
  (
  
    #application title
    titlePanel(HTML("<h1><center><font size=20>Data Analysis of Insurance Data</font></h1>")),
    sidebarLayout
    (
      #position="right",
      #sidebar panel take input from user
      sidebarPanel(h3("Prediction of your Medical Costs"),
      textInput("age1","Enter age","30"),
      radioButtons("sex1","Select Gender",list("male","female"),"male"),
      textInput("bmi1","Enter bmi","30.25"),
      sliderInput("children1","Select number of children",min =0,max=5,value = 0 ),
      radioButtons("smoker1","Do you Smoke?",list("yes","no"),"yes"),
      selectInput("region1","Select Region",c("northeast","northwest","southeast","southwest"),"northeast"),
      submitButton("Predict!"),
      
      h3(textOutput("pred11"))),
      
      #mainPanel here we display output
      mainPanel
      (
        h2("Description of Dataset"),
        h3("Details of Columns - "),
        h4("Age: Age of primary beneficiary"),
        h4("Gender: insurance contractor gender, female, male"),
        h4("BMI: Body mass index, providing an understanding of body"),
        h4("Children: Number of children covered by health insurance "),
        h4("Smoker: Smoking"),
        h4("Region: the beneficiary's residential area in the US"),
        h4("Charges: Individual medical costs billed by health insurance",sep="</br></br>"),
        
        #h3(textOutput("pred11")),
      
        h2("Summary of Data"),
        tabsetPanel
        (
          type="tab",
          tabPanel("Age",tableOutput("summaryage"), plotOutput("histage")),
          tabPanel("Gender",tableOutput("summarysex"), plotOutput("histsex")),
          tabPanel("BMI",tableOutput("summarybmi"), plotOutput("histbmi")),
          tabPanel("Children",tableOutput("summarychildren"), plotOutput("histchildren")),
          tabPanel("Smoke",tableOutput("summarysmoke"), plotOutput("histsmoker")),
          tabPanel("Region",tableOutput("summaryregion"), plotOutput("histregion")),
          tabPanel("Amount",tableOutput("summaryamount"), plotOutput("histcharges"))
        )
        
        #plotOutput("myhist")
      )
    )
  )
)