library(shiny)

shinyServer
(

  function(input,output)
  {
    #data<-read.csv("insurance.csv")
    info<-read.csv("insurance.csv")  
    output$myage1 <- renderText(paste("Age = ",input$age1))
    output$mybmi1 <- renderText(paste("BMI = ",input$bmi1))
    output$mysex1 <- renderText(paste("Gender = ",input$sex1))
    output$myregion1 <- renderText(paste("Region = ",input$region1))
    output$mysmoker1 <- renderText(paste("smoker = ",input$smoker1))
    output$mychildren1 <- renderText(paste("Number of children = ",input$children1))
    
    #prediction
    age1 <- reactive(as.integer(input$age1))
    bmi1 <- reactive(as.double(input$bmi1))
    reg <- lm(charges ~ age + sex + bmi + children + smoker + region , data=info)
    #new2<-reactive(data.frame(age=50,sex="male",bmi=29.26,children=2,smoker="yes",region="southeast"))
    new<-reactive(data.frame(age=age1(),sex=input$sex1,bmi=bmi1(),children=input$children1,smoker=input$smoker1,region=input$region1))
    pred1<-reactive(predict(reg,new()))
    output$pred11 <- renderText(paste("Approximate Amount = ",round(pred1(),0),"$"))
    
    #summary
    sage <- info[1]
    output$summaryage <- renderText(paste("", summary(sage),sep="<br/>"))
    ssex <- info[2]
    output$summarysex <- renderText(paste("", summary(ssex),sep="<br/>"))
    sbmi <- info[3]
    output$summarybmi <- renderText(paste("", summary(sbmi),sep="<br/>"))
    schildren <- info[4]
    output$summarychildren <- renderText(paste("", summary(schildren),sep="<br/>"))
    ssmoker <- info[5]
    output$summarysmoke <- renderText(paste("", summary(ssmoker),sep="<br/>"))
    sreg <- info[6]
    output$summaryregion <- renderText(paste("", summary(sreg),sep="<br/>"))
    samt <- info[7]
    output$summaryamount <- renderText(paste("", summary(samt),sep="<br/>"))
     
    #graphs
    output$histage <- renderPlot({hist(info$age,main="Frequency of Age",xlab="Age",col=c("red","blue"),border="black")})
    output$histsex <- renderPlot({barplot(table(info$sex),main = "Frequency of Gender",ylab = "Frequency",xlab = "Gender",col=c("red","blue"),border="black",xlim=c(0,7))})
    output$histbmi <- renderPlot({hist(as.integer(info$bmi),main="Frequency of BMI",xlab="BMI",col=c("red","blue"),border="black")})
    output$histchildren <- renderPlot({barplot(table(info$children),main = "Frequency of Children",ylab = "Frequency",xlab = "No of Children",col=c("red","blue"),border="black")})
    output$histsmoker <- renderPlot({barplot(table(info$smoker),main = "Frequency of Smokres",ylab = "Frequency",xlab = "Smoker or not?",col=c("red","blue"),border="black",xlim=c(0,7))})
    output$histregion <- renderPlot({barplot(table(info$region),main = "Frequency of Region",ylab = "Frequency",xlab = "Region of People",col=c("red","blue"),border="black")})
    output$histcharges <- renderPlot({hist(info$charges,main="Frequency of Charges",xlab="Medical Charges",col=c("red","blue"),border="black")})
  }
)