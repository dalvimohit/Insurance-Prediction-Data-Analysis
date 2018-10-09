library(shiny)
library(plotrix)
library(plotly)

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
    
    #pie chart
    male_smoker=nrow(subset(info, sex=="male" & smoker=="yes"))
    male_nonsmoker=nrow(subset(info, sex=="male" & smoker=="no"))
    male_smoker_nonsmoker=c(male_smoker,male_nonsmoker)
    male_piepercent<- round(100*male_smoker_nonsmoker/sum(male_smoker_nonsmoker), 1)
    female_smoker=nrow(subset(info, sex=="female" & smoker=="yes"))
    female_nonsmoker=nrow(subset(info, sex=="female" & smoker=="no"))
    female_smoker_nonsmoker=c(female_smoker,female_nonsmoker)
    female_piepercent<- round(100*female_smoker_nonsmoker/sum(female_smoker_nonsmoker), 1)
    labels1=c("Smoker","Non-Smoker")
    
    
    output$pie_smoke_male <- renderPlot({
      pie(male_smoker_nonsmoker,labels=male_piepercent,main="Male Smokers and Non-Smokers",init.angle = 90,clockwise = TRUE,radius=0.7,col=c("red","green"))
      legend("topright",labels1,cex=0.8,fill=c("red","green"))
    })
    output$pie_smoke_female <- renderPlot({
      pie(female_smoker_nonsmoker,labels=female_piepercent,main="Female Smokers and Non-Smokers",init.angle = 90,clockwise = TRUE,radius=0.7,col=c("red","green"))
      legend("topright",labels1,cex=0.8,fill=c("red","green"))
    })
    
    children0=nrow(subset(info,children==0))
    children1=nrow(subset(info,children==1))
    children2=nrow(subset(info,children==2))
    children3=nrow(subset(info,children==3))
    children4=nrow(subset(info,children==4))
    children5=nrow(subset(info,children==5))
    children_count <- c(children0,children1,children2,children3,children4,children5)
    labels2 <- c("Zero","One","Two","Three","Four","Five")
    children_piepercent<- round(100*children_count/sum(children_count), 1)
    output$pie_children <- renderPlot({
      pie3D(children_count,explode=0.1,labels=children_piepercent,main="Pie chart of No of Children",col=rainbow(6),start=2)
      legend("topright",labels2,cex=0.8,fill=rainbow(6))
    })
    
    output$age_bmi_plot <- renderPlot({plot(x=info$age,y=info$bmi,xlab="Age",ylab="BMI",main="Age vs BMI",col="green")})
    
    #no of smokers vs age
    #only_smoker <- subset(info,smoker=="yes")
    #output$smoker_age <- renderPlot({plot(x=only_smoker$age,type="h")})
  }
)
