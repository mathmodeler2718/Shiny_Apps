
##--------------------------------------------------------
# Matthew Anderson
# Final Exam
# December 14, 2018
##--------------------------------------------------------

#please change this directory to upload your data file, not general for any data set just the one assigned to us
final_df<-read.delim("C:/bios524/Final_exam/final.txt", stringsAsFactors=FALSE)

#running two functions to the global environment for the app, compare and compare2
compare<-function(x, c){
  
  x$binarypred<-ifelse(final_df$Biomarker>c,1,0)
  
  df<-as.matrix(table(x$Status, x$binarypred))
  
  sensitivity<-df[1,2]/sum(df[1,])
  specificity<-df[2,1]/sum(df[2,])
  PPV<-df[1,2]/sum(df[,2])
  NPV<-df[2,1]/sum(df[,1])
  sums<-sensitivity+specificity
  output<-cbind(c,sensitivity,specificity,PPV,NPV,sums)
  return(output)}

compare2<-function(x, alpha){
  
  x1<-final_df$Biomarker[1:100]
  x2<-final_df$Biomarker[101:200]
  
  
  t_test<-t.test(x1,x2,conf.level =alpha)
  
  
  output<-c(t_test$conf.int[1], t_test$conf.int[2])
  return(output)}

#
#begin the shiny process with the shiny library
library(shiny)

# Define UI for application 
ui <- fluidPage(
  
  # Application title
  titlePanel("Sensitivity and Specificity of Biomarker given C"),
  
  
  sidebarLayout(
    sidebarPanel(
      #here I am inputing cut off values, c1, c2, c3, c4, c5 to classify our data as diseased or healthy 
      numericInput("c1",
                   "Cut off Value, c1",
                   min=1,
                   max=6,
                   value = 2.5,
                   step=0.25),
      
      numericInput("c2",
                   "Cut off Value, c2",
                   min=1,
                   max=6,
                   value = 3,
                   step=0.25),
      
      numericInput("c3",
                   "Cut off Value, c3",
                   min=1,
                   max=6,
                   value = 3.5,
                   step=0.25),
      
      numericInput("c4",
                   "Cut off Value, c4",
                   min=1,
                   max=6,
                   value = 3.75,
                   step=0.25),
      
      numericInput("c5",
                   "Cut off Value, c5",
                   min=1,
                   max=6,
                   value = 4,
                   step=0.25),
      
      #Question 6
      #Here I am creating a confidence interval on the mean difference between the groups and observe the interval's behavior as we manipulate alpha
      numericInput("alpha1",
                   "alpha value",
                   min=0,
                   max=1,
                   value = .05,
                   step=0.05),
      
      numericInput("alpha2",
                   "alpha value",
                   min=0,
                   max=1,
                   value = .1,
                   step=0.05),
      
      
      numericInput("alpha3",
                   "alpha value",
                   min=0,
                   max=1,
                   value = .2,
                   step=0.05),
      
      numericInput("alpha4",
                   "alpha value",
                   min=0,
                   max=1,
                   value = .3,
                   step=0.05),
      
      numericInput("alpha5",
                   "alpha value",
                   min=0,
                   max=1,
                   value = .4,
                   step=0.05)
      
      
      
      
    ),
    
    # Comands for table and ROC Curve 
    mainPanel(
      tableOutput("Tables"), tableOutput("Tabless"), tableOutput("Tablesss"), tableOutput("Tablessss"), 
      tableOutput("Tablesssss"), plotOutput("Plot"), tableOutput("Tablea1"), tableOutput("Tablea2"), tableOutput("Tablea3"),
      tableOutput("Tablea4"),  tableOutput("Tablea5"), plotOutput("Plot2")
    )
  )
)

# Define server with input and output
server <- function(input, output) {
  
  #I am creating a table with sensitivity, specificity, PPV, NPV, and sum PPV and NPV for c1-c5 with my own function "compare"
  output$Tables <- renderTable( {
    
    
    c1<-input$c1
    
    
    final_df<-read.delim("C:/bios524/Final_exam/final.txt", stringsAsFactors=FALSE)
    
    compare<-function(x, c){
      
      x$binarypred<-ifelse(final_df$Biomarker>c,1,0)
      
      df<-as.matrix(table(x$Status, x$binarypred))
      
      sensitivity<-df[1,2]/sum(df[1,])
      specificity<-df[2,1]/sum(df[2,])
      PPV<-df[1,2]/sum(df[,2])
      NPV<-df[2,1]/sum(df[,1])
      sums<-sensitivity+specificity
      output<-cbind(c,sensitivity,specificity,PPV,NPV,sums)
      return(output)}
    
    compare(final_df, c1)
  })
  
  output$Tabless <- renderTable( {
    
    
    c2<-input$c2
    
    
    final_df<-read.delim("C:/bios524/Final_exam/final.txt", stringsAsFactors=FALSE)
    
    compare<-function(x, c){
      
      x$binarypred<-ifelse(final_df$Biomarker>c,1,0)
      
      df<-as.matrix(table(x$Status, x$binarypred))
      
      sensitivity<-df[1,2]/sum(df[1,])
      specificity<-df[2,1]/sum(df[2,])
      PPV<-df[1,2]/sum(df[,2])
      NPV<-df[2,1]/sum(df[,1])
      sums<-sensitivity+specificity
      output<-cbind(c,sensitivity,specificity,PPV,NPV,sums)
      return(output)}
    
    compare(final_df, c2)
  })
  
  
  output$Tablesss <- renderTable( {
    
    
    c3<-input$c3
    
    
    final_df<-read.delim("C:/bios524/Final_exam/final.txt", stringsAsFactors=FALSE)
    
    compare<-function(x, c){
      
      x$binarypred<-ifelse(final_df$Biomarker>c,1,0)
      
      df<-as.matrix(table(x$Status, x$binarypred))
      
      sensitivity<-df[1,2]/sum(df[1,])
      specificity<-df[2,1]/sum(df[2,])
      PPV<-df[1,2]/sum(df[,2])
      NPV<-df[2,1]/sum(df[,1])
      sums<-sensitivity+specificity
      output<-cbind(c,sensitivity,specificity,PPV,NPV,sums)
      return(output)}
    
    compare(final_df, c3)
  })
  
  output$Tablessss <- renderTable( {
    
    
    c4<-input$c4
    
    
    final_df<-read.delim("C:/bios524/Final_exam/final.txt", stringsAsFactors=FALSE)
    
    compare<-function(x, c){
      
      x$binarypred<-ifelse(final_df$Biomarker>c,1,0)
      
      df<-as.matrix(table(x$Status, x$binarypred))
      
      sensitivity<-df[1,2]/sum(df[1,])
      specificity<-df[2,1]/sum(df[2,])
      PPV<-df[1,2]/sum(df[,2])
      NPV<-df[2,1]/sum(df[,1])
      sums<-sensitivity+specificity
      output<-cbind(c,sensitivity,specificity,PPV,NPV,sums)
      return(output)}
    
    compare(final_df, c4)
  })
  
  output$Tablesssss <- renderTable( {
    
    
    c5<-input$c5
    
    
    final_df<-read.delim("C:/bios524/Final_exam/final.txt", stringsAsFactors=FALSE)
    
    compare<-function(x, c){
      
      x$binarypred<-ifelse(final_df$Biomarker>c,1,0)
      
      df<-as.matrix(table(x$Status, x$binarypred))
      
      sensitivity<-df[1,2]/sum(df[1,])
      specificity<-df[2,1]/sum(df[2,])
      PPV<-df[1,2]/sum(df[,2])
      NPV<-df[2,1]/sum(df[,1])
      sums<-sensitivity+specificity
      output<-cbind(c,sensitivity,specificity,PPV,NPV,sums)
      return(output)}
    
    compare(final_df, c5)
  })
  
  
  #ROC Plot
  output$Plot<- renderPlot( {
    
    matrix<-rbind(compare(final_df,input$c1),compare(final_df,input$c2),compare(final_df,input$c3),
                  compare(final_df,input$c4),compare(final_df,input$c5))
    
    plot(matrix[,3],matrix[,2],type="o",main="Specificity vs Sensitivity ROC Curve",xlab="Specificity", ylab="Sensitivity")
    
  })
  
  #for the open ended part of the project
  #I am creating a table with confidence intervals for the mean difference between healhty and diseased groups
  #for 5 different values of alpha  my own function "compare"
  output$Tablea1 <- renderTable( {
    
    
    alpha1<-input$alpha1
    
    
    final_df<-read.delim("C:/bios524/Final_exam/final.txt", stringsAsFactors=FALSE)
    
    compare2<-function(x, alpha){
      
      x1<-final_df$Biomarker[1:100]
      x2<-final_df$Biomarker[101:200]
      
      
      t_test<-t.test(x1,x2,conf.level =alpha)
      
      
      output<-c(t_test$conf.int[1], t_test$conf.int[2])
      return(output)}
    
    compare2(final_df, input$alpha1)
  })
  
  
  
  output$Tablea2 <- renderTable( {
    
    
    alpha2<-input$alpha2
    
    
    final_df<-read.delim("C:/bios524/Final_exam/final.txt", stringsAsFactors=FALSE)
    
    compare2<-function(x, alpha){
      
      x1<-final_df$Biomarker[1:100]
      x2<-final_df$Biomarker[101:200]
      
      
      t_test<-t.test(x1,x2,conf.level =alpha)
      
      
      output<-c(t_test$conf.int[1], t_test$conf.int[2])
      return(output)}
    
    compare2(final_df, input$alpha2)
  })
  
  
  output$Tablea3 <- renderTable( {
    
    
    alpha3<-input$alpha3
    
    
    final_df<-read.delim("C:/bios524/Final_exam/final.txt", stringsAsFactors=FALSE)
    
    compare2<-function(x, alpha){
      
      x1<-final_df$Biomarker[1:100]
      x2<-final_df$Biomarker[101:200]
      
      
      t_test<-t.test(x1,x2,conf.level =alpha)
      
      # x$binarypred<-ifelse(final_df$Biomarker>c,1,0)
      # 
      
      # 
      # sensitivity<-df[1,2]/sum(df[1,])
      # specificity<-df[2,1]/sum(df[2,])
      # PPV<-df[1,2]/sum(df[,2])
      # NPV<-df[2,1]/sum(df[,1])
      output<-c(t_test$conf.int[1], t_test$conf.int[2])
      return(output)}
    
    compare2(final_df, input$alpha3)
  })
  
  
  
  output$Tablea4 <- renderTable( {
    
    
    alpha4<-input$alpha4
    
    
    final_df<-read.delim("C:/bios524/Final_exam/final.txt", stringsAsFactors=FALSE)
    
    compare2<-function(x, alpha){
      
      x1<-final_df$Biomarker[1:100]
      x2<-final_df$Biomarker[101:200]
      
      
      t_test<-t.test(x1,x2,conf.level =alpha)
      
      # x$binarypred<-ifelse(final_df$Biomarker>c,1,0)
      # 
      
      # 
      # sensitivity<-df[1,2]/sum(df[1,])
      # specificity<-df[2,1]/sum(df[2,])
      # PPV<-df[1,2]/sum(df[,2])
      # NPV<-df[2,1]/sum(df[,1])
      output<-c(t_test$conf.int[1], t_test$conf.int[2])
      return(output)}
    
    compare2(final_df, input$alpha4)
  })
  
  
  output$Tablea5 <- renderTable( {
    
    
    alpha5<-input$alpha5
    
    
    final_df<-read.delim("C:/bios524/Final_exam/final.txt", stringsAsFactors=FALSE)
    
    compare2<-function(x, alpha){
      
      x1<-final_df$Biomarker[1:100]
      x2<-final_df$Biomarker[101:200]
      
      
      t_test<-t.test(x1,x2,conf.level =alpha)
      
      # x$binarypred<-ifelse(final_df$Biomarker>c,1,0)
      # 
      
      # 
      # sensitivity<-df[1,2]/sum(df[1,])
      # specificity<-df[2,1]/sum(df[2,])
      # PPV<-df[1,2]/sum(df[,2])
      # NPV<-df[2,1]/sum(df[,1])
      output<-c(t_test$conf.int[1], t_test$conf.int[2])
      return(output)}
    
    compare2(final_df, input$alpha5)
  })
  
  
  #cordinate plots of the confidence intervals lower and upper bound for each value of alpha
  output$Plot2<- renderPlot( {
    
    matrix<-rbind(compare2(final_df,input$alpha1), compare2(final_df,input$alpha2), compare2(final_df,input$alpha3),
                  compare2(final_df,input$alpha4), compare2(final_df,input$alpha5))
    
    plot(matrix[,1],matrix[,2],type="o",main="confidence interval of the mean difference between Healthy and Diseased"
         ,xlab="Lower Bound", ylab="Upper Bound")
    
  })
  
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

#I notice that changed the values of alpha keeps the coordinate's of the lower and upper bound for the confidence interval of difference in means
#is linear where as the ROC Curve is not when changing cut off values
#the mean difference does not contain 0 so there is probably a difference in the means


