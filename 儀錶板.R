library(shiny)
library("ggplot2")



ui <- fluidPage(
  (title=h2("data : iris", align="center")),
  sidebarLayout(
    sidebarPanel(
       h4("主成份分析"),
       radioButtons("A", "x軸主成份",
                     c("PC1" = "PC1",
                       "PC2" = "PC2",
                       "PC3" = "PC3",
                       "PC4" = "PC4"),
                     selected="PC1"),
       radioButtons("B", "y軸主成份",
                     c("PC1" = "PC1",
                       "PC2" = "PC2",
                       "PC3" = "PC3",
                     "PC4" = "PC4"),
                    selected="PC2"),
       width = 2),
  mainPanel(plotOutput("C"))
 ),
 sidebarLayout(
   sidebarPanel(
     h4("對應分析"),
     radioButtons("a", "x軸維度",
                  c("Dim1" = "Dim1",
                    "Dim2" = "Dim2",
                    "Dim3" = "Dim3"),
                  selected="Dim1"),
     radioButtons("b", "y軸維度",
                  c("Dim1" = "Dim1",
                    "Dim2" = "Dim2",
                    "Dim3" = "Dim3"),
                  selected="Dim2"),
     width = 2),
   mainPanel(plotOutput("D"))
 ) 
)


server <- function(input, output) {
  
  output$C <- renderPlot({
    PC_x <- switch(input$A,
                   PC1 = 1,
                   PC2 = 2,
                   PC3 = 3,
                   PC4 = 4
                   )
    PC_y <- switch(input$B,
                   PC1 = 1,
                   PC2 = 2,
                   PC3 = 3,
                   PC4 = 4
                   )
    data(iris)
    # log transform 
    log.ir <- log(iris[, 1:4])
    ir.species <- iris[, 5]
    # apply PCA - scale. = TRUE is highly advisable, but default is FALSE.
    ir.pca <- prcomp(log.ir,center = TRUE, scale. = TRUE)
    plots=data.frame(ir.pca$x,Species=iris$Species)
    percentage<-round(ir.pca$sdev / sum(ir.pca$sdev) * 100,2)
    percentage<-paste(colnames(plots),"(",paste(as.character(percentage), "%", ")", sep=""))
   
    p=ggplot(plots,aes(x=plots[,PC_x],y=plots[,PC_y],color=Species))+geom_point()+
      
      xlab(percentage[PC_x])+ylab(percentage[PC_y])+
      
      labs(title="PCA",subtitle = paste("principle component :",colnames(plots)[PC_x],"",colnames(plots)[PC_y]))+
      
      theme(plot.title=element_text(hjust = 0.5,face="bold"),
            plot.subtitle=element_text(hjust = 0.5,face="bold"),
            legend.position="top")+
      
      guides(col=guide_legend(title=NULL))+
      
      stat_ellipse(level = 0.95,show.legend=F,
                   geom="polygon",aes(fill = Species),alpha=0.4)
    print(p)
    
  })
  
  
  output$D <- renderPlot({
    Dim_x <- switch(input$a,
                    Dim1 = 1,
                    Dim2 = 2,
                    Dim3 = 3
    )
    Dim_y <- switch(input$b,
                    Dim1 = 1,
                    Dim2 = 2,
                    Dim3 = 3
    )
    load("data2.Rdata")
  
    plot2=data.frame(data2[1:150,],Species=iris$Species)
    q=ggplot(plot2,aes(x=plot2[,Dim_x],y=plot2[,Dim_y],color=Species))+geom_point()+
      
      xlab(colnames(plot2)[Dim_x])+ylab(colnames(plot2)[Dim_y])+
      
      xlim(-3,3)+
      
      labs(title="CA")+
      
      theme(plot.title=element_text(hjust = 0.5,face="bold"),
            legend.position="top")+
      
      guides(col=guide_legend(title=NULL))+
      
      stat_ellipse(level = 0.95,show.legend=F,
                   geom="polygon",aes(fill = Species),alpha=0.4)
    
    for(i in 1:4){
      q=q+geom_point(x=data2[150+i,Dim_x],y=data2[150+i,Dim_y],pch=17,col=1,cex=2)+
        geom_text(x=data2[150+i,Dim_x]-0.15,y=data2[150+i,Dim_y]-0.15,col=1,size=3,
                  label=row.names(data2)[150+i])
    } 
    print(q)
    
  })
  
}

shinyApp(ui, server)