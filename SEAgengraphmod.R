

SEAgengraphmodUI <- function(id) {
  ns <- NS(id)
  
  tagList(
  withSpinner(plotlyOutput(ns("graphplace")))
  )
}
  

SEAgengraphmodServer <- function(id, graphdata) {
  moduleServer(
    id,
    function(input, output, session) {
   
      
      yaxislab <-reactive({
        if(grepl("%", graphdata()$ylab[1], fixed=TRUE)) {
          c("%")
        }else {
          c("#")}

      })
      
     
      
output$graphplace <- renderPlotly({
      
        
    graphobj<-ggplot(data=graphdata(), 
                 aes(x=academic_year, 
                     y=yaxis, 
                     fill=primary_disagg_subgroup
                      )) +
            facet_wrap(~gender_disagg_subgroup)+
            theme(axis.title.x=element_blank())+
            ylab(yaxislab()) +
            scale_x_continuous(breaks=seq(2012, 2021, 1))+
            guides(fill = guide_legend(title = NULL)) +
            theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1))+
            geom_bar(stat="identity", 
                     position="dodge")+
            geom_text(aes(label = dilab),
                      vjust = -0.5, position=position_dodge(width=0.9), size=4)+
            aes(text = paste(academic_year, 
                             "<br>",
                             gender_disagg_subgroup, 
                             "<br>",
                             primary_disagg_subgroup, 
                             "<br>", 
                             ylab,
                             ditextlab))+
            scale_fill_viridis_d()
          
    ggply <- ggplotly(graphobj, tooltip=c("text")) %>% 
      config(displayModeBar = F) #%>%
    #layout(hovermode = "x")
    
    
    ggply
  
        
        
      }) 
      
    }
  )
}