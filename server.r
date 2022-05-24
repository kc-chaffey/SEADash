
server<-function(input,output){
 
output$SEAdisaggsview <- renderUI({  
 req(input$SEAgroup!="Gender")
   tabBox(title="Intersectionality Output", width=12, id="SEAgentabbox", 
         tabPanel(title="Outcomes", value="outcomes", 
                  SEAgengraphmodUI("SEAgengraph")), 
         tabPanel(title="Equity Number", value="eqnumbers", 
                  SEAgengraphmodUI("SEAgennumbergraph")),
         tabPanel(title="PPG-1", value="ppg", 
                  SEAgengraphmodUI("SEAgenppg")), 
         tabPanel(title="Data Table", value="datatable", 
                  DTOutput("SEAgendatatable"))
  )
})
  
  SEAreactive <- reactive({SEAdata %>%
                      mutate(genderppg=case_when(
                      !is.na(gender_ppg_py) ~ gender_ppg_py, 
                      !is.na(gender_ppg_pn) ~ gender_ppg_pn
                      ))
                          }) 
  
  SEAfiltered <- reactive({
    SEAreactive() %>% 
      filter(gender_disagg_subgroup=="Overall") %>% 
      filter(metric_description==input$SEAmetric) %>% 
      filter(primary_disagg==input$SEAgroup) %>% 
      filter(academic_year > (max(academic_year) - as.numeric(input$SEAtime))) %>%
      mutate(outperc=subgroup_outcome_rate*100) %>% 
      mutate(dilab = case_when(
        primary_di_observed_y == "Y" ~ "*",
        TRUE ~ ""
      )
      ) %>%
      mutate(ditextlab=case_when(
        primary_di_observed_y=="Y" ~ "DI Observed", 
        TRUE ~ "" 
      ))%>%
      mutate(outperlab=percent(subgroup_outcome_rate, accuracy=0.1)) %>%
      mutate(ppgperc=primary_ppg*100) %>% 
      mutate(ppgperlab=percent(primary_ppg, accuracy=0.1 )) 
  })
  
  SEAgraphout <- reactive({
    if(input$SEAtabbox=="outcomes") {
      SEAfiltered() %>% 
        mutate(yaxis=outperc, ylab=outperlab)
    } else if (input$SEAtabbox=="eqnumbers") {
      SEAfiltered() %>% 
        mutate(yaxis=primary_full_equity_number, 
               ylab=primary_full_equity_number)  
    } else if (input$SEAtabbox=="ppg") {
      SEAfiltered() %>% 
        mutate(yaxis=ppgperc, ylab=ppgperlab)
    } else if (input$SEAtabbox=="datatable") { 
      SEAfiltered()
    }
    
  })
  
  SEAgenfiltered <- reactive({
    SEAreactive() %>% 
      filter(gender_disagg_subgroup!="Overall") %>% 
      filter(metric_description==input$SEAmetric) %>% 
      filter(primary_disagg==input$SEAgroup) %>% 
      filter(academic_year > (max(academic_year) - as.numeric(input$SEAtime))) %>%
      mutate(outperc=subgroup_outcome_rate*100) %>% 
      mutate(dilab = case_when(
        gender_intersectional_di_observed_y == "Y" ~ "*",
        TRUE ~ ""
      )
      ) %>%
      mutate(ditextlab=case_when(
        gender_intersectional_di_observed_y=="Y" ~ "DI Observed", 
        TRUE ~ "" 
      ))%>%
      mutate(outperlab=percent(subgroup_outcome_rate, accuracy=0.1)) %>%
      mutate(ppgperc=genderppg*100) %>% 
      mutate(ppgperlab=percent(genderppg, accuracy=0.1 ))
  })

  SEAgengraphout <- reactive({
    if(input$SEAgentabbox=="outcomes") {
      SEAgenfiltered() %>% 
      mutate(yaxis=outperc, ylab=outperlab)
    } else if (input$SEAgentabbox=="eqnumbers") {
      SEAgenfiltered() %>% 
      mutate(yaxis=gender_intersectional_full_equity_number, 
             ylab=gender_intersectional_full_equity_number)  
    } else if (input$SEAgentabbox=="ppg") {
      SEAgenfiltered() %>% 
        mutate(yaxis=ppgperc, ylab=ppgperlab)
    } else if (input$SEAgentabbox=="datatable") { 
      SEAgenfiltered()
      }
      
  })
  

  
  SEAtabledata <- reactive({
    SEAgraphout() %>% 
    select(academic_year, metric_description, primary_disagg_subgroup,
           subgroup_outcome_rate, primary_ppg, primary_di_observed_y, primary_full_equity_number) %>% 
    rename("Academic Year"= academic_year,
           "Metric" = metric_description, 
           "Student Group" = primary_disagg_subgroup, 
           "Group Outcome Rate" = subgroup_outcome_rate, 
           "PPG-1 Measurement" = primary_ppg, 
           "DI Observed" = primary_di_observed_y, 
           "Equity Number" = primary_full_equity_number
           )  
  })
  
  output$SEAdatatable <- renderDT({
    datatable(SEAtabledata(),
              rownames = FALSE) %>%  
      formatPercentage(c("Group Outcome Rate", "PPG-1 Measurement"), 1)
    
  })
  
  SEAgentabledata <- reactive({
    SEAgengraphout() %>% 
     select(academic_year, metric_description, primary_disagg_subgroup, gender_disagg_subgroup,
           subgroup_outcome_rate, genderppg, gender_intersectional_di_observed_y, 
            gender_intersectional_full_equity_number) %>% 
      rename("Academic Year"= academic_year,
             "Metric" = metric_description,
             "Student Group" = primary_disagg_subgroup,
             "Gender Group" = gender_disagg_subgroup,
             "Group Outcome Rate" = subgroup_outcome_rate,
             "PPG-1 Measurement" = genderppg,
             "Intersectional DI Observed" = gender_intersectional_di_observed_y,
             "Intersectional Equity Number" = gender_intersectional_full_equity_number
      )
  })
  
  output$SEAgendatatable <- renderDT({
    datatable(SEAgentabledata(),
              rownames = FALSE) %>%  
      formatPercentage(c("Group Outcome Rate", "PPG-1 Measurement"), 1)
    
  })
  
  
  SEAgengraphmodServer(id="SEAgengraph", graphdata=SEAgengraphout)
  SEAgengraphmodServer(id="SEAgennumbergraph", graphdata=SEAgengraphout)
  SEAgengraphmodServer(id="SEAgenppg", graphdata=SEAgengraphout)
  
  SEAgraphmodServer(id="SEAgraph", graphdata=SEAgraphout)
  SEAgraphmodServer(id="SEAnumbergraph", graphdata=SEAgraphout)
  SEAgraphmodServer(id="SEAppg", graphdata=SEAgraphout)
  
  
}
