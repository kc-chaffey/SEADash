
ui <- dashboardPage(
  # md = TRUE,
  # skin = "blue",
  header=dashboardHeader(),
  sidebar=dashboardSidebar(width="0px"),
  body=dashboardBody(
    fluidRow(
      column(width=3, style = "padding:0px;",
             box(title="Description", width=12, 
                 p("This dashboard summarizes 2012-2021 student and equity achievement data for the college. Columns marked with an asterisk indicate observed disproportionate impact.")
                 ),
             box(title="Output Controls", width=12,    
              pickerInput(
                   inputId = "SEAmetric",
                   label = "Metric:",
                   choices = SEAmetriclist
                   
                 ), 
                 pickerInput(
                   inputId = "SEAgroup",
                   label = "Group:",
                   choices = SEAprimarylist
                   
                 ), 
                pickerInput(
                inputId = "SEAtime",
                label = "Years included:",
                choices = c("All"=11, "Last 2 Years"=2, "Last 5 years"=5), 
                selected=5
                
              )
                )
             
             ), 
      column(width=9, style = "padding:0px;",
             tabBox(title="Primary Group Output", width=12, id="SEAtabbox", 
                tabPanel(title="Outcomes", value="outcomes", 
                         SEAgraphmodUI("SEAgraph")), 
                tabPanel(title="Equity Number", value="eqnumbers", 
                         SEAgraphmodUI("SEAnumbergraph")),
                tabPanel(title="PPG-1", value="ppg", 
                         SEAgraphmodUI("SEAppg")), 
                tabPanel(title="Data Table", value="datatable", 
                         DTOutput("SEAdatatable"))
             )
             )

    ), #end row
    fluidRow(
      uiOutput("SEAdisaggsview")
    )#end row
  ) #end dashboardBody
) #end dashboardPage


