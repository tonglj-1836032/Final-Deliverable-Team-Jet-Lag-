tt<-tabPanel("desc",

      h1(textOutput("desc"))


         )

tt1<-tabPanel("line",sidebarLayout(
  
  sidebarPanel(
    selectInput("pp",
                "Mortality rates (%):",
                c("All"=0,
                  ">0"=1
                )
                
                
    )
  ),
  
  mainPanel(
    {
      
      
      plotlyOutput("line")

    }
  )

)


)
tt2<-tabPanel("desc",
             
             h2(textOutput("text"))
             
             
)
