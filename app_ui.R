x_input <- radioButtons(
  inputId = "x",
  label = h3("Choose a x-axis "),
  choices = list("Gender" = "sex",
                 "Age" = "age_group"),
  selected = "sex"
)

race_input <- selectInput(
  inputId = "race",
  label = h3("Select a race and ethnicity group"),
  choices = race_list,
  selected = "Black, Non-Hispanic"
)

page_two <- tabPanel(
  "Plot_race",
  titlePanel("Choose Your Interest"),
  sidebarLayout(
    sidebarPanel(
      race_input,
      x_input,
    ),
    mainPanel(
      plotlyOutput(outputId = "plot"),
      tags$br(),
      p("By looking at this chart, we would be able to examine the",
        em("hospital treatment rate differences between racial and ethnicity groups and also 
        within racial and ethnicity groups",
        "and have a general idea of how the hierarchy of power combine with 
        different characteristics of a person. Many different characteristics 
        make up who we are, and",
        em("intersectionality"),
        "is something we should consider. "),
      p("For example, when comparing White and Hispanic people, we can see that 
      the hospital treatment rate for white people is higher, and within 
      Hispanic people, female tend to have a lower hospital treatment rate 
      compared to men. Furthermore, when you compare the hospital treatment rate
      among differences between racial and ethnicity groups by gender, female’s 
      hospital treatment rate for all racial and ethnicity group appear lower 
      than male.")
      )
    )
  )
)



ui <- navbarPage(
  "Team Jet Lag Final Deliverable on Covid",
  page_two)