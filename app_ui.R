library(shiny)
library(ggplot2)
library(RColorBrewer)
library(plotly)

# This part is for age group cases
x_choices <- c("Male", "Female", "Other")
color_choice <- c("Paired", "Dark2")

color_input <- selectInput(
  inputId = "color_input",
  label = "Choose a color",
  choices = color_choice,
  selected = "Paired"
)

gender_input <- selectInput(
  inputId = "gender_input",
  label = "Choose a gender",
  choices = x_choices,
  selected = "Male"
)

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

# Introductory page.
intro_page <- tabPanel(
  "Introduction", # navbar tab label.
  
  titlePanel("Introduction"),
  
  # Topic paragraphs.
  p("Coronavirus is a hot topic this year, people around the world are 
  hugely affected by it. As international students, we are facing many 
  difficulties due to coronavirus. It affected our lives in so many ways. 
  And we believed that others have been through the same thing like us."
  ),
  
  p("Thus, we decided to engage in this field and create a website that could help 
    people in this complex situation. We hope that our design could help people 
    gain insights and reduce their stress, informing the public of the effect of
    COVID-19 on education, transportation, and work. We also want to go further 
    by adding information about where you could get a covid test and which 
    hospital you could go to for help."
    ),
  
  p(
    "The original data was downloaded from ",
    a("U.S. Government's open data. ",
      href = "https://catalog.data.gov/dataset/
      covid-19-case-surveillance-public-use-data"
    ),
    "The dataframe's column names (cdc_report_dt, 
      Race and ethnicity (combined), hosp_yn, death_yn) 
      has been modified to be more human readable. 
      The dataframe was filtered to remove columns that were not used 
      (pos_spec_dt, onset_dt, icu_yn, medcond_yn).
      The dataframe was sorted in the ascending order of date.
      The dataframe was sliced to show first 10 rows.
      The table was included to show used dataset.
      The table reveals information such as column names that helps understand 
      the dataset better."
  ),
  
  tableOutput('dataset'),
)

# Summary takeaways page.
summary_page <- tabPanel(
  "Summary Takeaways", # navbar tab label.
  
  titlePanel("Summary Takeaways"),
  
  # Summary paragraphs.
  p("After looking at the data closely and generating aggregate summary of it, 
    we realized how much impact COVID-19 had made to our society. 
    So many people from different ages groups and races are heavily affected by 
    it. For example, hospital treatment among all race and ethnicity is 15%, 
    which is rather low. This may be a sign indicating how compacted hospitals 
    are and the inability of hospitals for taking in all patients in need. 
    Among all race and ethnicity groups, black and non-hispanic people have a 
    hospital treatment rate of 22%, which is the highest among all race and 
    ethnicity groups."
  ),
  
  p("As for the age factors, most of COVID-19 cases are confirmed at the age 
    group between 20 - 29 Years, which takes up about 20% of the total confirmed 
    population. On the other hand, the age group 0 - 9 Years has the smallest 
    percentage of confirmed cases that takes up only about 3%. Thus, we should 
    rise the alter to be aware of the harsh situation and do everything possible 
    to protect ourselves form getting infected, avoiding risky behaviors during 
    this sensitive period of time."
  ),
  
  p(
    "The dataframe was grouped and summarized by current status.
    The table was included to show differences between laboratory 
    confirmed/probable cases.
    The table reveals number of laboratory confirmed/probable cases."
  ),
  
  tableOutput('summary')
)



ui <- fluidPage(
  tabsetPanel(
    intro_page,
    tabPanel(
      "CO2 Emission Plot",
      sidebarLayout(
        sidebarPanel(
          titlePanel("Climate Change Data"),
          gender_input,
          color_input,
          h3("Graph Summary"),
          textOutput("age_case_p1"),
          textOutput("age_case_p2")
        ),
        mainPanel(
          plotlyOutput("bar", height = 700)
        )
      )
    ),
    tabPanel(
      "Team Jet Lag Final Deliverable on Covid",
      page_two
    ),
    tabPanel(
      "death rate for every age group on Covid",
      tt,
	  tt1,
	  tt2
    
  ),
  summary_page
  )
)
