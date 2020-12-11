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

linx_type <- radioButtons(
  inputId = "line_type",
  label = h3("Choose a gener "),
  choices = list("Male" = "Male",
                 "Female" = "Female",
                 "All" = "All"),
  selected = "All"
)
page_two <- tabPanel(
  "Plot_race",
  titlePanel("Choose Your Interest"),
  sidebarLayout(
    sidebarPanel(
      uiOutput('race_list'),
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


tt1<-tabPanel("line",sidebarLayout(
  sidebarPanel(
    selectInput("pp",
                "Mortality rates (%):",
                c("All"=0,
                  ">0"=1
                )
    ),
    linx_type ,
    h3(textOutput("text"))
  ),
  
  mainPanel(
    {
      plotlyOutput("line")
    }
  )
)
)

age_gender_tab <- tabPanel(
  "Confirmed Cases by Age and Gender",
  sidebarLayout(
    sidebarPanel(
      h3("Percentage of Confirmed Cases by Age Group and Gender"),
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
)

# Introductory page.
intro_page <- tabPanel(
  "Introduction", # navbar tab label.
  
  titlePanel("Introduction"),
  
  # Covid Image.
  img(src = 'covid.png', height = '100px', width = '100px'),
  # Multiculture Image.
  img(src = 'multiculture.jpg', height = '100px', width = '100px'),
  
  # Topic paragraphs.
  p("Coronavirus is a hot topic this year. People around the world are 
  hugely affected by it. As international students, we are facing many 
  difficulties due to coronavirus. It affected our lives in so many ways. 
  And we believed that others have been through the same thing like us."
  ),
  
  p("Thus, we decided to engage in this field and create a website that could 
  help people understand this complex situation. We hope that our design could 
  help people gain insights and realize how the pandemic is affecting different 
  group of people in our society. We decided to explore different groups of 
  people categorized by age, race and sex. We will explore 
  Percentage of Confirmed Cases by Age Group, 
  Hospital Acceptance Rate by Race, and
  Death Rate for Every Age Group by Sex."
  ),
  
  p(
    "The original data was downloaded from ",
    a("U.S. Government's open data. ",
      href = "https://tinyurl.com/yxnf4wwe"
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
  
  tableOutput(outputId = "dataset"),
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
    percentage of confirmed cases that takes up only about 3%. Also, mortality 
    rate tends to be higher as people get old. Thus, we should be aware of the 
    harsh situation and do everything possible to protect ourselves form getting 
    infected, avoiding risky behaviors during this sensitive period of time. We
    could also use mortality rate data to esitmate priority age group for 
    COVID-19 vaccination."
  ),
  
  p(
    "The dataframe below was grouped and summarized by current status.
    The table was included to show differences between laboratory 
    confirmed/probable cases. The table reveals up-to-date number of laboratory 
    confirmed/probable cases. We wish upcoming vaccination slows down the
    increase of numbers below that we can go back on campus and resume everday
    life."
  ),
  
  tableOutput(outputId = "summary")
)

ui <- fluidPage(
  theme="style.css",
  tabsetPanel(
    intro_page,
    tabPanel(
      "Confirmed Cases by Age",
      age_gender_tab
    ),
    tabPanel(
      "Hospital Acceptance Rate by Race",
      page_two
    ),
    tabPanel(
      "Death Rate for Every Age Group by Gender",
      tt1
      ),
    summary_page
  )
)
