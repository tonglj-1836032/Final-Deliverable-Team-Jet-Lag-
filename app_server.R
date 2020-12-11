library(tidyverse)
library(dplyr)
library(ggplot2)
require(maps)
require(viridis)
library(openintro)
library(shiny)
library(plotly)

data_path <- file.path(
  getwd(),
  "data",
  "COVID-19_Case_Surveillance_Public_Use_Data.csv"
)

COVID <- read.csv(data_path,stringsAsFactors = F)
COVID2 <- COVID
data_raw <- COVID

basic <- COVID %>%
  select(current_status, age_group, sex)

confirmed_case <- basic %>%
  filter(current_status == "Laboratory-confirmed case") %>%
  filter(sex != "Missing") %>%
  filter(sex != "NA") %>%
  filter(sex != "Unknown") %>%
  filter(age_group != "Unknown") %>%
  filter(age_group != "NA")

all_cases <- confirmed_case %>%
  group_by(age_group, sex) %>%
  summarise(cases = n())

which_gender <- function(gender) {
  confirmed_case %>%
    group_by(age_group, sex) %>%
    filter(sex == gender) %>%
    summarise(cases = n())
}

age_case_p1 <- "Since so many people have been unfortunately confirmed to be the 
              carrier of COVID-19, the public went into panic at the beginning 
              of this chaos. Numerous people are still wondering what the age 
              group of people COVID-19 normally targets at, thus we hope to 
              generate a chart to display the statistical information about the 
              confirmed-cases age group, informing the public an potential 
              correlation between age and the possibility of getting COVID-19."

age_case_p2 <- "Based on this chart, we can easily observe that most of COVID-19 
              cases are confirmed at the age group between 40 - 49 Years, which 
              takes up about 23% of the total confirmed population. From this 
              chart, we can see that the majority of confirmed cases are founded 
              in the age group above 30 years old, which might indicate that 
              there is a positive correlation between the possibility of getting 
              COVID-19 and the age of individuals."

# Renames columns.
colnames(COVID2)[1] <- "cdc_report_date"
colnames(COVID2)[7] <- "race_ethnicity_combined"
colnames(COVID2)[8] <- "hospitalized"
colnames(COVID2)[10] <- "death"

# Selects relevant columns.
relevant_dataset <- COVID2 %>%
  select(
    cdc_report_date, 
    current_status, 
    sex, 
    age_group, 
    race_ethnicity_combined, 
    hospitalized, 
    death
  )

# Sorts dataset in the order of date.
relevant_dataset <- arrange(relevant_dataset, cdc_report_date)

# Slices dataset.
sliced_dataset <- relevant_dataset %>%
  slice(1:10)

# Calculates current status summary information.
current_status_summary <- relevant_dataset %>%
  group_by(current_status) %>%
  summarize(num_current_status = n())

data <- data_raw %>% 
  filter(hosp_yn != "Unknown") %>%
  filter(hosp_yn != "Missing") %>%
  select(sex,age_group,Race.and.ethnicity..combined., hosp_yn) %>% 
  filter(Race.and.ethnicity..combined. != "Unknown")

#organizing data & relavant calculation for GENDER
race_list <- as.list(
  data %>% 
    select(Race.and.ethnicity..combined.) %>% 
    unique()
)

server <- function(input, output) {
  
  race_sex_y <- data %>% 
    group_by(Race.and.ethnicity..combined.,sex) %>% 
    filter(hosp_yn == "Yes") %>%
    summarize(sex_sum_y = n())
  
  race_sex_sum <- data %>% 
    group_by(Race.and.ethnicity..combined.,sex) %>% 
    summarize(sex_sum = n())
  
  data_race_sex <- left_join(race_sex_y, race_sex_sum,
                             by = c("Race.and.ethnicity..combined.","sex")
  ) %>%
    mutate(y_rate = (sex_sum_y / sex_sum)*100) %>% 
    select(Race.and.ethnicity..combined.,sex,y_rate) %>% 
    filter(sex != "Missing" & sex != "Unknown")
  
  #organizing data & relavant calculation for AGE
  race_age_y <- data %>% 
    group_by(Race.and.ethnicity..combined.,age_group) %>% 
    filter(hosp_yn == "Yes") %>%
    summarize(age_sum_y = n())
  
  race_age_sum <- data %>% 
    group_by(Race.and.ethnicity..combined.,age_group) %>% 
    summarize(age_sum = n())
  
  data_race_age <- left_join(race_age_y, race_age_sum,
                             by = c("Race.and.ethnicity..combined.","age_group")
  ) %>%
    mutate(y_rate = (age_sum_y / age_sum)*100) %>% 
    select(Race.and.ethnicity..combined.,age_group,y_rate) %>% 
    filter(age_group != "Unknown") %>% 
    na.omit()
  
  #Interactive
  reactive_data <- reactive({
    if(input$x == "sex"){plot_data <- data_race_sex}
    if(input$x == "age_group"){plot_data <- data_race_age}
    plot_data <- filter(plot_data, Race.and.ethnicity..combined. == input$race)
    return(plot_data)
  })
  
  # wrapper for cleaning up labels
  wrapper <- reactive({
    if(input$x == "sex"){return("Sex")}
    if(input$x == "age_group"){return("Age Groups")}
  })
  
  #plot
  output$plot <- renderPlotly({
    plot <- ggplot(reactive_data(),
                   aes(x = get(input$x), y = y_rate)) +
      geom_col(fill = "#7B90D2") +
      coord_flip() +
      labs(
        title =  paste("Hospital Accecptance Rate for",
                       input$race,"People by",wrapper()),
        x = wrapper(),
        y = "Hospital Accecptance Rate (%)"
      ) +
      scale_fill_discrete(name = wrapper())
    ggplotly(plot)
  })
  
  # The bar graph for confirmed cases and age group
  output$bar <- renderPlotly({
    plot1 <- ggplot(data = which_gender(input$gender_input)) +
      geom_bar(
        mapping = aes(fill = age_group, y = cases, x = input$gender_input),
        position = "fill", stat = "identity"
      ) +
      labs(
        x = "Gender", y = "Confirmed Cases Percetage",
        title = "Percentage of Confirmed Cases by Age Group"
      ) +
      scale_fill_brewer(palette = input$color_input)
    ggplotly(plot1)
  })
  output$race_list<- renderUI(
    {
      selectInput(
        inputId = "race",
        label = h3("Select a race and ethnicity group"),
        choices =race_list,
        selected = "Black, Non-Hispanic"
      )
    }
  )
  output$age_case_p1 <- renderText(age_case_p1)
  output$age_case_p2 <- renderText(age_case_p2)  
  output$line<-renderPlotly(
    plot_ly(1,2)
    
  )
  
  output$desc<-renderText("Covid-19 is getting very serious in the United 
  States, but fortunately, the death rate for this pandemic is relatively lower
  than another serious pandemic. But what kind of group has a higher mortality
  rate? Is there any connection between gender and mortality rates or age and
  mortality rates? Thus, we would like to use this graph to find out how gender
  influences the mortality rates of people in each age group.")
  covid_death <- COVID %>%
    filter(death_yn != "Unknown" & death_yn != "Missing") %>%
    select(sex,age_group,death_yn) %>%
    filter(sex != "Unknown" & sex != "Missing" & sex != "Other") %>%
    filter(age_group != "Unknown" & age_group != "Missing") %>%
    na.omit()
  
  death_y <- covid_death %>% 
    group_by(sex,age_group) %>% 
    filter(death_yn == "Yes") %>%
    summarize(death_sum_y = n())
  
  death_yn <- covid_death %>% 
    group_by(sex,age_group) %>% 
    summarize(death_sum = n())
  
  death <- left_join(death_y, death_yn,by = c("sex","age_group")) %>%
    mutate(death_rate = (death_sum_y / death_sum)*100) %>%
    select(sex,age_group,death_sum_y,death_rate)
  
  death_max_m <-death %>% filter(sex == "Male") %>% filter(death_rate == max(death_rate)) 
  death_max_f <-death %>% filter(sex == "Female") %>% filter(death_rate == max(death_rate)) 
  
  text=paste0(death_max_m$death_rate,"% is the highest mortality rates for male, ",death_max_m$age_group," is the age group with the highest mortality rate for male. ", death_max_f$death_rate,"% is the highest mortality rates for female and ", death_max_f$age_group," age group with the highest mortality rate for female. We could learn that before 75 years old, the male has higher mortality rates than female. However, after 75 years old, female has high mortality rates than male. And one interesting thing I found in this chart is that people who have 'other' gender identity has almost 0% mortality the rate in every age group. ")
  
  tt_wrapper <- reactive({
    if(input$pp == "death_rate"){return("Mortality rates (%)")}
    if(input$pp == "death_sum_y"){return("Death Count")}
  })
  output$line<- renderPlotly({
    plot_line<-ggplot(filter(death,sex == input$line_type), aes(x = age_group, y = get(input$pp),group = sex))+
      geom_line()+
      ggtitle(paste0("COVID-19 Cases ", tt_wrapper(), " for ", input$line_type  ," in Each Age Group")) +
      labs(y = tt_wrapper(), x = "Age group")
    ggplotly(plot_line)
  })
  
  output$text<- renderText(
    {
      paste0("Based on the chart, we could see that gender and age did have an
      impact on the mortality rates. We could see that the mortality rates are
      nearly 0% for people from 0-29 years old. However, the mortality rates 
      keep increasedas age increased. And ",text)
    }
  )
  
  output$dataset <- renderTable(
    {
      sliced_dataset
    }
  )
  
  output$summary <- renderTable(
    {
      current_status_summary
    }
  )
  
}


