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

COVID <- read.csv(data_path)
data_raw <- read.csv(data_path)

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

server <- function(input, output) {
  #filtering missing value 
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
             title = "Percentage of Confirmed Cases by Age Group and Gender"
          ) +
          scale_fill_brewer(palette = input$color_input)
       ggplotly(plot1)
    })
    output$age_case_p1 <- renderText(age_case_p1)
    output$age_case_p2 <- renderText(age_case_p2)  
	output$line<-renderPlotly(
    plot_ly(1,2)
    
  )
  
  output$desc<-renderText("Covid-19 is getting very serious in the United States, but fortunately, the death rate for this pandemic
is relatively lower than another serious pandemic. But what kind of group has a higher mortality rate? Is
there any connection between gender and mortality rates or age and mortality rates? Thus, we would like to
use this graph to find out how gender influences the mortality rates of people in each age group.
")
  feature=colnames(COVID)
  covid_death <- COVID %>%
    filter(death_yn != "Unknown" & death_yn != "Missing")
  
  covid_death<-covid_death[,c("cdc_report_dt", "sex" ,"age_group","death_yn")]
  
  covid_death <- covid_death %>%
    filter(sex != "Unknown" & sex != "Missing" )
  covid_death <- covid_death %>%
    filter(age_group != "Unknown" & age_group != "Missing")
  
  age_group=unique(as.character(covid_death$age_group)) 
  sex=unique(as.character(covid_death$sex)) 
  n=length(age_group)
  res=c()
  for(i in 1:n){
    ss=which(as.character(covid_death$age_group)==age_group[i])
    dd=covid_death[ss,]
    m=length(sex)
    rr=c()
    for(j in 1:m)
    {
      dm=length(which(as.character(dd$sex)==sex[j] & as.character(dd$death_yn)=="Yes"))*100/length(ss)
      re=c(sex[j],round(dm,2))
      rr=rbind(rr,re)
    }
    re=cbind(age_group[i],rr)
    res=rbind(res,re)
  }
  colnames(res)=c("Age_group","Sex","Death_yn")
  res=data.frame(res)
  res$Death_yn=as.numeric(res$Death_yn)
  res$Death_yn[is.na(res$Death_yn)]=0
  res$Sex=factor(res$Sex,levels =sex)

  
  #ggplot(data = res, aes(x = Age_group, y = Death_yn, fill =Sex,group=Sex)) + geom_area()
  sex=as.character(res$Sex)
  M=which(sex=="Male")
  res_M=res[M,]
  max_M=order(as.numeric(res_M$Death_yn), decreasing =T)[1]
  max_M_A=res_M[max_M,1]
  max_M=res_M[max_M,3]
  F=which(sex=="Female")
  res_F=res[F,]
  max_F=order(as.numeric(res_F$Death_yn), decreasing =T)[1]
  max_F_A=res_F[max_F,1]
  max_F=res_F[max_F,3]
  text=paste0(max_M,"% is the highest mortality rates for male, ",max_M_A," is the age group with the highest mortality rate for male. "
              ,max_F,"% is the highest mortality rates for
female and ",max_F_A," age group with the highest mortality rate for female. We could learn that before 75 years old,
the male has higher mortality rates than female. However, after 75 years old, female has high mortality rates than male. 
And one interesting thing I found in this chart is that people who have 'other' gender identity has almost 0% mortality
the rate in every age group. ")

  output$line<- renderPlotly({
    pp=as.numeric(input$pp)
    if(pp==1){
      ssa=which(res$Death_yn>0)
      res=res[ssa,]
    }
  plot_line<-ggplot(data = res, aes(x = Age_group, y = Death_yn, group=Sex,color =Sex))+scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) + geom_line()+ ggtitle("COVID-19 cases mortality rates for every sex in each age group.") +
    labs(
      y = "Mortality rates (%)", x = " Age group"
    )
  ggplotly(plot_line)
  })
  
  output$text<- renderText(
    {
      paste0("Based on the chart, we could see that gender and age did have an impact on the mortality rates. We could see
that the mortality rates are nearly 0% for people from 0-29 years old. However, the mortality rates keep 
increasedas age increased. And ",text)
    }
  )
 
 }