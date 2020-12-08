data_raw <- read.csv("./data/COVID-19_Case_Surveillance_Public_Use_Data.csv")

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

 }