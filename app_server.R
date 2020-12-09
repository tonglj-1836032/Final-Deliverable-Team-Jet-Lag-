server <- function(input, output) {
  
  output$line<-renderPlotly(
    plot_ly(1,2)
    
  )
  
  output$desc<-renderText("Covid-19 is getting very serious in the United States, but fortunately, the death rate for this pandemic
is relatively lower than another serious pandemic. But what kind of group has a higher mortality rate? Is
there any connection between gender and mortality rates or age and mortality rates? Thus, we would like to
use this graph to find out how gender influences the mortality rates of people in each age group.
")

  
  COVID <- read.csv("data/COVID-19_Case_Surveillance_Public_Use_Data.csv")
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