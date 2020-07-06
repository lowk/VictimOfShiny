library(shiny)
library(tidyverse)
library(curl)
library(readxl)
library(ggtext)
library(paletteer)
library(lubridate)
library(forcats)
library(Rcpp)



#Bring in 2020 data for England & Wales (released by ONS on a Tuesday)
temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fhealthandsocialcare%2fcausesofdeath%2fdatasets%2fdeathregistrationsandoccurrencesbylocalauthorityandhealthboard%2f2020/lahbtablesweek25.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
data20 <- read_excel(temp, sheet=6, col_names=FALSE)[-c(1:4),]
colnames(data20) <- c("code", "type", "name", "cause", "week", "location", "deaths")

data20$deaths <- as.numeric(data20$deaths)
data20$week <- as.numeric(data20$week)

maxweek <- max(data20$week)
enddate.ew <- as.Date("2020-01-03")+weeks(maxweek-1)

#Read in 2015-19 historic data for England & Wales
temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/adhocs/11826fiveyearaverageweeklydeathsbylocalauthorityandplaceofoccurrenceenglandandwalesdeathsregistered2015to2019/weeklyfiveyearaveragesbylaandplaceofoccurrence20152019.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
data1519 <- read_excel(temp, sheet=2, col_names=FALSE)[-c(1:3),]
colnames(data1519) <- c("code", "name", "week", "location", "deaths")

data1519$deaths <- as.numeric(data1519$deaths)
data1519$week <- as.numeric(data1519$week)
data1519 <- data1519 %>% drop_na(name)

#Combine Cornwall & Isles of Scilly in both datasets
data20$code <- if_else(data20$code=="E06000053", "E06000052", data20$code)
data1519$code <- if_else(data1519$code=="E06000053", "E06000052", data1519$code)
data20$name <- if_else(data20$name=="Isles of Scilly", "Cornwall", data20$name)
data1519$name <- if_else(data1519$name=="Isles of Scilly", "Cornwall", data1519$name)

#Extract location data
data20$location <- case_when(
  data20$location %in% c("Elsewhere", "Home", "Hospice", "Other communal establishment") ~ "Home/Other",
  TRUE ~ data20$location)

data1519$location <- case_when(
  data1519$location %in% c("Elsewhere", "Home", "Hospice", "Other communal establishment") ~ "Home/Other",
  TRUE ~ data1519$location)

data20.loc <- data20 %>% 
  group_by(code, name, location, week) %>% 
  summarise(deaths20=sum(deaths))

data1519.loc <- data1519 %>% 
  group_by(code, name, location, week) %>% 
  summarise(deaths1519=sum(deaths))

data.loc <- merge(data1519.loc, data20.loc, by=c("name", "code", "location", "week"), all.x=TRUE)

#Calculate location-specific excess deaths
data.loc$excess <- data.loc$deaths20-data.loc$deaths1519
data.loc$location <- factor(data.loc$location, levels=c("Hospital", "Care home", "Home/Other"))

#Collapse location data
data20 <- data20 %>%
  group_by(code, name, cause, week) %>%
  summarise(deaths=sum(deaths)) %>%
  mutate(year="2020")

data1519 <- data1519 %>% 
  group_by(code, name, week) %>% 
  summarise(deaths=sum(deaths)) %>% 
  mutate(year="201519", cause="All")

data20.all <- data20 %>% 
  group_by(code, name, week) %>% 
  summarise(deaths=sum(deaths)) %>% 
  mutate(year="2020", cause="All")

#Cause-specific dataset
data20_wide <- spread(data20, cause, deaths)
data20_wide <- merge(data20_wide, data1519, by=c("code", "name", "week"))[,-c(4,8,9)]
data20_wide$Other <- data20_wide$`All causes`-data20_wide$deaths
data_bar <- gather(data20_wide, excesscause, deaths, c(5,7))

#All-cause only dataset
data.all <- merge(data1519, data20.all, by=c("code", "week", "name"), all.x=TRUE)[,c(1:4,7)]
colnames(data.all) <- c("code", "week", "name", "hist", "2020")
data.all <- data.all %>% drop_na(name)
data.all$excess <- data.all$`2020`-data.all$hist

#Bring in Scottish deaths data (released by NRS on a Wednesday)
#2020 data
temp <- tempfile()
source <- "https://www.nrscotland.gov.uk/files//statistics/covid19/weekly-deaths-by-date-council-area-location.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
data20.s <- read_excel(temp, sheet=2, range="A5:E3276", col_names=FALSE)
colnames(data20.s) <- c("week", "name", "location", "cause", "deaths")
data20.s$week <- as.numeric(data20.s$week)

data20.s$location <- case_when(
  data20.s$location=="Care Home" ~ "Care home",
  data20.s$location %in% c("Home / Non-institution", "Other institution") ~ "Home/Other",
  TRUE ~ "Hospital"
)

data20.s <- data20.s %>% 
  group_by(week, name, location, cause) %>% 
  summarise(deaths=sum(deaths))

data20.s <- spread(data20.s, cause, deaths)
data20.s$`COVID-19` <- if_else(is.na(data20.s$`COVID-19`),0,data20.s$`COVID-19`)
data20.s$`Non-COVID-19` <- if_else(is.na(data20.s$`Non-COVID-19`),0,data20.s$`Non-COVID-19`)
data20.s$All <- data20.s$`COVID-19`+data20.s$`Non-COVID-19`

maxweek.s <- max(data20.s$week)
enddate.s <- as.Date("2020-01-04")+weeks(maxweek.s-1)

#2015-19 data
temp <- tempfile()
source <- "https://www.nrscotland.gov.uk/files//statistics/covid19/weekly-deaths-by-date-council-area-location-15-19.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
data1519.s <- read_excel(temp, sheet=2, range="A5:E25207", col_names=FALSE)
colnames(data1519.s) <- c("week", "name", "location", "year", "deaths")
data1519.s$week <- as.numeric(data1519.s$week)

data1519.s$location <- case_when(
  data1519.s$location=="Care Home" ~ "Care home",
  data1519.s$location %in% c("Home / Non-institution", "Other institution") ~ "Home/Other",
  TRUE ~ "Hospital"
)

data1519.s <- data1519.s %>% 
  group_by(week, name, location) %>% 
  summarise(hist=mean(deaths))

#Faff about aligning Scottish data with English/Welsh data 
#(a clever person would have a much more elegant solution to all this, I'm sure)

data.s <- merge(data1519.s, data20.s, by=c("week", "name", "location"),all=TRUE)
data.s$`COVID-19` <- if_else(is.na(data.s$`COVID-19`) & data.s$week<=maxweek.s, 0, data.s$`COVID-19`)
data.s$`Non-COVID-19` <- if_else(is.na(data.s$`Non-COVID-19`) & data.s$week<=maxweek.s, 0, data.s$`Non-COVID-19`)
data.s$All <- if_else(is.na(data.s$All) & data.s$week<=maxweek.s, 0, data.s$All)

data.all.s <- data.s %>% 
  select(c(1:4,7)) %>% 
  group_by(week, name) %>% 
  summarise(hist=sum(hist), `2020`=sum(All)) %>% 
  mutate(excess=`2020`-hist)

data.all <- bind_rows(data.all, data.all.s)

data.s$otherexcess <- data.s$`Non-COVID-19`-data.s$hist

data_bar.s <- data.s %>% 
  group_by(name, week) %>% 
  summarise(`COVID 19`=sum(`COVID-19`), Other=sum(otherexcess))

data_bar.s <- gather(data_bar.s, excesscause, deaths, c(3,4))

data_bar <- bind_rows(data_bar, data_bar.s)

data.loc.s <- data.s[,c(1:4, 7)]
colnames(data.loc.s) <- c("week", "name", "location", "deaths1519", "deaths20")
data.loc.s$excess <- data.loc.s$deaths20-data.loc.s$deaths1519

data.loc <- bind_rows(data.loc, data.loc.s)

#Summary figures
data_summary.s <- data.s %>% 
  filter(week<=maxweek.s) %>% 
  group_by(name) %>% 
  summarise(hist=sum(hist), curr=sum(All)) %>% 
  mutate(excess=curr-hist, excessprop=excess/hist, country="Scotland")

data_summary <- data20_wide %>% 
  group_by(code, name) %>% 
  summarise(hist=sum(deaths), curr=sum(`All causes`)) %>% 
  mutate(excess=curr-hist, excessprop=excess/hist)

data_summary$country <- if_else(substr(data_summary$code,1,1)=="E", "England", "Wales")

data_summary <- bind_rows(data_summary, data_summary.s)

#Read in cases data for England
temp <- tempfile()
source <- "https://coronavirus.data.gov.uk/downloads/csv/coronavirus-cases_latest.csv"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

casedata.E <- read.csv(temp)[,c(1:5)]
colnames(casedata.E) <- c("name", "code", "geography", "date", "cases")

#Collapse to weeks
casedata.E$week <- week(as.Date(casedata.E$date)-days(4))

casedata.E <- casedata.E %>% 
  filter(geography=="Lower tier local authority" & week<=maxweek) %>% 
  group_by(name, code, week) %>% 
  summarise(cases=sum(cases))

#Read in cases data for Wales
temp <- tempfile()
source <- "http://www2.nphs.wales.nhs.uk:8080/CommunitySurveillanceDocs.nsf/3dc04669c9e1eaa880257062003b246b/77fdb9a33544aee88025855100300cab/$FILE/Rapid%20COVID-19%20surveillance%20data.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
casedata.W <- read_excel(temp, sheet=3)[,c(1:3)]

colnames(casedata.W) <- c("name", "date", "cases")

#Collapse to weeks
casedata.W$week <- week(as.Date(casedata.W$date)-days(4))

casedata.W <- casedata.W %>% 
  filter(week<=maxweek) %>% 
  group_by(name, week) %>% 
  summarise(cases=sum(cases))

casedata <- bind_rows(casedata.E, casedata.W)

function(input, output) {

  

  output$plot <- renderPlot({
  	LA <- input$LA
  	excess <- data_summary %>% filter(name==LA) 
	
	enddate <- if_else(excess[1,7]=="Scotland", enddate.s, enddate.ew)
	source <- if_else(excess[1,7]=="Scotland", "NRS", "ONS")
	
	labpos <- max(data.all$`2020`[data.all$week==maxweek & data.all$name==LA],
        data.all$hist[data.all$week==maxweek & data.all$name==LA])

	lab <- if_else(excess[1,5]<0, 
               paste0(round(excess[1,5],0), " (",round(excess[1,6]*100,0), "%) deaths in 2020\ncompared to the average in 2015-19"),
               paste0("+", round(excess[1,5],0), " (+",round(excess[1,6]*100,0), "%) deaths in 2020\ncompared to the average in 2015-19"))
               
   if (input$plottype == 1){
    p <- ggplot(subset(data.all, name==LA))+
  geom_line(aes(x=week, y=hist), colour="skyblue4")+
  geom_line(aes(x=week, y=`2020`), colour="red")+
  scale_x_continuous(name="Week")+
  scale_y_continuous(name="Deaths", limits=c(0,NA))+
  theme_classic()+
  theme(plot.subtitle=element_markdown())+
  annotate("text", x=maxweek-2, y=max(labpos*1.5, labpos+20), 
           label=lab,
           hjust=0, colour="red", size=3)+
  labs(title=paste0("Excess deaths in ", LA, " during the pandemic"),
       subtitle=paste0("Weekly deaths in <span style='color:red;'>2020</span> compared to <span style='color:Skyblue4;'>the average in 2015-19</span> by date of occurance<br>Data up to ", enddate),
       caption=paste0("Data from ", source," | Plot by @VictimOfMaths"))
       }

   if (input$plottype == 2){
    p <- ggplot(subset(data_bar, name==LA), aes(x=week, y=deaths, fill=excesscause))+
  geom_bar(stat="identity")+
  geom_segment(aes(x=0.5, xend=maxweek+0.5, y=0, yend=0), colour="Grey30")+
  scale_x_continuous(name="Week")+
  scale_y_continuous(name="Excess deaths vs. 2015-19 average")+
  scale_fill_paletteer_d("LaCroixColoR::PinaFraise", name="Cause", labels=c("COVID-19", "Other causes"))+
  theme_classic()+
  labs(title=paste0("Excess deaths in ", LA, " during the pandemic"),
       subtitle=paste0("Excess deaths by date of occurance in 2020 vs. 2015-19 average by cause\nData up to ", enddate),
       caption=paste0("Data from ", source," | Plot by @VictimOfMaths"))
       }
      
   if (input$plottype == 3){
	p <- ggplot(subset(data.loc, name==LA), aes(x=week, y=excess, fill=location))+
  geom_col()+
  geom_segment(aes(x=0.5, xend=maxweek+0.5, y=0, yend=0), colour="Grey30")+
  scale_x_continuous(name="Week")+
  scale_y_continuous(name="Excess deaths vs. 2015-19 average")+
  scale_fill_paletteer_d("ggsci::planetexpress_futurama", name="Place of death")+
  theme_classic()+
  labs(title=paste0("Excess deaths in ", LA, " during the pandemic"),
       subtitle=paste0("Excess deaths by place of death in 2020 vs. 2015-19 average by cause\nData up to ", enddate),
       caption=paste0("Data from ", source," | Plot by @VictimOfMaths"))
       }
  if (input$plottype == 4){
  	
 p <- ggplot()+

  geom_segment(aes(x=0.5, xend=maxweek+0.5, y=0, yend=0), colour="Grey30")+

  geom_line(data=subset(casedata, name==LA), aes(x=week, y=cases), colour="#B25D91")+

  geom_line(data=subset(data20, name==LA & cause=="COVID 19"), 

            aes(x=week, y=deaths), colour="#1BB6AF")+

  scale_x_continuous(name="Week", limits=c(0,maxweek+1))+

  scale_y_continuous(name="")+

  theme_classic()+

  theme(plot.subtitle=element_markdown())+

  labs(title=paste0("Timeline of COVID-19 in ", LA),

       subtitle=paste0("Confirmed new COVID-19 <span style='color:#B25D91;'>cases</span> compared to <span style='color:#1BB6AF;'>deaths</span> by week of occurance<br>Data up to ", enddate),

       caption=paste0("Data from ", source," | Plot by @VictimOfMaths"))
  }
  p     
  }, height=700)

}
