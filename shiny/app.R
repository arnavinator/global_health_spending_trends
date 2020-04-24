#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(tidyverse)
library(readr)
library(readxl)
library(rvest)
library(janitor)
library(fuzzyjoin)
library(shinythemes)
library(shiny)
library(gganimate)
library(gifski)

income <- read_excel("raw-data/Income class of countries.xls", skip = 4) %>% 
    clean_names() %>% 
    select(economy, income_group)
income <- income[-c(1),]
income <- income[-c(219:274),]

income <- income %>% 
    mutate(economy = str_replace(economy, "Antigua and Barbuda", "Antigua And Barbuda"),
           economy = str_replace(economy, "Bahamas, The", "Bahamas"),
           economy = str_replace(economy, "Congo, Dem. Rep.", "Democratic Republic of the Congo"),
           economy = str_replace(economy, "Egypt, Arab Rep.", "Egypt"),
           economy = str_replace(economy, "Gambia, The", "Gambia"),
           economy = str_replace(economy, "Iran, Islamic Rep.", "Iran, Islamic Republic of"),
           economy = str_replace(economy, "Korea, Dem. People's Rep.", "Democratic Peoples Republic of Korea"),
           economy = str_replace(economy, "Korea, Rep.", "Korea, Republic of"),
           economy = str_replace(economy, "Kyrgyz Republic", "Kyrgyzstan"),
           economy = str_replace(economy, "Moldova", "Moldova, Republic of"),
           economy = str_replace(economy, "St. Kitts and Nevis", "Saint Kitts"),
           economy = str_replace(economy, "St. Lucia", "Saint Lucia"),
           economy = str_replace(economy, "St. Vincent and the Grenadines", "Saint Vincent and the Grenadines"),
           economy = str_replace(economy, "São Tomé and Principe", "Sao Tome And Principe"),
           economy = str_replace(economy, "Tanzania", "Tanzania, United Republic of"),
           economy = str_replace(economy, "Slovak Republic", "Slovakia"),
           economy = str_replace(economy, "United States", "United States of America"),
           economy = str_replace(economy, "Venezuela, RB", "Venezuela"),
           economy = str_replace(economy, "Vietnam", "Viet Nam"),
           economy = str_replace(economy, "Yemen, Rep.", "Yemen"),
           economy = str_replace(economy, "Micronesia, Fed. Sts.", "Micronesia, Federated States of"),
           economy = str_replace(economy, "Congo, Rep.", "Congo")
    )


# pulls table from internet which classifies countries by their reigion...
# needed to skip the first line, and filter out all rows where countries were
# not listed (the table had rows to index all countries starting by the same
# letter). Selected for only country name and reigion, all else was irrelevant

page <- read_html("https://www.who.int/choice/demography/by_country/en/")
country_reigion <- html_nodes(page, "table") %>% .[[1]] %>%
    html_table(fill = TRUE) %>% 
    clean_names() %>% 
    mutate(country = x1,
           reigion_num = x2,
           reigion = x3) %>% 
    select(country, reigion) %>% 
    filter(reigion != "")
country_reigion <- country_reigion[-c(1),]
country_reigion$reigion <- factor(country_reigion$reigion, 
                                  levels = c("AFRO", "EMRO", "EURO", "PAHO", "SEARO", "WPRO"),
                                  labels = c("African", "Eastern Mediterranean", "European",
                                             "Americas", "South-East Asia", "Western Pacific")
)


# merges dataset identifying a country's WHO reigion and their WB Income
# Bracket... classification now has info from both!

classification <- country_reigion %>% 
    inner_join(income, by = c("country" = "economy"))



# import new dataset on health spending, and select for relavent columns. Then
# manually match country names for proper joining

health_spend <- read_csv("raw-data/IHME_HEALTH_SPENDING.CSV") %>% 
    select(location_name, year, the_total_mean, dah_total_mean, the_per_cap_mean, the_per_gdp_mean, ghes_per_cap_mean, ppp_per_cap_mean, oop_per_cap_mean, dah_per_cap_mean, dah_per_gdp_mean) %>% 
    mutate(location_name = str_replace(location_name, "Antigua and Barbuda", "Antigua And Barbuda"),
           location_name = str_replace(location_name, "Cote d'Ivoire", "Côte d'Ivoire"),
           location_name = str_replace(location_name, "Iran", "Iran, Islamic Republic of"),
           location_name = str_replace(location_name, "South Korea", "Korea, Republic of"),
           location_name = str_replace(location_name, "Federated States of Micronesia", "Micronesia, Federated States of"),
           location_name = str_replace(location_name, "Syria", "Syrian Arab Republic"),
           location_name = str_replace(location_name, "The Bahamas", "Bahamas"),
           location_name = str_replace(location_name, "The Gambia", "Gambia"),
           location_name = str_replace(location_name, "Sao Tome and Principe", "Sao Tome And Principe"),
           location_name = str_replace(location_name, "United States", "United States of America"),
           location_name = str_replace(location_name, "Vietnam", "Viet Nam"),
           location_name = str_replace(location_name, "Moldova", "Moldova, Republic of"),
           location_name = str_replace(location_name, "North Korea", "Democratic Peoples Republic of Korea")
    )

classified_spend <- classification %>% 
    inner_join(health_spend, by = c("country" = "location_name"))

# import new dataset on deaths by communicable/noncommunicable diseases by age/gender. removed irrelevant columns and changed names of countries manually for proper joining

deaths <- read_csv("raw-data/IHME-GBD_deaths.csv") %>% 
    clean_names() %>% 
    select(-measure, -metric, -upper, -lower) %>% 
    mutate(location = str_replace(location, "Antigua and Barbuda", "Antigua And Barbuda"),
           location = str_replace(location, "Cote d'Ivoire", "Côte d'Ivoire"),
           location = str_replace(location, "Iran", "Iran, Islamic Republic of"),
           location = str_replace(location, "South Korea", "Korea, Republic of"),
           location = str_replace(location, "Federated States of Micronesia", "Micronesia, Federated States of"),
           location = str_replace(location, "Syria", "Syrian Arab Republic"),
           location = str_replace(location, "The Bahamas", "Bahamas"),
           location = str_replace(location, "Sao Tome and Principe", "Sao Tome And Principe"),
           location = str_replace(location, "United States", "United States of America"),
           location = str_replace(location, "Vietnam", "Viet Nam"),
           location = str_replace(location, "Moldova", "Moldova, Republic of"),
           location = str_replace(location, "North Korea", "Democratic Peoples Republic of Korea"), 
           location = str_replace(location, "South Korea", "Korea, Republic of"),
           location = str_replace(location, "Brunei", "Brunei Darussalam"),
           location = str_replace(location, "Federated States of Micronesia", "Macedonia"),
           location = str_replace(location, "The Gambia", "Gambia"),
           location = str_replace(location, "Tanzania", "Tanzania, United Republic of"),
    )

classified_death <- classification %>% 
    inner_join(deaths, by = c("country" = "location"))



# merge classified death and classified spend to analyze relationships between
# deaths and global health spending

spend_death <- classified_death %>% 
    inner_join(classified_spend, by = c("country" = "country", 
                                        "reigion" = "reigion", 
                                        "income_group" = "income_group", 
                                        "year" = "year"))



# import new dataset on education, remove irrelevant columns and extract only
# relavent information of how many students are enrolled in each level of
# education. Then, for easier analysis, I pivotted_wider the table

education <- read_csv("raw-data/education.csv", skip = 1) %>% 
    clean_names() %>% 
    mutate(country = x2) %>% 
    mutate(ed_type = series) %>% 
    select(country, year, ed_type, value) %>% 
    filter(ed_type %in% c("Students enrolled in primary education (thousands)",
                          "Students enrolled in secondary education (thousands)",
                          "Students enrolled in tertiary education (thousands)")
    ) %>% 
    pivot_wider(names_from = ed_type, values_from = value) %>% 
    clean_names()


# import new dataset on immigrants/refugees; 

immig <- read_csv("raw-data/immigrants.csv", skip = 1) %>% 
    clean_names() %>% 
    mutate(country = x2) %>% 
    mutate(imm_type = series) %>% 
    select(country, year, imm_type, value) %>% 
    filter(imm_type %in% c("International migrant stock: Both sexes (% total population)",
                           "Total population of concern to UNHCR (number)")
    ) %>% 
    pivot_wider(names_from = imm_type, values_from = value) %>% 
    clean_names()

# create merged dataset of education and immigrants/refugees; need to change
# names to merge with pop dataset that will be added next

educ_immig <- education %>% 
    inner_join(immig, by = c("country" = "country", "year" = "year")) %>% 
    filter(year %in% c(2005, 2010, 2015, 2017)) %>% 
    mutate(country = str_replace(country, "Bahamas", "Bahamas, The"),
           country = str_replace(country, "Bolivia (Plurin. State of)", "Bolivia"),
           country = str_replace(country, "Congo", "Congo, Rep."),
           country = str_replace(country, "Dem. Rep. of the Congo", "Congo, Dem. Rep."),
           country = str_replace(country, "Egypt", "Egypt, Arab Rep."),
           country = str_replace(country, "Gambia", "Gambia, The"),
           country = str_replace(country, "Iran (Islamic Republic of)", "Iran, Islamic Rep."),
           country = str_replace(country, "Dem. People's Rep. Korea", "Korea, Dem. People’s Rep."),
           country = str_replace(country, "Republic of Korea", "Korea, Rep."),
           country = str_replace(country, "Republic of Moldova", "Moldova"),
           country = str_replace(country, "Kyrgyzstan", "Kyrgyz Republic"),
           country = str_replace(country, "Micronesia (Fed. States of)", "Micronesia, Fed. Sts."),
           country = str_replace(country, "Slovakia", "Slovak Republic"),
           country = str_replace(country, "Saint Kitts and Nevis", "St. Kitts and Nevis"),
           country = str_replace(country, "Saint Lucia", "St. Lucia"),
           country = str_replace(country, "Saint Vincent & Grenadines", "St. Vincent and the Grenadines"),
           country = str_replace(country, "United Rep. of Tanzania", "Tanzania"),
           country = str_replace(country, "United States of America", "United States"),
           country = str_replace(country, "Venezuela (Boliv. Rep. of)", "Venezuela, RB"),
           country = str_replace(country, "Viet Nam", "Vietnam"),
           country = str_replace(country, "Yemen", "Yemen, Rep.")
    )

# import datasets on total population of countries, and the proportion of male
# and female in each country over time... pivot_wider for easier analysis

pop <- read_csv("raw-data/pop_total.csv", skip = 3) %>% 
    clean_names() %>% 
    pivot_longer(c("x2000", "x2001", "x2002", "x2003", "x2004", "x2005", "x2006", "x2007", "x2008", "x2009", "x2010", "x2011", "x2012", "x2013", "x2014", "x2015", "x2016", "x2017"), 
                 names_to = "year", 
                 values_to = "pop"
    ) %>% 
    select(country_name, year, pop) %>% 
    mutate(year = str_replace(year, "x", "")) %>% 
    mutate(year = as.numeric(year))

male <- read_csv("raw-data/prop_male.csv", skip = 3) %>% 
    clean_names() %>% 
    pivot_longer(c("x2000", "x2001", "x2002", "x2003", "x2004", "x2005", "x2006", "x2007", "x2008", "x2009", "x2010", "x2011", "x2012", "x2013", "x2014", "x2015", "x2016", "x2017"), 
                 names_to = "year", 
                 values_to = "prop_male"
    ) %>% 
    select(country_name, year, prop_male) %>% 
    mutate(year = str_replace(year, "x", "")) %>% 
    mutate(year = as.numeric(year)) %>% 
    mutate(prop_male = prop_male/100)

female <- read_csv("raw-data/prop_female.csv", skip = 3) %>% 
    clean_names() %>% 
    pivot_longer(c("x2000", "x2001", "x2002", "x2003", "x2004", "x2005", "x2006", "x2007", "x2008", "x2009", "x2010", "x2011", "x2012", "x2013", "x2014", "x2015", "x2016", "x2017"), 
                 names_to = "year", 
                 values_to = "prop_female"
    ) %>% 
    select(country_name, year, prop_female) %>% 
    mutate(year = str_replace(year, "x", "")) %>% 
    mutate(year = as.numeric(year)) %>%
    mutate(prop_female = prop_female/100)


# now I join all of these datasets to create a comprehensive dataset of
# population and proportion of male/female for all countries from 2000-2017.
# Then, I mutate the country names manually for better joining of the data with
# educ_immig

pop <- pop %>% 
    inner_join(male, by = c("country_name" = "country_name", "year" = "year"))

pop <- pop %>% 
    inner_join(female, by = c("country_name" = "country_name", "year" = "year"))


# after importing new dataset on education, immigrants/refugees, and
# population in each country, we can join these datasets. I then mutate some
# cols such that instead of comparing metrics by total popluation, we compare by
# proportion of population (ie prop of students in primary school is a metric I
# can compare with any country since standardized measure)

pop_educ_immig <- pop %>%
    inner_join(educ_immig, by = c("country_name" = "country", "year" = "year")) %>% 
    mutate(prop_primary = students_enrolled_in_primary_education_thousands*1000/pop) %>% 
    mutate(prop_secondary = students_enrolled_in_secondary_education_thousands*1000/pop) %>% 
    mutate(prop_tertiary = students_enrolled_in_tertiary_education_thousands*1000/pop) %>% 
    mutate(prop_immigrants = international_migrant_stock_both_sexes_percent_total_population/100) %>% 
    mutate(num_refugee = total_population_of_concern_to_unhcr_number) %>% 
    select(-students_enrolled_in_primary_education_thousands, 
           -students_enrolled_in_secondary_education_thousands,
           -students_enrolled_in_tertiary_education_thousands,
           -international_migrant_stock_both_sexes_percent_total_population,
           -total_population_of_concern_to_unhcr_number)


# dataset on health spending and pop/educ/immig

spend_death_cap <- spend_death %>% 
    inner_join(pop_educ_immig, by = c("country" = "country_name", "year" = "year")) %>% 
    mutate(death_cap = val/pop) %>% 
    filter(!is.na(death_cap))


#######################
####BEGIN SHINY APP####
#######################

ui <- navbarPage("Global Health Spending",
                 theme = shinytheme("flatly"),
                 
                 ##########
                 ##ABOUT##
                 #########
                 
                 tabPanel("About",
                          
                          #load first image 
                          
                          imageOutput("earth_pic", width = "100%", height = "100%"),
                          br(),
                          
                          #title and subtitle
                          
                          h2("Global Healthcare Disparity", align = "center"),
                          h4(em("An analysis of trends observed with increased health spending per capita"), align = "center"),
                          br(),
                          div(),
                          
                          
                          
                          br(),
                          
                          fluidRow(column(2), column(8,
                                                     
                                                     h4(strong("About this Project")),          
                                                     
                                                     #text to introduce project
                                                     
                                                     p("For this project, I strive to analyze how global health expenditure varies based on
                                                     economic and geographic factors. Using data from the World Bank, the Global Data Health Exchange,
                                                     and the World Health Organization, I gathered information regarding population sex/age 
                                                     demographics; the burden of diseases; and historic health
                                                     care spending data and distribution of spending (government, foreign assistance, out-of-pocket, etc.);
                                                     for 195 countries from 2000-2018."),
                                                     
                                                     span(),
                                                     
                                                     p("With this data, I hope to explore association between health spending and factors such as income,
                                                     geography, and disease-driven deaths."),
                                                     
                                                     
                                                     br(),
                                                     
                                                     
                                                     h4("About Me"),
                                                     
                                                     p("My name is Arnav Srivastava, and I am currently a first-year at Harvard College. I am 
                                                         fascinated by global health and improving accessibility to the ever-growing frontier of
                                                        science; I hope to concentrate in biomedical engineering, electrical engineering, or 
                                                         statistics, and help advance the health of our global community. The work for my project 
                                                        can be found on my GitHub repo at ", 
                                                       a(href = "https://github.com/arnavinator/gov1005-final_project", "here."))
                          ))
                 ),
                 
                 
                 ###########
                 ###DATA###
                 ##########
                 
                 tabPanel("Income & Regional Spending Trends",
                          
                          fixedRow(column(1), column(9,
                          h2(em("Exploring health spending growth rate by income")),
                          
                          br(),
                          
                          fluidRow(column(7, 
                                          h4("First, I wanted to explore how total health spending per person changes 
                                                based on income level of countries. The following graph shows the change
                                                over time of health spending per person. In observing this plot, it appears 
                                                that there was relatively no growth
                                                in healthspending for low and lower middle income groups.")
                                          ),
                                   
                                   column(4, plotOutput("spending_by_income", width = "700px"))
                                   ),
                          br(),      
                          
                          fluidRow(column(7,
                                          h4("However, after comparing income reigions with their successors (viewing two income
                                          groups at a time), we instead observe that all income groups in most reigions
                                          have witnessed historical health spending increase. However, the historical health spending
                                          rate of increase of one income group is always slower than its next-higher income group. The rate of
                                          health spending growth is proportional to a country's income growth"),
                                          h4("In our linear regression model, we observe that wealthier income groups are 
                                             associated with a more accelerated increase in health spending over time."),
                                             
                                          helpText("Choose a set of income groups to get a closer look at 
                                                       health spending growth relative to income."),
                                          selectInput("income_group", "Income Groups",
                                                          choices = list("Low income & Lower middle income" = "low+lower",
                                                                         "Lower middle income & Upper middle income" = "lower+upper",
                                                                         "Upper middle income & High Income" = "upper+high"),
                                                          selected = "low+lower"),
                                          sidebarPanel(
                                              p("Click the graph to explore points!"),
                                              verbatimTextOutput("info")
                                              )
                                          ,
                                              
                                              
                                          ),
                          
                                   column(4,
                                          plotOutput("spending_by_reigion", width = "700px", click = "plot_click")
                                          )
                          ),
                          br(),
                          fluidRow(column(7,
                                          h4("Next, I wanted to see what type of healthcare spending is responsible
                                             for these relative increases in healthcare spending per income group. Here, we explore the
                                             components of total health care spending: government, out-of-pocket, pre-paid private, and 
                                             foreign development assistance for health."),
                                          h4("We observe that healthcare spending growth for high income countries is driven strongly by growth in government
                                             spending, whereas out-of-pocket spending and foreign assistance is much more influential in healthcare
                                             spending for low income countries. Nevertheless, we observe that the out-of-pocket costs per capita in high income countries
                                             surpasses the total health spending per capita in low income countries, reflected by the increase in spending power of high income
                                             countries."),
                                          helpText("Choose a income group to view components of healthcare spending"),
                                          selectInput("income_group_2", "Income Group",
                                                      choices = list("Low income" = "low",
                                                                     "Lower middle income" = "lower",
                                                                     "Upper middle income" = "upper",
                                                                     "High income" = "high"),
                                                      selected = "low"),
                                          sidebarPanel(
                                              p("Click the graph to explore points!"),
                                              verbatimTextOutput("info1")
                                          )
                                          ),
                                   column(4,
                                          plotOutput("spending_single_reigion", width = "700px", click = "plot_click1")
                                   )
                                   )
                          ))
                 ),
                 tabPanel("Disease-Induced Deaths and Spending",
                          tabsetPanel(
                              tabPanel("Relationship between Deaths and Health Spending",
                                       fixedRow(column(1), column(9,
                                       
                                       br(),
                                       fluidRow(column(6, 
                                                       h4("Next, I wanted to explore the impact the number of deaths in a country has
                                                          on the total health spending in that country."),
                                                       h4("Using data for all countries over multiple years, we find shocking results from our
                                                       regression model that a 1% increase in total health spending of a country is associated
                                                       with a .5% increase in deaths for a country!"),
                                                       h4("Although it seemed plausible that countries with greater total health spending have more
                                                       people and therefore greater potential deaths, I wanted to better explore the correlation between
                                                         number of deaths and health spending by observing deaths versus health spending per capita")
                                       ),
                                       
                                       column(4, plotOutput("total_health_deaths", width = "750px"))
                                       ),
                                       br(),
                                       span(),
                                       span(),
                                       span(),
                                       br(),
                                       fluidRow( 
                                                       h4("Upon observing the transition of health spending per capita in different income groups, we 
                                                          observe that lower income groups have an increase in health spending per capita associated 
                                                          with a decrease in deaths, while as income levels increase, an increasing health spending 
                                                          transitions to a positive correlation with deaths!"),
                                                       h4("Exploring the impact of communicable and noncommunicable diseases relative to income group,
                                                          we can likely appropriate the change in health spending relative to capita to the global epidemiological
                                                          transition: lower income countries are burdened by a relatively greater number of communicable disease 
                                                          deaths, while wealthier
                                                          countries with higher standards of life are able to prevent communicable disease deaths and are instead
                                                          burdened by a relatively greater number of noncommunicable disease deaths."),
                                                       h4("Since communicable disease deaths are preventable given proper planning and resources empowered by
                                                          greater healthcare spending, whereas numerous noncommunicable diseases lack cures rendering deaths unavoidable,
                                                          we witness", em("diminishing returns"), "of health spending on preventing disease-caused deaths. Initial health
                                                          spending is effective in preventing deaths, but once the relative burden of communicable disease decreases, 
                                                          increasing health spending is ineffective in preventing deaths. This suggest that if wealthy nations are interested in 
                                                          better increasing the global standard of living, then increasing health spending in low income countries will have a 
                                                          greater marginal benefit in the global quality of life compared to increasing domestic health spending."),
                                                       
                                       
                                       
                                       
                                       ),
                                       br(),
                                       fluidRow(
                                           column(5, imageOutput("epi_trans", width = "500px")),
                                           column(7, imageOutput("capita_health_deaths", width = "500px"))
                                       ),
                                       br(),
                                       ))),
                              
                              
                              tabPanel("Sensitivity with Demographic-Based Deaths",
                                       fixedRow(column(1), column(9,
                                          br(),                          
                                          fluidRow(column(7,
                                              h4("We can further explore changes in health spending based on age and gender based demographics."), 
                                              h4("First, I analyzed the relationship between global health spending per captia and in relation to 
                                                 deaths, inclusive of all income groups, age groups, and gender. Overall, we observe a weak negative 
                                                 correlation between spending and deaths, implying an increase in health spending leads to lower deaths.")
                                          ),  
                                          
                                          column(4, plotOutput("health_spend_neutral", width = "700px"))
                                          ),
                                          fluidRow(column(7,
                                                          h4("Now, we are able to see if there is a change on the relationship between
                                                             health spending and deaths driven by gender."),
                                                          h4("We observe that the linear regression between spending and deaths is the same
                                                             for both genders. This rules out the possibility that a country's increase in deaths
                                                             by one group has a strong influence on changing health spending compared to deaths for 
                                                             another gender. More broadly, while we cannot infer causality without a more holisitc
                                                             model, this suggests that public senitments for certain deaths do not influence health spending,
                                                             rather health spending influences."),
                                                          
                                                          sidebarPanel(
                                                              p("Click the graph to explore points!"),
                                                              verbatimTextOutput("info4")
                                                          )
                                                          
                                          ),  
                                          
                                          column(4, plotOutput("health_spend_gender", width = "700px", click = "plot_click4"))
                                          ),
                                          br(),
                                          br(),
                                          fluidRow(column(7,
                                                          h4("Finally, we observe the change in relationship between health spending and deaths in a country by
                                                             age group. The linear correlation between these two variables clearly flattens out as age increases. In other
                                                             words, an increase in health spending in a country deters deaths of young age groups much more effectively than older
                                                             age groups, and the effectiveness of increased spending appears to have a diminishing impact as age
                                                             increases."),
                                                          h4("However, we again cannot establish any causality between these two variables without a more holistic model: it 
                                                             is possible that younger aged deaths encourage a country to increase their healthcare spending more
                                                             influentially than increasing deaths of older civilians, or that young children's suffer from more 
                                                             preventable diseases (avoided with health spending) than older
                                                             age groups. Nevertheless, given that the average country did not appear to have a difference in 
                                                             spending based on gender, it is likely that deaths of different age groups are not as impactful on spending,
                                                             rather the later hypothesis that younger deaths are easier to prevent may be a more viable theory."),
                                                          
                                                          sidebarPanel(
                                                              p("Click the graph to explore points!"),
                                                              verbatimTextOutput("info3")
                                                          )
                                                          
                                          ),  
                                          
                                          column(4, plotOutput("health_spend_age", width = "700px", click = "plot_click3"))
                                          ),
                                          br(),
                                          br(),
                                          
                                       )
                                       )
                                       )
                          )
                 )
)
                         
                          
                          


server <- function(input, output, session) {
    
    ##########
    ##ABOUT##
    ########
    
    #output earth logo on first page
    
    output$earth_pic <- renderImage({
        
        list(src = 'raw-data/earth.jpg',
             height = 300,
             width = 540,
             style = "display: block; margin-left: auto; margin-right: auto;")},
        deleteFile = FALSE
    )
    
    
    ########
    ##DATA##
    ########
    
    
    
    output$spending_by_income <- renderPlot({
        
        classified_spend %>% 
            group_by(income_group, year) %>% 
            summarize(mean = mean(the_per_cap_mean)) %>% 
            ggplot(aes(x = year, y = mean, color = income_group)) +
            geom_line() +
            geom_point() +
            theme_classic() +
            labs(title = "Average Total Health Spending per Person Worldwide",
                 subtitle = "Average Spending Stratified by Income Group of Each Country",
                 x = "Year",
                 y = "Spending (2018 USD)",
                 color = "Income Group of Country")
        
    })
    
    
    
    output$spending_by_reigion <- renderPlot({
        
        # looks at user selection to decide which plot to show
        
        if(input$income_group == "low+lower") {
            classified_spend %>% 
                filter(income_group %in% c("Low income", "Lower middle income")) %>% 
                ggplot(aes(x = year, y = the_per_cap_mean, color = income_group)) +
                geom_point(alpha = 0.3) +
                scale_color_manual(values = c("maroon", "turquoise1")) +
                geom_smooth(method = "lm", se = FALSE) +
                facet_wrap(~reigion) +
                theme_classic() +
                labs(title = "Average Total Spending Per Person in Low and Lower Middle Income Groups",
                     x = "Year",
                     y = "Spending (2018 USD)",
                     color = "Income Group of Each Country")
        } else if (input$income_group == "lower+upper") {
            classified_spend %>% 
                filter(income_group %in% c("Lower middle income", "Upper middle income")) %>% 
                ggplot(aes(x = year, y = the_per_cap_mean, color = income_group)) +
                geom_point(alpha = 0.3) +
                scale_color_manual(values = c("blue", "orange")) +
                geom_smooth(method = "lm", se = FALSE) +
                facet_wrap(~reigion) +
                theme_classic() +
                labs(title = "Average Total Spending Per Person in Lower Middle and Upper Middle Income Groups",
                     x = "Year",
                     y = "Spending (2018 USD)",
                     color = "Income Group of Each Country")
        } else if (input$income_group == "upper+high") {
            classified_spend %>% 
                filter(income_group %in% c("Upper middle income", "High income")) %>% 
                ggplot(aes(x = year, y = the_per_cap_mean, color = income_group)) +
                geom_point(alpha = 0.3) +
                scale_color_manual(values = c("steelblue1", "pink3")) +
                geom_smooth(method = "lm", se = FALSE) +
                facet_wrap(~reigion) +
                theme_classic() +
                labs(title = "Average Total Spending Per Person in Upper Middle and High Income Groups",
                     x = "Year",
                     y = "Spending (2018 USD)",
                     color = "Income Group of Each Country")
        }
        
    })
    
    
    output$info <- renderText({
        paste0("x=", input$plot_click$x, "\ny=", input$plot_click$y)
    })

    
    output$spending_single_reigion <- renderPlot({
        
        # looks at user selection to decide which plot to show
        
        if(input$income_group_2 == "low") {
            classified_spend %>% 
                filter(income_group == "Low income") %>% 
                group_by(year) %>% 
                summarize(tot = mean(the_per_cap_mean),
                          oop = mean(oop_per_cap_mean),
                          gov = mean(ghes_per_cap_mean),
                          ppp = mean(ppp_per_cap_mean),
                          dah = mean(dah_per_cap_mean)
                ) %>% 
                ggplot() +
                geom_line(aes(x = year, y = tot, color = "Total"), size = 1.3) +
                geom_line(aes(x = year, y = oop, color = "Out-of-Pocket"), size = 1.3)  +
                geom_line(aes(x = year, y = gov, color = "Government"), size = 1.3) +
                geom_line(aes(x = year, y = ppp, color = "Prepaid Private"), size = 1.3) +
                geom_line(aes(x = year, y = dah, color = "Foreign Development Assistance"), size = 1.3) +
                labs(title = "Distribution of Total Health Spending Per Person in Low Income Countries",
                     subtitle = "Revealing Trends in Component Health Spending Streams",
                     x = "Year",
                     y = "Average Spending Per Person (2018 USD)",
                     color = "Health Spending Type") +
                theme_classic() 
            
        } else if (input$income_group_2 == "lower") {
            classified_spend %>% 
                filter(income_group == "Lower middle income") %>% 
                group_by(year) %>% 
                summarize(tot = mean(the_per_cap_mean),
                          oop = mean(oop_per_cap_mean),
                          gov = mean(ghes_per_cap_mean),
                          ppp = mean(ppp_per_cap_mean),
                          dah = mean(dah_per_cap_mean)
                ) %>% 
                ggplot() +
                geom_line(aes(x = year, y = tot, color = "Total"), size = 1.3) +
                geom_line(aes(x = year, y = oop, color = "Out-of-Pocket"), size = 1.3)  +
                geom_line(aes(x = year, y = gov, color = "Government"), size = 1.3) +
                geom_line(aes(x = year, y = ppp, color = "Prepaid Private"), size = 1.3) +
                geom_line(aes(x = year, y = dah, color = "Foreign Development Assistance"), size = 1.3) +
                labs(title = "Distribution of Total Health Spending Per Person in Lower Middle Income Countries",
                     subtitle = "Revealing Trends in Component Health Spending Streams",
                     x = "Year",
                     y = "Average Spending Per Person (2018 USD)",
                     color = "Health Spending Type") +
                theme_classic()
            
        } else if (input$income_group_2 == "upper") {
            classified_spend %>% 
                filter(income_group == "Upper middle income") %>% 
                group_by(year) %>% 
                summarize(tot = mean(the_per_cap_mean),
                          oop = mean(oop_per_cap_mean),
                          gov = mean(ghes_per_cap_mean),
                          ppp = mean(ppp_per_cap_mean)
                ) %>% 
                ggplot() +
                geom_line(aes(x = year, y = tot, color = "Total"), size = 1.3) +
                geom_line(aes(x = year, y = oop, color = "Out-of-Pocket"), size = 1.3)  +
                geom_line(aes(x = year, y = gov, color = "Government"), size = 1.3) +
                geom_line(aes(x = year, y = ppp, color = "Prepaid Private"), size = 1.3) +
                labs(title = "Distribution of Total Health Spending Per Person in Upper Middle Income Countries",
                     subtitle = "Revealing Trends in Component Health Spending Streams",
                     x = "Year",
                     y = "Average Spending Per Person (2018 USD)",
                     color = "Health Spending Type") + 
                theme_classic()
            
        } else if (input$income_group_2 == "high") {
            classified_spend %>% 
                filter(income_group == "High income") %>% 
                group_by(year) %>% 
                summarize(tot = mean(the_per_cap_mean),
                          oop = mean(oop_per_cap_mean),
                          gov = mean(ghes_per_cap_mean),
                          ppp = mean(ppp_per_cap_mean)
                ) %>% 
                ggplot() +
                geom_line(aes(x = year, y = tot, color = "Total"), size = 1.3) +
                geom_line(aes(x = year, y = oop, color = "Out-of-Pocket"), size = 1.3)  +
                geom_line(aes(x = year, y = gov, color = "Government"), size = 1.3) +
                geom_line(aes(x = year, y = ppp, color = "Prepaid Private"), size = 1.3) +
                labs(title = "Distribution of Total Health Spending Per Person in High Income Countries",
                     subtitle = "Revealing Trends in Component Health Spending Streams",
                     x = "Year",
                     y = "Average Spending Per Person (2018 USD)",
                     color = "Health Spending Type") +
                theme_classic()
        }
        
    })
    
    
    output$info1 <- renderText({
        paste0("x=", input$plot_click1$x, "\ny=", input$plot_click1$y)
    })
    
    
    
    output$total_health_deaths <- renderPlot({
        
        options(scipen=999)
        cor1 <- spend_death %>%  
            filter(age == "All Ages") %>% 
            filter(sex == "Both") %>% 
            mutate(log_mean = log10(the_total_mean)) %>% 
            mutate(log_val = log10(val)) %>% 
            summarize(cor = cor(log_mean, log_val)) %>% 
            pull(cor) %>% 
            round(digits = 2)
        
        spend_death %>%  
            filter(age == "All Ages") %>% 
            filter(sex == "Both") %>% 
            ggplot(aes(x = the_total_mean, y = val)) +
            geom_point() +
            geom_smooth(method = lm, formula = y ~ x) +
            scale_x_log10() +
            scale_y_log10() +
            annotate("text", x = 50000, y = 5000000, label = paste0("Correlation: ", cor1)) +
            theme_classic() +
            labs(title = "Total Healthcare Spending per Country versus Deaths per Country",
                 subtitle = "Strong correlation observed on logarithmic scale",
                 x = "Total Health Spending (2018 USD)",
                 y = "Total Deaths from Diseases")
    })
    
    
    output$capita_health_deaths <- renderImage({
        outfile <- tempfile(fileext='.gif')
        
        p = spend_death %>% 
            filter(age == "All Ages") %>% 
            filter(sex == "Both") %>% 
            ggplot(aes(x = the_per_cap_mean, y = val)) +
            geom_point(color = "red") +
            scale_x_log10() +
            scale_y_log10() +
            geom_smooth(method = lm, formula = y ~ x) +
            theme_classic() +
            transition_manual(factor(income_group, levels = c("Low income", "Lower middle income", 
                                                              "Upper middle income", "High income"))) +
            labs(title = "Health Spending per Capita versus\n Deaths by Income Group: {current_frame}",
                 x = "Total Health Spending per Capita (2018 USD)",
                 y = "Total Deaths from Disease")
        
        anim_save("outfile.gif", animate(p))
        
        list(src = "outfile.gif",
             contentType = 'image/gif',
             width = 550,
             style = "display: block; margin-left: auto; margin-right: auto;"
        )}, deleteFile = TRUE)
        
    
    output$epi_trans <- renderImage({
        outfile <- tempfile(fileext='.gif')
        
        p = spend_death_cap %>%  
            filter(age == "All Ages") %>% 
            filter(sex == "Both") %>% 
            group_by(cause, income_group) %>% 
            summarize(sum = sum(death_cap)) %>% 
            ggplot(aes(x = cause, y = sum)) +
            geom_col() +
            theme_classic() +
            scale_x_discrete(labels = c('Communicable Diseases','Noncommunicable Diseases')) +
            transition_manual(factor(income_group, levels = c("Low income", "Lower middle income", 
                                                              "Upper middle income", "High income"))) +
            labs(title = "Epidemiological Transition: {current_frame}",
                 x = "Cause of Death",
                 y = "Proportion of Deaths")
        
        anim_save("outfile.gif", animate(p))
        
        list(src = "outfile.gif",
             contentType = 'image/gif',
             width = 550,
             style = "display: block; margin-left: auto; margin-right: auto;"
        )}, deleteFile = TRUE)
    
    
    output$health_spend_neutral <- renderPlot({
        spend_death %>% 
            filter(age == "All Ages") %>% 
            filter(sex == "Both") %>% 
            ggplot(aes(x = the_per_cap_mean, y = val)) +
            geom_point() +
            geom_smooth(method = lm, formula = y ~ x) +
            scale_x_log10() +
            scale_y_log10() +
            labs(title = "Health Spending per Capita versus Total Deaths in a Country",
                 x = "Total Health Spending per Capita (2018 USD)",
                 y = "Total Deaths from Disease") +
            theme_classic()
    })
    
    output$health_spend_age <- renderPlot({
        spend_death %>% 
            filter(sex == "Both") %>% 
            mutate(age = factor(age, levels = c("Under 5", "5-14 years", "15-49 years",
                                                "50-69 years", "70+ years", "All Ages"))) %>% 
            ggplot(aes(x = the_per_cap_mean, y = val)) +
            geom_point(alpha = 0.3) +
            geom_smooth(method = lm, formula = y ~ x, color = "red") +
            scale_x_log10() +
            scale_y_log10() +
            facet_wrap(~age) +
            theme_classic()  +
            labs(title = "Health Spending per Capita versus Total Deaths by Age Group",
                 x = "Total Health Spending per Capita (2018 USD)",
                 y = "Total Deaths from Disease")
    })
    
    output$info3 <- renderText({
        paste0("x=", input$plot_click3$x, "\ny=", input$plot_click3$y)
    })
    
    
    output$health_spend_gender <- renderPlot({
        spend_death %>% 
            filter(age == "All Ages") %>%
            ggplot(aes(x = the_per_cap_mean, y = val)) +
            geom_point(alpha = 0.3) +
            geom_smooth(method = lm, formula = y ~ x, color = "orange") +
            scale_x_log10() +
            scale_y_log10() +
            facet_wrap(~sex) +
            theme_classic() +
            labs(title = "Health Spending per Capita versus Total Deaths by Gender",
                 x = "Total Health Spending per Capita (2018 USD)",
                 y = "Total Deaths from Disease")
    })
    
    
    output$info4 <- renderText({
        paste0("x=", input$plot_click4$x, "\ny=", input$plot_click4$y)
    })
}


# Run the application 
shinyApp(ui = ui, server = server)

