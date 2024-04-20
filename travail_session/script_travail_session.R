rm(list=ls())
library(lubridate)
# install.packages("rvest")
library(readxl)
library(rvest)
library(tidyverse)
library(RSelenium)
library(ggthemes)
library(netstat)
library(wdman)
library(pingr)
library(modelsummary)
# site web

chromeCommand <- chrome(retcommand = T, verbose = F, check = F ) 
chromeCommand


rD <- RSelenium::rsDriver(browser = "chrome",
                          chromever = "123.0.6312.106",
                          verbose = FALSE)
 remDr <- rD[["client"]]

 remDr$navigate("https://www.premierleague.com/stats/top/players/goals")
 
 Sys.sleep(4)
# enlever cookies et publicités
 acceptCookies <- remDr$findElement(using = "css selector",
                                    value = "#onetrust-accept-btn-handler")
 acceptCookies$clickElement()
 
 Sys.sleep(2)
 
 closeAdd <- remDr$findElement(using = "css selector",
                               value = "#advertClose")
 closeAdd$clickElement()

 source <- remDr$getPageSource()[[1]]
 # Obtenir les données
 list_seasons <- read_html(source) %>% 
   html_nodes("ul[data-dropdown-list=FOOTBALL_COMPSEASON] > li") %>% 
   html_attr("data-option-name")
 
 
 season93 <- which(list_seasons == "1992/93")
 season24 <- which(list_seasons == "2023/24")
 list_seasons <- list_seasons[c(season93, season24)] 


data_seasons <- vector("list", length(list_seasons))
 
for (j in seq_along(list_seasons)){
  
 
  season <- remDr$findElement(using = "css selector", 
                                 value = ".current[data-dropdown-current=FOOTBALL_COMPSEASON]")
  season$clickElement()
  Sys.sleep(2)
  
  
  ELEMseason <- remDr$findElement(using = "css selector", value = str_c("ul[data-dropdown-list=FOOTBALL_COMPSEASON] > li[data-option-name='", list_seasons[[j]], "']"))
  ELEMseason$clickElement()
  Sys.sleep(2) 
  
  data_seasons[[j]] <- remDr$getPageSource()[[1]] %>% 
    read_html() %>% 
    html_table() %>% 
    .[[1]] %>% 
    as_tibble(.name_repair = "unique") %>%
    select(-ncol(.)) 
  
  # Avoir les informations pour chaque page
  btnNextExists <- remDr$getPageSource()[[1]] %>% 
  read_html() %>% 
  html_node(".paginationNextContainer.inactive") %>% 
  html_text() %>% 
  is.na()
   
   
  while (btnNextExists){
    
    btnNext <- remDr$findElement(using = "css selector",
                                 value = ".paginationNextContainer")
    btnNext$clickElement()
    Sys.sleep(2)
    
    
    table_n <- remDr$getPageSource()[[1]] %>% 
      read_html() %>% 
      html_table() %>% 
      .[[1]] %>% 
      as_tibble(.name_repair = "unique") %>% 
      select(-ncol(.))
    
    
    data_seasons[[j]] <- bind_rows(data_seasons[[j]], table_n)
    
    
    btnNextExists <- remDr$getPageSource()[[1]] %>% 
      read_html() %>% 
      html_node(".paginationNextContainer.inactive") %>% 
      html_text() %>% 
      is.na()
    
    Sys.sleep(1)
  }
  
}
view(data_seasons[[1]])

view(data_seasons[[2]])

merged_data <- bind_rows(data_seasons, .id = "Seasons")





team_data_2023 <- data_seasons[[2]] %>%
  group_by(Club) %>%
  summarise(
    TotalPlayers = n(),
    TotalNationalities = n(),
    LocalNationality = sum(Nationality == "England")  
  ) %>%
  
  mutate(Coefficient = (TotalNationalities - LocalNationality) / TotalPlayers) %>% 
  slice(-1)


data_couleurs <- data_frame(Club = unique(team_data_2023$Club),
                            couleurs = c("#EF0107", "#95BFE5", "#DA291C","#cd3529" , "#0057B8", "#6C1D45", "#034694", "#1B458F","#003399", "#000000", 
                                         "#C8102E", "#F78F1E", "#6CABDD", "#DA291C", "#241F20", "#DD0000", "#EE2737", "#132257","#7A263A", "#FDB913" ))

team_data_2023 <- merge(team_data_2023, data_couleurs, by.x = "Club", by.y = "Club", all.x = TRUE)

ggplot(data = team_data_2023, mapping = aes(x = Club, y = Coefficient, fill = couleurs)) +
  
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = scales::percent(Coefficient)), 
            position = position_dodge(width = 1), vjust = -0.5) +  
  scale_fill_identity() +
  scale_y_continuous(labels = scales::percent) +
  
  labs(title = "Les joueurs internationaux", 
       subtitle = "saison (2023-2024), par club",
       x = "Club", 
       y = "coefficient") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),  
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 0.5),
    axis.title.x = element_text(hjust = 0.4)
  )

ggplot(data = data_seasons[[1]], mapping = aes(x = Nationality)) +
  geom_bar(fill = "cyan") +
  labs(title = "Les joueurs internationaux", 
       subtitle = "saison (1992-1993)",
       x = "Nationaité", 
       y = "effectif") +
  coord_flip() +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),  
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 0.5),
    axis.title.x = element_text(hjust = 0.4)
  )

ggplot(data = data_seasons[[2]], mapping = aes(x = Nationality)) +
  geom_bar(fill = "cyan", position = position_dodge(width = 2)) +
  labs(title = "Les joueurs internationaux", 
       subtitle = "saison (2023-2024)",
       x = "Nationaité", 
       y = "effectif") +
  coord_flip() +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),  
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 0.5),
    axis.title.x = element_text(hjust = 0.4)
  )
  

data_ownership <- read_excel("travail_session/data_ownership.xlsx")


data_2023_merged <- merge(team_data_2023, data_ownership, by.x = "Club", by.y = "Club", all.x = TRUE ) %>% 
  select(c(Club, Coefficient, owner))

data_2023_merged <- data_2023_merged %>% mutate(owner = replace(owner, is.na(owner), "British"))

regression_owner <- lm(Coefficient ~ owner, data = data_2023_merged)

modelsummary(regression_owner)

summary(regression_owner)

#system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)
#pingr::ping_port("localhost", 4567)
# Bibliographie pour le code
# (https://www.appsilon.com/post/webscraping-dynamic-websites-with-r, 
# https://joshuamccrain.com/tutorials/web_scraping_R_selenium.html,)


