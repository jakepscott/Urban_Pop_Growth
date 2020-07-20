
# Loading Libs ------------------------------------------------------------


library(tidyverse)
library(readr)
library(ggridges)
library(gghalves)
library(ggrepel)
windowsFonts(`Roboto Condensed` = windowsFont("Roboto Condensed"))


# Loading Data ------------------------------------------------------------


City_Growth <- read_csv("data/Growth_of_Cities_Over_300k.csv")
Location_Codes <- read_csv("Data/Location_Codes.csv") %>% select(-X4,-X5)
City_Pop <- read_csv("Data/Pop_of_Cities_over_300k.csv")


# Cleaning Data -----------------------------------------------------------


City_Growth <- City_Growth %>% pivot_longer(names_to = "Date", cols = `1950-1955`:`2030-2035`,values_to="Percentage_Growth") %>% 
  rename("City"=`Urban Agglomeration`,"Country"=`Country or area`,"City_Code"=`City Code`,
         "Country_Code"=`Country Code`) %>% 
  mutate(Percentage_Growth=str_remove(Percentage_Growth,"   "),
         Percentage_Growth=as.numeric(Percentage_Growth)) %>% 
  filter(Date=="2015-2020")

City_Pop <- City_Pop %>% pivot_longer(names_to = "Year", cols = `1950`:`2035`,values_to="Pop_Thousands") %>% 
  rename("City"=`Urban Agglomeration`,"Country"=`Country or area`,"City_Code"=`City Code`,
         "Country_Code"=`Country Code`) %>% 
  mutate(Pop_Thousands=str_remove(Pop_Thousands," "),
         Pop=as.numeric(Pop_Thousands)*1000) %>% 
  filter(Year=="2015" | Year=="2020")
  
Interpolation <- City_Pop %>% 
  group_by(Country,City) %>% 
  summarise(Avg_Pop=mean(Pop))

City_Pop <- left_join(City_Pop,Interpolation,by=c("Country","City")) %>% select(c(Country,City,Avg_Pop)) %>% distinct()

# Joining Data ------------------------------------------------------------

City_Growth <- left_join(City_Growth,Location_Codes,by="Country")
City_Growth <- left_join(City_Growth,City_Pop,by=c("Country","City"))


# Further Cleaning --------------------------------------------------------

City_Growth <- City_Growth %>% mutate(City_Size=case_when(Avg_Pop<500000~"Very Small City",
                                           Avg_Pop>=500000 & Avg_Pop<1000000~"Small City",
                                           Avg_Pop>=1000000 & Avg_Pop<5000000~"Medium City",
                                           Avg_Pop>=5000000 & Avg_Pop<10000000~"Large City",
                                           Avg_Pop>=10000000~"Megacity")) %>% 
  group_by(Continent) %>% 
  mutate(Average_Growth_Continent=mean(Percentage_Growth,na.rm=T)) %>% 
  ungroup() %>% 
  group_by(Continent,City_Size) %>% 
  mutate(Average_Growth_Continent_City_Size=mean(Percentage_Growth,na.rm=T)) 

City_Growth$City_Size <- factor(City_Growth$City_Size,
          levels=c("Megacity","Large City","Medium City","Small City", "Very Small City"))

# Graphing ---------------------------------------------------------------
City_Growth %>% filter(Avg_Pop>=500000) %>%
ggplot(aes(x=fct_reorder(Continent,.x = -Average_Growth_Continent),y=Percentage_Growth,color = City_Size,group=City_Size)) +
  geom_hline(yintercept = 0, linetype="dashed") +
  geom_point(aes(size=fct_rev(City_Size)),alpha=.5,na.rm=TRUE, position=position_dodge(width=0.6)) +
  labs(title="Cities have grown much faster in Africa and Asia than elsewhere, \nregardless of city size",
       caption = "Plot: @jakepscott2020 | Data: New York Times, MIT Election Lab") +
  theme_minimal(base_family = "Roboto Condensed", base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = rel(1.3)),
        plot.subtitle = element_text(face = "plain", size = rel(1), color = "grey70"),
        plot.caption = element_text(face = "italic", size = rel(0.8), 
                                    color = "grey70"),
        panel.grid = element_blank(),
        plot.title.position = "plot", 
        axis.title = element_blank(),
        axis.text.y = element_text(),
        axis.text.x = element_blank(),
        legend.position = "none")



City_Growth %>% filter(Country=="Japan",
                       Avg_Pop>500000) %>%
  ggplot(aes(x=Avg_Pop,y=Percentage_Growth,color = City_Size,group=City_Size)) +
  geom_hline(yintercept = 0, linetype="dashed") +
  geom_point() +
  geom_label_repel(aes(label=City)) +
  labs(title="Cities have grown much faster in Africa and Asia than elsewhere, \nregardless of city size",
       caption = "Plot: @jakepscott2020 | Data: New York Times, MIT Election Lab") +
  theme_minimal(base_family = "Roboto Condensed", base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = rel(1.3)),
        plot.subtitle = element_text(face = "plain", size = rel(1), color = "grey70"),
        plot.caption = element_text(face = "italic", size = rel(0.8), 
                                    color = "grey70"),
        panel.grid = element_blank(),
        plot.title.position = "plot")
