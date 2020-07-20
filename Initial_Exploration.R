
# Loading Libs ------------------------------------------------------------


library(tidyverse)
library(readr)
library(ggridges)
library(gghalves)


# Loading Data ------------------------------------------------------------


City_Growth <- read_csv("data/Growth_of_Cities_Over_300k.csv")
Location_Codes <- read_csv("Data/Location_Codes.csv") %>% select(-X4,-X5)
City_Pop <- read_csv("Data/Pop_of_Cities_over_300k.csv")


# Cleaning Data -----------------------------------------------------------


City_Growth <- City_Growth %>% pivot_longer(names_to = "Date", cols = `1950-1955`:`2030-2035`,values_to="Percentage_Growth") %>% 
  rename("City"=`Urban Agglomeration`,"Country"=`Country or area`,"City_Code"=`City Code`,
         "Country_Code"=`Country Code`) %>% 
  mutate(Percentage_Growth=as.numeric(Percentage_Growth)) %>% 
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

City_Growth <- City_Growth %>% mutate(City_Size=case_when(Avg_Pop>=500000 & Avg_Pop<1000000~"Small City",
                                           Avg_Pop>=1000000 & Avg_Pop<5000000~"Medium City",
                                           Avg_Pop>=5000000 & Avg_Pop<10000000~"Large City",
                                           Avg_Pop>=10000000~"Megacity")) %>% 
  group_by(Continent) %>% 
  mutate(Average_Growth_Continent=mean(Percentage_Growth,na.rm=T)) %>% 
  ungroup() %>% 
  group_by(Continent,City_Size) %>% 
  mutate(Average_Growth_Continent_City_Size=mean(Percentage_Growth,na.rm=T)) 

City_Growth$City_Size <- factor(City_Growth$City_Size,
          levels=c("Megacity","Large City","Medium City","Small City"))

# Graphing ---------------------------------------------------------------
City_Growth %>% filter(Avg_Pop>=500000) %>%
ggplot(aes(x=fct_reorder(Continent,.x = Average_Growth_Continent),y=Percentage_Growth)) +
  geom_half_point(aes(color = Continent), side = "l", size = 0.5) +
  geom_half_violin(aes(fill = Continent), side = "r",draw_quantiles=T) +
  guides(color = FALSE , fill = FALSE) + 
  coord_flip()

City_Growth %>% filter(Avg_Pop>=500000) %>%
ggplot(aes(x=fct_reorder(Continent,.x = Average_Growth_Continent),y=Percentage_Growth,color = City_Size,group=City_Size)) +
  geom_point(aes(size=fct_rev(City_Size)),na.rm=TRUE, position=position_dodge(width=0.3))



