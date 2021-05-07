library(tidytuesdayR)
library(tidyverse)
library(rworldmap)

# load data
tt_data <- tt_load("2021-05-04")

# attribute the dataset to tt
tt <- tt_data$water

# select interesting columns and drop NA, summarizing with count
df <- tt %>%
        select(status_id,water_source,country_name) %>%
        drop_na() %>%
        group_by(country_name, water_source, status_id) %>%
        summarize(count = n())

# put together data corresponding to Congo
unique(df$country_name)
df[df$country_name=="Congo - Brazzaville",]
df[df$country_name=="Congo - Kinshasa",]
df1 <- df
df1$country_name[df$country_name=="Congo - Kinshasa"] <- "Congo"
df1$count[df1$country_name=="Congo" & df1$water_source=="Borehole" & df$status_id=="n"] <- 
        df1$count[df1$country_name=="Congo" & df1$water_source=="Borehole" & df$status_id=="n"] + 
        df1$count[df1$country_name=="Congo - Brazzaville" & df1$water_source=="Borehole" & df$status_id=="n"]
df1$count[df1$country_name=="Congo" & df1$water_source=="Borehole" & df$status_id=="y"] <- 
        df1$count[df1$country_name=="Congo" & df1$water_source=="Borehole" & df$status_id=="y"] + 
        df1$count[df1$country_name=="Congo - Brazzaville" & df1$water_source=="Borehole" & df$status_id=="y"]
df1[df1$country_name=="Congo",]        

df1 <- df1 %>% filter(!(country_name=="Congo - Brazzaville"))

# clean water_sources data: 
unique(df1$water_source)
sum(df2$count[df2$water_source==unique(df2$water_source)[9]])/sum(df2$count)      
# put shallow well and spring (protected/undefined/unprotected) together, put all the sources with frequency <5% in "Other" category
df2 <- df1 %>% 
        mutate(water_source = replace(water_source,water_source == "Protected Shallow Well", "Shallow Well")) %>%
        mutate(water_source = replace(water_source,water_source == "Unprotected Shallow Well", "Shallow Well")) %>%
        mutate(water_source = replace(water_source,water_source == "Undefined Shallow Well", "Shallow Well")) %>%
        mutate(water_source = replace(water_source,water_source == "Unprotected Spring", "Spring")) %>%
        mutate(water_source = replace(water_source,water_source == "Protected Spring", "Spring")) %>%
        mutate(water_source = replace(water_source,water_source == "Undefined Spring", "Spring")) %>%
        mutate(water_source = replace(water_source,water_source == "Surface Water (River/Stream/Lake/Pond/Dam)", "Surface Water")) %>%
        mutate(water_source = replace(water_source,water_source == "Packaged water", "Other")) %>%
        mutate(water_source = replace(water_source,water_source == "Sand or Sub-surface Dam", "Other")) %>%
        mutate(water_source = replace(water_source,water_source == "Delivered Water", "Other")) %>%
        mutate(water_source = replace(water_source,water_source == "Rainwater Harvesting", "Other")) %>%
        mutate(water_source = replace(water_source,water_source == "Piped Water", "Other")) %>%
        group_by(country_name, water_source, status_id) %>%
        summarise(count=sum(count))

# doing a bar plot to summarize the data

g <- ggplot(df2,aes(x=water_source,y=count,fill=status_id)) +
        geom_bar(position="stack", stat="identity") +
        facet_wrap(~country_name,scales="free_y") +
        theme(legend.position="top",
              strip.text = element_text(size=8,face="bold"),
              plot.title = element_text(color="black", size=14, face="bold",hjust = 0.5),
              axis.title.x = element_blank(),
              axis.text.y = element_text(size=6),
              axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size=8)) +
        scale_fill_discrete(name = "Status", labels = c("without water", "undefined", "with water")) +
        ggtitle("Type of water sources and their water availability in some countries of Africa, America, and Asia")
ggsave(filename = "MyTT2021-05-04.png",width = 12, height = 8,device = "png")

  
     