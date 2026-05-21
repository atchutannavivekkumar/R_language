
# Load necessary libraries
library(dplyr)

library(tidyverse)

#load dataset and  Removing # comments
# as the csv file is seperated with semicolon (;)

datavolcano <-  read.csv("significant-volcanic-eruption-database.csv",header = FALSE, sep = ";" ,comment.char = "#")
view(datavolcano)

#to add row nuumbers in dataset
datavolcano$RowNumber <- seq_len(nrow(datavolcano)) 

#to save the file path and csv file in local folder
save_path <- "/Users/vivekkumar/Documents/r/aitproject/updated_volanno.csv"  

write.csv(datavolcano,"/Users/vivekkumar/Documents/r/aitproject/updated_volanno.csv", row.names = FALSE)

median_vei <- median(updated_volanno$`Volcanic Explosivity Index`, na.rm = TRUE)

updated_volanno[is.na(updated_volanno$`Volcanic Explosivity Index`),"Volcanic Explosivity Index"] <- median_vei

median_deaths <- median(updated_volanno$`Volcano:Deaths`, na.rm = TRUE)

updated_volanno[is.na(updated_volanno$`Volcano:Deaths`),"Volcano:Deaths"] <- median_deaths

names(updated_volanno) [10] <- "VolcanoType"
names(updated_volanno) [12] <- "VolcanicExplosivityIndex"
names(datavolcano) [13] <- "Volcanicdeaths"



ggplot(updated_volanno,aes(x=VolcanoType,y=Country)) +
  geom_point(shape =21 ,fill ="blue",size =3.7,colour ="black")+
  labs(x="Types of Volcanos",
       y="Country's of volcano erruption",
       title="Relation between volcano types and their countries") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(updated_volanno,aes(x=VolcanoType,y=Elevation)) +
  geom_point(shape =21 ,fill ="blue",size =3.7,colour ="black")+
  labs(x="Types of Volcanos",
       y="Country's of volcano erruption",
       title="Relation between volcano types and their countries") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(updated_volanno,aes(x=VolcanicExplosivityIndex,y=VolcanoType)) +
  geom_point(shape =21 ,fill ="blue",size =3.7,colour ="black")+
  labs(x="Types of Volcanos",
       y="Country's of volcano erruption",
       title="Relation between volcano types and their countries") +
  theme_minimal()

ggplot(updated_volanno, aes(x=VolcanicExplosivityIndex, y=Volcanicdeaths)) + 
  geom_point(shape =21 ,fill ="blue",size =3.7,colour ="black")+
  geom_smooth(method = "lm", se = FALSE, color = "red")

ggplot(updated_volanno, aes(x = Country, y =VolcanicExplosivityIndex, color = Elevation))+
  geom_point() +
  labs(title = "Elevation of Volcanoes by Country and VEI",
       x = "Country", y = "Elevation (m)", color= "Elevation") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))









