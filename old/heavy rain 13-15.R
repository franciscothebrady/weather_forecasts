
#Weather forecast offices by region and heavy rain frequency 
wfo <- as.data.frame(read.csv("WFO-freq.csv"))

#Grouped Barchart - had issues grouping by region so just went with year
ggplot(wfo, aes(YEAR , Freq)) + 
  geom_bar(aes(fill = region), stat = "identity", position = "dodge") +
 ggtitle("Heavy Rain Events by Year and Region") +
  theme(plot.title = element_text(hjust = 0.5))