install.packages('corrplot')
install.packages('dplyr')
install.packages('gdata')
install.packages('plotly')
library(gdata)
library(corrplot) 
library(dplyr)
library(plotly)
library(plotly)


mydata = read.csv("data/all_together_2nd.csv",header = TRUE,sep=",")
df2 <- subset(mydata, select = c( 5, 6,7,8,9,10,11))
M <- cor(df2) # get correlations
corrplot(M, method = "number", type = 'upper') #plot matrix
help(corrplot)

sapply(df2, mean, na.rm=TRUE)
sapply(df2, sd, na.rm=TRUE)

colnames(mydata)

tab %>%
  group_by(month, variable) %>%
  summarise(a_sum=sum(amount),
            a_mean=(mean(amount)))



mydata_mean = mydata %>%
  group_by(mydata$Address,mydata$Address)%>%
  summarise(Taste_Quantity= mean(Taste_Quantity),
            Price = mean(Price),
            Service_Speed =mean(Service_Speed),
            Eating_option = mean(Eating_option),
            Loyalty_cards = mean(Loyalty_cards),
            hospitality = mean(hospitality),
            Place_4_Hangout =mean(Place_4_Hangout)
            )
colnames(mydata_mean)[1] <- "Address"
colnames(mydata_mean)
mydata_mean$Score <- rowSums(mydata_mean[, -1])

help(plot_ly)
# parcoords
p <- mydata_mean %>%
  plot_ly(width = 1000, height = 600) %>%
  add_trace(type = 'parcoords', 
            line = list(color = mydata_mean$Address,
                        label=mydata_mean$Address,
                        #hoverlabel=~Address,
                        colorscale = 'Jet',
                        showscale = TRUE,
                        reversescale = TRUE,
                        cmin = -0,
                        cmax = -100),  
            
          dimensions = list( 
            list(range = c(1,10),
                 label = 'Taste_Quantity', values = ~Taste_Quantity),
            list(range = c(1,10),
                 label = 'Price', values = ~Price),
            list(range = c(1,10),
                 label = 'Service_Speed', values = ~Service_Speed),
            list(range = c(1,10),
                 label = 'Eating_option', values = ~Eating_option)
          )
  )
p


ggplot(mydata_mean) + 
  geom_line(aes(x = (mydata_mean$Taste_Quantity), 
                y = mydata_mean$Taste_Quantity, 
                group = mydata_mean$Taste_Quantity, 
                color = mydata_mean$Address))

install.packages('GGally')
library(GGally)
help(ggparcoord)
ggparcoord(mydata_mean, 
           columns=c(2:9), 
           groupColumn=1, 
           splineFactor=TRUE,
           title="",
           #alphaLines = mydata_mean$Address,
           order = "anyClass",
           scale ="uniminmax", scaleSummary = "median",
           showPoints =TRUE
           
           )
+theme(panel.grid.major.x=element_line(colour="grey10"))






