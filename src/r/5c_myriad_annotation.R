#Create a separate DF of Myriad Genetics (MYGN) data from case 187
#The case 187 filter happens to be unnecessary because MYGN only appears under this case,
#but this filter is good practice because some symbols are present in multiple cases.
Myriad <- all_Abr %>%
  filter(ticker =="MYGN", case ==187)

#ggplot code
  #The "rect" annotation would be great to show information incorporation or something else spanning time.
  #We can use "segment" to draw a line from the exact line point to the
    #annotation text so it doesn't have to sit on the line. This will take some tweaking so I want to verify first.
Myriad_gg <- ggplot(Myriad, aes(x=event_period, y=cum_abr, group = 1)) +
  geom_line() +
  scale_x_discrete(breaks=c(7, 19, 31, 43, 55, 67, 79, 85, 97, 109, 121, 133, 145), 
                   labels = c("10:00", "11:00", "12:00", "1:00", "2:00", "3:00", "Day 1 \n Close",
                              "10:00", "11:00", "12:00", "1:00", "2:00", "3:00")) +   
  ggtitle("Myriad Genetics June 12-13, 2013") +
  annotate("text", x = 3, y = -0.02, label = "Decision Announced \n 10:00 am ET", 
         angle = 30, fontface = "italic", hjust = -.01, color = "blue") + 
  annotate("text", x = 27, y = 0.095056, label = "Something Happened \n 11:40 am ET", 
           fontface = "italic", color = "blue") +
  annotate ("text", x = 125, y = -0.1, label = "Somethine Else Happened \n 1:20 pm ET",
            fontface = "italic", color = "blue") +
  annotate ("text", x = 136, y = -0.15, label = "Yet Another Thing Happened \n 2:15 pm ET",
            fontface = "italic", color = "blue") + 
  annotate("rect", xmin = 54, xmax = 83, ymin = -0.18, ymax = 0.06, alpha = .2) +
      #alpha controls the box shading
  annotate("segment", x = 40, xend = 42, y = 0.06, yend = 0.1, color = "red")


   


#Base Plot

  
#Add Custom x-axis Labels
Myriad_gg <- Myriad_gg + 

#Add Title
Myriad_gg <- Myriad_gg + 

#Add Annotations with Separate "annotate" calls rather then combining them into one large call
  #Optional Annotation Calls: 
    #fontface = "italic" (Bold + others available)
    #color
    #Angle: Somewhat necessary to keep text from running off graph
    #\n: Line break syntax for the label


#Choosing an approximate X-value: x= row number in "Myriad" dataframe 
  #that corresponds to the date/time you want from column: "timestamp"


#For Annoatation 1, had to play around with x, y, and  hjust values since its close to edge.

Myriad_gg <- Myriad_gg + 
  annotate("text", x = 2, y = -0.018, label = "Decision Announced \n 10:00 am ET", #Annotation 1
           angle = 30, fontface = "italic", hjust = -.01, color = "blue") + #hjust and angle keep text on graph
  annotate("text", x = 27, y = 0.095056, label = "Something Happened \n 11:40 am ET", #Annotation 2
           fontface = "italic", color = "blue") +
  annotate ("text", x = 125, y = -0.1, lable = "Somethine Else Happened \n 1:20 pm ET",
            fontface = "italic", color = "blue") +
  annotate ("text", x = 136, y = -0.15, lable = "Yet Anotherthing Happened \n 2:15 pm ET",
            fontface = "italic", color = "blue") 


  

0.095056
#Annotation 2: 11:40

Myriad_gg + annotate("text", x =="2013-06-13 11:40:00", y = -0.018, label = "Something Happened", 
                     angle = 30, fontface = "italic", hjust = -.01, color = "blue")










