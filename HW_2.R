###################################################################################
######################################  1  ########################################
###################################################################################
# I will use Violin Plot,as it is useful for studying the relationship of a numeric
# variable for several groups. A violin plot is a method of plotting numeric data. 
# Violin plots are similar to box plots, they also show the probability density of
# the data at different values.
# Our Violin plot is about vehicle's mileage in city by their transmission types.
# In order to develop plots we need to get ggplot2 library.
###################################################################################

library("shiny")
library("ggplot2")
library("plotly")
 
# Here is some data for Violin Plot. We assign our data(mpg) to df.

df <- mpg
head(df)         # It selects the first 6 rows from data(the last 6 rows-tail(df))
colnames(df)     # Selects all column names

###################################################################################
# We use ggplot, where data is df, x axis is transmission type,y axis is mileage
# in city, and we want to differentiate colours by transmission type(fill = trans).
# Then we use geom_violin to get violin plot(alpha is for transparency,
# trim is false to avoid trimming the tails of the violin to the range of the data)
###################################################################################

violin_plot <- ggplot(df, aes(x= trans,y=cty,fill=trans))+
geom_violin(trim = FALSE,alpha = 0.8) 

###################################################################################
# For more accuracy, I have added labs function to set title,subtitle,caption and
# labels for x and y. But it is not encouraged to set titles in plots, as it can
# cause difficulties later
###################################################################################

violin_plot <- violin_plot +
labs(title="Violin plot", 
     subtitle="Mileage in city and Transmission types",
     caption="Source: mpg",
     x="Transmission types",
     y="Mileage in city")

###################################################################################
# In order to have preferably color shades one of the functions is shown below.
# We can change colours by changing palette name.
###################################################################################

violin_plot <- violin_plot +
  scale_fill_brewer(palette = "Spectral")

###################################################################################
# And also it is possible to choose theme for our plot. There are several themes
# and theme_dark is one of them.
###################################################################################

violin_plot <- violin_plot +
  theme_dark()

###################################################################################
# To get rid of grids we equalize panel.grid.major and minor to element_blank() 
# Y axis grid lines are horizontal and x axis grid lines are vertical.  
###################################################################################

violin_plot <- violin_plot +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())

###################################################################################
# We can add dotplot(for which we should define the value of y) 
# and/or boxplot to our violin plot(the width, color and transparency are defined)  
###################################################################################

violin_plot <- violin_plot +
  geom_dotplot(y = df$cty)

violin_plot <- violin_plot +
  geom_boxplot(width = 0.1,fill="black",alpha = 0.5)

###################################################################################
# In order to flip cartesian coordinates so that horizontal becomes vertical,
# and vertical becomes horizontal, we use coord_flip function   
###################################################################################

violin_plot <- violin_plot +
  coord_flip() 

###################################################################################
# After adding different values and features to our viloin plot we can plot it 
# by using plot function.
###################################################################################

plot(violin_plot)

#ggplotly(violin_plot)


###################################################################################
######################################  2  ########################################
###################################################################################

######################
# ui part starts here#
######################

ui<- fluidPage(
  titlePanel("My web application"),
  sidebarLayout(
##sidebar starts here
    sidebarPanel(
#row1
      fluidRow(
        column(5, 
               sliderInput("slider1","Please, choose a value",min=0,max=100,value=50)
          ),
        column(5,offset=2,  
               sliderInput("slider2","Please, choose a value",min=0,max=100,value=c(20,50))
          )
    ),
#row2
      fluidRow(
        column(12, 
              checkboxGroupInput("movies","Please, choose a movie",
                                 choices = list("Titanic","The Intouchables","Karate Kid","Leon"),
                                 selected = "Titanic")
          )
    ),
#row3
    fluidRow(
      column(12, 
             dateRangeInput("dates","Check-in/Check-out",
                            start = Sys.Date(),
                            end = Sys.Date() + 10,
                            min = Sys.Date(),
                            format = "dd/M/yy",
                            separator = " to ")
          )
    ),
#row4 
    fluidRow(
      column(12, 
             numericInput("num", "Select a number", min = 0, max = 100, value = 50)
  )
),
  p("Click the button to see the value!"),  
#row5 
    fluidRow(
      column(12, 
             actionButton("btn1","Click here",
                      style = "color:orange") #; background-color:pink; border-color: black")
  )
)
  ),
##mainPanel starts here
    mainPanel(
#row1
      fluidRow(
        column(6, 
             textOutput("output1")
          )
    ),
br(),
#row2
      fluidRow(
        column(6, 
             textOutput("output2")
          )
    ),
br(),
h4("Selected movies"),
#row3
        fluidRow(
        column(6, 
             tableOutput("output3")) 
    ), 
br(),
#row4
        fluidRow(
        column(6, 
              textOutput("output4"))
),
br(),
#row5
        fluidRow(
        column(6, 
              verbatimTextOutput("output5"))
)
  ),
position = ("right")
 )
)


##########################
# server part starts here#
##########################
server<- function(input,output){
  output$output1 <- renderText( paste("Current value is",input$slider1))
  output$output2 <- renderText( paste("Difference in values is",diff(input$slider2)))
  output$output3 <- renderTable(input$movies,rownames = TRUE, colnames =FALSE,
                                spacing = c( "m"),align ="c") 
  output$output4 <- renderText(paste("Difference -",diff.Date(input$dates)," days"))
  output5 <- eventReactive(input$btn1, {
    input$num
  })
  output$output5 <- renderText({
    output5()
  })
}


shinyApp(ui=ui,server = server)



###################################################################################
######################################  3  ########################################
###################################################################################

set.seed(42)
data <- data.frame(value=rnorm(1000))

plot1 <- ggplot(data = data,mapping=aes(x=value)) +
  geom_histogram(col =("#B2BEB5"), fill=rgb(0.68,0.85,0.90,0.4),binwidth = 0.5) 
  
plot1 <- plot1 +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
# we can use theme_classic() to remove grid lines
  
plot1 <- plot1 +
  labs(x="Value",y="Count",title ="My Histogram",subtitle = "First histogram",
       caption = "My caption for this histogram" )

plot1 <- plot1 + 
  geom_density(aes(y= 0.5 * ..count..),color=("#FF5F5F"))
  
plot(plot1)

#ggplotly(plot1)

