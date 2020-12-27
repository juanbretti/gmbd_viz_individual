# https://stackoverflow.com/questions/51440496/using-gganimate-to-export-gif
# https://steakrecords.com/es/772788-how-to-create-and-display-an-animated-gif-in-shiny-shiny-gif-gganimate.html

library(gapminder)
library(ggplot2)
library(shiny)
library(gganimate)
theme_set(theme_bw())

ui <- basicPage(
  imageOutput("plot1"))

server <- function(input, output) {
  
  output$plot1 <- renderImage({
    # now make the animation
    p <- ggplot(mtcars, aes(factor(cyl), mpg)) + 
      geom_boxplot() + 
      transition_states(
        gear,
        transition_length = 2,
        state_length = 1
      ) +
      enter_fade() + 
      exit_shrink() +
      ease_aes('sine-in-out')
    
    # Save the animation
    anim_save("outfile.gif", p)
    
    # Return a list containing the filename
    list(src = "outfile.gif",
         contentType = "image/gif"
         # width = 400,
         # height = 300,
         # alt = "This is alternate text"
    )}, deleteFile = TRUE)

  }

shinyApp(ui, server)