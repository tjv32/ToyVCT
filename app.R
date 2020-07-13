library(shiny)
library(htmltools)
library(dplyr)
library(magrittr)
library(magick)
library(ggplot2)
library(formattable)
library(ggtext)


addline_format <- function(x,...){
  gsub('\\s','\n',x)
}


create_all_dot_plots <- function (final_img, input_percents) {
  i <- 1
  offsets <- c("+184+245", "+384+245", "+584+245", "+184+540", "+384+540", "+584+540")
  for(percent in input_percents) {
    img_plot <- image_graph(width = 90, height = 90, res = 96)
    print(create_waffle_plot(percent))
    dev.off()
    final_img <- image_composite(final_img, img_plot, offset = offsets[[i]])
    i <- i + 1
  }
  return(final_img)
}
create_all_log_plots <- function(final_img, events){
  i <- 1
  offsets <- c("+139+175", "+139+500")
  for(i in c(1,2)) {
    img_plot <- image_graph(width = 587, height = 210, res = 96)
    print(create_log_plot(events))
    dev.off()
    final_img <- image_composite(final_img, img_plot, offset = offsets[[i]])
    i <- i + 1
  }
  return(final_img)
}
  
create_log_plot <- function(events){
  
  i <- 0
  lolli_y <- c()
  lolli_x <- c()
  
  for (x in events[seq(1, 3)]) {
    i <- i + 1
    product <- as.numeric(x[3]) * 100
    lolli_x  = c(lolli_x, paste(x[1], x[2], paste(toString(product), "%", sep="")) )
    lolli_y  = c(lolli_y, log10(as.numeric(x[3])))
    lolli_df = data.frame(lolli_x, lolli_y)
  }
  lolli_x = c("<span style='font-size:11pt'>Sepal width vs. sepal length for three *Iris*
    species</span>","<span style='font-size:11pt'>Sepal width vs. sepal length for three *Iris*
                species</span>","<span style='font-size:11pt'>Sepal width vs. sepal length for three *Iris*
    species</span>")
  
  plot1 <- ggplot(lolli_df, aes(x = lolli_x, y = lolli_y)) +
    
    geom_segment(
      aes(
        x = lolli_x,
        xend = lolli_x,
        y = -4,
        yend = lolli_y
      ),
      color = "skyblue",
      size = 2
    ) +
    geom_point(color = "blue",
               size = 6,
               alpha = 1) +
    theme_light() +
    coord_flip() +
    theme(
      panel.grid.major.y = element_blank(),
      panel.border = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.y = element_blank(),
      axis.title.x = element_blank(),
      axis.text.y = element_markdown(),
      #axis.text.y = element_text(size = 8, color = "#1b1862", hjust = 0.5),
      axis.text.x = element_text(size = 12),
    )  +
    
    scale_y_continuous(
      position = "right",
      limits = c(-4, 0),
      label = function(x) {
        return(paste("1 in", 10 ^ (-1 * x)))
      }
    ) + 
    scale_x_discrete(labels = addline_format(lolli_x))
  
  return(plot1)
}
create_waffle_plot <- function (input_val) {
  i <- 0
  
  i <- i + 1
  rs <- c()
  cs <- c()
  num <- as.numeric(input_val)
  
  clrs <- c()
  for (v in seq(from = min(0), to = max(99))) {
    rs <- c(rs, v %% 10)
    cs <- c(cs, v %/% 10)
    clrs <- c(clrs, (v + 1) <= num)
  }
  df <- data.frame(rs, cs, clrs)
  as.integer(as.logical(df$clrs))
  
  
  plot <- ggplot(df, aes(x = rs, y = cs)) +
    geom_point(
      aes(fill = factor(clrs)),
      size = 1,
      colour = "black",
      shape = 21,
      stroke = 1,
    ) +
    theme_void() +
    theme(legend.position = "none") +
    scale_fill_manual(values = c("white", "black"))
  
  #theme(plot.caption = element_text(hjust=0.5, size=rel(1.2)))
  return(plot)
}
events <- c()
events[[1]] <- c("Pulmonary Complications", "Above Average", 0.125)
events[[2]] <- c("Bleeding/transfusion Complications", "Above Average", 0.091)
events[[3]] <- c("Cardiac Complications", "Average", 0.068)
events[[4]] <- c("Renal (Kidney) complications", "Above Average", 0.03)


create_log_image <- function (input_percents) {
  img1 <- image_read_svg('www/Background_2.svg')
  img2 <- image_read_svg('www/bot_small.svg')
  img3 <- image_read_svg('www/Background_3.svg')
  
  
  final_img <- image_composite(img1, img2)
  final_img <- image_composite(final_img, img3)
  
  final_img <- create_all_log_plots(final_img, input_percents)
  
  return(final_img)
}

  
create_dot_image <- function (input_percents) {
  img1 <- image_read_svg('www/Background_1.svg')
  img2 <- image_read_svg('www/bot_small.svg')
  img3 <- image_read_svg('www/Background_3.svg')
  
  final_img <- image_composite(img1, img2)
  final_img <- image_composite(final_img, img3)
  
  final_img <- create_all_dot_plots(final_img, input_percents)
  
  #out <- image_composite(out, fig, offset = "+190+250")
  
  #out <- image_annotate(final_plot1, "Renal (Kidney) Infection", size = 30, color = "purple")
  
return(final_img)}
  

ui <- shinyUI(
  mainPanel(
    sidebarLayout(
      
      sidebarPanel(
        downloadButton('downloadImage', 'Download modified image')
      ),
        
    imageOutput("myImage")
  ))
  
)

server <- shinyServer(function(input, output, clientData) {
  
  
  
  img1 <- image_read_svg('www/Background_1.svg')
  img2 <- image_read_svg('www/Background_3.svg')
  img3 <- image_read_svg('www/bot_small.svg')
  output$myImage <- renderImage({
    
    width  <- clientData$output_plot_width
    height <- clientData$output_plot_height
    mysvgwidth <- width/96
    mysvgheight <- height/96
    
    print('STARTING')
    final_plot <- create_dot_image(c(10,20,30,40,50,60))
    
    final_plot <- create_log_image(events)
    #print(plot_temp_file)
    #plot2 <- image_read(plot_temp_file)
    #plot2 <- image_resize(plot2, "100x100")

    #final_plot1 <- image_annotate(final_plot1, "Renal (Kidney) Infection", size = 30, color = "purple")
    print("MIDWay")
    tmpfile <- final_plot %>%
      image_write(tempfile(fileext='svg'), format = 'svg')
    
    # Return a list
    print("DONE")
    list(src = tmpfile, contentType = "image/svg+xml")
  })
  output$downloadImage <- downloadHandler(
    filename = "Modified_image.jpeg",
    contentType = "image/jpeg",
    content = function(file) {
      ## copy the file from the updated image location to the final download location
      plot2 <- create_waffle_plot(50)
      plot_temp_file <- tempfile(fileext='png')
      ggsave(plot=plot2, filename=plot_temp_file, device='png')
      final_plot <- create_plot()
      print(plot_temp_file)
      plot2 <- image_read(plot_temp_file)
      plot2 <- image_resize(plot2, "100x100")
      final_plot1 <- image_composite(final_plot, plot2, offset = "+190+250")
      tmpfile <- final_plot1 %>%
        image_write(tempfile(fileext='pdf'), format = 'pdf')
      file.copy(tmpfile, file)
    }
  ) 
})
shinyApp(ui = ui, server = server)