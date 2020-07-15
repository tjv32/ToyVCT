library(shiny)
library(htmltools)
library(dplyr)
library(magrittr)
library(magick)
library(ggplot2)
library(formattable)
library(ggtext)


functional_levels = c("Independent", "Partially Independent", "Totally Dependent")
names(functional_levels) = c("0", "1", "2")

patient_levels = c("ASA 1: Normal healthy patient", "ASA 2: Patient with mild systemic disease", "ASA 3: Patient with severe systemic disease",
                   "ASA 4: Patient with severe systemic disease that is a constant threat to life")
names(patient_levels) = c("1", "2", "3", "4")

create_waffle_and_caption = function(input_row){
  print(input_row)
  print("ah")
  value = as.numeric(input_row[[3]]) * 100
  type = as.character(input_row[[1]])
  av = input_row[[2]]
  print(value)
  #print(type)
  #print(av)
  plot <- image_graph(width = 90, height = 90, res = 72)
  print(create_waffle_plot(value))
  dev.off()
  white_left = image_blank(width = 53, height= 90, color = "none")
  white_right = image_blank(width = 53, height= 90, color = "none")
  plot = image_append(c(white_left, plot, white_right))
  text = image_blank(width = 196, height= 60, color = "none")
  text = image_annotate(text, type, font = 'Trebuchet', size = 15, gravity = "center",  location = "+0-17" )
  text = image_annotate(text, av, font = 'Trebuchet', style = "oblique", size = 12, gravity = "center",  location = "+0+0" )
  text = image_annotate(text, paste(format(round(value, 1), nsmall = 1), "%", sep = ""), font = 'Trebuchet', size = 14, gravity = "center",  location = "+0+15" )
  plot2 = image_append(c(plot, text), stack = TRUE)
  return (plot2)
}
bar_strength_cutoffs <- c(0, .01, .10, .20, .30, .50)
get_bar_img <- function(value) {
  return (paste("www/bar_imgs/bar", max(
    which(bar_strength_cutoffs <= as.numeric(value)) - 1
  ), ".png", sep = ""))
}
create_bar_and_caption = function(input_row){
  print(input_row)
  print("ah")
  value = as.numeric(input_row[[3]])
  type = as.character(input_row[[1]])
  av = input_row[[2]]
  print(value)
  #print(type)
  #print(av)
  plot <- image_read(get_bar_img(value))
  plot <- image_scale(plot,"90x90")
  value = value * 100
  white_left = image_blank(width = 53, height= 90, color = "none")
  white_right = image_blank(width = 53, height= 90, color = "none")
  plot = image_append(c(white_left, plot, white_right))
  text = image_blank(width = 196, height= 60, color = "none")
  text = image_annotate(text, type, font = 'Trebuchet', size = 15, gravity = "center",  location = "+0-17" )
  text = image_annotate(text, av, font = 'Trebuchet', style = "oblique", size = 12, gravity = "center",  location = "+0+0" )
  text = image_annotate(text, paste(format(round(value, 1), nsmall = 1), "%", sep = ""), font = 'Trebuchet', size = 14, gravity = "center",  location = "+0+15" )
  plot2 = image_append(c(plot, text), stack = TRUE)
  return (plot2)
}
create_waffle_ggplot <- function(params, final_image) {
  upperplots = image_append(c(create_waffle_and_caption(params$event_data[1, ]),
                              create_waffle_and_caption(params$event_data[2, ]),
                              create_waffle_and_caption(params$event_data[3, ])
  ))
  bottomplots = image_append(c(create_waffle_and_caption(params$event_data[4,]),
                               create_waffle_and_caption(params$event_data[5,]),
                               create_waffle_and_caption(params$event_data[6,])
  ))
  final_image = image_composite(final_image, upperplots, offset = "+132+242")
  final_image = image_composite(final_image, bottomplots, offset = "+132+536")
  
  return(final_image)
}

create_dot_ggplot <- function(params, final_image) {
  upperplots = image_append(c(create_bar_and_caption(params$event_data[1, ]),
                              create_bar_and_caption(params$event_data[2, ]),
                              create_bar_and_caption(params$event_data[3, ])
  ))
  bottomplots = image_append(c(create_bar_and_caption(params$event_data[4,]),
                               create_bar_and_caption(params$event_data[5,]),
                               create_bar_and_caption(params$event_data[6,])
  ))
  final_image = image_composite(final_image, upperplots, offset = "+132+242")
  final_image = image_composite(final_image, bottomplots, offset = "+132+536")
  
  return(final_image)
}

create_logarithmic_ggplot <- function(params, image){
  i <- 1
  offsets <- c("+139+175", "+139+522")
  for(i in c(1,2)) {
    img_plot <- image_graph(width = 587, height = 210, res = 96)
    print(create_log_plot(params$event_data))
    dev.off()
    image <- image_composite(image, img_plot, offset = offsets[[i]])
    i <- i + 1
  }
  return(image)
}

create_log_plot <- function(events){
  i <- 0
  lolli_y <- c()
  lolli_x <- c()
  print(events[[1]])
  print(class(events))
  print("events")
  for (row_num in 1:3) {
    x = events[row_num, ]
    i <- i + 1
    product <- as.numeric(x[[3]]) * 100
    lolli_x  = c(lolli_x, paste(x[[1]], x[[2]], paste(toString(product), "%", sep="")) )
    lolli_y  = c(lolli_y, log10(as.numeric(x[[3]])))
    lolli_df = data.frame(lolli_x, lolli_y)
  }
  lolli_x = c("<p style='line-height:10;color:#1b1862;font-size:8pt'><b>Pulmonary (Lung) Complications</b><br>
<b>Above Average</b><br>
<b>5.0%</b></p>",
              
              "<p style='line-height:10;color:#1b1862;font-size:8pt'><b>Cardiac (Heart) Complications</b><br>
<b>Above Average</b><br>
<b>5.0%</b></p>","<p style='line-height:10;color:#1b1862;font-size:8pt'><b>Renal (Kidney) Complications</b><br>
<b>Above Average</b><br>
<b>5.0%</b></p>")
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
      axis.text.y = element_markdown(hjust = 0.5),
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

plot_function_dict <- c(create_waffle_ggplot, create_dot_ggplot, create_logarithmic_ggplot)
names(plot_function_dict) <- c("waffle", "bar", "logarithmic")

generate_final_image <- function(params) {
  plot_function <- plot_function_dict[[params$plot_type]]
  basic_image <- generate_basic_image(params)
  final_image <- plot_function(params, basic_image)
  return(final_image)
}
add_profile <- function(params, basic_image) {
  info_font_size = 12 
  temp_blank = image_blank(width = image_info(basic_image)[['width']][[1]], height= image_info(basic_image)[['height']][[1]], color = "none")
  
  temp_blank = image_annotate(temp_blank, "Patient Age:",  location = "+200+55", font = 'Trebuchet', style = "oblique", size = info_font_size, gravity = "northeast") 
  temp_blank = image_annotate(temp_blank, params$age,  location = "+200+70", font = 'Trebuchet', style = "oblique", size = info_font_size, gravity = "northeast") 
  
  temp_blank = image_annotate(temp_blank, "ASA Class:",  location = paste("+200+",55 + 15 * 2, sep=""), font = 'Trebuchet', style = "oblique", size = info_font_size, gravity = "northeast") 
  temp_blank = image_annotate(temp_blank, patient_levels[[params$asa_level]],  location = paste("+200+",55 + 15 * 3, sep=""), font = 'Trebuchet', style = "oblique", size = info_font_size, gravity = "northeast") 
  

  temp_blank = image_annotate(temp_blank, "Functional Status:",  location = paste("+200+",55 + 15 * 4, sep=""), font = 'Trebuchet', style = "oblique", size = info_font_size, gravity = "northeast") 
  temp_blank = image_annotate(temp_blank, functional_levels[[params$functional_level]],  location = paste("+200+",55 + 15 * 5, sep=""), font = 'Trebuchet', style = "oblique", size = info_font_size, gravity = "northeast") 
  
  temp_blank = image_annotate(temp_blank, "Surgeon Specialty:",  location = paste("+50+",55 + 15 * 0, sep=""), font = 'Trebuchet', style = "oblique", size = info_font_size, gravity = "northeast") 
  temp_blank = image_annotate(temp_blank, params$specialty,  location = paste("+50+",55 + 15 * 1, sep=""), font = 'Trebuchet', style = "oblique", size = info_font_size, gravity = "northeast") 
  
  emergency = "Non-Emergancy"
  if(params$emergency == "1"){
    emergency = "Emergency"
  }
  temp_blank = image_annotate(temp_blank, "Emergancy Case:",  location = paste("+50+",55 + 15 * 2, sep=""), font = 'Trebuchet', style = "oblique", size = info_font_size, gravity = "northeast") 
  temp_blank = image_annotate(temp_blank, emergency,  location = paste("+50+",55 + 15 * 3, sep=""), font = 'Trebuchet', style = "oblique", size = info_font_size, gravity = "northeast") 
  
  in_out = "Inpatient"
  if(params$in_out_patient == "0"){
    in_out = "Outpatient"
  }
  temp_blank = image_annotate(temp_blank, "In/Out Patient:",  location = paste("+50+",55 + 15 * 4, sep=""), font = 'Trebuchet', style = "oblique", size = info_font_size, gravity = "northeast") 
  temp_blank = image_annotate(temp_blank, in_out,  location = paste("+50+",55 + 15 * 5, sep=""), font = 'Trebuchet', style = "oblique", size = info_font_size, gravity = "northeast") 

  basic_image = image_composite(basic_image, temp_blank)
  return(basic_image)
}
generate_basic_image <- function(params){
  #start with the background (log background vs dot and bar background)
  print("ok")
  if(params$plot_type == 'logarithmic'){
    basic_image <- image_read_svg('www/Background_2.svg')
  }
  else {
    basic_image <- image_read_svg('www/Background_1.svg')
  } 
  
  #add the appropriately sized arrows to the image
  basic_image <- image_composite(basic_image, image_read_svg(params$top_arrow))
  basic_image <- image_composite(basic_image, image_read_svg(params$middle_arrow))
  basic_image <- image_composite(basic_image, image_read_svg(params$bot_arrow))

  #add the discharge destination risk scores
  temp_blank = image_blank(width = image_info(basic_image)[['width']][[1]], height= image_info(basic_image)[['height']][[1]], color = "none")
  temp_blank = image_annotate(temp_blank, paste(format(round(params$destination_home, 1), nsmall = 1), "%", sep = ""),  font = 'Trebuchet', size = 24,location = "+890+290")
  temp_blank = image_annotate(temp_blank, paste(format(round(params$destination_readmit, 1), nsmall = 1), "%", sep = ""), font = 'Trebuchet', size = 24, location = "+890+433") 
  temp_blank = image_annotate(temp_blank, paste(format(round(params$destination_death, 1), nsmall = 1), "%", sep = ""),  font = 'Trebuchet', size = 24, location = "+890+576")
  basic_image = image_composite(basic_image, temp_blank)
  
  #add the patient information to top right of image
  basic_image = add_profile(params, basic_image)
  #insert cover over SURGERY bubble
  cover_image <- image_read_svg('www/Background_3.svg')
  basic_image <- image_composite(basic_image, cover_image)

  
  return(basic_image)
  }

add_ggplots <- function(plot_function){
  
}

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
      size = 2,
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
  
  
  
  output$myImage <- renderImage({
    
    
    load("fname.RData")
    params <- c(params, "plot_type" = "bar")
    params[["bot_arrow"]] = "www/bot1.svg"
    params[["middle_arrow"]] = "www/middle5.svg"
    params[["top_arrow"]] = "www/top2.svg"
    params[['event_data']][['V3']][[1]] = 0.2
    print(params)
    print(params$event_data)
    final_plot <- generate_final_image(params)
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