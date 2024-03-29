
function(data, startyear, endyear, colors){
   yearsums <- data %>%
      group_by(year) %>%
      summarize(cnt = n())
   yscale <- max(yearsums$cnt, na.rm = TRUE) + 2
   ybreaks <- ifelse(yscale > 10,2,1)

   n_unique_names <- length(unique(data$name))
   n_unique_colors <- length(colors)
   colors <- rep(colors, ceiling(n_unique_names / n_unique_colors)) %>% unlist()

   names(colors) <- NULL
   colors <- colors[1:n_unique_names]
   #colors <- sample(colors,size = n_unique_names)

   ggplot(data) + 
      geom_histogram(aes(x = year, fill = name), 
                     binwidth = 1)+
      theme_classic()+
      theme(axis.line.y = element_blank(),
            axis.title.y = element_text(size = 10),
            axis.title.x = element_blank(),
            axis.text.x = element_text(angle = 45,
                                       hjust = 1),
            panel.grid.major.x = element_line(color = 'gray'),
            panel.grid.minor.x = element_line(color = 'light gray'),
            legend.position = 'bottom',
            legend.title = element_blank(),
            legend.key.size = unit(0.2,units = 'cm'),
            legend.spacing.x = unit(0.1,units = 'cm'),
            legend.margin = margin(t = 0.1,r = 0.1,b = 0.1,l = 0.1, unit = 'cm'),
            plot.margin = margin(t = 0.3,l = 0.1,r = 0.3,unit = 'cm')) +
      scale_y_continuous(expand = c(0,0),
                         breaks = seq(0,yscale,ybreaks),
                         limits = c(0,yscale)) +
      scale_x_continuous(expand = c(0,0),
                         breaks = seq(1989, 2018, 1),
                         limits = c(startyear - 1, 
                                    endyear + 1)) +
      scale_fill_manual(values = colors) +
      labs(y = '')
}
 
