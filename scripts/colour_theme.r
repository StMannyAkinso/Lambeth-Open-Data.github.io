#require(gglaplot)
require(ggplot2)

# Sequence of colurs to use in lambeth graphs. From the Lambeth style guide
lambeth_palette_full = c("#0057b8", "#993399","#066e6b","#333333","#ff9900","#000066",
                         "#08802f","#cccc00","#ff6600","#cc0033","#4c2c92","#006699","#09a9a4")

lambeth_palette_graph = c("#ff9900", "#09a9a4", "#000066", "#066e6b", "#cc0033", "#993399",   "#cccc00",
                          "#006699" ,"#4c2c92",  "#ff6600", "#08802f", "#333333")

lambeth_highlight = c("#ff9900", "#09a9a4")

#library(colorspace)
#demoplot(lambeth_palette_full)

show_colors <- function(colors) { 
  ggplot(data.frame(id=seq_along(colors), color=colors)) + 
    geom_tile(aes(id, 1, fill=color)) + 
    scale_fill_identity()
}

#show_colors(lambeth_palette_graph)


show_colors2 <- function(colors) { 
  ggplot(data.frame(id=seq_along(colors), color=colors)) + 
    geom_tile(aes(1, id, fill=color)) + 
    geom_text(aes(1, id, label=color)) + 
    scale_fill_identity()
}



#show_colors2(rev(lambeth_palette_full))

show_colors2(rev(lambeth_palette_graph))

# Compare against the gla colours from gglaplot (will not use, but nice to see what they are using)
#show_gla_pals(inc_div = F)

##### Remake the theme to incorporate Lambeth palette

#lam_graph <- theme_set(theme_bw())

#theme_set(lam_graph)

#Define gppr_theme() function

theme_lam <- function(){ 
  #font <- "sans"   # I set the font in ggplotly, so this is not necessary here
  
  theme_minimal() %+replace%    #replace elements we want to change
    
    theme(
      
      #grid elements
      panel.grid.major = element_blank(),    #strip major gridlines
      panel.grid.minor = element_blank(),    #strip minor gridlines
      axis.ticks = element_blank(),          #strip axis ticks
      
      #since theme_minimal() already strips axis lines, 
      #we don't need to do that again
      
      #text elements
      plot.title = element_text(             #title
        #family = font,            #set font family
        size = 20,                #set font size
        face = 'bold',            #bold typeface
        hjust = 0,                #left align
        vjust = 2),               #raise slightly
      
      plot.subtitle = element_text(          #subtitle
        #family = font,            #font family
        size = 14),               #font size
      
      plot.caption = element_text(           #caption
        #family = font,            #font family
        size = 9,                 #font size
        hjust = 1),               #right align
      
      axis.title = element_text(             #axis titles
        #family = font,            #font family
        size = 10),               #font size
      
      axis.text = element_text(              #axis text
        #family = font,            #axis family
        size = 9),                #font size
      
      axis.text.x = element_text(            #margin for axis text
        margin=margin(5, b = 10)),
      
      update_geom_defaults("line", list(size = 2))
      
      #since the legend often requires manual tweaking 
      #based on plot content, don't define it here
    )
}


theme_lam_axisx <- function(){ 
  #font <- "sans"   # I set the font in ggplotly, so this is not necessary here
  
  theme_minimal() %+replace%    #replace elements we want to change
    
    theme(
      
      #grid elements
      panel.grid.major = element_blank(),    #strip major gridlines
      panel.grid.minor = element_blank(),    #strip minor gridlines
      axis.ticks = element_blank(),          #strip axis ticks
      
      #since theme_minimal() already strips axis lines, 
      #we don't need to do that again
      
      #text elements
      plot.title = element_text(             #title
        #family = font,            #set font family
        size = 20,                #set font size
        face = 'bold',            #bold typeface
        hjust = 0,                #left align
        vjust = 2),               #raise slightly
      
      plot.subtitle = element_text(          #subtitle
        #family = font,            #font family
        size = 14),               #font size
      
      plot.caption = element_text(           #caption
        #family = font,            #font family
        size = 9,                 #font size
        hjust = 1),               #right align
      
      axis.title = element_text(             #axis titles
        #family = font,            #font family
        size = 10),               #font size
      
      axis.text = element_text(              #axis text
        #family = font,            #axis family
        size = 9),                #font size
      
      # contour strips to match panel contour
      #strip.background = element_rect(fill = "grey85", colour = "grey20"),
      
      axis.text.x = element_text( size = 10, #margin for axis text
        margin=margin(5, b = 10)),
      
      update_geom_defaults("line", list(size = 2))
      
      #since the legend often requires manual tweaking 
      #based on plot content, don't define it here
    )
}

lam_ggplotly_font <- list(
  family = "arial")

#lam_graph

# Margin options

m <- list(
  l = 50,
  r = 50,
  b = 100,
  t = 100,
  pad = 4
)
