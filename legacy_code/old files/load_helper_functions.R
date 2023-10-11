###############################################################
# Code to read in inflation data from CPS website and begin analysis.
# This loads in several functions for analysis and graph creation.
# Mike Konczal
# Last updated 3/12/22
################# SECTION 2: CREATE HELPER FUNCTIONS FOR THE ANALYSIS ####################

shorten_item_names <- function(cpi_names){
  cpi_names <- str_replace_all(cpi_names, "New and used motor vehicles", "New and used cars")
  cpi_names <- str_replace_all(cpi_names, "Commodities less food and energy commodities", "Core Goods")
  #cpi_names <- str_replace_all(cpi_names, "Education and communication commodities", "Education and communication goods")
  #cpi_names <- str_replace_all(cpi_names, "Transportation commodities less motor fuel", "Transportation less fuel")
  return(cpi_names)
}

#Using monthly, weighted change, draw a line graph.
monthly_graph <- function(cpi, item_basket, Gdate="2019-01-01", graph_title="Monthly Inflation by Items, Annualized"){
  library(scales)
  Gcpi <- cpi %>%
    filter(item_name %in% item_basket) %>%
    filter(date >= Gdate)
  
  ggplot(data = Gcpi, aes(x=date, y=Wchange1a, color=item_name)) +
    geom_line(size=1.2) +
    theme_minimal() +
    labs(title = graph_title, x="", y="", color="") +
    theme(
      plot.title.position = "plot",
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      plot.title = element_text(size = 25),
      plot.subtitle = element_text(size = 18, colour = "darkred", face = "italic"),
      plot.caption = element_text(size = 12, lineheight = 1.2),
      legend.position = "top") +
    scale_y_continuous(labels = percent) +
    geom_text(aes(x=date, y=Wchange1a, label=(round(Wchange1a, 2))), nudge_y = .001, color = "steelblue", check_overlap = TRUE)
}


#This draws a chart with the last month, previous 3 months, and previous 12 months, all present.
Three_past_plot <- function(cpi, item_basket, title = ""){
  cpi1 <- cpi %>% filter(item_name %in% item_basket) %>% filter(date == max(date))
  cpi1 <- cpi1 %>% ungroup() %>% arrange(Wchange1) %>% mutate(g2_item_name=factor(item_name, Wchange1))
  
  MaxMonth <- format(cpi$date[1], "%B")
  MaxYear <- format(cpi$date[1], "%Y")
  #Full_Title <- paste(title, " Contribution to CPI Inflation, <span style = 'color:#8b0000;'>", MaxMonth, ", ", MaxYear, "</span>, the <span style = 'color:#F44336;'>Last Three Months</span> and <span style = 'color:#a188de;'>Twelve Months</span>, all Annualized", sep="")
  
  ggplot(cpi1) +
    geom_segment( aes(x=g2_item_name, xend=g2_item_name, y=Wchange1a, yend=Wchange12), color="grey") +
    geom_segment( aes(x=g2_item_name, xend=g2_item_name, y=Wchange1a, yend=Wchange3a), color="grey") +  
    geom_point( aes(x=g2_item_name, y=Wchange1a), color="#8b0000", size=3 ) +
    geom_point( aes(x=g2_item_name, y=Wchange3a), color="#F44336", size=3 ) +
    geom_point( aes(x=g2_item_name, y=Wchange12), color="#a188de", size=3 ) +
    coord_flip()+
    theme_light() +
    theme(panel.grid.major.x = element_blank(),
          plot.title = element_markdown()) +
    ggtitle(Full_Title) +
    theme(
      panel.grid.major.y = element_blank(),
      panel.border = element_blank(),
      plot.title.position = "plot",
      axis.ticks.y = element_blank()
    ) +
    xlab("") +
    ylab("") +
    geom_hline(yintercept=0, linetype="solid", color = "black", alpha=0.5) +
    labs(caption = "Data: BLS, CPI; Author's Calculations. @rortybomb")
}


Two_past_plot <- function(cpi, item_basket, title = "This Compares Two Values", subtitle = "You can see the change"){
  cpi <- cpi %>% filter(item_name %in% item_basket) %>% filter(date == max(date)) %>% mutate(Wchange3a = Wchange3a*100, Wchange12 = Wchange12*100)
  cpi <- cpi %>% ungroup() %>% mutate(item_name = shorten_item_names(item_name)) %>% arrange(Wchange3a) %>% mutate(g2_item_name=factor(item_name, item_name))
  
  MaxMonth <- format(cpi$date[1], "%b")
  MaxYear <- format(cpi$date[1], "%Y")
  Full_Title <- paste(title, ", <span style = 'color:#F44336;'>Last Three Months</span> and <span style = 'color:darkblue;'>Twelve Months</span> from ", MaxMonth, ", ", MaxYear, ", Annualized", sep="")
  Sub_Title <- paste(subtitle)
  
  #This removes labels if they are too close, currently set at 0.01. Can change in line below.
  cpi <- cpi %>%  mutate(Wchange3aL = ((abs(Wchange3a-Wchange12))>.01) ) %>% mutate(Wchange12aL = Wchange3aL)
  cpi$Wchange3aL <- na_if(cpi$Wchange3aL, 0)
  cpi$Wchange12aL <- na_if(cpi$Wchange12aL, 0)
  
  ggplot(cpi) +
    geom_segment( aes(x=g2_item_name, xend=g2_item_name, y=Wchange3a, yend=Wchange12), color="grey") +
    geom_point( aes(x=g2_item_name, y=Wchange3a), color="#F44336", size=3) +
    geom_point( aes(x=g2_item_name, y=Wchange12), color="darkblue", size=3) +
    coord_flip()+
    theme_minimal() +
    theme(panel.grid.major.x = element_blank(),
          plot.title = element_markdown(size = 18)) +
    ggtitle(Full_Title) +
    theme(
      plot.title.position = "plot",
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12, hjust = 0.5, color="#4421af"),
      plot.subtitle = element_text(size = 16, color = "darkblue", face = "italic"),
      plot.caption = element_text(size = 12, lineheight = 1.2),
      panel.grid = element_blank()) +
    xlab("") +
    ylab("") +
    labs(subtitle = Sub_Title,
         caption = "BLS, CPS, Seasonally Adjusted. Author's Calculation. Mike Konczal, Roosevelt Institute",
         x="", y="") +
    geom_text(aes(x=g2_item_name, y=Wchange3a, label=round(Wchange3aL*Wchange3a, 2)), nudge_x =.25, color = "#F44336") +
    geom_text(aes(x=g2_item_name, y=Wchange12, label=(round(Wchange12aL*Wchange12, 2))), nudge_x =.25, color = "darkblue")
}


# Returns monthly percent change and percent change of basket for item y in the cpi file x.
inflation_data <- function(x, item){
  inf_d <- filter(x, item_name == item)
  inf_d <- arrange(inf_d, date)
  inf_d$pct_change <- (inf_d$value/lag(inf_d$value) - 1) * 100
  inf_d$basket_change <- inf_d$pct_change*inf_d$weight/100
  return(inf_d)
}

# Takes in a vector of item names (items), cpi_file (x), starting date (YYYY-MM-DD format), with optional end date and seasonality
# and returns each item's contribution to inflation for each month.
create_basket <- function(x, items, startDate, endDate = "current", season = "S", lagM = 1, annualize = FALSE){
  x <- filter(x, period != "M13")
  if(endDate == "current"){
    endDate <- max(x$date)
  }
  x <- x %>%
    filter(seasonal == season) %>%
    filter(date >= startDate) %>%
    filter(date <= endDate)
  x <- arrange(x, date)
  tmp <- filter(x, item_name == items[1])
  cb <- tibble(.rows = nrow(tmp))
  cb[,1] <- tmp$date
  for (i in seq_along(items)) {
    inf_d <- filter(x, item_name == items[i])
    inf_d$pct_change <- (inf_d$value/lag(inf_d$value, lagM) - 1)
    inf_d <- inf_d$pct_change*inf_d$weight/100
    if(annualize == TRUE){
      inf_d <- (1 + inf_d)^(12/lagM) - 1
    }
    cb[,i+1] <- inf_d
  }
  items <- str_replace_all(items, c(' '), c('_'))
  items <- str_replace_all(items, c(','), c(''))
  colnames(cb) <- c('date', items)
  cb %>% clean_names()
  return(cb)
}

# Takes in a vector of item names and returns their weights.
create_weights <- function(x, items){
  for (i in seq_along(items)) {
    tmp_wght <- filter(x, item_name == items[i])
    if( i == 1)
      wght <- mean(tmp_wght$weight)
    else
      wght <- cbind(wght, mean(tmp_wght$weight))
  }
  if(length(items) == 1){
    names(wght) <- items
  }
  else{
    colnames(wght) <- items
  }
  return(wght)
}

##### SET UP BASKETS FOR ANALYSIS #####


item_basket_topline <- c("All items", "Energy", "Food", "Commodities less food and energy commodities", "Services less energy services")

item_basket_watch_categories <- c("All items", "New and used motor vehicles", "Shelter", "Other services",
                                  "Medical care services", "Food", "Energy", "Commodities less food and energy commodities")

item_basket_core_services <- c("Services less energy services",
                               "Shelter",
                               "Medical care services",
                               "Transportation services",
                               "Recreation services",
                               "Education and communication services",
                               "Other personal services")

item_basket_core_goods <- c("Commodities less food and energy commodities",
                            "Household furnishings and supplies",
                            "Apparel",
                            "Transportation commodities less motor fuel",
                            "Medical care commodities",
                            "Recreation commodities",
                            "Education and communication commodities",
                            "Alcoholic beverages",
                            "Other goods")


item_basket_transportation <- c("New vehicles", "Used cars and trucks", "Motor vehicle parts and equipment")

####### CREATE THEME #######


bbc_style <- function() {
  font <- "Helvetica"
  
  ggplot2::theme(
    
    #Text format:
    #This sets the font, size, type and colour of text for the chart's title
    plot.title = ggplot2::element_text(family=font,
                                       size=28,
                                       face="bold",
                                       color="#222222"),
    #This sets the font, size, type and colour of text for the chart's subtitle, as well as setting a margin between the title and the subtitle
    plot.subtitle = ggplot2::element_text(family=font,
                                          size=22,
                                          margin=ggplot2::margin(9,0,9,0)),
    plot.caption = ggplot2::element_text(family=font,
                                         size=14,
                                         margin=ggplot2::margin(19,0,11,0)),
    #This leaves the caption text element empty, because it is set elsewhere in the finalise plot function
    
    #Legend format
    #This sets the position and alignment of the legend, removes a title and backround for it and sets the requirements for any text within the legend. The legend may often need some more manual tweaking when it comes to its exact position based on the plot coordinates.
    legend.position = "top",
    legend.text.align = 0,
    legend.background = ggplot2::element_blank(),
    legend.title = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(family=font,
                                        size=18,
                                        color="#222222"),
    
    #Axis format
    #This sets the text font, size and colour for the axis test, as well as setting the margins and removes lines and ticks. In some cases, axis lines and axis ticks are things we would want to have in the chart - the cookbook shows examples of how to do so.
    axis.title = ggplot2::element_blank(),
    axis.text = ggplot2::element_text(family=font,
                                      size=18,
                                      color="#222222"),
    axis.text.x = ggplot2::element_text(margin=ggplot2::margin(5, b = 10)),
    axis.ticks = ggplot2::element_blank(),
    axis.line = ggplot2::element_blank(),
    
    #Grid lines
    #This removes all minor gridlines and adds major y gridlines. In many cases you will want to change this to remove y gridlines and add x gridlines. The cookbook shows you examples for doing so
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(color="#cbcbcb"),
    panel.grid.major.x = ggplot2::element_blank(),
    
    #Blank background
    #This sets the panel background as blank, removing the standard grey ggplot background colour from the plot
    panel.background = ggplot2::element_blank(),
    
    #Strip background (#This sets the panel background for facet-wrapped plots to white, removing the standard grey ggplot background colour and sets the title size of the facet-wrap title to font size 22)
    strip.background = ggplot2::element_rect(fill="white"),
    strip.text = ggplot2::element_text(size  = 22,  hjust = 0)
  )
}