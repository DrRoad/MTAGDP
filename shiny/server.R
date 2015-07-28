library(ggvis)
library(dplyr)
library(scales)
library(shiny)
library(tidyr)

CAGR <- function (ratio, period) {
  round((exp(log(ratio)/period) - 1) * 100, 1)
}

load("TAGDP_data.rda")
load("dimensions.rda")
load("ta_pops.rda")
load("ta_cols.rda")
load("Commentary.rda")

width <- "auto"

shinyServer(function(input, output, session) {
  
  # this next reactive function is just to redraw the 'Adjustments' checkbox occassionally when
  # the per population box has gone missing due to 'share of GDP' chosen earlier.
  GiveMeBackPP <- reactive({
    if(input$TheTabs != "Compare areas' growth"){
      updateCheckboxGroupInput(session, "Adjustments",
                               choices = c("Inflation" = "defl", "Per population" = "pp"), 
                               selected = input$Adjustments,
                               inline = TRUE)
    }
    return(input$TheTabs)
  })
  
  
  
  #-----------------------Tab 1 data-------------------
  # next line is useful to run when debugging  
  # input <- list(IndustryX = Industries_RGDP[1], IndustryY = Industries_RGDP[2], IndustryS = Industries_RGDP[2], Year = 2012, MotionValue = "Original dollar amount", TopNumber = 20, TA = "Wellington City", Ind1 = "Retail Trade", TAsTotal = c("Auckland", "Buller District"), LineType = "Index (earliest year = 100)", GrowthValue = "Total GDP", Type = "High level (recommended)", stringsAsFactors = FALSE, DateRange = c(2000, 2012))
  totals_data <- reactive({
 
    # First, if we are dealing with total GDP:
    if(input$GrowthValue == "Total GDP"){
      # put the per capita option back again
      updateCheckboxGroupInput(session, "Adjustments",
                               choices = c("Inflation" = "defl", "Per population" = "pp"), 
                               selected = input$Adjustments,
                               inline = TRUE)
      updateRadioButtons(session, "LineType", 
                         choices = c("Absolute amount", "Index (earliest year = 100)"),
                         selected = input$LineType)
      
      if("defl" %in% input$Adjustments){
        # Adjust for inflation
        tmp <- TheTotals %>% mutate(GDP = GDP_real)
        } else {
         tmp <- TheTotals
       }
      
      tmp <- tmp %>%
        filter(Year %in% input$DateRange[1]:input$DateRange[2]) %>%
        filter(TA %in% input$TAsTotal) %>%
        arrange(Year)
    } else {
      # From here on is dealing with just one industry.  More complicated as certain combinations of UI are unhelpful.
      if("defl" %in% input$Adjustments){
        # Adjust for inflation
        tmp <- tagdp_shiny %>% mutate(GDP = GDP_real)
      } else {
        tmp <- tagdp_shiny
      }
      tmp <- tmp  %>%
        filter(Year %in% input$DateRange[1]:input$DateRange[2]) %>%
        filter(TA %in% input$TAsTotal & Type == "RGDP")
      
      if(input$DollOrShare == "Share of total GDP"){
        tmp <- tmp %>%
          group_by(TA, Year) %>%
          mutate(GDP = GDP / sum(GDP) * 100) %>%
          ungroup()
        updateRadioButtons(session, "LineType", selected = "Absolute amount")
        
        # Remove some invalid options that make no sense when showing share of GDP
        updateCheckboxGroupInput(session, "Adjustments",
                                 choices = c("Inflation" = "defl"),
                                 selected = input$Adjustments[input$Adjustments != "pp"],
                                 inline = TRUE)
        updateRadioButtons(session, "LineType", 
                           choices = c("Absolute amount"))
      } else {
        # put the per capita and index options back again
        updateCheckboxGroupInput(session, "Adjustments",
                                 choices = c("Inflation" = "defl", "Per population" = "pp"), 
                                 selected = input$Adjustments,
                                 inline = TRUE)
        updateRadioButtons(session, "LineType", 
                           choices = c("Absolute amount", "Index (earliest year = 100)"),
                           selected = input$LineType)
      }
      
      tmp <- tmp  %>%
        filter(Industry == input$Ind1) %>%
        arrange(Year)
    }
        
    if("pp" %in% input$Adjustments){
      # Adjust to be per capita
      tmp <- left_join(tmp, ta_pops, by = c("Year", "TA")) %>%
        mutate(GDP = GDP / Population * 10 ^ 6) %>%
        select(-Population)
      
    }
    

    
    if(input$LineType == "Index (earliest year = 100)" & 
         input$GrowthPlotType == "Show whole time series"){
      tmp <- tmp %>%
        group_by(TA) %>%
        mutate(Value = GDP / GDP[1] * 100) %>%
        ungroup()
    } else {
      tmp$Value <- tmp$GDP
    }
    
    
    # work out order of final value
    final <- tmp %>%
      filter(Year == max(Year)) %>%
      arrange(-Value)
    
    # reorder factor levels
    tmp <- tmp %>%
      mutate(TA = factor(TA, levels = final$TA))
    
    # work out growth rate
    growth <- tmp %>%
      group_by(TA) %>%
      summarise(
        cagr_number = CAGR(Value[Year == max(Year)] / Value[Year == min(Year)],
                           period = max(Year) - min(Year)),
        cagr_label = paste0(unique(TA), "<br>", cagr_number,
                            "% per year average")
      )
    
    tmp <- left_join(tmp, growth, by = "TA")
      
    return(tmp)
  })
  
  #-------------Tab 1 Define the title of the plot ---------------------
  # rather complex as it depends on the combination of user inputs
  time_title <- reactive({
    tmp <- paste0("<h3>nominal GDP ", 
           input$DateRange[1],
           " to ",
           input$DateRange[2],
           " ($m) </h3>")
    if(input$GrowthValue != "Total GDP"){
      tmp <- paste0(tmp, "<b>", input$Ind1,  "</b>")  
    }
    
    if(input$GrowthPlotType == 'Average growth rate only'){
      tmp <- gsub("($m)", "(% per year)", tmp, fixed = TRUE)
      tmp <- gsub("<h3>", "<h3>Growth in ", tmp, fixed = TRUE)
    }
    
    if(input$LineType == 'Index (earliest year = 100)'){
      tmp <- gsub("($m)", "(Index)", tmp, fixed = TRUE)
    }
    if("pp" %in% input$Adjustments){
      tmp <- gsub("GDP", "GDP per capita", tmp)
      tmp <- gsub("($m)", "($)", tmp, fixed = TRUE)
    }
    
    
    if("defl" %in% input$Adjustments){
      tmp <- gsub("nominal", "real", tmp)
    }
    
    if(input$DollOrShare == "Share of total GDP" & input$GrowthValue != "Total GDP"){
      tmp <- gsub("(% per year)", "(average % growth in percentage share of GDP)", tmp, fixed = TRUE) 
      tmp <- gsub("($m)", "(percentage share of GDP)", tmp, fixed = TRUE) 
    }
    
    # capitalise the first letter afdter the <h3>
    substring(tmp, 5, 5) <- toupper(substring(tmp, 5, 5))
    
    return(tmp)
  })
  
  output$time_title <- renderText(time_title())
  
  

  
  #----------------Tab 1 time series line plot------------
  totals_data %>%
    ggvis(x = ~Year, y = ~Value, stroke = ~TA, key := ~cagr_label) %>%
    layer_lines(strokeWidth := 3) %>%
    add_axis("x", title = "", format="04d") %>%
    add_axis("y", title = " ", title_offset = 55) %>%
    scale_nominal("stroke", 
                  domain = ta_cols$TA,
                  range = ta_cols$Col) %>%
    hide_legend("stroke") %>%
    add_tooltip(function(input){
      if(is.null(input)) return(NULL)
      input$cagr_label
    }, on="hover") %>%
    filter(Year == max(Year)) %>%
    layer_text(text := ~TA, stroke := "grey", font := "Calibri", fontWeight := "normal") %>%
    set_options(width = width, height = 450, resizable = TRUE) %>%
    bind_shiny("totals_plot")
 
  
  #---------------------Tab 1 dot chart showing CAGRs------------
  # There's no geom_segment so we have to do an annoying workaround
  dot_plot_data <- reactive({
    tmp1 <- totals_data() %>%
      group_by(TA) %>%
      mutate(GDPLatest = Value[Year == max(Year)]) %>%
      ungroup() %>%
      mutate(TA = gsub(" District", "", TA),
             TA = gsub(" City", "", TA)) %>%
      select(cagr_number, cagr_label, TA, GDPLatest) %>%
      distinct() %>%
      arrange(-cagr_number)
    
    tmp2 <- tmp1 %>%
      mutate(cagr_number = 0, flag = "zero") %>%
      rbind(tmp1 %>% 
              select(cagr_number, cagr_label, TA, GDPLatest) %>% 
              mutate(flag = "real") %>% 
              distinct()) 
    
    if(input$DotSequence == "By growth rate"){
      tmp2 <- tmp2 %>%
        arrange(cagr_number) %>%
        mutate(TA = factor(TA,  levels = tmp1$TA)) %>%
        group_by(TA)
    } else {
      if(input$DotSequence == "North to south"){
        tmp2 <- tmp2 %>%
          mutate(TA = factor(TA,  levels = TAs_short$TA[TAs_short$TA %in% tmp2$TA])) %>%
          group_by(TA)
      } else{
        tmp1 <- tmp1 %>%
          arrange(-GDPLatest)
        
        tmp2 <- tmp2 %>%
          mutate(TA = factor(TA,  levels = tmp1$TA)) %>%
          group_by(TA)
      }
    }
    
    if(!input$Colour){
      tmp2$TA2 <- "Black"
    } else {
      tmp2$TA2 <- tmp2$TA
    }
    
    return(tmp2)
      
  })
  
  dot_plot_hover <- function(plot_input){
    if(is.null(plot_input)) return(NULL)
    round2 <- function(x, digits = 0){
      if(is.null(x)){
        return(NULL)
      } else {
        return(format(round(x, digits), big.mark = ","))
      }
    }
    
    tmp <- paste0(
        plot_input$TA,
        "<br>",
        round2(plot_input$cagr_number * 100, digits = 1), 
        "% per year<br>",
        round2(plot_input$GDPLatest)
      )
    
    return(tmp)
  }
  
 dot_plot <- reactive({dot_plot_data %>%
    ggvis(x = ~cagr_number / 100, y = ~TA, stroke = ~TA2) %>%
    layer_paths(strokeWidth := 4) %>%
    filter(flag == "real") %>%
    layer_points(size = ~GDPLatest) %>%
      scale_nominal("stroke", 
                    domain = ta_cols$TA_short,
                    range = ta_cols$Col) %>%
      hide_legend("stroke") %>%
      add_legend("size", title = "Latest value") %>%
    add_axis("x", title = "Average growth rate", format = ".1%") %>%
   add_axis("y", title = " ", title_offset = 105) %>%
     add_tooltip(dot_plot_hover) %>%
    set_options(width = width, height = 550)})
 

 
 dot_plot %>% bind_shiny("totals_dot_plot")
  
  #---------------------------Tab 2 District detail plot--------------
 ThisType <- reactive({
   tmp <- Types %>%
     filter(label == input$Type)
   
   # update the 'number of industries' slider
   if(tmp$code == "RGDP"){
     updateSliderInput(session, "TopNumber", 
                       max = length(Industries_RGDP),
                       value = min(input$TopNumber, length(Industries_RGDP)))   
   } else {
     updateSliderInput(session, "TopNumber", max = length(Industries_NGDP),
                       value = input$TopNumber)   
   }
  
   
   return(tmp$code)
 })
 
 detail_title <- reactive({
   tmp <- paste0("<h3><center>Nominal GDP by industry, ",
          input$TA,
          " ($m)</center></h3>")
   
   if("pp" %in% input$Adjustments){
     tmp <- gsub("GDP", "GDP per capita", tmp)
     tmp <- gsub("($m)", "($)", tmp, fixed = TRUE)
   }
   
   if("defl" %in% input$Adjustments){
     tmp <- gsub("Nominal", "Real", tmp, ignore.case = TRUE)
   }
   
   return(tmp)
 })
 
 output$detail_title <- renderText(detail_title())
 
 my_tagdp <- reactive({
   # give me back the per capita option...
   check_tab <-   GiveMeBackPP() 
   tmp1 <- tagdp_shiny %>%
        filter(TA == input$TA & Type == ThisType()) %>%
        # filter(TA == input$TA & Type == "RGDP") %>% # this line can be used instead of the previous during debugging
        filter(!is.na(GDP)) 
      
      if("defl" %in% input$Adjustments){
        # Adjust for inflation
        tmp1 <- tmp1 %>% mutate(GDP = GDP_real) 
      } 
      
      
      if("pp" %in% input$Adjustments){
        # Adjust to be per capita
        tmp1 <- left_join(tmp1, ta_pops, by = c("Year", "TA")) %>%
          mutate(GDP = GDP / Population * 10 ^ 6) %>%
          select(-Population)
        
      }
      
      
      
      tots <- tmp1 %>%
        filter(Year == max(Year)) %>%
        group_by(Industry) %>%
        summarise(Total = sum(GDP)) %>%
        arrange(Total) %>%
        tail(input$TopNumber)        
      
      tmp2 <- inner_join(tmp1, tots, by = "Industry") %>%
        arrange(Year, GDP_tot)
      
      return(tmp2)  
        
   })
      
     
my_tagdp %>%
  ggvis(y = ~Industry, x = ~GDP, fill = ~Year) %>%
  layer_points(stroke := "#006272", size := 200) %>%
  set_options(width = width, height = 500, resizable = TRUE, duration = 0) %>%
  add_relative_scales() %>%
  add_legend("fill", format="04d", 
             properties = legend_props(legend = 
                                         list(
                                           x = scaled_value("x_rel", 0.68)
                                           )
             )
  ) %>%
  scale_numeric("fill", reverse = FALSE, range = c("white", "#00B5E2")) %>%
  add_axis("y", title = "") %>%
  add_axis("x", title = "Gross Domestic Product", format = "$,") %>%
  add_tooltip(function(input){
              if(is.null(input)) return(NULL)
              paste0(input$Year, 
                    "<br>",
                    input$Industry,
                    "<br> $",
                    format(round(input$GDP), big.mark = ","),
                    "m</br>")
              }, on="hover") %>%
  bind_shiny("dot_plot") 
   


TheCommentary <- reactive({
  if(input$Type == Types[1, "label"]){
    tmp <- Commentary$Commentary_rgdp[Commentary$TA == input$TA]
    
  } else {
    tmp <- Commentary$Commentary_ngdp[Commentary$TA == input$TA]
    
  }
    tmp <- paste0(tmp, Commentary$closest[Commentary$TA == input$TA])
  return(tmp)
})

output$TA_commentary <- renderText(paste("<hr>", TheCommentary()))


#------------------Tab 3 Motion chart---------------------------
 
   the_flash_data <- reactive({
     # give me back the per person option
     check_tab <-   GiveMeBackPP() 
     
     tmp1 <- tagdp_shiny %>% 
       filter(Type == "RGDP")
     
     if("defl" %in% input$Adjustments){
       # Adjust for inflation
       tmp1 <- tmp1 %>% 
         mutate(GDP = GDP_real) 
     } 
     
     
       if("pp" %in% input$Adjustments){
       # Adjust to be per capita
       tmp1 <- left_join(tmp1, ta_pops, by = c("Year", "TA")) %>%
         mutate(GDP = GDP / Population * 10 ^ 6) %>%
         select(-Population)
       }
     
       if(input$MotionValue == "Index (2000 = 100)"){
         tmp1 <- tmp1 %>%
           group_by(TA, Industry) %>%
           mutate(GDP = GDP / GDP[1] * 100) %>%
           ungroup() 
       } 
     
     if(input$MotionValue == "Share of area's GDP"){
        tmp1 <- tmp1 %>%
         group_by(TA, Year) %>%
         mutate(GDP = GDP / sum(GDP) * 100) %>%
         ungroup() 
     }
     
     tmp2 <- tmp1 %>%
       select(Industry, Year, TA, GDP) %>%
       filter(Year == input$Year &
                Industry %in% c(input$IndustryX, input$IndustryY, input$IndustryS)) %>%
       spread(Industry, GDP) %>%
       data.frame(check.names = FALSE, stringsAsFactors = FALSE)
     
     names(tmp2)[names(tmp2) == input$IndustryX] <- "X"
     names(tmp2)[names(tmp2) == input$IndustryY] <- "Y"
     names(tmp2)[names(tmp2) == input$IndustryS] <- "S"          
     
     if(input$IndustryX == input$IndustryY & input$IndustryY == input$IndustryS){
       tmp2$Y <- tmp2$S  <- tmp2[, "X"]
     } else {
      if(input$IndustryY == input$IndustryS) {
        tmp2$S <- tmp2[ , "Y"]
      } 
      if(input$IndustryX == input$IndustryS) {
        tmp2$S <- tmp2[ , "X"]
      } 
      if(input$IndustryX == input$IndustryY){
        tmp2$Y <- tmp2[ , "X"]      
          }
        }
     
     
     # Create tooltip labels
     # I tried to do a proper little HTML table here but I think ggvis stops it working.  So
     # we just get the name instead.
     tmp2$Label <- with(tmp2, paste0("<em>", TA, "</em>")
     )
     
     # flag the ones to circle
     tmp2$Flag <- ifelse(tmp2$TA %in% input$TAsTotal, "Selected", "Not selected")
   
     return(tmp2)
   })


# I don't know why this doesn't work when you click on a point.  Might be because the 'select TAsTotal'
# is in a different tab, but I wouldn't have thought that would be a problem.
click_function <- function(plot_input){
  if(is.null(plot_input)) return(NULL)
  if(plot_input$TA %in% input$TAsTotal){
    updateSelectInput(session, "TAsTotal", selected = unique(c(input$TAsTotal, plot_input$TA)))
  }
}

the_flash_data %>%
  ggvis(x = ~X, y = ~Y, fill = ~TA, key := ~Label) %>%
  scale_nominal("fill", 
                domain = ta_cols$TA,
                range = ta_cols$Col) %>%
  hide_legend("fill") %>%
  hide_legend("size") %>%
  hide_legend("shape") %>%
  layer_points(size = ~S, shape = ~Flag) %>%
  add_tooltip(function(input){
    if(is.null(input)) return(NULL)
    input$Label
  }, on="hover") %>%
  add_tooltip(click_function, on = "click") %>%
  filter(Flag == "Selected") %>%
  layer_text(text := ~TA) %>%
  add_axis("x", title = "") %>%
  add_axis("y", title = " ", title_offset = 50) %>%
  bind_shiny("motion_plot")

output$x_label <- renderText(paste0("<p><center><b>", input$IndustryX, "</b></center></p>"))
output$y_label <- renderText(paste0("<p><b>", input$IndustryY, "</b></p>"))


flash_title <- reactive({
  tmp <- paste0("<h3><center>Nominal GDP ($m)            </center></h3>")
  
  
  if(input$MotionValue == "Index (2000 = 100)"){
    tmp <- gsub("($m)", "(Index)", tmp, fixed = TRUE)
  }
  
  
  if(input$MotionValue == "Share of area's GDP"){
    tmp <- gsub("($m)", "(%)", tmp, fixed = TRUE)
  }
  
  if("pp" %in% input$Adjustments){
    tmp <- gsub("GDP", "GDP per capita", tmp)
    tmp <- gsub("($m)", "($)", tmp, fixed = TRUE)
  }
  
  
  if("defl" %in% input$Adjustments){
    tmp <- gsub("Nominal", "Real", tmp)
  }
  
  return(tmp)
})

output$flash_title <- renderText(flash_title())

#============================Explanations that appear depending on user input=====================
pp_explanation <- reactive({
  if(("pp" %in% input$Adjustments && input$GrowthValue == 'Selected industry') |
       "pp" %in% input$Adjustments && input$TheTabs != "Compare areas' growth"){
    tmp <- "<p>When 'GDP per population' refers to a single industry, it still applies to the 
    entire population of the area.  Obviously, not all the population is involved in that industry.  
    'GDP per population' for a single industry gives an indication of the importance of that industry in that area, 
    and is not a measure of productivity or of welfare.</p>"
  } else {
    tmp <- ""
  }
  return(tmp)
})
output$pp_explanation <- renderText(pp_explanation())

defl_explanation <- reactive({
  if("defl" %in% input$Adjustments){
    tmp <- "<p>Adjustment for inflation to give a volume measure in 2012 prices is done on the basis of national price levels, 
    using the industry classifications 
    in the published chain volume national GDP series.  No account has been taken of differing regional prices.</p>"
  } else {
    tmp <- ""
  }
  return(tmp)
})
output$defl_explanation <- renderText(defl_explanation())

dots_explanation <- reactive({
  if(input$TheTabs == "One area's top industries"){
    tmp <- "<p>Each point represents a year from 2000 to 2012, with more recent years darker in colour.  Industries are listed in order of their importance for New Zealand overall, 
    so breaks in the pattern show industries that are characteristic of the selected Territorial Authority.</p>"
  } else {
    tmp <- ""
  }
  return(tmp)
})
output$dots_explanation <- renderText(dots_explanation())


})