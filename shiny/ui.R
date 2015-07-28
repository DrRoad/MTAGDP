library(shiny)
library(ggvis)
library(dplyr)


load("dimensions.rda")
# Define UI for application that draws a histogram
shinyUI(fluidPage(
   # insert some CSS.  Down the track this would be stored in a separate file.
   tags$head(
      tags$style(HTML("
                      body {
         font-family: 'Lucida Sans Unicode', 'Lucida Grande', Verdana, Lucida, Helvetica, Arial, Calibri, sans-serif;
         color: rgb(0,0,0);
                      font-size: 12.1px;
                      line-height: 18.15px;
                      margin-bottom: 9.075px; 
                      list-style-image: url(http://www.mbie.govt.nz/bullet_double_green_8x8.png);
                      }
                      
                      h2 {
                      font-size:20px;
                      line-height: 24px;
                      color: rgb(0, 139, 198);
                      }
                      
                      
                      h3 {
                      font-size:15px;
                      line-height: 18px;
                      color: rgb(0, 139, 198);
                      }
                      
                      .selectize-dropdown, .selectize-input, label { 
            font-family: 'Lucida Sans Unicode', 'Lucida Grande', Verdana, Lucida, Helvetica, Arial, Calibri, sans-serif;
            font-size: 11px;
            font-weight: normal;   
                      }"
      ))
      
      
      ),
   
   
  
      column(width = 8, h2("Modelled Territorial Authority Gross Domestic Product")),
   column(width = 4, 
          br(), 
          checkboxGroupInput("Adjustments", "Adjustments:",
                      choices = c("Inflation" = "defl", "Per population" = "pp"), 
                      inline = TRUE)),
   
      tabsetPanel(type = "tabs", id = "TheTabs",
                  
        
   tabPanel("Compare areas' growth",
            br(),
            column(width = 4,
              selectInput("TAsTotal",
                        "Choose some districts and cities:",
                        choices = TAs,
                        width="300px",
                        size = 5,
                        selected = sample(TAs, 4),
                        selectize=FALSE,
                        multiple = TRUE),
            p("Use shift or ctrl to select multiple districts and cities."),
            
            radioButtons("GrowthPlotType", "Plot type:",
                         choices = c("Show whole time series", "Average growth rate only")), 
            
            conditionalPanel(condition = "input.GrowthPlotType == 'Show whole time series'",
                             radioButtons("LineType", "Value for vertical axis:",
                                          choices = c("Absolute amount", "Index (earliest year = 100)"))
                            ),
            
            conditionalPanel(condition = "input.GrowthPlotType != 'Show whole time series'",
                             radioButtons("DotSequence", "Order in chart:",
                                          choices = c("North to south", 
                                                      "By growth rate",
                                                      "By latest value"))
            ),
            
             
            radioButtons("GrowthValue", "Showing:",
                         choices = c("Total GDP", "Selected industry"),
                         inline = TRUE),
            conditionalPanel(condition = "input.GrowthValue == 'Selected industry'",
                             selectInput("Ind1", "Choose an industry:",
                                         choices = Industries_RGDP,
                                         selectize = FALSE,
                                         selected = "Manufacturing"),
                             radioButtons("DollOrShare", label = NULL,
                                          choices = c("Size of industry", "Share of total GDP"),
                                          inline = TRUE)
                              ),
            sliderInput("DateRange", "Specify a date range:", 
              min = 2000, max = 2012, value = c(2000, 2012),
              sep = ""),
            conditionalPanel(condition = "input.GrowthPlotType != 'Show whole time series'",
                             checkboxInput("Colour", "Brightly coloured areas?", value = TRUE)
                             )
                             
            ),
            column(width = 8,
                  htmlOutput("time_title"),
                  conditionalPanel(condition = "input.GrowthPlotType == 'Show whole time series'",
                    ggvisOutput("totals_plot")
                  ),
                  conditionalPanel(condition = "input.GrowthPlotType != 'Show whole time series'",
                                   ggvisOutput("totals_dot_plot")
                  )
            
     )
     ),
   tabPanel("One area's top industries", 
            br(),
            
            fixedRow(    
              column(width = 4, 
                     selectInput("TA",
                                 "Choose a Territorial Authority:",
                                 choices = TAs[TAs != "All New Zealand"],
                                 width="200px",
                                 selected = sample(TAs, 1),
                                 selectize=FALSE)
                     ),
              column(width = 4, 
                     selectInput("Type",
                                 "Choose an industry classification:",
                                 choices = Types$label,
                                 width="300px",
                                 selected = Types[1, "label"],
                                 selectize=FALSE)
                     ),
              column(width = 4, 
                     sliderInput("TopNumber",
                                 "Choose a maximum number of industries to show",
                                 min = 10, max = length(Industries_RGDP), 
                                 value = length(Industries_RGDP),
                                 step = 1)
                     )
              
            ),
            fixedRow(
              column(width = 9,
                htmlOutput("detail_title"),
                ggvisOutput("dot_plot")
              ),
              column(width = 3,
                htmlOutput("TA_commentary")       
                     
              )
              
            )
   ),
   tabPanel("Explore", 
      column(width = 4,
             selectInput("MotionValue", "Select a value variable",
                         choices = c("Original dollar amount", "Index (2000 = 100)", "Share of area's GDP"),
                         selected = "Share of area's GDP",
                         selectize = FALSE,
                         width = "300px"),
             selectInput("IndustryY", "Vertical axis variable",
                         choices = Industries_RGDP,
                         selected = Industries_RGDP[2],
                         selectize = FALSE,
                         width = "300px"),
             selectInput("IndustryX", "Horizontal axis variable",
                         choices = Industries_RGDP,
                         selectize = FALSE,
                         width = "300px"),
             selectInput("IndustryS", "Size variable",
                         choices = Industries_RGDP,
                         selected = Industries_RGDP[3],
                         selectize = FALSE,
                         width = "300px"),
             sliderInput("Year", "Year", min = 2000, max = 2012, value = 2012, animate = TRUE,
                         sep = "")
             ),
      column(width = 8,
             htmlOutput("flash_title"),
             htmlOutput("y_label"),
             ggvisOutput("motion_plot"),
             htmlOutput("x_label")
      )
            
   )
      ),

  HTML('<p>Source: New Zealand Ministry of Business, Innovation and Employment; 
<i>Modelled Territorial Authority Gross Domestic Product</i></p>'),
  htmlOutput("dots_explanation"),
  htmlOutput("pp_explanation"),
  htmlOutput("defl_explanation"),
  HTML('
<hr>
<p>Caveats and disclaimers:
<ul>
<li>These estimates are at a more detailed level of granularity than available in the Statistics New Zealand official Tier 1 regional GDP series.  
They are experimental 
in nature and should be used with caution.	The data are modelled and produced by the Ministry of Business Innovation and Employment (MBIE) 
(not by Statistics New Zealand), according to the methods outlined in [link to come].
<li>These estimates are not a Tier 1 statistic and have been created by MBIE for 
research purposes. While various Statistics New Zealand collections form the source data, 
Statistics New Zealand will not be held accountable for any error, inaccurate findings or interpretation within the publication.
<li>One of the sources used in the modelling is a customised dataset, with confidentiality applied, provided by Statistics New Zealand.  
Access to that data was provided to MBIE by Statistics New Zealand under conditions 
designed to give effect to the security and confidentiality provisions of the Statistics Act 1975.
<li>MBIE is not responsible for the results of any actions taken on the basis of this information.
</ul>
<hr>
 <div>
<p><a rel="license" href="http://creativecommons.org/licenses/by/3.0/nz/">
<img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by/3.0/nz/88x31.png" /></a>
This work is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by/3.0/nz/">
Creative Commons Attribution 3.0 New Zealand License</a>.</p>
</div>

<div>
<p><a href = "http://nz-mbie.github.io/MTAGDP/"><img src="Octocat.jpg" alt = "GitHub Octocat logo" title = "Link to source code on GitHub" width = "80" height = "66"></a>
Go to GitHub to get the <a href = "http://nz-mbie.github.io/MTAGDP/">source code</a> the Ministry used to create these modelled estimates and this web app.
</p>
</div>

</p>
')

   
))