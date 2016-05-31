library(shiny)
library(ggvis)
library(dplyr)


load("dimensions.rda")
# Define UI for application that draws a histogram
shinyUI(fluidPage(
    tags$head(
      includeCSS("mbie-styles.css"),
      includeCSS("fstyles.css"),
      tags$script(src = "underscore_throttle.js"),
      tags$script(src = "iframeResizer.contentWindow.js")
      ),
   
   fixedRow(
  #h2("Modelled Territorial Authority Gross Domestic Product"),
          checkboxGroupInput("Adjustments", "Adjustments:",
                      choices = c("Inflation" = "defl", "Per population" = "pp"), 
                      inline = TRUE)
                      ),
   
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
              min = 2000, max = 2015, value = c(2000, 2015),
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
             sliderInput("Year", "Year", min = 2000, max = 2013, value = 2013, animate = TRUE,
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
<li>These estimates are at a more detailed level of granularity than available in the Statistics New Zealand 
official Tier 1 regional GDP series.  
They are experimental in nature and should be used with caution.  The data are modelled and produced by 
the Ministry of Business Innovation and Employment (MBIE) (not by Statistics New Zealand), according to 
the methods outlined in 
http://www.mbie.govt.nz/info-services/sectors-industries/regions-cities/research/modelled-territorial-authority-gross-domestic-product/about-mtagdp
<li>These estimates are not a Tier 1 statistic and have been created by MBIE for research purposes. While 
various Statistics New Zealand collections form the source data, Statistics New Zealand will not be held 
accountable for any error, inaccurate findings or interpretation within the data or related publications.
One of the sources used for the modelling is a customised dataset created in a way that protects confidentiality, 
provided by Statistics New Zealand.   Access to that data was provided to MBIE by Statistics New Zealand under 
conditions designed to give effect to the security and confidentiality provisions of the Statistics Act 1975.
<li>While all care and diligence has been used in processing, analysing, and extracting data and information 
for this publication, MBIE gives not warranty it is error free and will not be liable for any loss or damage
suffered by the use directly, or indirectly, of the information.
</ul>
<hr>
 <div>
<p><a rel="license" href="http://creativecommons.org/licenses/by/3.0/nz/">
<img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by/3.0/nz/88x31.png" /></a>
This work is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by/3.0/nz/">
Creative Commons Attribution 3.0 New Zealand License</a>.</p>
</div>

<div>
<p><a href = "https://github.com/nz-mbie/MTAGDP"><img src="Octocat.jpg" alt = "GitHub Octocat logo" title = "Link to source code on GitHub" width = "80" height = "66"></a>
Go to GitHub to get the <a href = "https://github.com/nz-mbie/MTAGDP" target="_blank">source code [External Link]</a> the Ministry used to create these modelled estimates and this web app.
</p>
</div>
<div>
<p> Some mobile users may wish to <a href = "https://mbienz.shinyapps.io/mtagdp" target="_blank">open the tool in its own window [External link]</a> to make best use of screen real estate.
</p>
</div>

</p>
')

   
))