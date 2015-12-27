library(shiny)

shinyUI(pageWithSidebar(
     headerPanel("VISN and Facility Staffing Analysis"),
     sidebarPanel(
          selectInput(inputId = "comboBox",
                      label = "Select a Facility:",
                      choices = list("Las Vegas" = "Las Vegas",
                                     "Long Beach" = "Long Beach",
                                     "Loma Linda" = "Loma Linda",
                                     "San Diego" = "San Diego",
                                     "Greater LA" = "Greater LA",
                                     "VISN" = "VISN")),
          
          numericInput(inputId = 'periods',
                    label = "Select the Number of Periods to Forecast (Between 3 and 60):",
                    min = 3,
                    max = 60,
                    value = 3,
                    step = 3),
          
          tags$p('Note:  The first period starts with October 2015
                 and each period is equal to one month.'),
          tags$br(),
          tags$a(href="https://github.com/dcraig35/Data_Products",
                 "Click here to view the code that created this app.")
     ),
     mainPanel(
          tabsetPanel(
               tabPanel("Documentation", 
                        tags$div(
                             tags$br(),
                             tags$b("Background"),
                             tags$p("This is a prototype of a workload and staffing analysis tool 
                                    for a hospital department that specializes in durable medical
                                    equipment procurement.  It uses the number of unique outpatients
                                    seen at a given facility, the number of consults (requests for 
                                    services), and the number of unique facility providers to forecast
                                    the volume of facility unique patients, consults, and the resulting
                                    workload that will need to be produced to meet the demand.  The
                                    forecasted estimates are obtained through a series of linear regression
                                    models using data from Fiscal Year 2013 through 2015 (October 2012 through 
                                    September 2015).  Based on the amount of estimated workload the current staffing
                                    level (as of September 2015) and a production standard, a determination
                                    is made about the staffing need or excess."),
                             tags$b("Usage and Output"),
                             tags$p("To use the tool select the facility of interest (or VISN for
                                    the entire region) and select the number of forecast periods.  The 
                                    range of forecast periods is between 3 and 60 (each period equals one
                                    month, so the models will project out up to five years in the
                                    future).  The following information is then displayed on each tab:"), 
                             tags$ul(
                                  tags$li("Summary:  Gives the numeric values for both the starting and ending
                                         periods for the key variables of interest."), 
                                  tags$li("Unique Outpatient Volume:  Gives a graphical presentation of the anticipated  
                                         unique patient volume on a month by month basis for the number of forecast 
                                         periods selected."), 
                                  tags$li("Consult Volume:  Gives a graphical presentation of the anticipated  
                                         consult volume on a month by month basis for the number of forecast 
                                         periods selected."),
                                  tags$li("Workload Volume:  Gives a graphical presentation of the anticipated  
                                         workload volume on a month by month basis for the number of forecast 
                                         periods selected."),
                                  tags$li("Staff Level:  Gives a graphical presentation of the anticipated  
                                         staff need or excess on a month by month basis for the number of forecast 
                                         periods selected.  Note:  A negative value indicates being understaffed, 
                                         while a positive number indicates being overstaffed.")
                             ),
                             tags$b("Technical Note"),
                             tags$p("This application has been tested using the Firefox, Chrome, and Internet Explorer
                                    browsers.  No issues were noted with either the Firefox or Chrome browser, however
                                    the selection control for selecting the number of forecast periods did not function
                                    consistently in Internet Explorer.  The workaround for this is manually entering the
                                    number of periods manually (any number between 3 and 60).  It should also be noted
                                    that even with the other browsers there is the option to manually enter the number of
                                    forecast periods if you choose not to use the selection control.")
               )),
               tabPanel("Summary", tableOutput("textDisplay")),
               tabPanel("Unique Outpatient Volume", plotOutput("uniGraph")),
               tabPanel("Consult Volume", plotOutput("conGraph")),
               tabPanel("Workload Volume", plotOutput("wlGraph")),
               tabPanel("Staff Level", plotOutput("staffGraph"))
               
          )
     )
))