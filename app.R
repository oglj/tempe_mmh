# To all embarking on this journey:
# I highly recommend watching Joe Chen's seminar
# on reactive Shiny programming
# https://www.rstudio.com/resources/webinars/shiny-developer-conference/
# In particular, setting:
# options(shiny.reactlog = TRUE)
# and pressing CTRL + F3 after running the app
# will provide a visual means of exploring
# connections between inputs, outputs,
# observers, and reactive objects

source('global.R', local = TRUE)

# shiny : header ####

header <- dashboardHeader(
  titleWidth = 400,
  title = tags$div(
    style = "font-size:0",
    p(
      span(
        "COL", style = "color:white; font-size: 20px"
      ),
      span(
        "LAB", style = "color:red; font-size: 20px"
      ),
      span(
        "LOCATION: micromobility hubs", style = "color: white; font-size: 20px"
      )
    )
  ),
  
# This will display group, stage, and iteration info
  tags$li(
    uiOutput(
      "gSI_header",
      inline = TRUE
    ),
    class = "dropdown",
    style = "
        font-size: 20px;
        color: white;
        vertical-align: middle;
        margin-right: 20px;
        margin-top: 12px
      "
  )
)

# shiny : body ####

body <- dashboardBody(
  
# call Javascript for use in Shiny
  useShinyjs(),
  
# reload the page when calling function 'shinyjs.refresh'
  extendShinyjs(
    text = "shinyjs.refreshPage = function() { location.reload(); }",
    functions = c("refreshPage")
  ),


# css : settings ####

# These styles set the vertical height of the maps to fill the entirety of the
# screen, except for the header space, regardless of screen size
# This is a fixed value, so the map size will not change if the user scrolls
  
  tags$style(
    type = "text/css",
    "#map_main {height: calc(100vh - 122px) !important;}"
  ),
  
  tags$style(
    type = "text/css",
    "#map_compare {height: calc(100vh - 122px) !important;}"
  ),

  tags$style(
    type = "text/css",
    "#statsPanel_main_div {height: calc(100vh - 242px) !important;}"
  ),

  tags$style(
    type = "text/css",
    "body {min-height: 0px !important;}"
  ),
  
  tags$head(tags$script(HTML(panel_main_toggle_jscode))),
  tags$head(tags$script(HTML(panel_comp_toggle_jscode))),
# Access the HTML <head> element

    tags$head(
      
      # Create an HTML <style> element
      
      tags$style(
        
        # Write HTML code and set styles for elements that we'll construct later.
        # This could be done in CSS as well.  Might be cleaner that way.
        
        # slider customization:
        # https://stackoverflow.com/questions/40415471/sliderinput-max-min-text-labels
        
        HTML(
          "
            .form-group{
            margin-top: 0px;
            margin-bottom: 0px;
            }
            .selectize-control{
            margin-top: 0px;
            margin-bottom: 0px;
            }
            .checkbox{
            margin-top: 0px;
            margin-bottom: 0px;
            }
            
           
           #addPreviousGroups {
           background-color: #3a9ed8;
           color: #fff;
           width: 100%;
           }
           
           #stageSubmit {
           background-color: #3cb371;
           color: #fff;
           width: 100%;
           border: none
           }
           
           #groupNameModal {
           font-weight: normal
           }
           
           #statsPanel_main_div {
           overflow: auto
           }
           
           #plotBtn {
           opacity: .80; 
           color: #fff; 
           background-color: 
           #a662e3; 
           border-color: #a153e5
           }
           
           "
      )
    )
  ),

  
# make a fluid row in the body. This makes the platform scaleable across screenwidths
  
  fluidRow(
    

    
# This is a set of tabs that Shiny uses for dashboard navigation
# It has id = "Geodesign"
# We'll want this to have two tabs: "design" and "compare"
# idk maybe we change "design" to something else
    
    tabsetPanel(
      id = "Geodesign",
      type = "tabs",
      
      # shiny : make mainTab ####
      
      tabPanel(
        title = strong("Main"),
        id = "mainTab",
        value = "mainTab",

        # mainTab_leaflet ####
        column(
          width = 10,
          div(
            leafletOutput(
              outputId = "map_main"
            ),
            absolutePanel(
              id = "statsPanel_main_div", 
              class = "panel panel-default collapse in", 
              fixed = FALSE,
              draggable = FALSE, 
              top = 84, 
              left = "auto", 
              right = "auto", 
              bottom = 10,
              width = 330,
              h5(textOutput(
                "n_sitesSelec_textOutput"
              )),
              # h5(textOutput(
              # "percent_coverage_textOutput"
              # )),
              plotOutput(
                outputId = "main_cov_plot",
                height = 100
              ),
              plotOutput(
                outputId = "main_busCount_plot",
                height = 100
              ),
              plotOutput(
                outputId = "main_income_plot",
                height = 100
              ),
              plotOutput(
                outputId = "main_scootOD_plot",
                height = 100
              ),
              plotOutput(
                outputId = "main_zoning_plot",
                height = 150
              ),
              div(
                
              )
            ),
            # mainTab : absolutePanel ####
            absolutePanel(
              id = "plotBtn_div",
              bottom = 10, 
              left = "auto",
              height = 30,
              actionButton(
                width = 330,
                inputId = 'plotBtn', 
                label = textOutput(
                  "collapseButton_textOutput_main"
                ), 
                "data-toggle" ='collapse', 
                "data-target" = '#statsPanel_main_div'
              )
            )
          )
        ),
        
        # mainTab_sidebar ####
        
        column(
          width = 2,
          height = 1200,
          style = mainTab_sidebar_css,
          
          # mainTab_sidebar_submitButton ####
          wellPanel(
            tags$div(
              # style = mainTab_sidebar_submitButton_css,
              title = mainTab_sidebar_submitButton_title,
              actionButton(
                inputId = "stageSubmit",
                label = strong(mainTab_sidebar_submitButton_label),
                style = mainTab_sidebar_submitButton_css
              )
            ),
            
            style = mainTab_sidebar_submitButtonPanel_css
          ),
          
          # mainTab_layerControls ####
# Performance might be improved using the solution in https://stackoverflow.com/questions/42658225/put-leaflet-controls-outside-of-map-div-in-shiny-environment
          wellPanel(
            div(
              title = mainTab_layerControls_title,
              p(
                style = mainTab_layerControls_label_css,
                strong(mainTab_layerControls_label)
              )
            ),
            # HTML(mainTab_layerControls_cand_html),
            div(
              checkboxInput(
                inputId = "selected_button",
                label = HTML(mainTab_layerControls_cand_html),
                TRUE
              ),
              checkboxInput(
                inputId = "covered_button",
                label = HTML(covered_html),
                TRUE
              ),
              checkboxInput(
                inputId = "prevSelected_button",
                label = HTML(mainTab_layerControls_prev_html),
                TRUE
              ),
              HTML("<br>"),
              checkboxInput(
                inputId = "busStops_button",
                label = HTML(mainTab_layerControls_GRiDBikes_label),
                TRUE
              ),
              checkboxInput(
                inputId = "bikeways_button",
                label = HTML("<img src='bikeway_icon.png' width='36'> bikeways"),
                TRUE
              ),
              checkboxInput(
                inputId = "scooterOD_button",
                label = HTML(scootOD_legend_HTML),
                TRUE
              # ),
              # sliderInput(
              #   inputId = "scooterOD_slider"
              #   , label = HTML(scootOD_legend_HTML)
              #   , min = .1
              #   , max = 1
              #   , value = .5
              ),
              checkboxInput(
                inputId = "mainCompSites_button",
                label = HTML("spatial comparison sites"),
                FALSE
                
              ),
              
              HTML("<img src='studyArea_icon.png' width='36'> study area")
            ),
            # div(img(src = 'candidate_icon.png', height = 18),
            # # DEBUG
            # div(
            #   title = "
            #     places selected as candidates for micromobility hubs
            #     ",
            #   checkboxInput(
            #     inputId = "candidates_button",
            #     label = HTML("<img src = 'candidate_icon.png' width = '18'>candidate sites"),
            #     TRUE
            #   )
            # ),
            # styling for horizontal lines lifed from this example: http://www.iraqtimeline.com/maxdesign/basicdesign/principles/prinhorizontal.html
            HTML(horizontalLine_html),
            style = "
              padding: 5px;
              margin-bottom: 10px;
              border-color: #ccc;
              border-width: 3px;
              "
          ),
          
          # mainTab_addPreviousButton ####
          wellPanel(
            
            # The action button functions as the title
            div(
              title = mainTab_addPreviousButton_title,
              actionButton(
                inputId = "addPreviousGroups",
                label = span(
                  HTML(mainTab_addPreviousButton_label_image),
                  strong(mainTab_addPreviousButton_label)
                ),
                style = mainTab_addPreviousButton_css
              )
            ),
            shinyTree(
              outputId = "previousGroupList",
              checkbox = TRUE,
              theme = "default",
              themeIcons = FALSE,
              sort = TRUE
            ),
            # mainTab_updateTreeButton ####
            div(
                style = compareTab_updateTree_css,
                title = compareTab_updateTree_title,
                actionButton(
                  inputId = "updateMain",
                  label = compareTab_updateTree_label
              )
            ),
            style = mainTab_addPreviousButtonPanel_css
          
        ),
        
          
          # bootstrapPage(
          #   
          #   # This div contains a button for visualizing selected groups & stages
          #   
          #   div(
          #     style = "
          #     display: inline-block;
          #     padding: 0px 5px 15px 0px
          #     ",
          #     title = "
          #     Visualize selected groups / stages
          #     ",
          #     actionButton(
          #       "visSpatial",
          #       "Visualize Points"
          #     )
          #   ),
          #   
          #   div(
          #     style = "
          #     display: inline-block;
          #     padding: 0px 5px 15px 0px
          #     ",
          #     title = "
          #     Remove selected points on map
          #     ",
          #     actionButton(
          #       "hideSelectedGroups_sp",
          #       "Hide Points"
          #     )
          #   ),
          #   
          #   div(
          #     style = "
          #     display: inline-block;
          #     padding: 0px 5px 15px 0px
          #     ",
          #     title = "
          #     Download selected data as .csv file
          #     ",
          #     downloadButton(
          #       "downloadSelectedData_sp",
          #       "Download selected data"
          #     )
          #   )
          # ),
          
          style = "
                    padding: 0px 5px 0px 5px;
                    margin-bottom: 10px;
                    "
        )
      ),
    

      # compareTab ####
      tabPanel(
        title = strong(
          "Spatial Comparison"
        ),
        id = "formTabAllGroupSelections",
        value = "formTabAllGroupSelections",
        
        # This column contains the leaflet widget
        
        # compareTab_map ####
        column(
          width = 10,
          div(
            leafletOutput(
              outputId = "map_compare"
            # ),
            # absolutePanel(
            #   id = "statsPanel_comp", 
            #   class = "panel panel-default collapse in", 
            #   fixed = FALSE,
            #   draggable = FALSE, 
            #   top = 84, 
            #   left = "auto", 
            #   right = "auto", 
            #   bottom = 10,
            #   width = 330,
            #   # h5(textOutput(
            #   #   "n_sitesSelec_textOutput"
            #   # )),
            #   # h5(textOutput(
            #   #   "percent_coverage_textOutput"
            #   # ))
            # ),
            # absolutePanel(
            #   bottom = 10, 
            #   left = "auto",
            #   height = 30,
            #   actionButton(
            #     width = 330,
            #     inputId = 'plotBtn_comp', 
            #     label = textOutput(
            #       "collapseButton_textOutput_comp"
            #     ), 
            #     "data-toggle" ='collapse', 
            #     "data-target" = '#statsPanel_comp',
            #     style="opacity: .80; color: #fff; background-color: #a662e3; border-color: #a153e5"
            #   )
            )
          )
        ),
        
        # compareTab_sidebar ####
        column(
          width = 2,
          style = mainTab_sidebar_css,
          # compareTab_layerControls ####
          # Performance might be improved using the solution in https://stackoverflow.com/questions/42658225/put-leaflet-controls-outside-of-map-div-in-shiny-environment
          wellPanel(
            div(
              title = compareTab_layerControl_title,
              p(
                style = mainTab_layerControls_label_css,
                strong(mainTab_layerControls_label)
              )
            ),
            checkboxInput(
              inputId = "comp_busStops_button",
              label = HTML(mainTab_layerControls_GRiDBikes_label),
              TRUE
            ),
            checkboxInput(
              inputId = "comp_bikeways_button",
              label = HTML("<img src='bikeway_icon.png' width='36'> bikeways"),
              TRUE
            ),
            checkboxInput(
              inputId = "comp_scooterOD_button",
              label = HTML(scootOD_legend_HTML),
              TRUE
              # ),
              # sliderInput(
              #   inputId = "scooterOD_slider"
              #   , label = HTML(scootOD_legend_HTML)
              #   , min = .1
              #   , max = 1
              #   , value = .5
            ),
            HTML("<img src='studyArea_icon.png' width='36'> study area"),
         
            HTML(horizontalLine_html),
            style = compareTab_layerControlPanel_css
          ),
          
          # compareTab_selectGSI_panel ####
          wellPanel(
            div(
              p(
                style = compareTab_selectGSI_title_css,
                strong(compareTab_selectGSI_title)
              )
            ),
            tags$div(
              style =compareTab_selectGSI_tree_css,
              class = ".jstree-node",
              shinyTree(
                outputId = "comparePrevGroupList",
                checkbox = TRUE,
                theme = "default",
                themeIcons = FALSE,
                sort = TRUE
              )
            ),
            style = compareTab_selectGSI_panel_css
          ),
          
          # compareTab_updateTreeButton ####
          wellPanel(
            tags$div(
              style = compareTab_updateTree_css,
              title = compareTab_updateTree_title,
              actionButton(
                inputId = "updateSpatial",
                label = compareTab_updateTree_label
              )
            ),
            
            # bootstrapPage(
            #   
            #   # This div contains a button for visualizing selected groups & stages
            #   
            #   div(
            #     style = "
            #     display: inline-block;
            #     padding: 0px 5px 15px 0px
            #     ",
            #     title = "
            #     Visualize selected groups / stages
            #     ",
            #     actionButton(
            #       "visSpatial",
            #       "Visualize Points"
            #     )
            #   ),
            #   
            #   div(
            #     style = "
            #     display: inline-block;
            #     padding: 0px 5px 15px 0px
            #     ",
            #     title = "
            #     Remove selected points on map
            #     ",
            #     actionButton(
            #       "hideSelectedGroups_sp",
            #       "Hide Points"
            #     )
            #   ),
            #   
            #   div(
            #     style = "
            #     display: inline-block;
            #     padding: 0px 5px 15px 0px
            #     ",
            #     title = "
            #     Download selected data as .csv file
            #     ",
            #     downloadButton(
            #       "downloadSelectedData_sp",
            #       "Download selected data"
            #     )
            #   )
            # ),
            
            style = "
            padding: 0px 5px 0px 5px;
            margin-bottom: 10px;
            "
          )
        )
      # ),
      # 
      # # This tab panel contains the .pdf that contains the collablocation information
      # 
      # tabPanel(
      #   title = strong("Help"), id = "about", value = "help",
      #   br(),
      #   htmlOutput("quickStartGuide") # show a pdf with default size
      ) # tabPanel
    ) # tabSet
  ) # fluidRow
) # body

# shiny : sidebar ####

sidebar <- dashboardSidebar(
  disable = TRUE
) # sidebar

# shiny : ui ####

ui <- dashboardPage(
  title = "Collablocation: Geodesign Platform",
  header = header,
  sidebar = sidebar,
  body = body
) # ui

# shiny : server ####

server <- function(
  input,
  output,
  session
) {
  
  # allGroupInfo : connect to allGroupInfo_db mongo ####

  allGroupInfo_db <- mongo(
    collection = "allGroupInfo_db",
    db = workshop_dbName_gv,
    url = mongoURL_gv,
    options = ssl_options(weak_cert_validation = T, key = "rds-combined-ca-bundle.pem")
  )
  
  # allGroupInfo : make reactiveObject allGroupInfo() ####
  
  allGroupInfo <- reactive({
    input$stageSubmit
    print(c(
      "input$stageSubmit : allGroupInfo debug"
    ))
    return(
      allGroupInfo_db$find('{}')
    )
  }
  )
  
  # allSites_db : connect to allSites_db mongo ####
  
  allSites_db <- mongo(
    collection = "allSites_db",
    db = workshop_dbName_gv,
    url = mongoURL_gv,
    options = ssl_options(weak_cert_validation = T, key = "rds-combined-ca-bundle.pem")
  )
  
  # # X allSites() : make reactiveObject allSites() ####
  # 
  # allGroupInfo <- reactive({
  #   
  #   return(
  #     allSites_db$find('{}')
  #   )
  # }
  # )
  
  # tempe_candidates : connect to tempe_candidates_db mongo ####

  tempe_candidates_db <- mongo(
    collection = "tempe_candidates_db",
    url = mongoURL_gv,
    options = ssl_options(weak_cert_validation = T, key = "rds-combined-ca-bundle.pem")
  )
  
  # iterSelected_joinId : connect to groupSelected_joinId_db mongo ####

  groupSelected_joinId_db <- mongo(
    collection = "groupSelected_joinId_db",
    url = mongoURL_gv,
    options = ssl_options(weak_cert_validation = T, key = "rds-combined-ca-bundle.pem")
  )
  
  # # X groupSelected_joinId : make reactiveObject groupSelected_joinId() ####
  # groupSelected_joinId <- reactive(
  #   return(
  #     groupSelected_joinId_db$find('{}')
  #   )
  # )
  
  # grid_serviceArea_join_db : connect to grid_serviceArea_join_db mongo ####
  
  grid_serviceArea_join_db <- mongo(
    collection = "grid_serviceArea_join_db",
    url = mongoURL_gv,
    options = ssl_options(weak_cert_validation = T, key = "rds-combined-ca-bundle.pem")
  )
  
  servArea_4thMi_join_db <- mongo(
    collection = "servArea_4thMi_join_db",
    url = mongoURL_gv,
    options = ssl_options(weak_cert_validation = T, key = "rds-combined-ca-bundle.pem")
  )
  
  servArea_8thMi_join_db <- mongo(
    collection = "servArea_8thMi_join_db",
    url = mongoURL_gv,
    options = ssl_options(weak_cert_validation = T, key = "rds-combined-ca-bundle.pem")
  )
  
  
# shiny : openingScreen : users input GSI popup #####
  popUp_openingScreen_function()
  
# shiny : openingScreen : validate input$groupName ####
  output$groupNameModal <- renderText(
    validateGroupName_function(
      groupName_lv = input$groupName
    )
  )
  
# shiny : openingScreen : users click 'Begin', clear popup ####

  observeEvent(
    eventExpr = input$groupName_submit,
    handlerExpr = {
      validateGroupName_function(
        groupName_lv = input$groupName
      )
      removeModal()
    }
  )
# groupStageIter : make reactiveValue iterNum_activeSesh() ####
  
# begins as a NULL value
  iterNum_activeSesh <- reactiveVal(
    val = NULL
  )
  
# groupStageIter : make reactiveObject iterNum() ####
  
# When users click input$submitStage, this sets the iterNum for
# the active session to (the number of previous iterations for
# this group&stage pair) + 1
  iterNum <- reactive({
    setIterationNumber_function(
      groupName_lv = input$groupName,
      stageNum_lv = input$stageNum,
      iterNum_lv = iterNum_activeSesh(),
      gSI_database_lv = allGroupInfo_db
    )
  })
  
# groupStageIter : make padded iter number reactiveObject paddedIter() ####
  paddedIter <- reactive({
    return(
      makePaddedIterNum_function(
        iterNum_lv = iterNum()
      )
    )
  })

# groupStageIterInfo() : reactive object for GSI data ####
# make unique GSI ID strings
  groupStageIterInfo <- reactive({
    makeUniqueGSI_function(
      groupName_lv = input$groupName,
      stageNum_lv = input$stageNum,
      iterNum_lv = paddedIter()
    )
  })
  

  
# temp_db : check mongo for existing db ####
  # if none exists, make one
  
  gsi_dbName <- reactive({
    gsi_dbName <- glue(
      "{gsi_lv}_db",
      gsi_lv = groupStageIterInfo()
    )
  })

  temp_db <- reactive({
    mongo(
      collection = gsi_dbName(),
      db = workshop_dbName_gv,
      url = mongoURL_gv,
      options = ssl_options(weak_cert_validation = T, key = "rds-combined-ca-bundle.pem")
    )
  })

  observeEvent(
    eventExpr = input$groupName_submit,
    handlerExpr = {
    print(c(
      "debug temp_db initialize 1"
    ))
    if(
      gsi_dbName() %in% allGroupInfo_db$run('{"listCollections":1}')$cursor$firstBatch$name
    ){
      print(c(
        "debug temp_db initialize 2"
      ))
      
      s <- st_sf(st_sfc(crs = projectCRS_gv))
      t <- temp_db()$find("{}")
      
      print(c(
        "debug temp_db initialize 3",
        t
      ))
      
      if(nrow(t)>0){
        for(i in 1:nrow(t)){
          
          g <- st_point(
            x = c(
              t[i,]$geometry$coordinates[[1]][1],
              t[i,]$geometry$coordinates[[1]][2]
            )
          ) %>% st_sfc(
            crs = projectCRS_gv
          )
          
          s <- s %>% rbind(
            st_sf(
              geometry = g,
              cellId = t[i,]$cellId,
              siteId = t[i,]$siteId
            )
          )
        }
        
        print(c(
          "debug temp_db initialize 4"
        ))
        
        tempe_sites_selected$sf <- tempe_sites_selected$sf %>% rbind(
          s
        )
      }
      
      
      print(c(
        "debug temp_db initialize 6"
      ))
    }
  })
  
  
  # mongo : insert to tempSelected_db ####

    observeEvent(

      # TODO Make this selectable in output$prevGroupList

      # Just making a group-specific database with
      # the current groupStageIteration's gsi_info & candidate uniqueId pairs

      eventExpr = tempe_sites_selected$sf,
      handlerExpr = {

        # ct_S <- tempe_selected$sf
        #
        # gSII <- groupStageIterInfo()
        #
        # # Now we create a dataframe comprising the groupStageIterInfo and the
        # # selected stations' uniqueId values:
        #
        # # repreat the string for the number of selected stations
        #
        # gSIInfo_nTimes <- rep( gSII, nrow(ct_S) )
        #
        # # join the groupStageIterInfo unique ID with the ct_selected unique ID
        # # so that each row contains a unique combination of groupStageIterInfo
        # # and uniqueId
        #
        # currentTempSelection <- data.frame(
        #   groupStageIterInfo = gSIInfo_nTimes,
        #   siteId = tempe_sites_selected$sf$siteId,
        #   cellId = tempe_sites_selected$sf$cellId,
        #   lat =
        # )

        temp_db()$remove('{}')
        temp_db()$insert(tempe_sites_selected$sf)

      }
    )



    # temporarySessionInfo : drop tempSelec db on input$stageSubmit ####

    observeEvent(
      eventExpr = input$stageSubmit,
      handlerExpr = {

        temp_db()$drop()

      }
    )
  
  
# shiny : render GSI in header ####
  output$gSI_header <- renderUI({
    makeGSIheader_ui_function(
      groupName_lv = input$groupName,
      stageNum_lv = input$stageNum,
      padIterNum_lv = paddedIter()
    )
  })
  
  # tempe_candidates$sf : reactive values for grid cells ####
  
  # this needs to be tempe_vectorGrid$sf
  
  tempe_candidates <- reactiveValues(
    sf = candGridDebug # debug
  )
  
  # tempe_centroids$sf : reactive value for grid centroids
  
  tempe_centroids <- reactiveValues(
    sf = candGridDebug %>% st_centroid()
  )
  
  # tempe_overlayRaster : overlay grid for user iteraction ####
  
  # This needs to be as dumb as possible. a grid of cells with their unique ids
  
  tempe_overlayRaster <- candGridDebug %>% transmute(
    overlay_id = glue(
      "overlay_{id_lv}",
      id_lv = .$id
    )
  )
  
  # tempe_selected : reactive object for selected grid cells ####
  
  tempe_selected <- reactive({
    
    selectedSubset <- tempe_candidates$sf$id %in% tempe_sites_selected$sf$cellId
    
    return(
      tempe_candidates$sf[
        selectedSubset,
      ]
    )
  })
  
  # tempe_covered : reactive object for covered grid cells ####
  
  tempe_covered <- reactive({
    
    # any way to speed this up at scale? Maybe store covered ids as string list for each
    # cell in mongo, then dynamically merge the covered id lists of each 
    # cell in tempe_selected()
    
    coveredSubset <- tempe_candidates$sf$id %in% tempe_covered_id()
    
    c <- tempe_candidates$sf[coveredSubset,]
    
    return(c)
    
  })
  
  tempe_4thMi_covered <- reactive({

    coveredSubset <- tempe_candidates$sf$id %in% tempe_4thMi_covered_id()
    
    return(tempe_candidates$sf[coveredSubset,])
    
  })
  
  tempe_8thMi_covered <- reactive({
    
    coveredSubset <- tempe_candidates$sf$id %in% tempe_8thMi_covered_id()
    
    return(tempe_candidates$sf[coveredSubset,])
    
  })
  
  # tempe_sites_selected$sf : reactive value for selected locations ####
  
  tempe_sites_selected <- reactiveValues(
    sf = {
      st_sf(st_sfc()) %>% st_set_crs(projectCRS_gv)
    }
  )
  
  # tempe_covered_id : make covered list ####
  
  # we need to set the "covered" value to 1 for any cells whose id links via the 
  # join table to cells with "selected" == 1 
  
  # build a list of cell ids with "selected" == 1
  # query the join table for associated cell ids
  # set "covered" to 1 in the tempe_candidates reactive object
  
  tempe_4thMi_covered_id <- reactive({
    
    if(nrow(tempe_selected()) > 0){
      serviceAreaQuery <- makeDBquery_valuesInList_function(
        listToQuery_lv = tempe_selected()$id,
        fieldToCheck_lv = "servAreaId"
      )
      
      
      coveredGridIdList <- servArea_4thMi_join_db$find(
        
        # the $find() operation queries the database using JSON syntax
        query = serviceAreaQuery
        
        # the $find() operation returns all fields except the '_id' field by default. Since we only want the 'groupStageIterInfo', 'stageNum', and iterNum' fields, we need to explicitly disable '_id'
        # fields = '{{"_id" : false}}'
      )
      
      c <- c(coveredGridIdList$gridCell, tempe_selected()$id) %>% unique()
      
      return(c)
      
    }
    
  })
  
  tempe_8thMi_covered_id <- reactive({
    
    if(nrow(tempe_selected()) > 0){
      serviceAreaQuery <- makeDBquery_valuesInList_function(
        listToQuery_lv = tempe_selected()$id,
        fieldToCheck_lv = "servAreaId"
      )
      
      
      coveredGridIdList <- servArea_8thMi_join_db$find(
        
        # the $find() operation queries the database using JSON syntax
        query = serviceAreaQuery
        
        # the $find() operation returns all fields except the '_id' field by default. Since we only want the 'groupStageIterInfo', 'stageNum', and iterNum' fields, we need to explicitly disable '_id'
        # fields = '{{"_id" : false}}'
      )
      
      c <- c(coveredGridIdList$gridCell, tempe_selected()$id) %>% unique()
      
      return(c)
      
    }
    
  })
  
  tempe_covered_id <- reactive({
    
    if(nrow(tempe_selected()) > 0){
      serviceAreaQuery <- makeDBquery_valuesInList_function(
        listToQuery_lv = tempe_selected()$id,
        fieldToCheck_lv = "servAreaId"
      )
      
      
      coveredGridIdList <- grid_serviceArea_join_db$find(
        
        # the $find() operation queries the database using JSON syntax
        query = serviceAreaQuery
        
        # the $find() operation returns all fields except the '_id' field by default. Since we only want the 'groupStageIterInfo', 'stageNum', and iterNum' fields, we need to explicitly disable '_id'
        # fields = '{{"_id" : false}}'
      )
      
      return(coveredGridIdList$gridCell)

    }
  })
  
  # tempe_sites_selected$sf : update on map_shape_click ####
  
  observeEvent(
    eventExpr = input$map_main_shape_click,
    handlerExpr = {
      
      clicked <- input$map_main_shape_click
      
      if(
        !is.null(clicked$id)
      ){
        if(
          grepl(
            "overlay_",
            clicked$id
          )
        ){
          clicked_rawId <- clicked$id %>% str_remove(
            "overlay_"
          ) %>% as.numeric()
          
          site_lng <- sprintf(
            "%.5f",
            clicked$lng
          ) %>% as.numeric()
          
          site_lat <- sprintf(
            "%.5f",
            clicked$lat
          ) %>% as.numeric()
          
          site_cellId <- clicked_rawId 
          
          sitePoint_sf <- st_point(
            x = c(
              site_lng,
              site_lat
            )
          ) %>% st_sfc()
          
          siteId_lv <- glue(
            "site_{lat_lv}_{lng_lv}",
            lat_lv = site_lat,
            lng_lv = site_lng
          )
          
          
          if(!(siteId_lv %in% tempe_sites_selected$sf$siteId)){
            
            tempe_sites_selected$sf <- tempe_sites_selected$sf %>% rbind(
              st_sf(
                geometry = sitePoint_sf,
                cellId = site_cellId,
                siteId = siteId_lv
              ) %>% st_set_crs(projectCRS_gv)
            )
          } else{
            # TODO debug 
            # This needs to also remove the would-be site from tempe_sites_selected
            
            # leafletProxy(
            #   mapId = "map_main"
            # ) %>% removeMarker(
            #   layerId = clicked$id
            # )
            
            tempe_sites_selected$sf <- tempe_sites_selected$sf %>% subset(
              !(siteId == siteId_lv)
            )
          }
        }
        
        # print(c(
        #   "debug tempe_sites_selected : update on map_shape_click",
        #   paste("cellId = ", tempe_sites_selected$sf$cellId),
        #   paste("siteId = ", tempe_sites_selected$sf$siteId),
        #   "------"
        # ))
      }
    }
  )
  
  # tempe_sites_selected : deselect on click ####
  
  # TODO still need to make the marker go away if we click on the same spot
  
  observeEvent(
    eventExpr = input$map_main_marker_click,
    handlerExpr = {
      clicked <- input$map_main_marker_click
      
      # check if the marker has a layerId
      if(
        !is.null(clicked$id)
      ){
        # check if the marker is in the "site_" group
        if(
          grepl(
            x = clicked$id,
            pattern = "site_" # debug
          )
        ){
          
          # print(c(
          #   "debug : click to deselect",
          #   paste0(
          #     "clicked$id is : ",
          #     clicked$id
          #   ),
          #   paste0(
          #     "tempe_sites_selected$sf$id is : ",
          #     tempe_sites_selected$sf$id
          #   ),
          #   paste0(
          #     "nrow tempe_sites_selected$sf is : ",
          #     nrow(tempe_sites_selected$sf)
          #   )
          #   
          # ))
          
          tempe_sites_selected$sf <- tempe_sites_selected$sf %>% subset(
            siteId != clicked$id
          )
          
          # print(c(
          #   "debug : click to deselect",
          #   paste(
          #     "nrow tempe_sites_selected$sf after subset() is : ",
          #     nrow(tempe_sites_selected$sf)
          #   ),
          #   paste(
          #     "nrow tempe_selected$sf is : ",
          #     nrow(tempe_selected())
          #   ),
          #   "------"
          # ))
          
          leafletProxy(
            mapId = "map_main"
          ) %>% removeMarker(
            layerId = clicked$id
          )
        }
      }
    }
  )
  
  
  
  
  # currentGroupSelection() : data frame to save to allGroupInfo_db ####
  
  # NOTE: This is doing way too many things.
  # On input$stageSubmit, it : 
  # - inserts allGroupInfo_db with the submitted GSI, 
  # - inserts groupSelected_joinId_db with all pairs of GSI and siteId,
  # - upserts all selected site data into allSites_db
  
  
  currentGroupSelection <- reactive({
   
    if(
      nrow(tempe_sites_selected$sf) > 0
    ) {
      # create a JSON to insert in mLab database
      gSII_lv <- groupStageIterInfo()
      sites_lv <- tempe_sites_selected$sf
      
      sites_csv_lv <- glue_collapse(
        x = double_quote(
          sites_lv$siteId
        ),
        sep = ", "
      )
      
      cells_csv_lv <- glue_collapse(
        x = double_quote(
          sites_lv$cellId
        ),
        sep = ", "
      )
      
      currentGroupSelection <- c(
        glue('{{
            "groupStageIterInfo" : "{gSII}",
            "dateCreated" : "{dC}",
            "timeCreated" : "{tC}",
            "groupName" : "{gN}",
            "stageNum" : "{sN}",
            "iterNum" : "{iN}",
            "cells_selected" : [{cells_csv}],
            "sites_selected" : [{sites_csv}]
            }}',
             
             gSII = gSII_lv,
             dC = Sys.Date(),
             tC = Sys.time(),
             gN = input$groupName,
             sN = input$stageNum,
             iN = iterNum(),
             cells_csv = cells_csv_lv,
             sites_csv = sites_csv_lv
        )
      )
      
      return(
        currentGroupSelection
      )
    }
  })
  # mongo: save currentGroupSelection to db on input$stageSubmit ####
  observeEvent(
    eventExpr = input$stageSubmit,
    handlerExpr = {
      
      if(
        nrow(tempe_sites_selected$sf) > 0
      ) {

        allGroupInfo_db$insert(currentGroupSelection())

      }
    })
  
  # TODO: split this into at least two separate observeEvent operations
  # mongo: save gsi_info & siteId pairs to groupSelected_joinId_db on input$stageSubmit ####
  
  observeEvent(
    eventExpr = input$stageSubmit,
    handlerExpr = {
      
      if(
        nrow(tempe_sites_selected$sf) > 0
      ) {
        # create a JSON to insert in mLab database
        gSII_lv <- groupStageIterInfo()
        sites_lv <- tempe_sites_selected$sf
        
        # Now we create a dataframe comprising the groupStageIterInfo and the
        # selected stations' uniqueId values:
        
        # repreat the string for the number of selected stations
        
        gSIInfo_nTimes <- rep( gSII_lv, nrow(sites_lv) )
        
        # join the groupStageIterInfo unique ID with the ct_selected unique ID
        # so that each row contains a unique combination of groupStageIterInfo
        # and uniqueId
        
        currentGroupSelected_joinId <- data.frame(
          groupStageIterInfo = gSIInfo_nTimes,
          siteId = sites_lv$siteId
        )
        
        # upload it to groupSelected_join_db
        
        groupSelected_joinId_db$insert(currentGroupSelected_joinId)
      }
    })
  
  
  observeEvent(
    eventExpr = input$stageSubmit,
    handlerExpr = {
      
      if(
        nrow(tempe_sites_selected$sf) > 0
      ) {
        # create a JSON to insert in mLab database
        # then upload all newly selected points as geoJSON to allSites_db
        
        for(i in 1:nrow(tempe_sites_selected$sf)){
          x <- tempe_sites_selected$sf[i,]
          
          allSites_mongoUpdateQuerySiteId_lv <- glue(
            '{{
              "siteId" : "{siteId_glue}"
            }}',
            siteId_glue = x$siteId
          )
          
          site_JSON_toUpdate <- glue(
            # This is in geoJSON format. 
            # glue('{{"$set" : {{sf_geojson(x)}}}}) might also be viable, idk
            # '{{"$set":{{"type":"FeatureCollection","features":[{{"type":"Feature","properties":{{"cellId":{cellId_glue},"siteId":"{siteId_glue}"}},"geometry":{{"type":"Point","coordinates":[{lng_glue},{lat_glue}]}}}}]}}}}',
            '{{
              "$set" : {{
                "cellId" : {cellId_glue},
                "siteId" : "{siteId_glue}",
                "lng" : {lng_glue},
                "lat" : {lat_glue}
              }}
            }}',
            
            cellId_glue = x$cellId,
            siteId_glue = x$siteId,
            lng_glue = st_coordinates(x)[1],
            lat_glue = st_coordinates(x)[2]
          )
          
          allSites_db$update(
            query = allSites_mongoUpdateQuerySiteId_lv,
            update = site_JSON_toUpdate,
            upsert = TRUE,
            multiple = TRUE
          )
        }
      }
    })
  
  observeEvent(
    eventExpr = input$stageSubmit,
    handlerExpr = {
      
      if(
        nrow(tempe_sites_selected$sf) > 0
      ) {
        
        # increment the iteration number
        
        # newIterNum <- iterNum() + 1
        
        iterNum_activeSesh(
          iterNum() + 1
        )
      }
    })
  
  observeEvent(
    eventExpr = input$stageSubmit,
    handlerExpr = {

      if(
        nrow(tempe_sites_selected$sf) == 0
      ) {
        showModal(
          modalDialog(
            title = "Invalid submission",
            "Please select at least one candidate site",
            easyClose = TRUE
          )
        )
      }
    }
  )
  
  # nLOPGTR_bS : update on input$stageSubmit ####
  # The goal here is to make leaflet display the points that have been previously
  # selected by the groups chosen by the user in the input list.

  nestedListOfPrevGroupsToRender_byStage <- reactive({
    # print(c(
    #   "nestedListOfPrevGroupsToRender_byStage() debug 1"
    # ))
    g <- nestedList_allGroups_function(
      allGroups_lv = allGroupInfo()
    )
    
    # print(c(
    #   "nestedListOfPrevGroupsToRender_byStage() debug 2"
    # ))
    
    return(g)

  })
  
  # prevGroups_all_listNamesGSII() : format names for output$previousGroupList ####

  # TODO: generate formatted names beforehand, then assign them for the tree

  prevGroups_all_listNamesGSII <- reactive({
    
    # input$updateSpatial
    # input$updateMain
    
    # print(c(
    #   "prevGroups_all_listNamesGSII() debug 1"
    # ))

    # get all groups

    allGroupInfo <- allGroupInfo()

    prevGroupNamesFormatted <- allGroupInfo %>% glue_data(

      # format the groupName, stageNum, and iteration number (padded)

      "{groupName}.{stageNum}.{paddedIterNum}",
      groupName = .$groupName,
      stageNum = .$stageNum,
      paddedIterNum = str_pad(
        string = .$iterNum,

        # NOTE: the padded to two digits is hard-coded, we assume
        # that we will never get to 100 iterations in one stage.

        width = maxIterDigits_gv,
        pad = "0"
      )
    )
    
    # set the names of allGroupInfo$groupStageIterInfo to the formatted names

    prevGroups_listWithNames <- setNames(
      as.list(
        allGroupInfo$groupStageIterInfo
      ),
      prevGroupNamesFormatted
    )

    return(
      prevGroups_listWithNames
    )
  })
  
  # prevGroups_selectedNames : format as slices ####
  
  prevGroups_selectedNames <- reactive({
    
    # print(c(
    #   "prevGroups_selectedNames() debug 1"
    # ))
    
    selectedGroups <- names(
      as.data.frame(
        get_selected(
          input$previousGroupList, 
          format = "slices"
        )
      )
    )
    
    # print(c(
    #   "prevGroups_selectedNames() debug 2",
    #   selectedGroups
    # ))
    
    return(selectedGroups)
  })
  
  # prevGroups_selected() : update on input$previousGroupList ####
  
  prevGroups_selected <- reactive({
    
    # print(c(
    #   "prevGroups_selected() debug 1"
    # ))
    
    if(
        length(prevGroups_selectedNames()) > 0 
    ){
      
      # print(c(
      #   "prevGroups_selected() debug 2"
      # ))

      # this selects the groupStageIterInfo by name, where that name is in the list of selected groups
      
      prevGroups_selectedMatched <- prevGroups_all_listNamesGSII()[
        prevGroups_selectedNames()
      ]
      
      # print(c(
      #   "prevGroups_selected() debug 3"
      # ))
      
      # This returns the unnamed list as a list of groupStageIterInfo selected
      
      return(
        unname(prevGroups_selectedMatched)
      )
    } else {
      print(c(
        "prevGroups_selected() debug 4"
      ))
    }
  })
  
  # groupStationPairsToAdd() : query groupSelected_joinId_db for selected siteIds ####

  groupStationPairsToAdd <- reactive({
    
    # print(c(
    #   "main_groupStationPairsToAdd() debug 1"
    # ))

    # # if the list of selected groups is not null:
    # if(
    #   !is.null(prevGroups_selected())
    # ){
      
      # first we need to create a list of previous groups to add
      # that is a string of groupStageIteration unique identifiers
      # We'll use this to build the mongolite query as a JSON object

      prevGroupsToAdd_query <- makeDBquery_valuesInList_function(
        listToQuery_lv = prevGroups_selected() %>% double_quote(),
        fieldToCheck_lv = "groupStageIterInfo"
      )
      
      # print(c(
      #   "main_groupStationPairsToAdd() debug 2",
      #   "prevGroupsToAdd_query is :",
      #   prevGroupsToAdd_query
      # ))

      # Then we're using the query to call from the joining database
      # This gets us the uniqueIds for sites that were in
      # previous groups
      
      # We'll later query a db for all sites ever selected by a group
      # and add that to the map

      groupSitePairs <- groupSelected_joinId_db$find(
        query = prevGroupsToAdd_query,
        fields = '{"_id" : false}'
      )
      
      print(c(
        "main_groupStationPairsToAdd() debug 3",
        "groupSitePairs$siteId %>% unique() is : ",
        groupSitePairs$siteId %>% unique(),
        "is.null(groupSitePairs$siteId %>% unique()) : ",
        is.null(groupSitePairs$siteId %>% unique())
      ))
      
      return(
        groupSitePairs$siteId %>% unique()
      )
  # }
    # else{}
  })
  
  # sitesToAdd() : generate list of siteIds ####
  
  # next up we need to query allSites_db$find({}) for the sitesToAdd
  
  prevSelecSitesToAdd <- reactive({
    
    print(c(
      "prevSelecSitesToAdd() debug 1"
    ))
    
        prevSitesToAdd_query <- makeDBquery_valuesInList_function(
          listToQuery_lv = double_quote(
            groupStationPairsToAdd()
          ),
          fieldToCheck_lv = "siteId"
        )
        
        print(c(
          "prevSelecSitesToAdd() debug 2",
          "prevSitesToAdd_query is : ",
          prevSitesToAdd_query,
          "is.null(prevSitesToAdd_query) : ",
          is.null(prevSitesToAdd_query),
          "class(prevSitesToAdd_query) : ",
          class(prevSitesToAdd_query)
        ))
        
        prevSelecSitesToAdd <- allSites_db$find(
          query = prevSitesToAdd_query,
          fields = '{"_id" : false}'
        )
        
        print(c(
          "prevSelecSitesToAdd() debug 3"
        ))
    
        return(prevSelecSitesToAdd)
  })
  
  # tempe_sites_prevSelected() : make reactiveObject tempe_sites_prevSelected ####
  
  
  tempe_sites_prevSelected <- reactive({
    
    print(c(
      "debug tempe_sites_prevSelected() 1",
      "nrow(prevSelecSitesToAdd)",
      nrow(prevSelecSitesToAdd())
    ))

    if(nrow(prevSelecSitesToAdd()) > 0){
      
    g <- prevSelecSitesToAdd() %>% st_as_sf(
      coords = c(
        "lng",
        "lat"
      )
    ) %>% st_set_crs(
      value = projectCRS_gv
    )

    return(
      g
    )
    
    } else {
      return(
        data.frame()
      )
    }
  })

  
  # tempe_prevCovered_id() : query grid_serviceArea_join_db for covered ids ####
  
  tempe_4thMi_prevCovered_id <- reactive({
    
    if(nrow(tempe_sites_prevSelected()) > 0){
      
      # print(c(
      #   "debug tempe_prevCovered_id() : "
      # ))
      
      
      serviceAreaQuery <- makeDBquery_valuesInList_function(
        listToQuery_lv = tempe_sites_prevSelected()$cellId,
        fieldToCheck_lv = "servAreaId"
      )
      
      
      prevCoveredGridIdList <- servArea_4thMi_join_db$find(
        
        # the $find() operation queries the database using JSON syntax
        query = serviceAreaQuery
        
        # the $find() operation returns all fields except the '_id' field by default. Since we only want the 'groupStageIterInfo', 'stageNum', and iterNum' fields, we need to explicitly disable '_id'
        # fields = '{{"_id" : false}}'
      )
      
      return(prevCoveredGridIdList$gridCell)
      
    }
  })
  
  tempe_8thMi_prevCovered_id <- reactive({
    
    if(nrow(tempe_sites_prevSelected()) > 0){
      
      # print(c(
      #   "debug tempe_prevCovered_id() : "
      # ))
      
      
      serviceAreaQuery <- makeDBquery_valuesInList_function(
        listToQuery_lv = tempe_sites_prevSelected()$cellId,
        fieldToCheck_lv = "servAreaId"
      )
      
      
      prevCoveredGridIdList <- servArea_8thMi_join_db$find(
        
        # the $find() operation queries the database using JSON syntax
        query = serviceAreaQuery
        
        # the $find() operation returns all fields except the '_id' field by default. Since we only want the 'groupStageIterInfo', 'stageNum', and iterNum' fields, we need to explicitly disable '_id'
        # fields = '{{"_id" : false}}'
      )
      
      return(prevCoveredGridIdList$gridCell)
      
    }
  })
  
  tempe_prevCovered_id <- reactive({
    
    if(nrow(tempe_sites_prevSelected()) > 0){
      
      # print(c(
      #   "debug tempe_prevCovered_id() : "
      # ))
      
      
      serviceAreaQuery <- makeDBquery_valuesInList_function(
        listToQuery_lv = tempe_sites_prevSelected()$cellId,
        fieldToCheck_lv = "servAreaId"
      )
      
      
      prevCoveredGridIdList <- grid_serviceArea_join_db$find(
        
        # the $find() operation queries the database using JSON syntax
        query = serviceAreaQuery
        
        # the $find() operation returns all fields except the '_id' field by default. Since we only want the 'groupStageIterInfo', 'stageNum', and iterNum' fields, we need to explicitly disable '_id'
        # fields = '{{"_id" : false}}'
      )
      
      return(prevCoveredGridIdList$gridCell)
      
    }
  })
  
  # tempe_prevCovered() : make reactiveObject tempeCovered ####
  
  tempe_prevCovered <- reactive({
    
    # print(c(
    #   "debug tempe_prevCovered() : "
    # ))
    
    # any way to speed this up at scale? Maybe store covered ids as string list for each
    # cell in mongo, then dynamically merge the covered id lists of each 
    # cell in tempe_selected()
    
    prevCoveredSubset <- tempe_candidates$sf$id %in% tempe_prevCovered_id()
    
    g <- tempe_candidates$sf[prevCoveredSubset,] %>% mutate(
      layerId = glue(
        "prevCovered_{id_lv}",
        id_lv = .$id
      )
    )
    
    return(g)
    
  })
  
  tempe_8thMi_prevCovered <- reactive({
    
    # print(c(
    #   "debug tempe_prevCovered() : "
    # ))
    
    # any way to speed this up at scale? Maybe store covered ids as string list for each
    # cell in mongo, then dynamically merge the covered id lists of each 
    # cell in tempe_selected()
    
    prevCoveredSubset <- tempe_candidates$sf$id %in% tempe_8thMi_prevCovered_id()
    
    g <- tempe_candidates$sf[prevCoveredSubset,] %>% mutate(
      layerId = glue(
        "prevCovered_{id_lv}",
        id_lv = .$id
      )
    )
    
    return(g)
    
  })
  
  tempe_4thMi_prevCovered <- reactive({
    
    # print(c(
    #   "debug tempe_prevCovered() : "
    # ))
    
    # any way to speed this up at scale? Maybe store covered ids as string list for each
    # cell in mongo, then dynamically merge the covered id lists of each 
    # cell in tempe_selected()
    
    prevCoveredSubset <- tempe_candidates$sf$id %in% tempe_4thMi_prevCovered_id()
    
    g <- tempe_candidates$sf[prevCoveredSubset,] %>% mutate(
      layerId = glue(
        "prevCovered_{id_lv}",
        id_lv = .$id
      )
    )
    
    return(g)
    
  })
  
  # input$addPreviousGroups : update tempe_sites_selected$sf ####
  observeEvent(
    eventExpr = input$addPreviousGroups,
    handlerExpr = {
      
      print(c(
        "debug input$addPreviousGroups : ",
        # "is.null(tempe_sites_prevSelected()) : ",
        # is.null(tempe_sites_prevSelected()),
        # "tempe_sites_prevSelected() is : ",
        # tempe_sites_prevSelected(),
        "nrow(tempe_sites_prevSelected()) : ",
        nrow(tempe_sites_prevSelected()),
        "input$addPreviousGroups : ",
        input$addPreviousGroups
      ))
      
      if(
        # need to clear tempe_sites_prevSelected() whenever 
        # input$previousGroupList is empty
        # also need to clear the markers when we click on em
        nrow(tempe_sites_prevSelected()) > 0
      ) {
        for(i in 1:nrow(tempe_sites_prevSelected())){
      
          x <- tempe_sites_prevSelected()[i,]
      
          if(!(x$siteId %in% tempe_sites_selected$sf$siteId)){
      
            tempe_sites_selected$sf <- tempe_sites_selected$sf %>% rbind(x)
      
          }
        }
      }
    }
  )
  
  # input$stageSubmit : clear leaflet markers prevSelec ####

  observeEvent(
    eventExpr = input$stageSubmit, # ALT: COULD ACTIVATE ON input$addPreviousGroups
    handlerExpr = {
      
      print(c(
        "input$stageSubmit : clear prevSelec debug"
      ))
      
      leafletProxy(
        "map_main"
      )  %>% clearGroup(
        group = "prevSelec"
      )
    }
  )
  
  # shiny : input$stageSubmit : pop up notification ####
  observeEvent(
    eventExpr = input$stageSubmit,
    handlerExpr = {
      print(c(
        "input$stageSubmit : pop-up debug"
      ))
      if(nrow(tempe_sites_selected$sf) > 0) {
        showModal(
          modalDialog(
            title = "Selection submitted",
            easyClose = TRUE
          )
        )
      }
    },
    priority = -1
  )
  
  # mainTab : render output$map_main ####
  
  output$map_main <- renderLeaflet({
    
    leaflet(
      options = leafletOptions(
        minZoom = 8
      )
    ) %>%
      makeLeafletMap_function(
        initialLat_lv = mapMain_startLat_gv,
        initialLng_lv = mapMain_startLng_gv,
        initialZoom_lv = mapMain_startZoom_gv
      ) %>% addLeafletMapPane_function(
      ) %>% addRasterImage(
        x = scooterOD_raster,
        colors = palette_scootOD,
        group = "scooterODLayer",
        opacity = .5,
        project = FALSE
        # # this will work with the next leaflet update; it currently does not
        # now and the raster needs to be added to the map in order
        # , options = tileOptions(
        #   pane = mapMain_scooterODPane_gv
        # )
      ) %>% addPolylines(
        data = valley_bikeways_sf,
        group = "bikewaysLayer",
        color = "lime",
        dashArray = "3,10",
        weight = 6,
        opacity = .9,
        options = pathOptions(
          pane = mapMain_bikewaysPane_gv
        )
      ) %>% addPolylines(
        data = busRoutesFixed,
        group = "busStopsLayer",
        color = "orangered",
        opacity = .9,
        weight = 8,
        options = pathOptions(
          pane = mapMain_busRoutesPane_gv
        ) 
      ) %>% addPolylines(
        data = busRoutesFlag,
        group = "busStopsLayer",
        color = "darkviolet",
        weight = 8,
        opacity = .9,
        options = pathOptions(
          pane = mapMain_busRoutesPane_gv
        )
      ) %>% addMarkers(
        data = tempe_streetcar_sf,
        icon = streetcar_icon,
        group = "busStopsLayer",
        options = pathOptions(
          pane = mapMain_busStopsPane_gv
        )
      ) %>% addMarkers(
        data = valley_busStops_sf,
        icon = busStation_icon,
        group = "busStopsLayer",
        options = pathOptions(
          pane = mapMain_busStopsPane_gv
        )
      ) %>% addPolygons(
        data = tempe_overlayRaster, # debug
        layerId = tempe_overlayRaster$overlay_id,
        group = "overlayLayer",
        # label = ~overlay_id,
        fillColor = "transparent",
        fillOpacity = 0,
        color = "transparent",
        weight = 1,
        opacity = 0,
        # options = pathOptions(pane = "candidates")
        options = pathOptions(
          pane = mapMain_overlayGridPane_gv
        )
      ) %>% addPolygons(
        data = studyArea,
        group = "studyArea",
        fillColor = "transparent",
        fillOpacity = 0,
        color = "black",
        dashArray = "3,10",
        weight = 6,
        opacity = .9,
        options = pathOptions(
          pane = mapMain_studyAreaPane_gv
        )
      )
  })
  
  
  
  # max data values ####
  
  max_scootOD_gv <- reactive({
    max(
      tempe_candidates$sf$scoot_OD_b, na.rm = TRUE
    )
  })
  
  sum_scootOD_gv <- reactive({
    sum(
      tempe_candidates$sf$scoot_OD_b, na.rm = TRUE
    )
  })
  
  
  maxPCT_INCOME_gv <- reactive({
    sum(
      tempe_candidates$sf$povHHCount, na.rm = TRUE
    )
  })
  
  # dataframes for stats charts ####
  
    tempe_covered_plot <- reactive({
    
    # print(
    #   c(
    #     "tempe_8thMi_covered() fields debug",
    #     colnames(tempe_8thMi_covered()),
    #     mean(tempe_8thMi_covered()$PCT_INCOME,
    #          na.rm = TRUE)
    #   )
    # )
    
    max_scootOD_lv <- sum_scootOD_gv()
    
    c = tempe_4thMi_covered()
    d = tempe_8thMi_covered()
    
    scootOD_4thMi_covered <- sum(
      c$scoot_OD_b, 
      na.rm = TRUE
    )
    pct_income_4thMi_covered <- sum(
      c$povHHCount, 
      na.rm = TRUE
    ) / maxPCT_INCOME_gv() * 100
    
    scootOD_8thMi_covered <- sum(
      d$scoot_OD_b,
      na.rm = TRUE
    )
    pct_income_8thMi_covered <- sum(
      d$povHHCount,
      na.rm = TRUE
    ) / maxPCT_INCOME_gv() * 100
    
    
    x <- data.frame(
      coveredRange = c(
        "1/8 mi.",
        "1/4 mi."
      ),
      pct_cov = c(
        as.integer(
          {(nrow(d) / nrow(tempe_candidates$sf) * 100)}
        ),
        as.integer(
          {(nrow(c) / nrow(tempe_candidates$sf) * 100)}
        )
      ),
      pct_scoot_OD = c(
        scootOD_8thMi_covered / max_scootOD_lv*100,
        scootOD_4thMi_covered / max_scootOD_lv*100
      ),
      pct_income = c(
        pct_income_8thMi_covered,
        pct_income_4thMi_covered
      )
    )
    return(x)
  })
  
  tempe_coveredZoning_plot <- reactive({
    
    # print(
    #   c(
    #     "tempe_coveredZoning_plot()  debug 1"
    #   )
    # )
    
    c = tempe_4thMi_covered()
    d = tempe_8thMi_covered()
    
    # print(
    #   c(
    #     "tempe_coveredZoning_plot()  debug 2"
    #   )
    # )
    
    cMax = sum(
      c(
        c$ag_areaFt,
        c$com_areaFt,
        c$ind_areaFt,
        c$mix_areaFt,
        c$res_areaFt,
        c$non_areaFt
      )
    )
    
    dMax = sum(
      c(
        d$ag_areaFt,
        d$com_areaFt,
        d$ind_areaFt,
        d$mix_areaFt,
        d$res_areaFt,
        d$non_areaFt
      )
    )
    
    area_zoneAg_4 = sum(
      c$ag_areaFt
    ) / cMax
    area_zoneCom_4 = sum(
      c$com_areaFt
    ) / cMax
    area_zoneInd_4 = sum(
      c$ind_areaFt
    ) / cMax
    area_zoneMix_4 = sum(
      c$mix_areaFt
    ) / cMax
    area_zoneRes_4 = sum(
      c$res_areaFt
    ) / cMax
    area_zoneNon_4 = sum(
      c$non_areaFt
    ) / cMax
    area_zoneAg_8 = sum(
      d$ag_areaFt
    ) / dMax
    area_zoneCom_8 = sum(
      d$com_areaFt
    ) / dMax
    area_zoneInd_8 = sum(
      d$ind_areaFt
    ) / dMax
    area_zoneMix_8 = sum(
      d$mix_areaFt
    ) / dMax
    area_zoneRes_8 = sum(
      d$res_areaFt
    ) / dMax
    area_zoneNon_8 = sum(
      d$non_areaFt
    ) / dMax
    
    # print(
    #   c(
    #     "tempe_coveredZoning_plot()  debug 3"
    #   )
    # )
    
    coverage_lv <- c(
      rep(
        "1/8 mi",
        6
      ),
      rep(
        "1/4 mi",
        6
      )
    )
    
    # print(
    #   c(
    #     "tempe_coveredZoning_plot()  debug 4",
    #   coverage_lv
    #   )
    # )
    
    zoneType_lv <- rep(
      c(
        "agricultural",
        "commercial",
        "industry",
        "mixed use",
        "residential",
        "unincorporated"
      ),
      2
    )
    # print(
    #   c(
    #     "tempe_coveredZoning_plot()  debug 5",
    #     zoneType_lv
    #   )
    # )
    # 
    zoneArea_lv <- c(
      area_zoneAg_8,
      area_zoneCom_8,
      area_zoneInd_8,
      area_zoneMix_8,
      area_zoneRes_8,
      area_zoneNon_8,
      area_zoneAg_4,
      area_zoneCom_4,
      area_zoneInd_4,
      area_zoneMix_4,
      area_zoneRes_4,
      area_zoneNon_4
    )
    
    # print(
    #   c(
    #     "tempe_coveredZoning_plot()  debug 6",
    #     zoneArea_lv
    #   )
    # )
    
    x <- data.frame(
      coverage = coverage_lv,
      zoneType = zoneType_lv,
      zoneArea = zoneArea_lv
    ) %>% na_if(0)
    
    # print(
    #   c(
    #     "tempe_coveredZoning_plot()  debug 7"
    #   )
    # )
    
    x$coverage <- factor(x$coverage)
    x$zoneType <- factor(x$zoneType)
    
    # print(
    #   c(
    #     "tempe_coveredZoning_plot()  debug 8"
    #   )
    # )
    return(x)
  })
  
  # comp_covered_plot <- reactive({
  #    
  # })
  
  # observe({ # debug
  #   print({
  #     c(
  #       "tempe_covered stats debug :",
  #       head(tempe_covered()),
  #       sum(tempe_covered()$scoot_OD_b, na.rm = TRUE)
  #       # tempe_covered_plot()
  #     )
  #   })
  # })
  
  # absolutePanel : stats charts ####
  
  # this section is a mess. Needs organization & clean-up
  
  observeEvent(
    eventExpr =  input$statsGraph_checkCSS,
    handlerExpr = print(
      input$statsGraph_checkCSS
    )
  )
  
  observeEvent(
    eventExpr = input$statsPanelChange_main,
    handlerExpr = {
      output$collapseButton_textOutput_main <- renderText(
        ifelse(
          input$statsPanelChange_main,
          "Hide performance charts",
          "Show performance charts"
        )
      )
    }
  )
  
  observeEvent(
    eventExpr = input$statsPanelChange_comp,
    handlerExpr = {
      output$collapseButton_textOutput_comp <- renderText(
        ifelse(
          input$statsPanelChange_comp,
          "Hide performance charts",
          "Show performance charts"
        )
      )
    }
  )
  
  output$n_sitesSelec_textOutput <- renderText(glue(
    "{nrow_lv} sites selected",
    nrow_lv = nrow(tempe_sites_selected$sf)
  ))
  
  # output$percent_coverage_textOutput <- renderText(glue(
  #   "{percent_lv}% coverage",
  #   percent_lv = as.integer(
  #     {(nrow(tempe_covered()) / nrow(tempe_overlayRaster) * 100)}
  #     )
  # ))
  
  
  output$main_cov_plot <- renderPlot({
    ggplot(
      tempe_covered_plot(),
      aes(
        x = tempe_covered_plot()$coveredRange,
        y = tempe_covered_plot()$pct_cov
      )
    ) + geom_bar(stat="identity", width = 0.5, aes(fill=coveredRange)) + coord_flip() +
      ylim(c(0,100)) + labs(
        title = "% area covered",
        y = "%",
        x = "coverage"
      ) + theme(
        legend.position = "None"
      ) + scale_fill_brewer(palette = "Paired")
  })
  
  
  output$main_income_plot <- renderPlot({
    ggplot(
      tempe_covered_plot(),
      aes(
        x = tempe_covered_plot()$coveredRange,
        y = tempe_covered_plot()$pct_income
      )
    ) + geom_bar(stat="identity", width = 0.5, aes(fill=coveredRange)) + coord_flip() +
      ylim(c(0,100)) + labs(
        title = "% households in poverty",
        y = "%",
        x = "coverage"
      ) + theme(
        legend.position = "None"
      ) + scale_fill_brewer(palette = "Paired")
  })
  
  output$main_scootOD_plot <- renderPlot({
    ggplot(
      tempe_covered_plot(),
      aes(
        x = tempe_covered_plot()$coveredRange,
        y = tempe_covered_plot()$pct_scoot_OD
      )
    ) + geom_bar(stat="identity", width = 0.5, aes(fill=coveredRange)) + coord_flip() +
      ylim(c(0,100)) + labs(
        title = "% scooter drop off & pickups",
        y = "%",
        x = "coverage"
      ) + theme(
        legend.position = "None"
      ) + scale_fill_brewer(palette = "Paired")
  })
  
  # output$main_zoning_plot <- renderPlot({
  #   ggplot(
  #     tempe_coveredZoning_plot(),
  #     aes(
  #       x = "",
  #       y = zoneArea,
  #       fill = zoneType
  #     )
  #   ) + geom_bar(
  #     stat = "identity",
  #     width = 1
  #   ) + coord_polar(
  #     "y",
  #     start = 0
  #   ) + theme_void()
  # })
  
  output$main_zoning_plot <- renderPlot({
    ggplot(
      tempe_coveredZoning_plot(), 
      aes(
        x = coverage, 
        y = zoneArea, 
        fill = zoneType
      )

    ) + geom_bar(
      stat = "identity",
      width = .75
    ) + scale_x_discrete(
      labels = c(
        "1/8 mi",
        "1/4 mi"
      )
    ) + coord_polar(
      "y"
      # , start = 0
    )  + theme_void(
      
    # ) + geom_text(
    #   aes(
    #     label = Seller, 
    #     y = 1
    #   ), 
    #   color = "black", 
    #   hjust = 0
    )
    
  })
  
  
    
  max_busStop_gv <- reactive({
    sum(tempe_candidates$sf$busStops)
  })
      
  output$main_busCount_plot <- renderPlot({
    c <- sum(tempe_selected()$busStops)
    c8th <- sum(tempe_8thMi_covered()$busStops) - c
    c4th <- sum(tempe_4thMi_covered()$busStops) - (c8th + c)
    cFar <- max_busStop_gv() - (c4th + c8th + c)
    
    d <- data.frame(
      x = c(
        "very close",
        "close",
        "far",
        "very far"
      ),
      y = c(
        c,
        c8th,
        c4th,
        cFar
      )
    )
    
    d$x <- factor(
      d$x,
      levels = d$x
    )
    
    ggplot(
      d,
      aes(
        x = x,
        y = y
      )
    ) + geom_bar(stat="identity", width = 0.5, aes(fill=x)) + coord_flip() +
      ylim(c(0,max_busStop_gv())) + labs(
        title = "Bus Stop Count",
        y = "# of bus stops",
        x = "distance"
      ) + theme(
        legend.position = "None"
      ) + scale_fill_brewer(palette = "Paired")
    
  })
  # mainTab : toggle layers on checkbox input ####
  
  observe(
    layerToggle_function(
      mapId_lv = "map_main",
      layerButton_lv = input$gridBikeStations_button,
      layerGroup_lv = "gridBikes"
    )
  )
  
  observe(
    layerToggle_function(
      mapId_lv = "map_main",
      layerButton_lv = input$prevSelected_button,
      layerGroup_lv = "prevSelec"
    )
  )
  
  observe(
    layerToggle_function(
      mapId_lv = "map_main",
      layerButton_lv = input$prevSelected_button,
      layerGroup_lv = "prevCoveredLayer"
    )
  )
  
  
  observe(
    layerToggle_function(
      mapId_lv = "map_main",
      layerButton_lv = input$selected_button,
      layerGroup_lv = "candidateLayer"
    )
  )
  
  observe(
    layerToggle_function(
      mapId_lv = "map_main",
      layerButton_lv = input$covered_button,
      layerGroup_lv = "coveredLayer"
    )
  )
  
  observe(
    layerToggle_function(
      mapId_lv = "map_main",
      layerButton_lv = input$busStops_button,
      layerGroup_lv = "busStopsLayer"
    )
  )
  
  observe(
    layerToggle_function(
      mapId_lv = "map_main",
      layerButton_lv = input$bikeways_button,
      layerGroup_lv = "bikewaysLayer"
    )
  )
  
  observe(
    layerToggle_function(
      mapId_lv = "map_main",
      layerButton_lv = input$scooterOD_button,
      layerGroup_lv = "scooterODLayer"
    )
  )
  
  # observe(
  #   layerToggle_function(
  #     mapId_lv = "map_main",
  #     layerButton_lv = input$mainCompSites_button,
  #     layerGroup_lv = "comp_sites"
  #   )
  # )
  # 
  # observe(
  #   ifelse(
  #     test = input$mainCompSites_button,
  #     yes = leafletProxy(
  #       mapId = "map_main"
  #     ) %>%
  #       showGroup(
  #         group = "comp_sites"
  #       ),
  #     no = leafletProxy(
  #       mapId = "map_main"
  #     ) %>% clearMinicharts()
  #   )
  # )
  
  
  # mainTab : render output$previousGroupList ####
  
  output$previousGroupList <- renderTree({
    
    nLOPGTR_bS <- nestedListOfPrevGroupsToRender_byStage()
    
    ifelse(
      test = {
        length(nLOPGTR_bS) > 0
      },
      yes = {
        
        # if no previous groups exist, return an empty list
        return(
          nLOPGTR_bS
        )
      },
      no = {
        return(
          list()
        )
      }
    )
  })
  
  # mainTab : render tempe_sites_selected$sf ####
  
  observe({
    
    leafletProxy(
      mapId = "map_main"
    ) %>% addMarkers(
      data = tempe_sites_selected$sf, # debug
      layerId = tempe_sites_selected$sf$siteId,
      icon = selectedsite_icon,
      group = "candidateLayer",
      # opacity = .5, # debug
      # options = pathOptions(pane = mapMain_candLayerPane_gv)
      options = pathOptions(
        pane = mapMain_siteMarkersPane_gv
      )
    )
    
  })
  
  # mainTab : render tempe_covered() ####
  
  
  # observe(
  #   
  #   
  #   leafletProxy(
  #     mapId = "map_main"
  #   )  %>% clearGroup("coveredLayer") %>% addPolygons(
  #     data = tempe_covered(),
  #     layerId = tempe_covered()$layerId,
  #     group = "coveredLayer",
  #     fillColor = "green",
  #     fillOpacity = .3,
  #     color = "#444444",
  #     weight = 0,
  #     # opacity = .5, # debug
  #     # options = pathOptions(pane = mapMain_candLayerPane_gv)
  #     options = pathOptions(
  #       pane = mapMain_selecCoveredGridPane_gv
  #     )
  #   )
  #   
  # )
  
  observe(
    
    
    leafletProxy(
      mapId = "map_main"
    )  %>% clearGroup("coveredLayer") %>% addPolygons(
      data = tempe_4thMi_covered(),
      layerId = tempe_4thMi_covered()$layerId,
      group = "coveredLayer",
      fillColor = "green",
      fillOpacity = .3,
      color = "#444444",
      weight = 0,
      # opacity = .5, # debug
      # options = pathOptions(pane = mapMain_candLayerPane_gv)
      options = pathOptions(
        pane = mapMain_selecCovGridPane_4_gv
      )
    ) %>% addPolygons(
      data = tempe_8thMi_covered(),
      layerId = tempe_8thMi_covered()$layerId,
      group = "coveredLayer",
      fillColor = "green",
      fillOpacity = .6,
      color = "#444444",
      weight = 0,
      # opacity = .5, # debug
      # options = pathOptions(pane = mapMain_candLayerPane_gv)
      options = pathOptions(
        pane = mapMain_selecCovGridPane_8_gv
      )
    )
    
  )
  
  
  
  # mainTab : render tempe_sites_prevSelected() ####
  
  observeEvent(
    eventExpr = input$previousGroupList,
    handlerExpr = {
      
      print(c(
        "input$previousGroupList leafletProxy debug 1",
        "nrow(tempe_sites_prevSelected()) : ",
        nrow(tempe_sites_prevSelected())
      ))
      
      ifelse(
        test = {
          nrow(tempe_sites_prevSelected()) > 0
        },
        yes = {
          
          print(c(
            "input$previousGroupList : render prevSelected debug"
          ))
          
          # if any previous groups are selected, clear the existing displayed
          # groups and redisplay the new selection
          
          
          leafletProxy(
            mapId = "map_main"
          ) %>% clearGroup(
            group = "prevSelec"
          ) %>% clearGroup(
            group = "prevCoveredLayer"
          ) %>% addMarkers(
            data = tempe_sites_prevSelected(),
            group = "prevSelec",
            icon = prevSelected_icon,
            # layerId = stationsToAddOnClick$layerId,
            options = pathOptions(
              pane = mapMain_siteMarkersPane_gv
            )
          ) %>% addPolygons(
            data = tempe_prevCovered(),
            layerId = tempe_prevCovered()$layerId,
            group = "prevCoveredLayer",
            fillColor = "orange",
            fillOpacity = .3,
            color = "#444444",
            weight = 0,
            options = pathOptions(
              pane = mapMain_prevCoveredGridPane_gv
            )
          )
          
        },
        no = {
          
          print(c(
            "input$previousGroupList : clear prevSelected debug"
          ))
          
          leafletProxy(
            mapId = "map_main"
          ) %>% clearGroup(
            group = "prevSelec"
          ) %>% clearGroup(
            group = "prevCoveredLayer"
          )
        }
      )
    }
  )
  
  # mainTab : render compSites ####
  
  observe({
    ifelse(
      test = {
        nrow(comp_prevSitesSelected()) > 0 && input$mainCompSites_button
      },
      yes = {
        r <- comp_prevSitesSelected()$gsi_info %>% unique() %>% sort()
        
        comp_palette <- compareSites_palette()
        
        # if any previous groups are selected, clear the existing displayed
        # groups and redisplay the new selection
        
        # d <- comp_prevSitesSelected_minichart() 
        
        d <- comp_prevSitesSelected_minichart()
        
        e <- compareSitesMinichartsOutline()
        
        leafletProxy(
          mapId = "map_main"
        )  %>% clearGroup(
          group = "comp_sites"
          # ) %>% clearGroup(
          #   group = "comp_coveredCells"
        ) %>% clearMinicharts() %>% addCircleMarkers(
          data = e,
          group = "comp_sites",
          fillColor = "transparent",
          fillOpacity = 0,
          color = "white",
          opacity = 1,
          weight = 1,
          label = lapply(e$label_lv, htmltools::HTML), # %>% HTML(),
          # icon =  # debug, make an icon,
          # layerId = stationsToAddOnClick$layerId,
          options = pathOptions(
            pane = miniChart_outlinePane_gv
          )
        ) %>% addMinicharts(
          d$lng,
          d$lat,
          type = "pie",
          chartdata = d[,r],
          fillColor = rainbow(
            n = length(r)
          )[1],
          colorPalette = rainbow(
            n = length(r),
            s = .75
          ), 
          width = 20, 
          transitionTime = 0,
          layerId = d$keyId# ,
          # options = pathOptions(
          #       pane = mapMain_siteMarkersPane_gv
          #     )
          # ) %>% addPolygons(
          #   data = comp_prevCellsCovered(),
          #   layerId = comp_prevCellsCovered()$layerId,
          #   group = "comp_coveredCells",
          #   fillColor = ~comp_colorList()(gsi_info),
          #   fillOpacity = .3,
          #   color = "transparent",
          #   opacity = 0,
          #   weight = 1,
          #   options = pathOptions(
          #     pane = "comp_coverage"
          #   )
        )
      },
      no = {
        leafletProxy(
          mapId = "map_main"
        ) %>% clearGroup(
          group = "comp_sites"
        ) %>% clearGroup(
          group = "comp_coveredCells"
        ) %>% clearMinicharts()
      }
    )
  })

  # compareTab : render output$map_compare ####
  output$map_compare <- renderLeaflet({
    
    leaflet() %>%
      makeLeafletMap_function(
        initialLat_lv = mapMain_startLat_gv,
        initialLng_lv = mapMain_startLng_gv,
        initialZoom_lv = mapMain_startZoom_gv
      # ) %>% addMapPane(
      #   name = "comp_coverage",
      #   zIndex = 399
      ) %>% addMapPane(
        name = miniChart_outlinePane_gv,
        zIndex = 499
      ) %>% addMapPane(
        name = mapMain_busStopsPane_gv,
        zIndex = 399
      ) %>% addMapPane(
        name = mapMain_bikewaysPane_gv,
        zIndex = 398
      ) %>% addMapPane(
        name = mapMain_busRoutesPane_gv,
        zIndex = 397
      ) %>% addMapPane(
        name = mapMain_studyAreaPane_gv,
        zIndex = 396
      ) %>% addMapPane(
        name = mapMain_scooterODPane_gv,
        zIndex = 395
      ) %>% addRasterImage(
        x = scooterOD_raster,
        colors = palette_scootOD,
        group = "scooterODLayer",
        opacity = .5,
        project = FALSE
        # # this will work with the next leaflet update; it currently does not
        # now and the raster needs to be added to the map in order
        # , options = tileOptions(
        #   pane = mapMain_scooterODPane_gv
        # )
      ) %>% addPolylines(
        data = valley_bikeways_sf,
        group = "bikewaysLayer",
        color = "lime",
        dashArray = "3,10",
        weight = 6,
        opacity = .9,
        options = pathOptions(
          pane = mapMain_bikewaysPane_gv
        )
      ) %>% addPolylines(
        data = busRoutesFixed,
        group = "busStopsLayer",
        color = "orangered",
        opacity = .9,
        weight = 8,
        options = pathOptions(
          pane = mapMain_busRoutesPane_gv
        ) 
      ) %>% addPolylines(
        data = busRoutesFlag,
        group = "busStopsLayer",
        color = "darkviolet",
        weight = 8,
        opacity = .9,
        options = pathOptions(
          pane = mapMain_busRoutesPane_gv
        )
      ) %>% addMarkers(
        data = tempe_streetcar_sf,
        icon = streetcar_icon,
        group = "busStopsLayer",
        options = pathOptions(
          pane = mapMain_busStopsPane_gv
        )
      ) %>% addMarkers(
        data = valley_busStops_sf,
        icon = busStation_icon,
        group = "busStopsLayer",
        options = pathOptions(
          pane = mapMain_busStopsPane_gv
        )
      ) %>% addPolygons(
        data = studyArea,
        group = "studyArea",
        fillColor = "transparent",
        fillOpacity = 0,
        color = "black",
        dashArray = "3,10",
        weight = 6,
        opacity = .9,
        options = pathOptions(
          pane = mapMain_studyAreaPane_gv
        )
      )
      
      
      # addLeafletMapPane_function( # debug
      # ) %>% addMarkers(
      #   data = valley_GRiDBikeshareDocks_sf, # debug
      #   icon = list(
      #     iconUrl = "www/scooter_selected.png",
      #     iconSize = c(20,20)
      #   ),
      #   group = "gridBikes",
      #   layerId = valley_GRiDBikeshareDocks_sf$StationID,
        # , # debug
        # options = pathOptions(
        #   pane = mapMain_miscMarkersPane_gv
        # )
      
  })

  
  # compareTab : render output$comparePrevGroupList ####
  output$comparePrevGroupList <- renderTree({
    
    
    nLOPGTR_bS <- nestedListOfPrevGroupsToRender_byStage()
    
    ifelse(
      test = {
        length(nLOPGTR_bS) > 0
      },
      yes = {
        
        # if no previous groups exist, return an empty list
        return(
          nLOPGTR_bS
        )
      },
      no = {
        return(
          list()
        )
      }
    )
  })
  
  # compTab : toggle layers on checkbox input ####
  
  observe(
    layerToggle_function(
      mapId_lv = "map_compare",
      layerButton_lv = input$comp_busStops_button,
      layerGroup_lv = "busStopsLayer"
    )
  )
  
  observe(
    layerToggle_function(
      mapId_lv = "map_compare",
      layerButton_lv = input$comp_bikeways_button,
      layerGroup_lv = "bikewaysLayer"
    )
  )
  
  observe(
    layerToggle_function(
      mapId_lv = "map_compare",
      layerButton_lv = input$comp_scooterOD_button,
      layerGroup_lv = "scooterODLayer"
    )
  )
  
  # comp_prevGroups_selectedNames() : prev group list selected as classid ####

  # TODO Make this a function so we don't have to keep doing it

  comp_prevGroups_selectedNames <- reactive({
    
    print(c(
      "comp_prevGroups_selectedNames() debug 1"# ,
      # "nestedListOfPrevGroupsToRender_byStage is :",
      # nestedListOfPrevGroupsToRender_byStage()
    ))
    
    selectedGroups <- names(
      as.data.frame(
        get_selected(
          input$comparePrevGroupList, 
          format = "slices"
        )
      )
    )
    
    # print(c(
    #   "STATS CHART DEBUG",
    #   "comp_prevGroups_selectedNames() debug 2",
    #   
    #   selectedGroups
    # ))
    
    return(selectedGroups)
  })
  
  #   
  #   reactive({
  #   selectedGroups <- unlist(
  #     shinyTree::get_selected(
  #       tree = input$comparePrevGroupList,
  #       format = "classid"
  #     )
  #   )
  # 
  #   # this selects the groupStageIterInfo by name, where that name is in the list of selected groups
  #   # NOTE: we're reusing the prevGroups_all_listNamesGSII reactive object here
  # 
  #   spatialComparison_selectedMatched <- prevGroups_all_listNamesGSII()[
  #     selectedGroups
  #   ] %>% unname()
  # 
  #   # This returns the unnamed list as a list of groupStageIterInfo selected
  #   # Though for some reason it's also giving me two "NULL" entries so let's get rid of those...
  # 
  #   return(
  #     Filter(
  #       Negate(
  #         is.null
  #       ),
  #       spatialComparison_selectedMatched
  #     )
  #   )
  # 
  # })
  
  
  # comp_prevGroups_selected() : update on input$previousGroupList ####
  
  comp_prevGroups_selected <- reactive({
    
    print(c(
      "comp_prevGroups_selected() debug 1"
    ))
    
    if(
      length(comp_prevGroups_selectedNames()) > 0 
    ){
      
      print(c(
        "comp_prevGroups_selected() debug 2"
      ))
      
      # this selects the groupStageIterInfo by name, where that name is in the list of selected groups
      
      prevGroups_selectedMatched <- prevGroups_all_listNamesGSII()[
        comp_prevGroups_selectedNames()
      ]
      
      # print(c(
      #   "STATS CHART DEBUG",
      #   "comp_prevGroups_selected() debug 3",
      #   unname(prevGroups_selectedMatched)
      # ))
      
      # This returns the unnamed list as a list of groupStageIterInfo selected
      
      return(
        unname(prevGroups_selectedMatched)
      )
    } else {
      print(c(
        "comp_prevGroups_selected() debug 4"
      ))
    }
  })
  

  # comp_groupSitePairsToAdd() : prev group selected joinId pair list ####
  # This generates the list of pairs to add
  # TODO make this a function as well

  comp_groupSitePairsToAdd <- reactive({
    
    # print(c(
    #   "comp_groupSitePairsToAdd() debug 1",
    #   "length(comp_prevGroups_selected() is : ",
    #   length(comp_prevGroups_selected())
    # ))
    
    # # if the list of selected groups is not null:
    # if(
    #   !is.null(prevGroups_selected())
    # ){
    
    # first we need to create a list of previous groups to add
    # that is a string of groupStageIteration unique identifiers
    # We'll use this to build the mongolite query as a JSON object
    
    prevGroupsToAdd_query <- makeDBquery_valuesInList_function(
      listToQuery_lv = comp_prevGroups_selected() %>% double_quote(),
      fieldToCheck_lv = "groupStageIterInfo"
    )
    
    # print(c(
    #   "comp_groupSitePairsToAdd() debug 2",
    #   "prevGroupsToAdd_query is :",
    #   prevGroupsToAdd_query
    # ))
    
    # Then we're using the query to call from the joining database
    # This gets us the uniqueIds for sites that were in
    # previous groups
    
    # We'll later query a db for all sites ever selected by a group
    # and add that to the map
    
    groupSitePairs <- groupSelected_joinId_db$find(
      query = prevGroupsToAdd_query,
      fields = '{"_id" : false}'
    )
    
    print(c(
      "comp_groupSitePairsToAdd() debug 3"#,
      # "comp_groupSitePairsToAdd() is : ",
      # groupSitePairs
    ))
    
    return(
      groupSitePairs#$siteId %>% unique()
    )
  })
  
  

  
  # comp_prevSitesSelected() : all compPrevGroupList selected sites to render ####
  
  comp_prevSitesSelected <- reactive({
    # function for querying a database with a list of ids
    # this is completely randomly placed here. Could be preloaded, and could
    # replace or supplement every other query reactive object definition
    
    mongoQuery_function <- function(
      mongo_db_lv,
      queryInputData = list(),
      queryKeyIdField = character()
    ){
      l <- makeDBquery_valuesInList_function(
        listToQuery_lv = queryInputData,
        fieldToCheck_lv = queryKeyIdField
      )
      
      m <- mongo_db_lv$find(
        query = l,
        fields = '{"_id" : false}'
      )
      
      return(m)
    }
    
    ### end random function ###
    
    print(c(
      "comp_prevSitesSelected() debug 1"
    ))
    
    groupId_siteId <- comp_groupSitePairsToAdd()
    
    print(c(
      "comp_prevSitesSelected() debug 2"
    ))
    
    # data frame that we're going to populate
    
    # this was the easiest way I figured to keep gsi_info with the data
    g <- data.frame(
      gsi_info = factor(),
      siteId = character(),
      cellId = integer(),
      lat = double(),
      lng = double()
    )
    
    print(c(
      "comp_prevSitesSelected() debug 3"
    ))
    
    for(i in 1:nrow(groupId_siteId)){
      
      print(c(
        "comp_prevSitesSelected() debug 4"#,
        # "groupId_siteId[i,] is :",
        # groupId_siteId[i,]
        
      ))
      
      t <- groupId_siteId[i,]$siteId
      
      m <- mongoQuery_function(
        queryInputData = t %>% double_quote(),
        queryKeyIdField  = "siteId",
        mongo_db_lv = allSites_db
      )
      
      print(c(
        "comp_prevSitesSelected() debug 5"#,
        # "m is :",
        # m
      ))
        
      g <- g %>% rbind(
        data.frame(
        gsi_info = groupId_siteId[i,]$groupStageIterInfo,
        siteId = t,
        lat = m$lat,
        lng = m$lng,
        cellId = m$cellId,
        layerId = glue(
          "s_{gsi_info_lv}_{id_lv}",
          gsi_info_lv = groupId_siteId[i,]$groupStageIterInfo,
          id_lv = t
        )
      ))
    }
    
    print(c(
      "comp_prevSitesSelected() debug 6"#,
      # "g is : ",
      # g
    ))
    
    return(g # %>% st_as_sf(
    #   coords = c(
    #     "lng",
    #     "lat"
    #   )
    # ) %>% st_set_crs(
    #   projectCRS_gv
    # )
    )
    
    # siteId_cellId_lat_lng <- compare_sites_prevSelected()
    # 
    # s <- data.frame(
    #   siteId = siteId_cellId_lat_lng$siteId,
    # ) %>% cbind(
    #   groupId_siteId
    # )
  })
  
  # comp_prevSitesSelected_minichart() : all compPrevGroupList selected sites to render ####
  
  comp_prevSitesSelected_minichart <- reactive({
    d<- comp_prevSitesSelected()  %>% mutate(
      exists = 1,
      keyId = glue(
        "comp_{lng}_{lat}"
      )
    ) %>% pivot_wider(
      names_from = gsi_info,
      names_sort = TRUE,
      values_from = exists,
      unused_fn = list(lng = max, lat = max),
      values_fill = 0,
      id_cols = keyId
    )
    # print(c(
    #   "comp_prevsitesSelected_minichart debug",
    #   d
    # ))
    return(d)
  })
  
  # # X comp_prevCellsCovered() : all compPrevGroupList covered cells to render ####
  # 
  # comp_prevCellsCovered <- reactive({
  #   # function for querying a database with a list of ids
  #   
  #   mongoQuery_function <- function(
  #     mongo_db_lv,
  #     listToQuery = list(),
  #     keyIdField_in = character()
  #   ){
  #     l <- makeDBquery_valuesInList_function(
  #       listToQuery_lv = listToQuery,
  #       fieldToCheck_lv = keyIdField_in
  #     )
  #     
  #     m <- mongo_db_lv$find(
  #       query = l,
  #       fields = '{"_id" : false}'
  #     )
  #     
  #     return(m)
  #   }
  #   
  #   # print(c(
  #   #   "comp_prevCellsCovered() debug 1"
  #   # ))
  #   
  #   d <- comp_prevSitesSelected()
  #   
  #   # data frame that we're going to populate
  # 
  #   g <- st_sf(st_sfc()) %>% st_set_crs(projectCRS_gv)
  #   
  #   # print(c(
  #   #   "comp_prevCellsCovered() debug 2"
  #   # ))
  #   
  #   for(i in 1:nrow(d)){
  #     
  #     # print(c(
  #     #   "comp_prevCellsCovered() debug iterator 1"
  #     # ))
  #     
  #     q <- mongoQuery_function(
  #       listToQuery = d[i,]$cellId,
  #       keyIdField_in = "servAreaId",
  #       mongo_db_lv = servArea_4thMi_join_db
  #       # mongo_db_lv = grid_serviceArea_join_db
  #     )
  #     
  #     # print(c(
  #     #   "comp_prevCellsCovered() debug iterator 2"
  #     # ))
  #     c <- tempe_candidates$sf$id %in% q$gridCellId
  #     
  #     # print(c(
  #     #   "comp_prevCellsCovered() debug iterator 3"
  #     # ))
  #     
  #     b <- tempe_candidates$sf[c,] %>% mutate(
  #       layerId = glue(
  #         "c_{gsi_info_lv}_{id_lv}",
  #         gsi_info_lv = d[i,]$gsi_info,
  #         id_lv = .$id
  #       ),
  #       gsi_info = d[i,]$gsi_info,
  #       geometry = .$geometry
  #     )
  #     
  #     # print(c(
  #     #   "comp_prevCellsCovered() debug iterator 4"
  #     # ))
  #     
  #     g <- g %>% rbind(
  #       b
  #     )
  #     
  #     # print(c(
  #     #   "comp_prevCellsCovered() debug iterator 5"
  #     # ))
  #   }
  #   
  #   # print(c(
  #   #   "comp_prevCellsCovered() debug 3"
  #   # ))
  #   
  #   return(g)
  #   
  #   # siteId_cellId_lat_lng <- compare_sites_prevSelected()
  #   # 
  #   # s <- data.frame(
  #   #   siteId = siteId_cellId_lat_lng$siteId,
  #   # ) %>% cbind(
  #   #   groupId_siteId
  #   # )
  # })
  
  
  # compareTab : render compare_sites_prevSelected() ####
  
  compareSites_palette <- reactive({
    ifelse(
      test = {
        nrow(comp_prevSitesSelected()) > 0
      },
      yes = {
        
        r <- comp_prevSitesSelected()$gsi_info %>% unique() %>% sort()
        
        comp_palette <- colorFactor(
          # palette = "Pastel1",
          palette = rainbow(
            n = length(r)
          ),
          domain = comp_prevSitesSelected()$gsi_info
        )
        
        return(comp_palette)
      },
      no = {}
    )
  })
  
  compareSitesMinichartsOutline <- reactive({
    ifelse(
      test = {
        nrow(comp_prevSitesSelected()) > 0
      },
      yes = {
        
        r <- comp_prevSitesSelected()$gsi_info %>% unique() %>% sort()
        
        comp_palette <- compareSites_palette()
        
        # if any previous groups are selected, clear the existing displayed
        # groups and redisplay the new selection
        
        # d <- comp_prevSitesSelected_minichart() 
        
        d <- comp_prevSitesSelected_minichart()
        
        for(i in 1:nrow(d)){
          
          n <- c()
          for(j in colnames(d)){
            if(j %in% r && d[i,j] == 1){
              n <- n %>% append(as.character(j))
            }
          }
          
          d$label_lv[i] <- glue_collapse(
            # i want a vector of all the column names for D that both exist in R
            # and whose values = 1
            n,
            sep = "<br>"
          )
        }
        
        e <- d %>% st_as_sf(
          coords = c(
            "lng",
            "lat"
          )
        ) %>% st_set_crs(
          projectCRS_gv
        )
        
        return(e)
        
        
      },
      no = {}
    )
  })
  
  observeEvent(
    eventExpr = input$comparePrevGroupList,
    handlerExpr = {
      
      ifelse(
        test = {
          nrow(comp_prevSitesSelected()) > 0
        },
        yes = {
        
          r <- comp_prevSitesSelected()$gsi_info %>% unique() %>% sort()
          
          comp_palette <- compareSites_palette()
          
          # if any previous groups are selected, clear the existing displayed
          # groups and redisplay the new selection
          
          # d <- comp_prevSitesSelected_minichart() 
          
          d <- comp_prevSitesSelected_minichart()

          e <- compareSitesMinichartsOutline()
          
          leafletProxy(
            mapId = "map_compare"
          ) %>% clearGroup(
            group = "comp_sites"
          ) %>% clearMinicharts() %>% addCircleMarkers(
            data = e,
            group = "comp_sites",
            fillColor = "transparent",
            fillOpacity = 0,
            color = "white",
            opacity = 1,
            weight = 1,
            label = lapply(e$label_lv, htmltools::HTML), # %>% HTML(),
            # icon =  # debug, make an icon,
            # layerId = stationsToAddOnClick$layerId,
            options = pathOptions(
              pane = miniChart_outlinePane_gv
            )
          ) %>% addMinicharts(
            d$lng,
            d$lat,
            type = "pie",
            chartdata = d[,r],
            fillColor = rainbow(
              n = length(r)
            )[1],
            colorPalette = rainbow(
              n = length(r),
              s = .75
            ), 
            width = 20, 
            transitionTime = 0,
            layerId = d$keyId
          )
          
        },
        no = {
          
          print(c(
            "render comp_sites_prevSelected() debug 3"
          ))
          
          leafletProxy(
            mapId = "map_compare"
          ) %>% clearGroup(
            group = "comp_sites"
          ) %>% clearGroup(
            group = "comp_coveredCells"
          ) %>% clearMinicharts()
        }
      )
    }
  )
  
  # comp_colorList() : make palette for comp_prevSelected ####
  
  comp_colorList <- reactive({
    g <- comp_groupSitePairsToAdd()$groupStageIterInfo %>% unique() %>% sort()
    s <- comp_groupSitePairsToAdd()$siteId
    
    # print(c(
    #   "comp_colorList debug 1"
    # ))
    
    # We need unique groups selected
    
    comp_palette <- colorFactor(
      # palette = "Pastel1",
      palette = rainbow(
        n = length(g)
      ),
      domain = g
    )
    
    # print(c(
    #   "comp_colorList debug 2"
    # ))
    
    return(comp_palette)
    
    # colorList <- list(
    #   selectedByOne = ~ function() {colorpal_comparison(stationsToAddOnClick$groupsSelected)},
    #   selectedByMany = "gray"
    # )
    
  })
  # 
  # observeEvent(
  #   eventExpr = input$comparePrevGroupList,
  #   handlerExpr = {
  # 
  #     prevGroups_selected <- compare_sitesToAdd()
  #     stationsToAddOnClick <- spC_stationsToAdd()
  #     # 
  #     # colorpal_comparison <- colorFactor(
  #     #   rainbow(
  #     #     NROW(
  #     #       unique(
  #     #         stationsToAddOnClick[stationsToAddOnClick$selecCount == 1,]$groupsSelected
  #     #       )
  #     #     )
  #     #   ),
  #     #   stationsToAddOnClick[stationsToAddOnClick$selecCount == 1,]$groupsSelected
  #     # )
  #     # 
  #     # colorList <- list(
  #     #   selectedByOne = ~ function() {colorpal_comparison(stationsToAddOnClick$groupsSelected)},
  #     #   selectedByMany = "gray"
  #     # )
  # 
  # 
  #     ifelse(
  #       test = {
  #         length(prevGroups_selected) > 0
  #       },
  #       yes = {
  # 
  #         # if any previous groups are selected, clear the existing displayed
  #         # groups and redisplay the new selection
  # 
  #         leafletProxy(
  #           mapId = "map_compare"
  #         ) %>% clearGroup(
  #           group = "spCompare"
  #         ) %>% addCircleMarkers(
  #           data = stationsToAddOnClick,
  #           group = "spCompare",
  #           layerId = stationsToAddOnClick$layerId,
  #           radius = 5,
  #           fill = TRUE,
  #           color = ~ ifelse(
  #             test = stationsToAddOnClick$selecCount > 1,
  #             yes = "gray",#colorList$many,
  #             no = colorpal_comparison(stationsToAddOnClick$groupsSelected)#~colorList$one
  #           ),
  #           fillColor = ~ ifelse(
  #             test = stationsToAddOnClick$selecCount > 1,
  #             yes = "gray",# colorList$many,
  #             no = colorpal_comparison(stationsToAddOnClick$groupsSelected)# ~colorList$one
  #           ),
  #           opacity = 0.7,
  #           fillOpacity = 0.7
  #         ) %>% addLegend(
  #           "bottomleft",
  #           pal = colorpal_comparison,
  #           values = stationsToAddOnClick[stationsToAddOnClick$selecCount == 1,]$groupsSelected,
  #           opacity = 3,
  #           title = "selected groups & stages",
  #           layerId = "spC_legend"
  #         )
  #       },
  #       no = {
  #         leafletProxy(
  #           mapId = "map_compare"
  #         ) %>% clearGroup(
  #           group = "spCompare"
  #         ) %>% removeControl(
  #           layerId = "spC_legend"
  #         )
  #       }
  #     )
  #   }
  # )
  
  # # X compareTab : download selected prev group data ####
  # output$downloadSelectedData_spatial <- downloadHandler(
  #   filename = function() {
  #     paste("spatial_comparison.csv", sep = "")
  #   },
  #   content = function(file) {
  #     write.csv(
  #       spC_stationsToAdd_df,
  #       file
  #     )
  #   }
  # )
  
  # tempSelected_db() : connect to {groupName}_tempSelected_db mongo ####
  
  # this needs to be a function
  
  # tempSelected_db <- reactive(
  #   return(
  #     mongo(
  #       collection = glue(
  #         "{gN}_temp_db",
  #         gN = req(
  #           expr = input$groupName
  #         )
  #       ),
  #       url = mongoURL_gv
  #     )
  #   )
  # )
  
  # # X mLab : make tempSelec db ####
  # 
  # observe({
  # 
  #   temp_db <- tempSelected_db()
  # 
  #   temp_gsi_siteId_cellId_lng_lat <- data.frame(
  #     gsi_info = character(),
  #     siteId = character(),
  #     cellId = integer(),
  #     lng = double(),
  #     lat = double()
  #   )
  # 
  # 
  # 
  # 
  #   temp_db$remove('{}')
  #   temp_db$insert(temp_gsi_siteId_cellId_lat_lng)
  # 
  # })



  # # X helpTab : quick start guide ####
  # output$quickStartGuide <- renderUI(
  #   return(
  #     tags$iframe(
  #       style = "
  #           height:  calc(100vh - 122px) !important;
  #           width: 100%;
  #           scrolling=yes
  #         ",
  #       src = "collablocation_ctH2_quickStartGuide.pdf"
  #     )
  #   )
  # )
} # server

# shiny : shinyApp(ui, server) ####
shinyApp(ui,server)