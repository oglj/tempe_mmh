# libraries ####

# Shiny libraries
library(shiny)
library(shinydashboard)
library(shinyjs)
library(V8) # required by shinyjs
library(shinyTree) # generating an expandable/collapsed checkboxes (nested checkboxes)

# Connecting to Remote Database
library(rsconnect)
library(mongolite) # connecting to mongo lab, a web-based mongodb
library(RCurl)
library(Rcpp) # debug

# Spatial + mapping packages
library(leaflet)
library(leaflet.minicharts)
library(leaflet.extras)
library(leaflet.extras2)
library(sf)
library(geojsonsf) # debug
library(rjson) # debug
library(terra) # debug
library(raster) # debug
library(rgdal)

# Miscellaneous
library(htmltools) # for popups
library(httr)
library(tidyverse)
library(glue)
library(DT)
library(dplyr)
library(ggvis)
library(ggplot2)
library(rjson)

# debug
library(reactlog)
options(shiny.reactlog = TRUE)

# load workspace ####
# load("tempeMMH_workspace_test_debug.RData")

# mongoDB connections ####

# Field name of the unique ID number that mongo automatically assigns to database rows. This only needs to change if we switch database hosts
mongoID_dbField_gv <- "_id"

mlab_host_gv <- "mongoHost.net"
mlab_username_gv <- "mongoUsername"
mlab_password_gv <- "mongoPassword"

workshop_dbName_gv <- "mongoDB"

mongoURL_gv <- glue(
  "mongodb+srv://{un}:{pw}@{h}/{db}?retryWrites=true&w=majority",
  un = mlab_username_gv,
  pw = mlab_password_gv,
  h = mlab_host_gv,
  db = workshop_dbName_gv
)


makeDBquery_valuesInList_function <- function(
  listToQuery_lv = list(),
  fieldToCheck_lv = as.character()
){
  print("debug makeDBquery 1")
  
  print("debug makeDBquery 2")
  x <- ifelse(
    test = {length(listToQuery_lv) > 0},
    yes = {
      glue_collapse(
        listToQuery_lv,
        sep = ", "
      )},
    no = {""}
  )
  
  print("debug makeDBquery 3")
  
  g <- glue(
    '{{
          "{field_glue}" : {{
            "$in" : [
              {listToQuery_glue}
            ]
          }}
        }}',
    field_glue = fieldToCheck_lv,
    listToQuery_glue = x
  )
  return(
    g
  )
}

# mainTab HTML ####

mainTab_sidebar_css <- "padding: 0px 5px 0px 5px;"
mainTab_sidebar_submitButton_css <- "padding: 5px 0px 5px 0px;"
mainTab_sidebar_submitButton_title <- "Click this button to submit selected network for comparison to other submissions"
mainTab_sidebar_submitButton_label <- "Save current selection"
mainTab_sidebar_submitButton_css <- "font-size: 1.5em; white-space: normal;" # setting "white-space" to "normal" allows for flexible multiline action button labels
mainTab_sidebar_submitButtonPanel_css <- "
        padding: 0px 0px;
        margin-bottom: 10px;
        border-width: 3px;
        border-color: #3cb371;
        background-color: #3cb371
        "
mainTab_layerControls_title <- "
                Click the checkbox next to each layer to display or hide it on the map.1&#013;
                Hover over each layer name for more information.
                "
mainTab_layerControls_label_css <- "
            padding-left: 1em;
            margin-bottom: 5px;
            font-size: 1.5em;
            background-color: #ccc;
            color: #fff;
            border-radius: 3px;
            "
mainTab_layerControls_label <- "Layer Controls"
mainTab_layerControls_cand_html <- "<span style='padding-left:12px'><img src = 'mmhSite_selected.png' width = '24'> selected site</span>"
mainTab_layerControls_prev_html <- "<span style='padding-left:12px'><img src = 'prevSelected_icon.png' width = '24'> previous selection</span>"
mainTab_layerControls_GRiDBikes_title <- "
                Bus Stops | source: Valley Metro
                "
covered_html <- "<span style='padding-left:36px'> network coverage </span><br><svg width='36' height='24'><rect fill='rgb(0,128,0)' width='24' height='24' x='12' opacity='.75'></svg> 1/8 mi. coverage<br><svg width='36' height='24'><rect fill='rgb(0,128,0)' width='24' height='24' x='12' opacity='.5'></svg> 1/4 mi. coverage"
mainTab_layerControls_GRiDBikes_label <- "<span style='padding-left:24px'><img src = 'busStation_icon.png' width = '12'> Valley Metro bus stops</span><br><span style='padding-left:24px'><img src = 'streetcar_icon.png' width = '12'> Valley Metro streetcar stops</span><br><img src='busRouteFixed_icon.png' width='36'> bus route (fixed section)<br><img src='busRouteFlag_icon.png' width='36'> bus route (flag-down section)"
scootOD_legend_HTML <- '<div height="29" line-height="29"># of scooter trip starts & ends<br><span>low <svg width="96" height="24" opacity=".5"><rect fill="rgb(230,97,1)" width="24" height="24" x="0"></rect><rect fill="rgb(253,184,99)" width="24" height="24" x="24"></rect><rect fill="rgb(178,171,210)" width="24" height="24" x="48"></rect><rect fill="rgb(94,60,153)" width="24" height="24" x="72"></rect></svg> high</span></div>'

# styling for horizontal lines lifed from this example: http://www.iraqtimeline.com/maxdesign/basicdesign/principles/prinhorizontal.html
horizontalLine_html <- "<hr style = 'display: block; position: relative; padding: 0; margin: 8px auto; height: 0; max-height: 0; font-size: 1px; line-height: 0; clear: both;border: none; border-top: 1px solid #aaaaaa; border-bottom: 1px solid #ffffff'>"
mainTab_addPreviousButton_title <- "
                Click this button to add the candidates from previous sessions to the current selection
                "
mainTab_addPreviousButton_label_image <- "<img src = 'prevSelected_icon.png' height = '18'>"
mainTab_addPreviousButton_label <- "Add previous selections to current selection"
mainTab_addPreviousButton_css <- "font-size: 1.3em; white-space: normal;" # setting "white-space" to "normal" allows for flexible multiline action button labels
mainTab_addPreviousButtonPanel_css <- "
        padding: 5px;
        margin-bottom: 5px;
        border-color: #3a9ed8;
        border-width: 3px;
        "

# compare tab HTML ####

compareTab_layerControl_title <- "
                Click the checkbox next to each layer to display or hide it on the map.1&#013;
                Hover over each layer name for more information.
                "
compareTab_layerControlPanel_css <- "
              padding: 5px;
              margin-bottom: 10px;
              border-color: #ccc;
              border-width: 3px;
              "
compareTab_selectGSI_title_css <- "
                padding: 0px;
                margin-bottom: 0px;
                "
compareTab_selectGSI_title <- "Select group(s) & stage(s)"
compareTab_selectGSI_tree_css <-  "
            font-size: 10pt;
            padding: 0px;
            margin-bottom: 0px;
            margin-top: 0px;
            "
compareTab_selectGSI_panel_css <- "
          padding: 0px 0px 20px 0px;
          margin-bottom: 0px;
          height: 800px;
          overflow-y: scroll;
          "
compareTab_updateTree_css <- "padding: 5px 0px 15px 0px;"
compareTab_updateTree_title <- "Update list of groups & stages"
compareTab_updateTree_label <- "Update list"

 # spatial data management ####

projectCRS_gv <- 4326
cand_defaultString_gv <- "candidate"

spatialDataGet <- function(
  fileLocation, 
  fileName, 
  crs_lv = projectCRS
){
  rawGeoFile <- if(
    missing(fileName)
  ){
    return(
      st_read(
        dsn = fileLocation
      ) %>% 
        st_as_sf() 
    )
  } else {
    return(
      st_read(
        dsn = fileLocation,
        layer = fileName
      ) %>% 
        st_as_sf()
    )
  }
  
  if(
    st_crs(rawGeoFile)$epsg != crs_lv
  ){
    rawGeoFile %>% st_transform(
      crs = crs_lv
    )
  } else {
    rawGeoFile
  }
}


valley_bikeways_sf <- spatialDataGet(
  fileLocation = "www",
  fileName = "bikeways_test2"
)

tempe_streetcar_sf <- spatialDataGet(
  fileLocation = "www",
  fileName = "streetCarStops_epsg4326"
)

valley_busStops_sf <- spatialDataGet(
  fileLocation = "www",
  fileName = "busStations_test"
)

busRoutesFixed <- spatialDataGet(
  fileLocation = "www",
  fileName = "busFixedRtes_epsg4326"
)

busRoutesFlag <- spatialDataGet(
  fileLocation = "www",
  fileName = "busFlagRtes_epsg4326"
)

candGrid88_gv <- spatialDataGet(
  fileLocation = "www",
  fileName = "candGrid88_allCriteria_epsg2223_v08"
)

studyArea <- spatialDataGet(
  fileLocation = "www",
  fileName = "studyArea_epsg4326"
)

rowMaxDigits <- max(nchar(
  candGrid88_gv$id
))

candGrid88_gv <- candGrid88_gv %>% st_transform(projectCRS_gv) %>% mutate(
  id = as.integer(.$id),
  layerId = glue(
    "{category}{paddedId}",
    category = cand_defaultString_gv,
    paddedId = str_pad(
      string = .$id,
      width = rowMaxDigits,
      pad = "0"
    )
  )
) 

# candGrid88_centroids <- candGrid88_gv %>% st_centroid()

scooterOD_raster <- raster(
  "www/scooter_OD_classified_epsg2223.tif"
) %>% projectRasterForLeaflet(
  method = "ngb"
)



# candGrid88_gv <- candGrid88_gv %>% mutate(
#   centroid_geom = candGrid88_centroids$geometry
# )

# for testing purposes only!
candGrid88_3_gv <- candGrid88_gv[grepl(3,candGrid88_gv$area_list),]

testCellIdList <- c(
  5070:5111,
  5271:5312,
  5472:5513,
  5673:5714,
  5874:5915,
  6075:6116,
  6276:6317,
  6477:6518,
  6678:6719,
  6879:6920,
  7080:7121,
  7281:7322,
  7482:7523,
  7683:7724,
  7884:7925,
  8085:8126,
  8286:8327,
  8487:8528,
  8688:8729,
  8889:8930,
  9090:9131,
  9291:9332,
  9492:9533,
  9693:9734,
  9894:9935
  

)

candGridDebug <- candGrid88_gv[candGrid88_gv$id %in% testCellIdList,]


# gsi_info management ####
# Field names for group name, stage number, iteration number, and those values pasted together to create a unique identifier
groupName_dbField_gv <- "groupName"
stageNum_dbField_gv <- "stageNum"
iterNum_dbField_gv <- "iterNum"
gSI_databaseField_gv <- "groupStageIterInfo"
maxIterDigits_gv <- 2

# gsi_info input pop-up ####
popUp_openingScreen_function <- function(
  textInputId = "groupName",
  textOutputId = "groupNameModal",
  textLabel = "Please enter a group name:",
  textInitialValue = "",
  textPlaceholder = "(20 characters max)",
  radioInputId = "stageNum",
  radioLabel = "Please select a workshop stage",
  radioChoices = c(
    "1" = 1,
    "2" = 2
    # , "3" = 3
  ),
  footerText = "Please wait while the ColLabLocation map loads, then click \"Begin\":",
  footerStyle = "
    padding-right: 1em
  ",
  actionButtonInputId = "groupName_submit",
  actionButtonLabel = "Begin"
  
){
  showModal(
    modalDialog(
      textInput(
        inputId = textInputId,
        label = span(
          textLabel,
          textOutput(
            outputId = textOutputId,
          )
        ),
        value = textInitialValue,
        placeholder = textPlaceholder
      ),
      
      radioButtons(
        inputId = radioInputId,
        label = radioLabel,
        choices = radioChoices,
        inline = TRUE
      ),
      
      footer = tagList(
        span(
          strong(
            footerText
          ),
          style = footerStyle
        ),
        actionButton(
          inputId = actionButtonInputId,
          label = actionButtonLabel
        )
      ),
      easyClose = FALSE
    )
  )
}

# gsi_info validate input ####


validInputPattern_gv <- "^[a-zA-Z0-9_.-]+$"
minGroupNameLength_gv <- 0
maxGroupNameLength_gv <- 20

validateGroupName_function <- function(
  groupName_lv, # input$groupName,
  validInputPattern_lv = validInputPattern_gv,
  minGroupNameLength_lv = minGroupNameLength_gv,
  maxGroupNameLength_lv = maxGroupNameLength_gv,
  groupNameTooLongMessage_lv = "twenty (20) characters or fewer, please",
  groupNameInvalidMessage_lv = "alphanumeric characters, periods, dashes, and underscores only
    A-Z a-z 0-9 . - _",
  groupNameBlankMessage_lv = "group name cannot be blank"
  
){
  validate(
    need(
      expr = {
        if(
          nchar(groupName_lv) > minGroupNameLength_lv
        ){
          nchar(groupName_lv) < maxGroupNameLength_lv
        } else(
          TRUE
        )
      },
      message = groupNameTooLongMessage_lv
    ),
    need(
      expr = {
        if(
          nchar(groupName_lv) > minGroupNameLength_lv
        ) {
          str_detect(
            string = groupName_lv,
            pattern = validInputPattern_lv
          )
        } else(
          TRUE
        )
      },
      message = HTML(groupNameInvalidMessage_lv)
    ),
    need(
      expr = groupName_lv,
      message = groupNameBlankMessage_lv
    )
  )
}

# gis_info set iterNumber ####


setIterationNumber_function <- function(
  gSI_database_lv,
  groupName_lv = input$groupName,
  stageNum_lv = input$stageNum,
  iterNum_lv = iterNum_activeSesh(),
  validInputPattern_lv = validInputPattern_gv,
  minGroupNameLength_lv = minGroupNameLength_gv,
  maxGroupNameLength_lv = maxGroupNameLength_gv,
  gSI_databaseField_lv = gSI_databaseField_gv,
  groupName_dbField_lv = groupName_dbField_gv,
  stageNum_dbField_lv = stageNum_dbField_gv,
  iterNum_dbField_lv = iterNum_dbField_gv
){
  if({
    nchar(groupName_lv) > minGroupNameLength_lv
    nchar(groupName_lv) < maxGroupNameLength_lv
    str_detect(
      string = groupName_lv,
      pattern = validInputPattern_lv
    ) 
    
    # if the groupname is more than 0, less than the maximum group length, and contains only valid characters:
    
  }) {
    
    # then if the current iteration number is NULL (it is NULL by default):
    
    ifelse(
      test = is.null(iterNum_lv),
      
      # query the database for current group & stage info
      
      yes = {
        currentGroupStage <- gSI_database_lv$find(
          
          # the $find() operation queries the database using JSON syntax
          query = glue(
            '{{
                  "{group_dbField_glue}" : "{groupName_glue}",
                  "{stage_dbField_glue}" : "{stageNum_glue}"
                }}',
            group_dbField_glue = groupName_dbField_lv,
            stage_dbField_glue = stageNum_dbField_lv,
            groupName_glue = req(
              expr = groupName_lv
            ),
            stageNum_glue = req(
              expr = stageNum_lv
            )
          ),
          
          # the $find() operation returns all fields except the '_id' field by default. Since we only want the 'groupStageIterInfo', 'stageNum', and iterNum' fields, we need to explicitly disable '_id'
          fields = glue(
            '{{
                "{gSI_dbField_glue}" : true,
                "{stage_dbField_glue}" : true,
                "{iter_dbField_glue}" : true,
                "{mongoId_dbField_glue}" : false
              }}',
            gSI_dbField_glue = gSI_databaseField_lv,
            stage_dbField_glue = stageNum_dbField_lv,
            iter_dbField_glue = iterNum_dbField_lv,
            mongoId_dbField_glue = mongoID_dbField_gv
          )
        )
        
        # identify the maximum iteration number among iterations in current group & stage, then return that + 1
        
        return(
          ifelse(
            test = nrow(currentGroupStage) > 0,
            no = 1,
            yes = {max(as.integer(currentGroupStage$iterNum)) + 1}
          )
        )
      },
      
      # if iterNum_activeSesh is not null, use iterNum_activeSesh as the current iteration number
      no = {
        iterNum_lv
      }
    )
  }
}

makePaddedIterNum_function <- function(
  iterNum_lv, # = iterNum()
  maxDigits_lv = maxIterDigits_gv,
  padCharacter = "0"
){
  str_pad(
    string = iterNum_lv,
    width = maxDigits_lv,
    pad = padCharacter
  )
}

makeUniqueGSI_function <- function(
  groupName_lv = input$groupName,
  stageNum_lv =  input$stageNum,
  iterNum_lv = 1, 
  minGroupNameLength_lv = minGroupNameLength_gv,
  maxGroupNameLength_lv = maxGroupNameLength_gv,
  validInputPattern_lv = validInputPattern_gv
){
  
  if({
    nchar(req(groupName_lv)) > minGroupNameLength_lv 
    nchar(req(groupName_lv)) < maxGroupNameLength_lv
    str_detect(
      string = req(groupName_lv),
      pattern = validInputPattern_lv
    )
  }) {
    return(
      glue(
        "{groupName}_{stageNum}_{iterNum}",
        groupName = groupName_lv,
        stageNum = stageNum_lv,
        iterNum = iterNum_lv
      )
    )
  }
}

makeGSIheader_ui_function <- function(
  GSIheaderFormat_lv = "{gName_glue} | Stage: {sNum_glue}.{padIterNum_glue}",
  groupName_lv,
  stageNum_lv,
  padIterNum_lv
  
){
  glue(
    GSIheaderFormat_lv,
    gName_glue = groupName_lv,
    sNum_glue = stageNum_lv,
    padIterNum_glue = padIterNum_lv
  )
}

# leaflet UI management ####

mapMain_startLat_gv <- 33.412778
mapMain_startLng_gv <- -111.943056
mapMain_startZoom_gv <- 13
# Note: We're going to make these values the centroid of the bounding box of the current iteration, and set the zoom to encompass the current iteration.
# at some point down the line we'd like to make these reactive values that reflect the centroid of the largest selected iteration
mapCompare_startLat_gv <- 33.412778
mapCompare_startLng_gv <- -111.943056
mapCompare_startZoom_gv <- 13


layerToggle_function <- function(
  mapId_lv,
  layerButton_lv,
  layerGroup_lv
){
  ifelse(
    test = layerButton_lv,
    yes = leafletProxy(
      mapId = mapId_lv
    ) %>%
      showGroup(
        group = layerGroup_lv
      ),
    no = leafletProxy(
      mapId = mapId_lv
    ) %>%
      hideGroup(
        group = layerGroup_lv
      )
  )
}

makeLeafletMap_function <- function(
  mapToRender_lv,
  initialLat_lv,
  initialLng_lv,
  initialZoom_lv
){
  
  mapToRender_lv %>%
    
    # set initial viewpoint
    
    setView(
      lng = initialLng_lv,
      lat = initialLat_lv,
      zoom = initialZoom_lv
    ) %>%
    
    # TODO set max bounds
    
    # setMaxBounds(
    #   lng1 = ,
    #   lat1 = ,
    #   lng2 = ,
    #   lat2 = 
    # ) %>%
    
    # add Scale bar
    
    addScaleBar(
      position = "bottomright",
      options = scaleBarOptions(
        maxWidth = 100,
        metric = FALSE,
        imperial = TRUE,
        updateWhenIdle = TRUE
      )
    ) %>%
    
    # add basemap
    
    addTiles(
      group = "OpenStreetMap",
      options = tileOptions(
        zIndex = -10
      )
    ) %>%
    
    # add theme tiles
    
    addProviderTiles(
      "Esri.WorldImagery",
      group = "Satellite View",
      providerTileOptions(
        zIndex = -10
      )
    ) %>%
    
    addTiles(
# see https://developer.tomtom.com/map-display-api/api-explorer
      "https://api.tomtom.com/map/1/tile/labels/night/{z}/{x}/{y}.png?tileSize=256&view=Unified&language=NGT&key=RxvBVbRQ2QK0qtJ8CNTBeIdlgq4w4PAd",
      group = "Satellite View"
      # , options = tileOptions(
      #   zIndex = -9
      # )

    ) %>%
    
    # addProviderTiles(
    #   "CartoDB.VoyagerOnlyLabels",
    #   group = "Satellite View",
    #   providerTileOptions(
    #     zIndex = -9
    #   )
    # ) %>%


    addLayersControl(
      baseGroups = c(
        "OpenStreetMap",
        "Satellite View"
      ),
      options = layersControlOptions(
        collapsed = TRUE,
        autoZIndex = FALSE
      )
    )
}

# leaflet : make icons ####

selectedsite_icon <- makeIcon(
  iconUrl = "www/mmhSite_selected.png",
  iconWidth = 30,
  iconHeight = 40,
  iconAnchorX = 15,
  iconAnchorY = 39
)

prevSelected_icon <- makeIcon(
  iconUrl = "www/prevSelected_icon.png",
  iconWidth = 29,
  iconHeight = 20,
  iconAnchorX = 1,
  iconAnchorY = 19
)

busStation_icon <- makeIcon(
  iconUrl = "www/busStation_icon.png",
  iconWidth = 18,
  iconHeight = 28,
  iconAnchorX = 9,
  iconAnchorY = 27
)

streetcar_icon <- makeIcon(
  iconUrl = "www/streetcar_icon.png",
  iconWidth = 21,
  iconHeight = 28,
  iconAnchorX = 10,
  iconAnchorY = 27
)

# leaflet color palette ####

scootOD_brewer <- "PuOr"

palette_scootOD <- colorNumeric(
  palette = scootOD_brewer
  # palette = c("#ffffffcc","#ffffff99","#ffffff66","#ffffff11")#,"#ff00ff"),
  # palette = c("#FF0000FF","#00FF00CC","#00FFFF99","#FFFF0066","#FF00FF33","#00FFFF00"),# "YlOrRd",
  , domain = c(0,3)
  , na.color = "#00000000"
  #  , alpha = TRUE
  # , reverse = TRUE
)


# leaflet map pane management ####

mapMain_siteMarkersPane_gv <- "siteMarkers"
mapMain_busStopsPane_gv <- "busStops"
mapMain_bikewaysPane_gv <- "bikeways"
mapMain_busRoutesPane_gv <- "busRoutes"
mapMain_overlayGridPane_gv <- "overlayGrid"
mapMain_miscMarkersPane_gv <- "miscMarkers"
mapMain_selecCovGridPane_4_gv <- "selecCov_4"
mapMain_selecCovGridPane_8_gv <- "selecCov_8"
mapMain_prevCoveredGridPane_gv <- "prevCovered"
mapMain_studyAreaPane_gv <- "studyArea"
mapMain_scooterODPane_gv <- "scooterOD"
miniChart_outlinePane_gv <- "comp_overlay_markers"

mapMain_panesTopToBottom_gv <- c(
  mapMain_siteMarkersPane_gv, # needs to be first
  mapMain_overlayGridPane_gv, # needs to be second
  miniChart_outlinePane_gv,
  mapMain_busStopsPane_gv,
  mapMain_bikewaysPane_gv,
  mapMain_busRoutesPane_gv,
  mapMain_miscMarkersPane_gv,
  mapMain_selecCovGridPane_8_gv,
  mapMain_selecCovGridPane_4_gv,
  mapMain_prevCoveredGridPane_gv,
  mapMain_studyAreaPane_gv,
  mapMain_scooterODPane_gv
)

makeMapPaneZindices_function <- function(
  mapPaneNames = as.list()
){
  interval <- as.integer(100 / length(mapPaneNames))
  
  mapPaneZindices <- c()
  
  i <- 1
  
  while(
    i <= length(mapPaneNames)
  ){
    mapPaneZindices[i] <- (500 - (i * interval))
    i <- (i + 1)
  }
  
  mapPaneZindices
}

makeMapPaneList_function <- function(
  mapPaneNameVector_lv = mapMain_panesTopToBottom_gv
){
  mapPaneZ_lv <- makeMapPaneZindices_function(
    mapPaneNames =  mapPaneNameVector_lv
  )
  
  # print(mapPaneZ_lv) # debug
  
  mapPaneList <- list(
    mapPaneNames = mapPaneNameVector_lv,
    mapPaneZindex = mapPaneZ_lv
  )
  
  # print(mapPaneList) # debug
  
  mapPaneList
}

mapPaneList_gv <- makeMapPaneList_function(
  mapPaneNameVector_lv = mapMain_panesTopToBottom_gv
)



addLeafletMapPane_function <- function(
  leafletMap,
  mapPaneList_lv = mapPaneList_gv
){
  
  for(i in 1:length(mapPaneList_lv$mapPaneNames)){
    
    leafletMap <- leafletMap %>% addMapPane(
      name = mapPaneList_lv$mapPaneNames[i],
      zIndex = mapPaneList_lv$mapPaneZindex[i]
    )
  }
  return(leafletMap)
}

# shinyTree nested list management ####

nestedList_allGroups_function <- function(
  allGroups_lv
){
  # given that allGroups_lv refers to allGroupInfo()
  
  # Split each groupName by stageNum
  allGroupInfo_byStage <- allGroups_lv %>% split(
    .$stageNum,
    .$groupName
  ) %>% set_names(
    
    # set the names of the top levels to "Stage #"
    
    nm = glue(
      "Stage {sN}",
      sN = names(.)
    )
    
  ) %>% map(
    
    # in each group that we already split by stageNum and groupName, 
    # split it again by groupName
    # I don't quite know how this works but it does
    
    .f = function(x) split(
      x,
      x$groupName
    )
  ) %>% map_depth(
    
    # at depth level 2, split by "{groupName} {stageNum}.{paddedIterNum}"
    # hold groupStageIterInfo as factor
    
    .depth = 2,
    .f = function(x) {
      split( 
        x$groupStageIterInfo,
        glue(
          "{gN} {sN}.{paddedIterNum}",
          gN = x$groupName,
          sN = x$stageNum,
          paddedIterNum = str_pad(
            string = as.vector(x$iterNum),
            width = maxIterDigits_gv,
            pad = "0"
          )
        )
      )
    }
  ) 
  
  return(
    allGroupInfo_byStage
  )
}

# stats panel management ####


panelToggle_js2R_function <- function(
  elementId_lv = character(),
  shinyInputId_lv = character()
  
){
  jsCode <- glue(
    '
$(document).on("shiny:sessioninitialized", function(){{
  
  const targetNode = document.getElementById("{elementId_glue}");

  const config = {{ attributes: true, childList: true, subtree: true }};

  const callback = function() {{
    statsPanelOpen = targetNode.classList.contains("in");
    console.log(statsPanelOpen);
    Shiny.onInputChange("{shinyInputId_glue}", statsPanelOpen);
  }};

  const observer = new MutationObserver(callback);

  observer.observe(targetNode, config);
}});
',
    elementId_glue = elementId_lv,
    shinyInputId_glue = shinyInputId_lv,
    
  )
}

panel_main_toggle_jscode <- panelToggle_js2R_function(
  elementId_lv = "statsPanel_main_div",
  shinyInputId_lv = "statsPanelChange_main"
)

panel_comp_toggle_jscode <- panelToggle_js2R_function(
  elementId_lv = "statsPanel_comp",
  shinyInputId_lv = "statsPanelChange_comp"
)
