
library(tidyverse) # contains dplyr, ggplot2, readr, tidyr, stringr, purrr and forcats
library(tidytable) # for case
library(shiny)
#library(shinyjs)
library(data.table)
library(shinyBS)

# See https://cran.r-project.org/web/packages/data.table/vignettes/datatable-intro.html for data.table stuff

#urlfile="https://raw.githubusercontent.com/AlexGibsonPlymouth/CoastalData/main/00_DataList.csv"
#gitData<-read_csv(url(urlfile))
#print(head(gitData,10))
#names(gitData)
#write

#Directory = "H:/000_Coastal_Related/00_ESRC Coastal Classification/03_DataPortal/02_PrepareData/02_CSVDataForMaps/DataSeries/"
#DataList = setDT(read.csv(paste0(Directory,"00_DataList.csv")))
print(getwd())
DataList = setDT(read.csv("./00_DataList.csv"))
#DataList = setDT(gitData)
#print(paste0("Input DataList contains ",nrow(DataList)," items"))
#print("Headings of DataList follows:")
#print(names(DataList))
#print("########################################################")

print("##### 17")
GeographyChoiceList = DataList %>% group_by(Geography) %>% summarise(Count=n()) %>% select(Geography) %>% pull(.)
GeographyChoiceListDefault = "LSOA 2021"
#print(paste0("GeographyChoiceList follows:"))
#print(GeographyChoiceList)
#print(paste0("GeographyChoiceListDefault = ",GeographyChoiceListDefault))
#print("########################################################")

print("##### 25")
PickerGroupChoiceList = DataList %>% filter(Geography == GeographyChoiceListDefault) %>% group_by(PickerListGroup) %>% summarise(Count=n()) %>% select(PickerListGroup) %>% pull(.)
PickerGroupChoiceListDefault = PickerGroupChoiceList[1]
#print(paste0("PickerGroupChoiceList follows:"))
#print(PickerGroupChoiceList)

print(paste0("PickerGroupChoiceListDefault = ",PickerGroupChoiceListDefault))
#print("########################################################")

print("##### 34")
PickerItemChoiceList = DataList %>% filter(Geography == GeographyChoiceListDefault & PickerListGroup == PickerGroupChoiceListDefault) %>% select(PickerItem) %>% pull(.)
#print(paste0("PickerItemChoiceList follows:"))
#print(PickerItemChoiceList)
PickerItemChoiceListDefault = PickerItemChoiceList[1]
#print(paste0("PickerItemChoiceList follows:"))
#print(PickerItemChoiceList)

#print(paste0("PickerItemChoiceListDefault = ",PickerItemChoiceListDefault))
#print("########################################################")
print("##### 42")

# Uncomment next line to look at ONLY variables not yet live!
#DataList = DataList[Live == 0,]
print(paste0("Working DataList has ",nrow(DataList)))

ui <- fluidPage(
  title ="Data Intergrity Test",
  
  tags$head( 
    tags$style(type="text/css", ".container-fluid {  max-width: 1366px;}"),   # This is the standard low-cost laptop size
    #tags$style(HTML("h3 {color: red; }")),
    tags$style(HTML("p {margin-bottom: 3px; }")),
    tags$style(HTML("ul {margin-bottom: 3px; }")),
    tags$style(HTML("#first {border: 2px solid black; width: 600pt;}"))
  ),

  titlePanel(HTML("<h3>To test the integrity of data <u>and</u> DataList entries prior to inclusion in CESA Data Portal</h4>")),
  
  fluidRow(
    column(width = 12,
           div(style = "height: 2pt;"),
           htmlOutput("IntroductionText"),
           div(style = "height: 5pt;")
           )
  ),
  fluidRow(
    column(width = 2,
           div(style = "height: 2pt; "),
           selectInput("GeographyName", label = "Geog/Date ..",
                       choices = GeographyChoiceList, 
                       selected = GeographyChoiceListDefault,
                       width="200px")
    ),
    column(width = 4,
           div(style = "height: 2pt; "),
           uiOutput("PickerGroupNameInterface")
    ),
    column(width = 6,
           div(style = "height: 2pt; "),
           uiOutput("PickerItemNameInterface")
    )
  ),
  fluidRow(
     column(width = 12,
           div(style = "height: 2pt; "),
           htmlOutput("PreliminaryReport")
    )
  ),
  fluidRow(
    column(width = 7,
           div(style = "height: 5pt;"),
           htmlOutput("LongTitleText"),
           div(style = "height: 5pt;")
    ),
    column(width = 5,
           div(style = "height: 5pt;"),
           htmlOutput("ShortTitleText"),
           div(style = "height: 5pt;")
    )
  ),
  fluidRow(
    column(width = 6,
           div(style = "height: 2pt;"),
           htmlOutput("TextReport1"),
    ),
    column(width = 6,
           div(style = "height: 2pt;"),
           htmlOutput("TextReport2"),
           #htmlOutput("PlotIntro"),
           #plotOutput("SamplePlot", height = "200pt")
    )
  ),
  fluidRow(
    column(width = 12,
           div(style = "height: 5pt;"),
           htmlOutput("TextReport3"),
    )
  ),
  fluidRow(
    column(width = 8,
           div(style = "height: 5pt;"),
           htmlOutput("TextReport4"),
    ),
    column(width = 4,
           div(style = "height: 5pt;"),
           htmlOutput("TextReport5"),
    )
  )
  
  
  ) # end of fluidPage

server <- function(input, output, session) {

  Target = reactiveVal(NULL)
  
  observe({
    print("##### 112")
    output$IntroductionText = renderText(HTML("
      <p style='font-size:105%; margin-right: 100px; line-height:1.3; margin-bottom:10px;'>The <strong>CESA Data Portal</strong> 
      reads data from RDS files. These are created (using <em>00_PrepareDataSeries.R</em>) from csv files in the 
      <em>./02_DataCollection/00_DataForPortal/</em> directory.  A 'driver file' called <em>DataList.csv</em> provides all the 
      instructions needed to ensure that the data (and accompanying metadata) is displayed properly. <span style='color: red;'>It 
      is <strong>vital</strong> these instructions are correct</span>.</p>
      
      <p style='font-size:105%; margin-right: 100px; line-height:1.3; margin-bottom:10px;'>Following the 'Data Entry Guide', add new data files to the
      <em>./02_DataCollection/00_DataForPortal/</em>  directory and, <u>having saved a backup copy</u>, edit 
      the <em>DataList.csv</em> file as instructed. This app checks that all is as it should be. Once all is 
      well, update the entries in the 'Live' column of the <em>DataList.csv</em> to '1'. The new data will now be 'seen' by the portal and, for
      added security, the new data should be also tested within the <strong>CESA Data Portal</strong></p>"))
  })  
  

  observeEvent(input$GeographyName,{
    print("##### 131")
    
    PickerGroupChoiceList = DataList %>% filter(Geography == input$GeographyName) %>% group_by(PickerListGroup) %>% summarise(Count=n()) %>% select(PickerListGroup) %>% pull(.)
    PickerGroupChoiceListDefault = PickerGroupChoiceList[1]
    #print(paste0("PickerGroupChoiceList follows:"))
    #print(PickerGroupChoiceList)
    
    output$PickerGroupNameInterface <- renderUI({
      selectInput("PickerGroupName", label = "Select the 'PickerItem' ..",
                  choices = PickerGroupChoiceList, 
                  selected = PickerGroupChoiceListDefault,
                  width="550pt")
    })
  })

      
  observeEvent(input$PickerGroupName,{
    print("##### 146")
    
    PickerItemChoiceList = DataList %>% filter(Geography == input$GeographyName & PickerListGroup == input$PickerGroupName) %>% 
                                group_by(PickerItem) %>% summarise(Count=n()) %>% select(PickerItem) %>% pull(.)
    #print(paste0("PickerItemChoiceList follows:"))
    #print(PickerItemChoiceList)
    PickerItemChoiceListDefault = PickerItemChoiceList[1]
    
    output$PickerItemNameInterface <- renderUI({
        selectInput("PickerItemName", label = "Select the 'PickerItem' ..",
                    choices = PickerItemChoiceList, 
                    selected = PickerItemChoiceListDefault,
                    width="550pt")
    })
  })
 
   
  observeEvent(input$PickerItemName,{
    print("##### 164")
    print(paste0(" input$GeographyName = ",input$GeographyName))
    print(paste0(" input$PickerGroupName = ",input$PickerGroupName))
    print(paste0(" input$PickerItemName = ",input$PickerItemName))
    Target(DataList %>% filter(Geography == input$GeographyName & 
                                   PickerListGroup == input$PickerGroupName & 
                                    PickerItem == input$PickerItemName) %>% pull(UniqueName))
    #print(paste0("UniqueName(Target) = ",Target()))
  })
 
  # Now we produce the report
  observeEvent(Target(),{
    print("##### 175")
    print(paste0("UniqueName(Target) = ",Target()))
    Target_Group = DataList %>% filter(Geography == input$GeographyName & PickerListGroup == input$PickerGroupName & PickerItem == input$PickerItemName) %>% pull(Group)
    Target_Geography = DataList %>% filter(Geography == input$GeographyName & PickerListGroup == input$PickerGroupName & PickerItem == input$PickerItemName) %>% pull(Geography)
    Target_Origin = DataList %>% filter(Geography == input$GeographyName & PickerListGroup == input$PickerGroupName & PickerItem == input$PickerItemName) %>% pull(Origin)
    Target_PickerListGroup = DataList %>% filter(Geography == input$GeographyName & PickerListGroup == input$PickerGroupName & PickerItem == input$PickerItemName) %>% pull(PickerListGroup)
    Target_PickerItem = DataList %>% filter(Geography == input$GeographyName & PickerListGroup == input$PickerGroupName & PickerItem == input$PickerItemName) %>% pull(PickerItem)
    Target_CSVDataFile = DataList %>% filter(Geography == input$GeographyName & PickerListGroup == input$PickerGroupName & PickerItem == input$PickerItemName) %>% pull(CSVDataFile)
    Target_ValueColumn = DataList %>% filter(Geography == input$GeographyName & PickerListGroup == input$PickerGroupName & PickerItem == input$PickerItemName) %>% pull(ValueColumn)
    Target_RankColumn = DataList %>% filter(Geography == input$GeographyName & PickerListGroup == input$PickerGroupName & PickerItem == input$PickerItemName) %>% pull(RankColumn)
    Target_ShinyDataObject = DataList %>% filter(Geography == input$GeographyName & PickerListGroup == input$PickerGroupName & PickerItem == input$PickerItemName) %>% pull(ShinyDataObject)
    Target_MetaDataNotes = DataList %>% filter(Geography == input$GeographyName & PickerListGroup == input$PickerGroupName & PickerItem == input$PickerItemName) %>% pull(MetaDataNotes)
    Target_UniqueName = DataList %>% filter(Geography == input$GeographyName & PickerListGroup == input$PickerGroupName & PickerItem == input$PickerItemName) %>% pull(UniqueName)
    Target_LongTitle = DataList %>% filter(Geography == input$GeographyName & PickerListGroup == input$PickerGroupName & PickerItem == input$PickerItemName) %>% pull(LongTitle)
    Target_ShortTitle = DataList %>% filter(Geography == input$GeographyName & PickerListGroup == input$PickerGroupName & PickerItem == input$PickerItemName) %>% pull(ShortTitle)
    Target_Comments = DataList %>% filter(Geography == input$GeographyName & PickerListGroup == input$PickerGroupName & PickerItem == input$PickerItemName) %>% pull(Comments)
    Target_MetadataEntry = DataList %>% filter(Geography == input$GeographyName & PickerListGroup == input$PickerGroupName & PickerItem == input$PickerItemName) %>% pull(MetadataEntry)
    Target_DataGeogCode = DataList %>% filter(Geography == input$GeographyName & PickerListGroup == input$PickerGroupName & PickerItem == input$PickerItemName) %>% pull(DataGeogCode)
    Target_Year = DataList %>% filter(Geography == input$GeographyName & PickerListGroup == input$PickerGroupName & PickerItem == input$PickerItemName) %>% pull(Year)
    Target_Live = DataList %>% filter(Geography == input$GeographyName & PickerListGroup == input$PickerGroupName & PickerItem == input$PickerItemName) %>% pull(Live)
    Target_RankForLowValue = DataList %>% filter(Geography == input$GeographyName & PickerListGroup == input$PickerGroupName & PickerItem == input$PickerItemName) %>% pull(RankForLowValue)
    Target_SourceDirectory = DataList %>% filter(Geography == input$GeographyName & PickerListGroup == input$PickerGroupName & PickerItem == input$PickerItemName) %>% pull(SourceDirectory)
    
    if (Target_RankForLowValue == "High")
      {
      RankInterpretation = "When mapping and plotting values, <strong>high values</strong> are deemed 
      to indicate <strong>greater policy challenges</strong> and (following the IMD approach) given a
      <strong>low ranking</strong>."
    }
    else if (Target_RankForLowValue == "Low")
      {
      RankInterpretation = "When mapping and plotting values, <strong>low values</strong> are deemed 
      to indicate <strong>greater policy challenges</strong> and (following the IMD approach) given a
      <strong>low ranking</strong>."
    }
    else if (Target_RankForLowValue == "notapplicable")
      {
      RankInterpretation = "When mapping and plotting values it is expressly stated that there are <u>no
      obvious policy implications<u>. Accepting that, <strong>high values</strong> are simply given a
      <strong>high ranking</strong>."
    }
    else 
      {
      RankInterpretation = "Explanatory text needed"
    }
      
    
    TargetCount = length(DataList %>% filter(UniqueName == Target()) %>% pull(.))
    if (TargetCount > 1) 
      {
      output$PreliminaryReport <- renderPrint({
        HTML(paste0("<p style='font-size:105%; border-width:2px; border-style:solid; border-color:#FF0000; padding: 2px;'>
                    The UniqueName entry (<strong>",Target(),"</strong>) on the DataList file is not unique. It must be - so change it!"))
      })
      
    }
    else if (TargetCount == 0) 
      {
      output$PreliminaryReport <- renderPrint({
        HTML(paste0("<p style='font-size:105%; border-width:2px; border-style:solid; border-color:#FF0000; padding: 2px;'>
                    The UniqueName entry (<strong>",Target(),"</strong>) cannot be found - this really shouldn't EVER happen!</p>"))
        
      })
      
    }
    else if (TargetCount == 1)
      {
      output$PreliminaryReport <- renderPrint({
        HTML(paste0("<p style='font-size:105%; border-width:2px; border-style:solid; border-color:#006600; padding: 2px;'>
                      The UniqueName entry = <strong>",Target(),"</strong>"))
      })
      
      output$LongTitleText <- renderPrint({
        HTML(paste0("<p style='font-size:105%; border-width:2px; border-style:solid; border-color:#006600; padding: 2px;'>
                      Long Title: <strong><em>",Target_LongTitle,"</strong></em>"
        ))
      })
      
      output$ShortTitleText <- renderPrint({
        HTML(paste0("<p style='font-size:105%; border-width:2px; border-style:solid; border-color:#006600; padding: 2px;'>
                      Short Title: <strong><em>",Target_ShortTitle,"</strong></em>"
        ))
      })
      
      output$TextReport1 <- renderPrint({
        HTML(paste0("<p style='font-size:105%; color:#000000; border-width:2px; border-style:solid; border-color:#006600; padding: 2px;'>
                    <strong><em>General Source Information</strong></em><br>
                    DataList Group <em>[Group]</em> = <strong>",Target_Group,"</strong><br>
                    Data file <em>[CSVDataFile]</em> = <strong>",Target_CSVDataFile,"</strong><br>
                    Column with values <em>[ValueColumn]</em> = <strong>",Target_ValueColumn,"</strong><br>
                    Column with ranks <em>[RankColumn]</em> = <strong>",Target_RankColumn,"</strong><br>
                    Internal R object name <em>[ShinyDataObject]</em> = <strong>",Target_ShinyDataObject,"</strong><br>
                    Whether visible to Portal <em>[Live]</em> = <strong>",Target_Live,"</strong> (0 = 'not visible' & 1 = 'visible')<br>
                    Data trail for RAP <em>[SourceDirectory]</em> = <strong>",Target_SourceDirectory,"</strong>"))
      })
        
      output$TextReport2 <- renderPrint({
        HTML(paste0("<p style='font-size:105%; border-width:2px; border-style:solid; border-color:#006600; padding: 2px;'>
                    <strong><em>Geography, Date & Classification Information</strong></em><br>
                    Geography = <strong>",Target_Geography,"</strong><br>
                    DataGeogCode = <strong>",Target_DataGeogCode,"</strong><br>
                    Year = <strong>",Target_Year,"</strong><br>
                    PickerListGroup = <strong>",Target_PickerListGroup,"</strong><br>
                    PickerItem = <strong>",Target_PickerItem,"</strong><br><br>"
                    ))
      })
        
      output$TextReport3 <- renderPrint({
        HTML(paste0("<p style='font-size:105%; border-width:2px; border-style:solid; border-color:#006600; padding: 2px;'>
                    <strong><em>How rank order is calculated</strong> [RankForLowValue = <strong>",Target_RankForLowValue,"</strong>]</em><br>
                    &#8680;</strong> ",
                    RankInterpretation
        ))
      })
 
      output$TextReport4 <- renderPrint({
        HTML(paste0("<p style='font-size:105%; border-width:2px; border-style:solid; border-color:#006600; padding: 2px;'>
                    <strong><em>MetaData Entry:</strong> [Target_MetaDataNotes]</em><br>",Target_MetaDataNotes
        ))
      })
      
      output$TextReport5 <- renderPrint({
        HTML(paste0("<p style='font-size:105%; border-width:2px; border-style:solid; border-color:#006600; padding: 2px;'>
                    <strong><em>Short Title:</strong></em>",Target_ShortTitle
        ))
      })
        
    }
})
  
}

shinyApp(ui, server)


