## melanoma explorer

##### Defining global objects####
# source functions
source("load_libraries.R")
library(scales)

# read in static layers
modDF=read.csv("data/master_dataframe.csv") %>% dplyr::select(-c(X,STATEFP,NAME)) %>% .[complete.cases(.),]%>% mutate(Pop=as.numeric(gsub(",","",as.character(Pop))))
seer=read.csv("data/Melanoma_insitu_vs_invasive.csv") %>% mutate(COUNTY_FIPS=as.integer(FIPS))
master=left_join(modDF,seer) %>% 
  rename("Seasonality of temperature"=seasonality_temperature) %>%
  rename("Dermatologists"=derm_pk) %>%
  rename("UV exposure"=cancer_gov_UV_exposure) %>%
  rename("Elevation"=elevation) %>%
  rename("Primary care"=pcp_pk) %>%
  rename("Seasonality of cloud"=seasonality_cloud) %>%
  rename("UV irradiance"=UV_irradiance) %>%
  rename("Median household income"=incm_mh) %>%
  rename("Income per capita"=incm_pc) 


  colnames(master)[4]="Annual range temperature"
  colnames(master)[6]="Mean cloud cover"
  colnames(master)[11]="Sun exposure"
  colnames(master)[12]="UV daily dose"
  colnames(master)[18]="Doctors"
  colnames(master)[19]="Households >$50,000"
  colnames(master)[20]="Households >$100,000"
  colnames(master)[21]="Health insurance < age 65"
  colnames(master)[8]="Mean temperature"

masterUV=master[,c(4:13)]
masterME=master[,c(14:21)]
##### UI code

# Define UI for application 
ui <- dashboardPage(skin="black",
                    dashboardHeader(
                      title="Melanoma incidence rate explorer",
                      titleWidth = 300
                      ),
                    dashboardSidebar(
                      width = 300,
                      sidebarMenu(id = 'sidebarmenu',
                                  menuItem("Select model inputs", tabName='model',icon=icon("chart-line",lib='font-awesome')),
                                  varSelectInput("UV", "UV exposure variables", masterUV, selected = "Elevation", multiple = T,
                                                 selectize = T, width = NULL, size = NULL),
                                  varSelectInput("ME", "Medical care exposure variables", masterME, selected = "Primary care", multiple = T,
                                                 selectize = T, width = NULL, size = NULL))
                                  
                                  
                    ),
                    
                    dashboardBody(
                      fluidRow(width=12,
                               plotOutput("hcontainer")

                      ),
                      fluidRow(width=4,
                      absolutePanel(verbatimTextOutput("placeholderME"), top=100, right=100),
                      absolutePanel(verbatimTextOutput("placeholderUV"), top=140, right=100))
                    ))




server <- shinyServer(function(input, output) {
  output$hcontainer <- renderPlot({
    UVDF=masterUV %>% dplyr::select(!!!input$UV)%>% mutate(Pop=master$Pop)%>% mutate(SEER_rate=master$SEER_rate,In.situ=master$In.situ,Invasive=master$Invasive) 
    MEDF=masterME %>% dplyr::select(!!!input$ME)%>% mutate(Pop=master$Pop)%>% mutate(SEER_rate=master$SEER_rate,In.situ=master$In.situ,Invasive=master$Invasive) 
    # allin=cbind(UVDF,MEDF) %>% mutate(Pop=master$Pop)%>% mutate(SEER_rate=master$SEER_rate,In.situ=master$In.situ,Invasive=master$Invasive) %>% filter(Pop>input$slider1[1]&Pop<input$slider1[2])
      # fm2=lm(as.formula(glue("SEER_rate ~ .")),data=test)
     UV_SEER_rate=r.squaredGLMM(lm(SEER_rate ~ .,data=dplyr::select(UVDF,-c(In.situ,Invasive))))[1]
     UV_In.situ=r.squaredGLMM(lm(In.situ ~ .,data=dplyr::select(UVDF,-c(SEER_rate,Invasive))))[1]
     UV_Invasive=r.squaredGLMM(lm(Invasive ~ .,data=dplyr::select(UVDF,-c(In.situ,SEER_rate))))[1]
     
     ME_SEER_rate=r.squaredGLMM(lm(SEER_rate ~ .,data=dplyr::select(MEDF,-c(In.situ,Invasive))))[1]
     ME_In.situ=r.squaredGLMM(lm(In.situ ~ .,data=dplyr::select(MEDF,-c(SEER_rate,Invasive))))[1]
     ME_Invasive=r.squaredGLMM(lm(Invasive ~ .,data=dplyr::select(MEDF,-c(In.situ,SEER_rate))))[1]
     
     # UV_SEER_rate=lm(SEER_rate ~ .,data=dplyr::select(UVDF,-c(In.situ,Invasive)))
     # UV_In.situ=lm(In.situ ~ .,data=dplyr::select(UVDF,-c(SEER_rate,Invasive)))
     # UV_Invasive=lm(Invasive ~ .,data=dplyr::select(UVDF,-c(In.situ,SEER_rate)))
     # ME_SEER_rate=lm(SEER_rate ~ .,data=dplyr::select(MEDF,-c(In.situ,Invasive)))
     # ME_In.situ=lm(In.situ ~ .,data=dplyr::select(MEDF,-c(SEER_rate,Invasive)))
     # ME_Invasive=lm(Invasive ~ .,data=dplyr::select(MEDF,-c(In.situ,SEER_rate)))
     
     # form=paste("Melanoma ~",colnames(UVDF,sep=" + "))
     
     names=c("All","In_situ","Invasive","All","In_situ","Invasive")
     type=c("Environment","Environment","Environment","Medical","Medical","Medical")
     rsq=list(UV_SEER_rate,UV_In.situ,UV_Invasive,ME_SEER_rate,ME_In.situ,ME_Invasive) %>% unlist()
     
     rsqDF=data.frame(Names=as.character(names),
                      type=as.character(type),
                      rsq=as.numeric(rsq),
                      stringsAsFactors = F)
     
     
     ggplot(data=rsqDF,aes(x=names,y=rsq))+geom_bar(aes(group=type,fill=type),stat="identity",position = "dodge")
    
    
    
    
    
  })
  
  output$placeholderUV=renderText({
    UVDF=masterUV %>% dplyr::select(!!!input$UV)
    form=paste(colnames(UVDF),sep="",collapse="+")
    form1=glue("Melanoma ~ {form}")
    form1
    
    })
  
  output$placeholderME=renderText({
    MEDF=masterME %>% dplyr::select(!!!input$ME)
    forma=paste(colnames(MEDF),sep="",collapse="+")
    form1a=glue("Melanoma ~ {forma}")
    form1a
    
  })
  
  

})

# Run the application 
shinyApp(ui = ui, server = server)
