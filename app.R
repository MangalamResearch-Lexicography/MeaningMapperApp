# MEANING MAPPER APP

# !!!  @Luis and @Bruno !!! => before launching the app:
#  in the PARENT DIRECTORY of the MeaningMapper folder create 2 folders,
# one called "RawConc" and one called "PreEditVersions"
# save the SketchEngine csv as Conc.txt in the data folder.
# remove everything else from the data folder.

## @Ligeia => TO DO :
#  version with ConcRel.
#  When All works publish that verison online and remove previous one

## ADD ConcRel :
# "leadingTo"
# "causedBy"
# "possessing"
# "belongingTo"
# "locusOf"
# "locatedIn"
# "byMeansOf"
# "achievedThrough"
# "goalOf"
# "takesGoal"
# think of conll export



library(DT)
library(tidyverse)
library(tokenizers)
library(stringr)
library(ggplot2)
library(plotly)
library(data.tree)
library(collapsibleTree)
library(DiagrammeR)
library(wordcloud)
AddWordID <- function(KwicDF_lemCol_9_KwicCol_11){
  KwicDF_lemCol_9_KwicCol_11$citWithID
  for (i in 1:nrow(KwicDF_lemCol_9_KwicCol_11)){
    
    lemma <- KwicDF_lemCol_9_KwicCol_11[i,9]
    sentence <- KwicDF_lemCol_9_KwicCol_11[i,11]
    lemma <- as.character(lemma)
    sentence <- as.character(sentence)
    # sentence <- gsub(" @ "," - ",sentence)
    sentence <- gsub("\\s+([-|@])","\\1",sentence)# uncomment this to assign a signle ID to STEM+suffix together (this will not take advantage of pseudo lemmatisation afforder by stemming.)
    #print(sentence)
    sentencetok <- tokenize_regex(sentence,"\\s+")
    sentencetok <- unlist(sentencetok)
    #print(sentencetok)
    IDs <- seq(1:length(sentencetok))
    sentencetok <- tolower(sentencetok)
    sentencetok <- gsub("([-|@])"," \\1",sentencetok) #uncomment this is you uncommnted the line sentence <- gsub("\\s+-","-",sentence)
    sentenceP <- paste(IDs,sentencetok)
    sentenceP <- paste(sentenceP,collapse = " ")
    #  print(sentenceP)
    KwicDF_lemCol_9_KwicCol_11$citWithID[i]<-  sentenceP
  }
  return(KwicDF_lemCol_9_KwicCol_11)
}


DataFrameTokens_CAPS2 <- function(Cleaned_Segmented_Text){
  Textname <- deparse(substitute(Cleaned_Segmented_Text))
  Text_TokensDF <- as.data.frame(table(unlist(str_extract_all(Cleaned_Segmented_Text," ([A-Z]|[ĀĪŪṚḶṆḌÑṄḤŚṢṬḌṂ])+ "))))
  #write.table(Text_TokensDF,file= paste(Textname,"TokensDF.csv", sep=""), quote=F,sep=",",row.names=F)
  return(Text_TokensDF)
}

ConcPrepR <- function(filePath){
  library(tidyverse)
  library(tokenizers)
  library(stringr)
  library(readtext)
  AddWordID <- function(KwicDF_lemCol_10_KwicCol_12){
    KwicDF_lemCol_10_KwicCol_12$citWithID
    for (i in 1:nrow(KwicDF_lemCol_10_KwicCol_12)){
      
      lemma <- KwicDF_lemCol_10_KwicCol_12[i,10]
      sentence <- KwicDF_lemCol_10_KwicCol_12[i,12]
      lemma <- as.character(lemma)
      sentence <- as.character(sentence)
      sentence <- gsub(" @ "," - ",sentence)
      sentence <- gsub("\\s+-","-",sentence)
      #print(sentence)
      sentencetok <- tokenize_regex(sentence,"\\s+")
      sentencetok <- unlist(sentencetok)
      #print(sentencetok)
      IDs <- seq(1:length(sentencetok))
      sentencetok <- tolower(sentencetok)
      sentencetok <- gsub("-"," -",sentencetok)
      sentenceP <- paste(IDs,sentencetok)
      sentenceP <- paste(sentenceP,collapse = " ")
      #  print(sentenceP)
      KwicDF_lemCol_10_KwicCol_12$citWithID[i]<-  sentenceP
    }
    return(KwicDF_lemCol_10_KwicCol_12)
  }
  
  SAMPLE <- readtext(filePath)
  OriginalConc <- SAMPLE$text
  SAMPLE <- SAMPLE$text
  ## fix missing starting <s> => this is a problem with sketch engine
  MissingS <- unlist(regmatches(SAMPLE,gregexpr(".....................................,\\d+\",\".[^s]..........",SAMPLE, perl = T)))
  for (m in MissingS){
    fixed <- gsub("(,\\d+\"),\"(.)",'\\1,"<s> \\2',m)
    SAMPLE <- gsub(m,fixed,SAMPLE)
  }
  rm(m)
  
  SAMPLE <- gsub("<g/>", "",SAMPLE)
  SAMPLE <- gsub('"',"",SAMPLE)
  SAMPLE <- gsub('^corpus.*?\n',"",SAMPLE)
  SAMPLE <- gsub('^subcorpus.*?\n',"",SAMPLE)
  SAMPLE <- gsub('^concordance.*?\n',"",SAMPLE)
  SAMPLE <- gsub('^query.*?\n',"",SAMPLE)
  SAMPLE <- gsub('^_,',"",SAMPLE)
  SAMPLE <- gsub('\n_,',"\n",SAMPLE)
  #find and fill missing page and sID values
  # fixMissing <- gsub('(([a-z]|[āīūṛḷṇḍñṅḥśṣṭḍṃ])([a-z]|[āīūṛḷṇḍñṅḥśṣṭḍṃ])+,\\d+[vr]?),<s>',"\\1,0000,<s>",SAMPLE) # cases where structure c is missing altogether. remove this line when you fix corpus <s>
  # fixMissing <- gsub(',,',",0000,",fixMissing)
  # Missing <- unlist(regmatches(fixMissing,gregexpr("................0000.....................",fixMissing)))
  # n <- 1
  # #print("the following missing pages and sID are being filled with placeholders")
  # print("CHECK that there are only 2 colums containing numerals for page and sID")
  # for (m in Missing){
  #   print(m)
  #  makeUnique <- gsub("0000", paste0("0000",n), b)
  #
  #   SAMPLE <- gsub(m, makeUnique, SAMPLE)
  #   n <- n+1
  # }
  # rm(m)
  
  
  HeaderString <- "title,text.type,author,tradition,discourse,period,page,sID,kwicLeft,lemma,kwicRight"
  SAMPLE <- paste(HeaderString,SAMPLE, sep="\n")
  write(SAMPLE,"./RawConcordanceToProcess.csv")
  SAMPLE <- read.csv("./RawConcordanceToProcess.csv", stringsAsFactors = F )
  SAMPLE$kwicRight <- gsub("\\s+"," ",SAMPLE$kwicRight)
  SAMPLE$kwicLeft <- gsub("\\s+"," ",SAMPLE$kwicLeft)
  SAMPLE$Kwic <- paste0(SAMPLE$kwicLeft,SAMPLE$lemma,SAMPLE$kwicRight)
  SAMPLE$Kwic <- gsub("</?s>","",SAMPLE$Kwic)
  SAMPLE$Kwic <- gsub("\\s+"," ",SAMPLE$Kwic)
  SAMPLE$lemma <- gsub("\\s","",SAMPLE$lemma)
  colnames(SAMPLE)
  SAMPLE <- AddWordID(SAMPLE)
  SAMPLE$transl <- ""
  SAMPLE$sense <- ""
  SAMPLE$subsense <- ""
  SAMPLE$domain <- ""
  SAMPLE$sem.field <- ""
  SAMPLE$sem.cat <- ""
  SAMPLE$is_object_or_patient_of <- ""
  SAMPLE$takes_as_object_or_patient <- ""
  SAMPLE$is_subject_or_agent_of <- ""
  SAMPLE$takes_as_subject_or_agent <- ""
  SAMPLE$takes_as_oblique <- ""
  SAMPLE$is_oblique_of <- ""
  SAMPLE$is_modified_by <- ""
  SAMPLE$modifies <- ""
  SAMPLE$is_listed_with <- ""
  SAMPLE$is_contrasted_with <- ""
  SAMPLE$is_glossed_by <- ""
  SAMPLE$glosses <- ""
  SAMPLE$is_juxtaposed_to <- ""
  SAMPLE$sem.pros <- ""
  SAMPLE$uncertainty <- ""
  SAMPLE$Notes <- ""
  SAMPLE$ref <- paste0(SAMPLE$title,SAMPLE$sID)
  SAMPLE$lemma1 <- tolower(SAMPLE$lemma)
  SAMPLE$in_compound <-""
  for(i in 1:nrow(SAMPLE)){
    
    SAMPLE$in_compound[[i]] <- ifelse(length(grep("@\\s?$", SAMPLE$kwicLeft[i]))>0,"YES","NO")
    SAMPLE$in_compound[[i]] <- ifelse(length(grep("^\\s?-([a-z]|[āīūṛḷṇḍñṅḥśṣṭḍṃ])+ @", SAMPLE$kwicRight[i]))>0,"YES",SAMPLE$in_compound[i])
  }
  zu <- paste(str_extract_all(SAMPLE$kwicLeft,"([A-Z]|[ĀĪŪṚḶṆḌÑṄḤŚṢṬḌṂ])+"),str_extract_all(SAMPLE$kwicRight,"([A-Z]|[ĀĪŪṚḶṆḌÑṄḤŚṢṬḌṂ])+"),sep = " ")
  SAMPLE$cotext <- gsub(',|character\\(0\\)|\\(|\\)|\\c|\"',"", zu)
  SAMPLE$case_or_voice <- ""
  SAMPLE$number <- ""
  ## Add ConcRel:
  SAMPLE$leadingTo <- ""
  SAMPLE$causedBy <- ""
  SAMPLE$possessing <- ""
  SAMPLE$belongingTo <- ""
  SAMPLE$locusOf <- ""
  SAMPLE$locatedIn <- ""
  SAMPLE$byMeansOf <- ""
  SAMPLE$achievedThrough <- ""
  SAMPLE$goalOf <- ""
  SAMPLE$takesGoal <- ""
  
  
  #print(colnames(SAMPLE))
  # move author column to the end
  SAMPLE <- SAMPLE[,c(1,2,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51, 3)]
  SAMPLE <- SAMPLE[!duplicated(SAMPLE$ref),]
  # print(colnames(SAMPLE))
  write.csv(SAMPLE, "./data/ConcordancesReady.csv", row.names = F)
  
  FileName <- paste0("../RawConc/", unique(SAMPLE$lemma)[1],"_RawConc",Sys.Date(),".txt")
  write(OriginalConc, FileName)
  #write.csv(SAMPLE, "~/Desktop/SAMPLE.csv", row.names = F)
  #print("your file is ready in the data folder")
}




if(length(grep("ConcordancesReady.csv",dir("./data/")))>0){
  sample <- read.csv("./data/ConcordancesReady.csv", stringsAsFactors = F)
  sample[is.na(sample)] <- ""
}else{
ConcPrepR("./data/Conc.txt")
  sample <- read.csv("./data/ConcordancesReady.csv", stringsAsFactors = F)
  sample[is.na(sample)] <- ""
}




LexicalData <- readRDS("./www/LexicalData_Revised20Nov2019.rds")
#colnames(LexicalData)

ui <- fluidPage(
  titlePanel(HTML("<font color='#3B444C' size='+4'>Meaning Mapper: a lexical annotation tool</font>")),
  
  # tags$h1("Meaning Mapper: a lexical annotation tool"),
  tags$div(HTML("created by <font color = '#4682B4'> Ligeia Lugli</font> as part of <a href='https://btw.mangalamresearch.org/en-us/' target='_blank'>The Buddhist Translators Workbench</a>.")),
  tags$br(),
  sidebarLayout(
    sidebarPanel(
      tags$div(HTML("<font size='+3'>see <a href='Meaning Mapper App_ draft documentation.html' target='_blank'>documentation</a></font>")),
      tags$hr(),
      tags$h3("visualise annotations"),
      tags$hr(),
      selectInput(inputId = "DataVis",
                  label = "Which aspect of the lemma would you like to explore?",
                  choices = c("sense",
                              "subsense",
                              "sem.pros",
                              "domain",
                              "sem.field",
                              "sem.cat",
                              "text.type",
                              "period",
                              "number",
                              "case_or_voice",
                              "title"),
                  selected = "domain"),
      
      # Select variable to colour plot by
      selectInput(inputId = "fill",
                  label = "By which variable would you like to chart your chosen aspect of the lemma?",
                  choices = c("sense",
                              "subsense",
                              "sem.pros",
                              "domain",
                              "sem.field",
                              "sem.cat",
                              "text.type",
                              "period",
                              "number",
                              "case_or_voice",
                              "title"),
                  selected = "sem.pros"),
      
      selectInput(inputId = "barchart_type",
                  label = "which type of bartchart would you like to view?",
                  choices = c("side-by-side"="dodge",
                              "stacked" ="stack",
                              "percentage"="fill"),
                  selected = "stack"),
      
      tags$hr(),
      tags$br(),
      tags$hr(),
      tags$h3("annotate"),
      tags$hr(),
      selectInput("cit",
                  "select a citation to annotate",
                  choices = sort(as.character(sample$ref[nchar(sample$sem.pros)<3]))
                  #choices = setNames(c(1:length(sort(sample$ref[sample$sem.pros==""]))), as.character(sort(sample$ref[sample$sem.pros==""])))
      ),
      tags$br(),
      textInput("trans","input translation here:", value=NULL),
      # tags$br(),
      # tags$h4("semantic prosody"),
      # tags$br(),
      tags$br(),
      # tags$h4("syntactic dependencies"),
      
      textInput("Grammar","lemma,case,number:", value= NULL),
      
      actionButton('SaveDraft', 'Save draft'),
      tags$br(),
      selectInput("SynDep1",
                  "dep.rel",
                  
                  choices = c("select",colnames(sample[,19:26])),
                  # selected = "select"#NULL
                  selected = NULL
      ),
      # textInput("text1","Text 1:",value="dep"),
      textInput("text1","dep.head",value= NULL),
      tags$br(),
      selectInput("SynDep2",
                  "additional dep.rel",
                  choices=c("select",colnames(sample[,19:26])),
                  selected=NULL
      ),
      # textInput("text1","Text 1:",value="dep"),
      textInput("text2","additional dep.head",value=NULL),
      tags$br(),
      # tags$h4("prosodic relations"),
      selectInput("ProsRel",
                  "prosodic relations",
                  choices=c("select",colnames(sample[,27:31])),
                  selected=NULL),
      textInput("text3","pros.head",value=NULL),
      
      selectInput("ConcRel",
                  "conceptual relations",
                  choices=c("select",colnames(sample[,41:50])),
                  selected=NULL),
      textInput("text4","conc.head",value=NULL),
      
      actionButton('SaveDraft2', 'Save draft'),
      tags$h4("Semantics"),
      selectizeInput("Sense","sense",
                     
                     choices=c("",levels(factor(sample$sense))),
                     options = list(create = TRUE),
                     selected= NULL
                     
      ),
      selectizeInput("Subsense","subsense",
                     
                     choices=c("",levels(factor(sample$subsense))),
                     options = list(create = TRUE),
                     selected= NULL
                     
      ),
      selectizeInput("Dom","domain",
                     
                     choices=c("",levels(factor(LexicalData$domain))),
                     options = list(create = TRUE),
                     selected= NULL
                     
      ),
      
      selectizeInput("SemField","sem.field",
                     choices=c("",levels(factor(LexicalData$sem.field))),
                     options = list(create = TRUE),
                     selected= NULL
                     
      ),
      selectizeInput("SemCat","sem.cat",
                     choices=c("",levels(factor(LexicalData$sem.cat))),
                     options = list(create = TRUE),
                     selected= NULL
                     
      ),
      
      
      selectInput("SemPros",
                  "select sem.pros",
                  choices=c("","neu","neu.neg","neg","pos"),
                  selected = NULL),
      
      selectInput("Uncertainty",
                  "uncertainty",
                  choices=c("","vague","disputed","philological","other"),
                  selected= NULL),
      
      textInput("Notes",
                "add note",
                value=NULL),
      tags$br(),
      actionButton('Save', 'Save'),
      tags$br(),
      tags$head(
        tags$style(HTML('#Edit{background-color:red}')),
        tags$style(HTML('#Save{background-color:lightgreen}'))
      ),
      
      tags$br(),
      
      downloadButton('downloadData', 'Download csv'),
      tags$br(),
      tags$hr(),
      tags$div(HTML("<font color='red' size='+2'><b>EDIT</b></font>")),
      tags$div(HTML("<font color='red'>SAVE before editing!</font>")),
      tags$hr(),
      textInput("find","find",value=""),
      
      textInput("replace","replace",value=""),
      
      selectInput("where","in which column?",
                  
                  choices = c("",colnames(sample)),
                  selected=""),
      selectizeInput("whereRow","in which citation?",
                     
                     choices = c("",paste0(sample$title,sample$sID),"all"),
                     selected=""),
      
      
      actionButton('Edit', 'EDIT')
      #actionButton('update', 'update')
   
      
      
      
    ),
    mainPanel(
      htmlOutput("Intro"),
      
      fluidRow(
        
        column(width=5, plotlyOutput("progress",height="300")),
        column(width=2, plotlyOutput("progress0", height="300")),
        column(width=5, plotlyOutput("SemanticSummary",height="300"))
      ),
      collapsibleTreeOutput("SemanticTree"),
      tags$hr(),
      # htmlOutput("prova"),
      tags$hr(),
      htmlOutput("MetadataIntro"),
      tags$hr(),
      tableOutput("Metadata"),
      #  tags$hr(),
      
      htmlOutput("segmented"),
      htmlOutput("Citation0"),
      htmlOutput("translation"),
      tags$hr(),
      
      htmlOutput("Citation"),
      tags$br(),
      tableOutput("annotatedDepRelTable"),
      tags$br(),
      tableOutput("annotatedDepRelTable2"),
      tags$br(),
      tableOutput("annotatedProsRelTable"),
      tags$br(),
      tableOutput("annotatedConcRelTable"),
      tags$br(),
      
      tags$hr(),
      htmlOutput("wordcloudIntro"),
      plotOutput("cotextWordcloud"),
      tags$hr(),
      htmlOutput("SimilarityIntro"),
      tags$hr(),
      tags$hr(),
      DT::dataTableOutput("SimilarCit"),
      tags$hr(),
      tags$hr(),
      htmlOutput("CitationSetIntro"),
      tags$hr(),
      DT::dataTableOutput("CitationSet"),
      tags$hr(),
      tags$br(),
      tags$hr(),
      htmlOutput("workSummaryIntro"),
      DT::dataTableOutput("workSummary"),
      tags$hr(),
      tags$br(),
      tags$br()
    )
  )
)

server <- function(input, output, session) {
  sample <- read.csv("./data/ConcordancesReady.csv", stringsAsFactors = F)
  sample[is.na(sample)] <- ""
  # SAMPLE <- reactive(
  #   read.csv("./data/ConcordancesReady.csv", stringsAsFactors = F))
  
   SAMPLE <- reactive(sample)  ### Experiment
  # 
  # 
  observeEvent(input$update, {
    SAMPLE <- read.csv("./data/ConcordancesReady.csv", stringsAsFactors = F)
  })
  
  # updateData  <- function() {
  #   # sample <- read.csv("./data/ConcordancesReady.csv", stringsAsFactors = F, envir = .GlobalEnv)
  #   # sample[is.na(sample)] <- ""
  #   SAMPLE <- get(sample, .GlobalEnv)
  # }
  

  
  observeEvent(input$cit,{
    updateTextInput(session,'trans',
                    value = #print(sample$Translation[sample$ref==input$cit])
                      ifelse(
                        nchar(as.character(sample$transl[sample$ref==input$cit]))>1,
                        print(sample$transl[sample$ref==input$cit]),
                        print("translation not available")
                      )
    )
  })
  
  observeEvent(input$cit,{
    updateSelectInput(session,  "SynDep1",
                      selected  = #print(sample$Translation[sample$ref==input$cit])
                        ifelse(nchar(as.character(sample$takes_as_object_or_patient[sample$ref==input$cit]))>1, "takes_as_object_or_patient",
                               ifelse(nchar( as.character(sample$is_subject_or_agent_of[sample$ref==input$cit]))>1,"is_subject_or_agent_of",
                                      ifelse(nchar( as.character(sample$is_object_or_patient_of[sample$ref==input$cit]))>1,"is_object_or_patient_of",
                                             ifelse(nchar( as.character(sample$takes_as_subject_or_agent[sample$ref==input$cit]))>1,"takes_as_subject_or_agent",
                                                    ifelse(nchar( as.character(sample$takes_as_oblique[sample$ref==input$cit]))>1,"takes_as_oblique",
                                                           ifelse(nchar( as.character(sample$is_oblique_of[sample$ref==input$cit]))>1,"is_oblique_of",
                                                                  ifelse(nchar( as.character(sample$is_modified_by[sample$ref==input$cit]))>1,"is_modified_by",
                                                                         ifelse(nchar( as.character(sample$modifies[sample$ref==input$cit]))>1,"modifies", "select")
                                                                         
                                                                  )
                                                           )))))))
    
    
  })
  
  observeEvent(input$cit,{
    updateTextInput(session,'text1',
                    value =
                      ifelse(nchar(as.character(sample$takes_as_object_or_patient[sample$ref==input$cit]))>1, str_extract(sample$takes_as_object_or_patient[sample$ref==input$cit],"\\d+"),
                             ifelse(nchar(as.character(sample$is_subject_or_agent_of[sample$ref==input$cit]))>1, str_extract(sample$is_subject_or_agent_of[sample$ref==input$cit],"\\d+"),
                                    ifelse(nchar(as.character(sample$is_object_or_patient_of[sample$ref==input$cit]))>1, str_extract(sample$is_object_or_patient_of[sample$ref==input$cit],"\\d+"),
                                           ifelse(nchar(as.character(sample$takes_as_subject_or_agent[sample$ref==input$cit]))>1, str_extract(sample$takes_as_subject_or_agent[sample$ref==input$cit],"\\d+"),
                                                  ifelse(nchar(as.character(sample$takes_as_oblique[sample$ref==input$cit]))>1, str_extract(sample$takes_as_oblique[sample$ref==input$cit],"\\d+"),
                                                         ifelse(nchar(as.character(sample$is_oblique_of[sample$ref==input$cit]))>1, str_extract(sample$is_oblique_of[sample$ref==input$cit],"\\d+"),
                                                                ifelse(nchar(as.character(sample$is_modified_by[sample$ref==input$cit]))>1, str_extract(sample$is_modified_by[sample$ref==input$cit],"\\d+"),
                                                                       ifelse(nchar(as.character(sample$modifies[sample$ref==input$cit]))>1, str_extract(sample$modifies[sample$ref==input$cit],"\\d+"),
                                                                              print("") )
                                                                )))))))
                    
                    
                    
    )
  })
  
  observeEvent(input$Edit,{
    sample <- read.csv("./data/ConcordancesReady.csv", stringsAsFactors = F)
    sample[is.na(sample)] <- ""
    write.csv(sample,paste0("../PreEditVersions/",Sys.time(),as.character(unique(sample$lemma)[1]),"_pre_",as.character(input$replace),".csv"), row.names=F)
    
    if(input$whereRow!="all"){
      ROWsID <- gsub("^.*?(\\d+)$","\\1", input$whereRow)
      ROWtitle <- gsub("^(.*?)\\d+$","\\1", input$whereRow)
      # EditLog <- paste0("replaced: ",as.character(input$find),"\nwith: ",as.character(input$replace), "\ncolumn: ",as.character(input$where), " row: ", as.character(ROWsID), "\non: ", Sys.time())
      sample[sample$sID==ROWsID & sample$title==ROWtitle,colnames(sample)==input$where] <- gsub(input$find, input$replace,sample[sample$sID==ROWsID & sample$title==ROWtitle,colnames(sample)==input$where])
      
      ExistingLog <- read.csv("./www/MeaningMapperEditLog.csv",stringsAsFactors = F)
      newLog <- c(input$where,input$whereRow,input$find,input$replace,as.character(Sys.time()))
      EditLog <- rbind(ExistingLog,newLog)
      write.csv(EditLog, file="./www/MeaningMapperEditLog.csv", row.names = F)
      
      if(input$where==colnames(sample)[11]){
        sample[sample$sID==ROWsID & sample$title==ROWtitle,] <- AddWordID(sample[sample$sID==ROWsID & sample$title==ROWtitle,])
        
      }
      write.csv(sample,"./data/ConcordancesReady.csv", row.names=F)
      sample <- read.csv("./data/ConcordancesReady.csv", stringsAsFactors = F)
      sample[is.na(sample)] <- ""
    }else{
      sample[,colnames(sample)==input$where] <- gsub(input$find, input$replace,sample[,colnames(sample)==input$where])
      ExistingLog <- read.csv("./www/MeaningMapperEditLog.csv",stringsAsFactors = F)
      
      newLog <- c(input$where,"all rows",input$find,input$replace,as.character(Sys.time()))
      EditLog <- rbind(ExistingLog,newLog)
      write.csv(EditLog, file="./www/MeaningMapperEditLog.csv", row.names = F)
      if(input$where==colnames(sample)[11]){
        
        sample[grep(input$find, input$where),] <- AddWordID(sample[grep(input$find, input$where),])
      }
      write.csv(sample,"./data/ConcordancesReady.csv", row.names=F)
      sample <- read.csv("./data/ConcordancesReady.csv", stringsAsFactors = F)
      sample[is.na(sample)] <- ""
    }
    #updateData()  #experiment
    SAMPLE <- sample
  })
  
  
  observeEvent(input$cit,{
    input$Edit
    sample <- read.csv("./data/ConcordancesReady.csv", stringsAsFactors = F)
    sample[is.na(sample)] <- ""
    updateTextInput(session,'Grammar',
               
                    value = paste(as.character(sample$lemma1[sample$ref==input$cit]),as.character(sample$case_or_voice[sample$ref==input$cit]),as.character(sample$number[sample$ref==input$cit])  ,sep=",")
                  #  value = paste(as.character(SAMPLE$DF$lemma1[SAMPLE$DF$ref==input$cit]),as.character(SAMPLE$DF$case_or_voice[SAMPLE$DF$ref==input$cit]),as.character(SAMPLE$DF$number[SAMPLE$DF$ref==input$cit])  ,sep=",")
                   # value = paste(as.character(SAMPLE()$lemma1[SAMPLE()$ref==input$cit]),as.character(SAMPLE()$case_or_voice[SAMPLE()$ref==input$cit]),as.character(SAMPLE()$number[SAMPLE()$ref==input$cit])  ,sep=",")
                    
                        )
  })
  
  observeEvent(input$cit,{
    #sample <- SAMPLE()
    updateSelectInput(session,  "SynDep2",
                      selected  = #print(sample$Translation[sample$ref==input$cit])
                        ifelse(nchar(as.character(sample$modifies[sample$ref==input$cit]))>1, "modifies",
                               ifelse(nchar( as.character(sample$is_modified_by[sample$ref==input$cit]))>1,"is_modified_by",
                                      ifelse(nchar( as.character(sample$is_object_or_patient_of[sample$ref==input$cit]))>1,"is_object_or_patient_of",
                                             ifelse(nchar( as.character(sample$takes_as_oblique[sample$ref==input$cit]))>1,"takes_as_oblique",
                                                    ifelse(nchar( as.character(sample$is_oblique_of[sample$ref==input$cit]))>1,"is_oblique_of",
                                                           ifelse(nchar( as.character(sample$takes_as_subject_or_agent[sample$ref==input$cit]))>1,"takes_as_subject_or_agent",
                                                                  ifelse(nchar( as.character(sample$is_object_or_patient_of[sample$ref==input$cit]))>1,"is_object_or_patient_of",
                                                                         ifelse(nchar( as.character(sample$is_subject_or_agent_of[sample$ref==input$cit]))>1,"is_subject_or_agent_of",
                                                                                ifelse(nchar( as.character(sample$takes_as_object_or_patient[sample$ref==input$cit]))>1,"takes_as_object_or_patient", "select")
                                                                         )))))))))
    
    
    
  })
  
  observeEvent(input$cit,{
    updateTextInput(session,'text2',
                    value =
                      ifelse(nchar(as.character(sample$modifies[sample$ref==input$cit]))>1, str_extract(sample$modifies[sample$ref==input$cit],"\\d+"),
                             ifelse(nchar(as.character(sample$is_modified_by[sample$ref==input$cit]))>1, str_extract(sample$is_modified_by[sample$ref==input$cit],"\\d+"),
                                    ifelse(nchar(as.character(sample$is_object_or_patient_of[sample$ref==input$cit]))>1, str_extract(sample$is_object_or_patient_of[sample$ref==input$cit],"\\d+"),
                                           ifelse(nchar(as.character(sample$takes_as_oblique[sample$ref==input$cit]))>1, str_extract(sample$takes_as_oblique[sample$ref==input$cit],"\\d+"),
                                                  ifelse(nchar(as.character(sample$is_oblique_of[sample$ref==input$cit]))>1, str_extract(sample$is_oblique_of[sample$ref==input$cit],"\\d+"),
                                                         ifelse(nchar(as.character(sample$takes_as_subject_or_agent[sample$ref==input$cit]))>1, str_extract(sample$takes_as_subject_or_agent[sample$ref==input$cit],"\\d+"),
                                                                ifelse(nchar(as.character(sample$is_object_or_patient_of[sample$ref==input$cit]))>1, str_extract(sample$is_object_or_patient_of[sample$ref==input$cit],"\\d+"),
                                                                       ifelse(nchar(as.character(sample$is_subject_or_agent_of[sample$ref==input$cit]))>1, str_extract(sample$is_subject_or_agent_of[sample$ref==input$cit],"\\d+"),
                                                                              ifelse(nchar(as.character(sample$takes_as_object_or_patient[sample$ref==input$cit]))>1, str_extract(sample$takes_as_object_or_patient[sample$ref==input$cit],"\\d+"),
                                                                                     
                                                                                     print("") )))
                                                         ))))))
                    
                    
                    
    )
  })
  
  
  observeEvent(input$cit,{
    updateSelectInput(session,  "SemRel",
                      selected  = #print(sample$Translation[sample$ref==input$cit])
                        ifelse(nchar(as.character(sample$is_listed_with[sample$ref==input$cit]))>1, "is_listed_with",
                               ifelse(nchar( as.character(sample$is_contrasted_with[sample$ref==input$cit]))>1,"is_contrasted_with",
                                      ifelse(nchar( as.character(sample$is_glossed_by[sample$ref==input$cit]))>1,"is_glossed_by",
                                             ifelse(nchar( as.character(sample$glosses[sample$ref==input$cit]))>1,"glosses",
                                                    ifelse(nchar( as.character(sample$is_juxtaposed_to[sample$ref==input$cit]))>1,"is_juxtaposed_to",
                                                           "select")
                                             ) ))))
  })
  
  observeEvent(input$cit,{
    updateTextInput(session,'text3',
                    value =
                      ifelse(nchar(as.character(sample$is_listed_with[sample$ref==input$cit]))>1, str_extract(sample$is_listed_with[sample$ref==input$cit],"\\d+"),
                             ifelse(nchar(as.character(sample$is_contrasted_with[sample$ref==input$cit]))>1, str_extract(sample$is_contrasted_with[sample$ref==input$cit],"\\d+"),
                                    ifelse(nchar(as.character(sample$is_glossed_by[sample$ref==input$cit]))>1, str_extract(sample$is_glossed_by[sample$ref==input$cit],"\\d+"),
                                           ifelse(nchar(as.character(sample$glosses[sample$ref==input$cit]))>1, str_extract(sample$glosses[sample$ref==input$cit],"\\d+"),
                                                  ifelse(nchar(as.character(sample$is_juxtaposed_to[sample$ref==input$cit]))>1, str_extract(sample$is_juxtaposed_to[sample$ref==input$cit],"\\d+"),
                                                         
                                                         print("") )   )))))
  })
  
  observeEvent(input$cit,{
    updateSelectInput(session,  "ConcRel",
                      selected  = #print(sample$Translation[sample$ref==input$cit])
                        ifelse(nchar(as.character(sample$leadingTo[sample$ref==input$cit]))>1, "leadingTo",
                               ifelse(nchar( as.character(sample$causedBy[sample$ref==input$cit]))>1,"causedBy",
                                      ifelse(nchar( as.character(sample$possessing[sample$ref==input$cit]))>1,"possessing",
                                             ifelse(nchar( as.character(sample$belongingTo[sample$ref==input$cit]))>1,"belongingTo",
                                                    ifelse(nchar( as.character(sample$locusOf[sample$ref==input$cit]))>1,"locusOf",
                                                           ifelse(nchar( as.character(sample$locatedIn[sample$ref==input$cit]))>1,"locatedIn",
                                                                  ifelse(nchar( as.character(sample$byMeansOf[sample$ref==input$cit]))>1,"byMeansOf",
                                                                         ifelse(nchar( as.character(sample$achievedThrough[sample$ref==input$cit]))>1,"achievedThrough",
                                                                                ifelse(nchar( as.character(sample$goalOf[sample$ref==input$cit]))>1,"goalOf",
                                                                                       ifelse(nchar( as.character(sample$takesGoal[sample$ref==input$cit]))>1,"takesGoal",
                                                                                              
                                                                                              "select")
                                                                                ) )))) )))))
  })
  
  observeEvent(input$cit,{
    updateTextInput(session,'text4',
                    value =
                      ifelse(nchar(as.character(sample$leadingTo[sample$ref==input$cit]))>1, str_extract(sample$leadingTo[sample$ref==input$cit],"\\d+"),
                             ifelse(nchar(as.character(sample$causedBy[sample$ref==input$cit]))>1, str_extract(sample$causedBy[sample$ref==input$cit],"\\d+"),
                                    ifelse(nchar(as.character(sample$possessing[sample$ref==input$cit]))>1, str_extract(sample$possessing[sample$ref==input$cit],"\\d+"),
                                           ifelse(nchar(as.character(sample$belongingTo[sample$ref==input$cit]))>1, str_extract(sample$belongingTo[sample$ref==input$cit],"\\d+"),
                                                  ifelse(nchar(as.character(sample$locusOf[sample$ref==input$cit]))>1, str_extract(sample$locusOf[sample$ref==input$cit],"\\d+"),
                                                         ifelse(nchar(as.character(sample$locatedIn[sample$ref==input$cit]))>1, str_extract(sample$locatedIn[sample$ref==input$cit],"\\d+"),
                                                                ifelse(nchar(as.character(sample$byMeansOf[sample$ref==input$cit]))>1, str_extract(sample$byMeansOf[sample$ref==input$cit],"\\d+"),
                                                                       ifelse(nchar(as.character(sample$achievedThrough[sample$ref==input$cit]))>1, str_extract(sample$achievedThrough[sample$ref==input$cit],"\\d+"),
                                                                              ifelse(nchar(as.character(sample$goalOf[sample$ref==input$cit]))>1, str_extract(sample$goalOf[sample$ref==input$cit],"\\d+"),
                                                                                     ifelse(nchar(as.character(sample$takesGoal[sample$ref==input$cit]))>1, str_extract(sample$takesGoal[sample$ref==input$cit],"\\d+"),
                                                                                            
                                                                                            print("") )   ))))))))))
  })
  
  observeEvent(input$cit,{
    updateTextInput(session,  "Sense",
                    value=
                      ifelse(nchar(as.character(sample$sense[sample$ref==input$cit]))>1, as.character(sample$sense[sample$ref==input$cit]), "")
    )
  })
  
  observeEvent(input$cit,{
    updateTextInput(session,  "Subsense",
                    value=
                      ifelse(nchar(as.character(sample$subsense[sample$ref==input$cit]))>1, as.character(sample$subsense[sample$ref==input$cit]), "")
    )
  })
  
  observeEvent(input$cit,{
    updateTextInput(session,  "Dom",
                    value=
                      ifelse(nchar(as.character(sample$domain[sample$ref==input$cit]))>1, as.character(sample$domain[sample$ref==input$cit]), "")
    )
  })
  
  observeEvent(input$cit,{
    updateTextInput(session,  "SemField",
                    value=
                      ifelse(nchar(as.character(sample$sem.field[sample$ref==input$cit]))>1, as.character(sample$sem.field[sample$ref==input$cit]), "")
    )
  })
  
  observeEvent(input$cit,{
    updateTextInput(session,  "SemCat",
                    value=
                      ifelse(nchar(as.character(sample$sem.cat[sample$ref==input$cit]))>1, as.character(sample$sem.cat[sample$ref==input$cit]), "")
    )
  })
  
  observeEvent(input$cit,{
    updateSelectInput(session,  "Uncertainty",
                      selected=
                        ifelse(nchar(as.character(sample$uncertainty[sample$ref==input$cit]))>1, as.character(sample$uncertainty[sample$ref==input$cit]), "")
    )
  })
  
  observeEvent(input$cit,{
    updateSelectInput(session,  "SemPros",
                      selected=
                        ifelse(nchar(as.character(sample$sem.pros[sample$ref==input$cit]))>1, as.character(sample$sem.pros[sample$ref==input$cit]), "")
    )
  })
  
  observeEvent(input$cit,{
    updateTextInput(session,  "Notes",
                    value=
                      ifelse(nchar(as.character(sample$Notes[sample$ref==input$cit]))>1, as.character(sample$Notes[sample$ref==input$cit]), "")
    )
  })
  
  observeEvent(input$Dom,{
    updateSelectInput(session,'SemField',
                      choices=sort(unique(LexicalData$sem.field[LexicalData$domain==input$Dom])))
  })
  
  
  observeEvent(input$SemField,{
    updateSelectInput(session,'SemCat',
                      choices=sort(unique(LexicalData$sem.cat[LexicalData$sem.field==input$SemField])))
  })
  
  
  
  
  
  observeEvent(input$SaveDraft,{
    sample <- read.csv("./data/ConcordancesReady.csv", stringsAsFactors = F)
    sample[is.na(sample)] <- ""
    
    sample[sample$ref==input$cit,colnames(sample)=="sem.pros"] <- input$SemPros
    sample[sample$ref==input$cit,colnames(sample)=="sense"] <- input$Sense
    sample[sample$ref==input$cit,colnames(sample)=="subsense"] <- input$Subsense
    sample[sample$ref==input$cit,colnames(sample)=="domain"] <- input$Dom
    sample[sample$ref==input$cit,colnames(sample)=="sem.field"] <- input$SemField
    sample[sample$ref==input$cit,colnames(sample)=="sem.cat"] <- input$SemCat
    sample[sample$ref==input$cit,colnames(sample)=="transl"] <- input$trans
    sample[sample$ref==input$cit,colnames(sample)=="uncertainty"] <- input$Uncertainty
    sample[sample$ref==input$cit,colnames(sample)=="Notes"] <- input$Notes
    sample[sample$ref==input$cit,colnames(sample)=="lemma1"] <- gsub("^(.*?),(.*?),(.*?)$","\\1",input$Grammar)
    sample[sample$ref==input$cit,colnames(sample)=="case_or_voice"] <- gsub("^(.*?),(.*?),(.*?)$","\\2",input$Grammar)
    sample[sample$ref==input$cit,colnames(sample)=="number"] <- gsub("^(.*?),(.*?),(.*?)$","\\3",input$Grammar)
    
    if (input$SynDep1 != "select"){
      citation <- sample[sample$ref==input$cit,12]
      sample[sample$ref==input$cit,colnames(sample)==input$SynDep1] <- str_extract(citation, paste0(as.character(input$text1), "\\s([a-z]|[āīūṛḷṇḍñṅḥśṣṭḍṃ])+\\s"))
    }
    if (input$SynDep2 != "select"){
      citation <- sample[sample$ref==input$cit,12]
      sample[sample$ref==input$cit,colnames(sample)==input$SynDep2] <-   str_extract(citation, paste0(as.character(input$text2), "\\s([a-z]|[āīūṛḷṇḍñṅḥśṣṭḍṃ])+\\s"))
    }
    if (input$ConcRel != "select"){
      citation <- sample[sample$ref==input$cit,12]
      sample[sample$ref==input$cit,colnames(sample)==input$ConcRel] <-   str_extract(citation, paste0(as.character(input$text4), "\\s([a-z]|[āīūṛḷṇḍñṅḥśṣṭḍṃ])+\\s"))
    }
    if (input$ProsRel != "select"){
      citation <- sample[sample$ref==input$cit,12]
      sample[sample$ref==input$cit,colnames(sample)==input$ProsRel] <- str_extract(citation, paste0(as.character(input$text3), "\\s([a-z]|[āīūṛḷṇḍñṅḥśṣṭḍṃ])+\\s"))
    }
    
    write.csv(sample,"./data/ConcordancesReady.csv", row.names=F)
    # sample <- read.csv("./data/ConcordancesReady.csv", stringsAsFactors = F)
    # sample[is.na(sample)] <- ""
  })
  
  observeEvent(input$SaveDraft2,{
    sample <- read.csv("./data/ConcordancesReady.csv", stringsAsFactors = F)
    sample[is.na(sample)] <- ""
    
    sample[sample$ref==input$cit,colnames(sample)=="sem.pros"] <- input$SemPros
    sample[sample$ref==input$cit,colnames(sample)=="sense"] <- input$Sense
    sample[sample$ref==input$cit,colnames(sample)=="subsense"] <- input$Subsense
    sample[sample$ref==input$cit,colnames(sample)=="domain"] <- input$Dom
    sample[sample$ref==input$cit,colnames(sample)=="sem.field"] <- input$SemField
    sample[sample$ref==input$cit,colnames(sample)=="sem.cat"] <- input$SemCat
    sample[sample$ref==input$cit,colnames(sample)=="transl"] <- input$trans
    sample[sample$ref==input$cit,colnames(sample)=="uncertainty"] <- input$Uncertainty
    sample[sample$ref==input$cit,colnames(sample)=="Notes"] <- input$Notes
    sample[sample$ref==input$cit,colnames(sample)=="lemma1"] <- gsub("^(.*?),(.*?),(.*?)$","\\1",input$Grammar)
    sample[sample$ref==input$cit,colnames(sample)=="case_or_voice"] <- gsub("^(.*?),(.*?),(.*?)$","\\2",input$Grammar)
    sample[sample$ref==input$cit,colnames(sample)=="number"] <- gsub("^(.*?),(.*?),(.*?)$","\\3",input$Grammar)
    
    if (input$SynDep1 != "select"){
      citation <- sample[sample$ref==input$cit,12]
      sample[sample$ref==input$cit,colnames(sample)==input$SynDep1] <- str_extract(citation, paste0(as.character(input$text1), "\\s([a-z]|[āīūṛḷṇḍñṅḥśṣṭḍṃ])+\\s"))
    }
    if (input$SynDep2 != "select"){
      citation <- sample[sample$ref==input$cit,12]
      sample[sample$ref==input$cit,colnames(sample)==input$SynDep2] <-   str_extract(citation, paste0(as.character(input$text2), "\\s([a-z]|[āīūṛḷṇḍñṅḥśṣṭḍṃ])+\\s"))
    }
    if (input$ConcRel != "select"){
      citation <- sample[sample$ref==input$cit,12]
      sample[sample$ref==input$cit,colnames(sample)==input$ConcRel] <-   str_extract(citation, paste0(as.character(input$text4), "\\s([a-z]|[āīūṛḷṇḍñṅḥśṣṭḍṃ])+\\s"))
    }
    if (input$ProsRel != "select"){
      citation <- sample[sample$ref==input$cit,12]
      sample[sample$ref==input$cit,colnames(sample)==input$ProsRel] <- str_extract(citation, paste0(as.character(input$text3), "\\s([a-z]|[āīūṛḷṇḍñṅḥśṣṭḍṃ])+\\s"))
    }
    
    write.csv(sample,"./data/ConcordancesReady.csv", row.names=F)
    # sample <- read.csv("./data/ConcordancesReady.csv", stringsAsFactors = F)
    # sample[is.na(sample)] <- ""
  })
  
  observeEvent(input$Save,{
    sample <- read.csv("./data/ConcordancesReady.csv", stringsAsFactors = F)
    sample[is.na(sample)] <- ""
    
    sample[sample$ref==input$cit,colnames(sample)=="sem.pros"] <- input$SemPros
    sample[sample$ref==input$cit,colnames(sample)=="sense"] <- input$Sense
    sample[sample$ref==input$cit,colnames(sample)=="subsense"] <- input$Subsense
    sample[sample$ref==input$cit,colnames(sample)=="domain"] <- input$Dom
    sample[sample$ref==input$cit,colnames(sample)=="sem.field"] <- input$SemField
    sample[sample$ref==input$cit,colnames(sample)=="sem.cat"] <- input$SemCat
    sample[sample$ref==input$cit,colnames(sample)=="transl"] <- input$trans
    sample[sample$ref==input$cit,colnames(sample)=="uncertainty"] <- input$Uncertainty
    sample[sample$ref==input$cit,colnames(sample)=="Notes"] <- input$Notes
    sample[sample$ref==input$cit,colnames(sample)=="lemma1"] <- gsub("^(.*?),(.*?),(.*?)$","\\1",input$Grammar)
    sample[sample$ref==input$cit,colnames(sample)=="case_or_voice"] <- gsub("^(.*?),(.*?),(.*?)$","\\2",input$Grammar)
    sample[sample$ref==input$cit,colnames(sample)=="number"] <- gsub("^(.*?),(.*?),(.*?)$","\\3",input$Grammar)
    
    if (input$SynDep1 != "select"){
      citation <- sample[sample$ref==input$cit,12]
      sample[sample$ref==input$cit,colnames(sample)==input$SynDep1] <- str_extract(citation, paste0(as.character(input$text1), "\\s([a-z]|[āīūṛḷṇḍñṅḥśṣṭḍṃ])+\\s"))
    }
    if (input$SynDep2 != "select"){
      citation <- sample[sample$ref==input$cit,12]
      sample[sample$ref==input$cit,colnames(sample)==input$SynDep2] <-   str_extract(citation, paste0(as.character(input$text2), "\\s([a-z]|[āīūṛḷṇḍñṅḥśṣṭḍṃ])+\\s"))
    }
    if (input$ConcRel != "select"){
      citation <- sample[sample$ref==input$cit,12]
      sample[sample$ref==input$cit,colnames(sample)==input$ConcRel] <-   str_extract(citation, paste0(as.character(input$text4), "\\s([a-z]|[āīūṛḷṇḍñṅḥśṣṭḍṃ])+\\s"))
    }
    if (input$ProsRel != "select"){
      citation <- sample[sample$ref==input$cit,12]
      sample[sample$ref==input$cit,colnames(sample)==input$ProsRel] <- str_extract(citation, paste0(as.character(input$text3), "\\s([a-z]|[āīūṛḷṇḍñṅḥśṣṭḍṃ])+\\s"))
    }
    
    write.csv(sample,"./data/ConcordancesReady.csv", row.names=F)
     sample <- read.csv("./data/ConcordancesReady.csv", stringsAsFactors = F)
     sample[is.na(sample)] <- ""
     invalidateLater(1000)
  })
  
  
  

  
 #  
 # PROGRESS <- reactive({
 #    input$Save
 #    input$Edit
 #    sample <- read.csv("./data/ConcordancesReady.csv", stringsAsFactors = F)
 #    sample[is.na(sample)] <- ""
 #    ProgressDF <- sample[,c(1,7,6,32)]
 #    ProgressDF$progress <- 1
 #    ProgressDF$progress[ProgressDF$sem.pros==""] <- 0
 #    ProgressDF
 #    # colnames(sample)
 #    # ggplot(ProgressDF, aes(title,fill=progress))+geom_bar(position="fill")+coord_flip()+
 #    #   scale_fill_manual(values = c("done" = "green", "toDo" = "orange"))+theme_void()
 #    
 #    # Progress20Perc <-ProgressDF %>%
 #    #   group_by(title)%>%
 #    #   count(progress) %>%
 #    #   summarise(n()/nrow(.))
 #    #   mutate(highlight = ifelse(sum(progress>0) & min(prop.table(table(progress)))> 0.2,T,F))
 #  })
  
  
  
  output$Intro<- renderText({
    input$Save
    input$Edit

    sample <- read.csv("./data/ConcordancesReady.csv", stringsAsFactors = F)
    sample[is.na(sample)] <- ""
    ProgressDF <- sample[,c(1,7,6,32)]
    ProgressDF$progress <- "done"
    ProgressDF$progress[ProgressDF$sem.pros==""] <- "toDo"
   # ProgressDF <- PROGRESS()
    paste0("<font size='+2'><font color='#cd5c5c'> ",unique(sample[,9]),"</font><font color='#708090'> ",nrow(ProgressDF), " citations ",round(nrow(ProgressDF[ProgressDF$progress=="done",])/nrow(ProgressDF)*100),"% complete</font>" )
  })
  
  output$progress <- renderPlotly({
   #Progress20Perc <- PROGRESS()
   input$Save
   input$Edit
   sample <- read.csv("./data/ConcordancesReady.csv", stringsAsFactors = F)
   sample[is.na(sample)] <- ""
   ProgressDF <- sample[,c(1,7,6,32)]
   ProgressDF$progress <- 1
   ProgressDF$progress[ProgressDF$sem.pros==""] <- 0

    Progress20Perc <-ProgressDF %>%
      group_by(title)%>%
      mutate(highlight = ifelse(sum(progress>0) & min(prop.table(table(progress)))> 0.2,T,F))
    plot <- ggplot(Progress20Perc, aes(title,fill=factor(progress), alpha=factor(highlight)))+geom_bar(position="fill")+coord_flip()+
      scale_alpha_discrete(range = c(0.8,0.1))+scale_fill_manual(values = c("1" = "forestgreen", "0" = "steelblue")) +
      theme_void()+theme(legend.position = "none")  #theme_minimal()+theme(legend.position = "none",panel.grid = element_blank(), text=element_text(size=14),axis.title.x = element_blank(), axis.ticks.x = element_blank(),axis.title.y = element_blank(),axis.text.x = element_blank(), axis.ticks.y = element_blank())
    ggplotly(plot, fill="title")
    
  })
  
  output$progress0 <- renderPlotly({
    input$Save
    input$Edit
    sample <- read.csv("./data/ConcordancesReady.csv", stringsAsFactors = F)
    sample[is.na(sample)] <- ""
    ProgressDF <- sample[,c(1,7,6,32)]
    ProgressDF$progress <- "done"
    ProgressDF$progress[ProgressDF$sem.pros==""] <- "toDo"
    #ProgressDF <- PROGRESS()
    plot <- ggplot(ProgressDF, aes(title))+geom_bar(alpha=0.2)+coord_flip()+theme_void()
    ggplotly(plot, fill="title")
  })
  
  output$SemanticSummary <- renderPlotly({
    input$Save
    input$SaveDraft
    input$Edit
    sample <- read.csv("./data/ConcordancesReady.csv", stringsAsFactors = F)
    sample[is.na(sample)] <-""
    plot <- ggplot(sample, aes_string(x = input$DataVis, fill= input$fill))+
      geom_bar(position = input$barchart_type)+coord_flip()+
      theme(panel.grid = element_blank(), text=element_text(size=10),axis.title.x = element_blank(), axis.ticks.x = element_blank(),axis.title.y = element_blank(),axis.text.x = element_blank(), axis.ticks.y = element_blank())
    
    ggplotly(plot)
  })
  
  output$SemanticTree <-   renderCollapsibleTree({
    input$Save
    input$SaveDraft
    input$Edit
    sample <- read.csv("./data/ConcordancesReady.csv", stringsAsFactors = F)
    sample[is.na(sample)] <-""
    if (is.na(sum(nchar(sample$sense)))){
      sample %>%
        group_by(lemma1,genre, period) %>%
       # summarize("Number of citations" = n()) %>%
        collapsibleTree(
          hierarchy = c("lemma1","genre","period"),
          root = "lemma",
         # attribute = "Number of citations",
          #nodeSize = "Number of citations",
          fontSize= 14,
          tooltip=TRUE,
          collapsed = FALSE,
          zoomable = FALSE
        )
      
    }else{
      sample %>%
        group_by(lemma1,domain, sense, subsense) %>%
       # summarize("Number of citations" = n()) %>%
        collapsibleTree(
          hierarchy = c("lemma1","domain","sense", "subsense"),
          root = "lemma",
          # attribute = "Number of citations",
          # nodeSize = "Number of citations",
          fontSize= 14,
          tooltip=TRUE,
          collapsed = FALSE,
          zoomable = FALSE
        )
    }
  })
  
  
  
  output$MetadataIntro <- renderText({
    
    sample <- read.csv("./data/ConcordancesReady.csv", stringsAsFactors = F)
    sample[is.na(sample)] <- ""
    
    
    paste0("<font color='#4682b4' size='+2'><b> ",input$cit," </b></font>")
  })
  
  output$Metadata <- renderTable({
    input$Edit
    #  sample <- read.csv("./data/ConcordancesReady.csv", stringsAsFactors = F)
    #  sample[is.na(sample)] <-""
     sample <- SAMPLE()  # Exp
    sample[is.na(sample)] <-""
    sample[sample$ref==input$cit,colnames(sample)=="lemma1"] <- gsub("^(.*?),(.*?),(.*?)$","\\1",input$Grammar)
    sample[sample$ref==input$cit,colnames(sample)=="case_or_voice"] <- gsub("^(.*?),(.*?),(.*?)$","\\2",input$Grammar)
    sample[sample$ref==input$cit,colnames(sample)=="number"] <- gsub("^(.*?),(.*?),(.*?)$","\\3",input$Grammar)
    
    
    sample[sample$ref==input$cit,c(1,7,6,36,39,40)]
  })
  
  output$segmented <- renderText({
    input$Edit
    sample <- read.csv("./data/ConcordancesReady.csv", stringsAsFactors = F)
    sample[is.na(sample)] <- ""
    paste0("<font size='-1' color='#708090'>",sample[sample$ref==input$cit,11],"</font>")
    
  })
  
  output$Citation0 <- renderText({
    input$Edit
    sample <- read.csv("./data/ConcordancesReady.csv", stringsAsFactors = F)
    sample[is.na(sample)] <- ""
    lemma <- sample[sample$ref==input$cit,9]
    l <-tolower(lemma)
    sentence <- sample[sample$ref==input$cit,11]
    sentence <- gsub("\\s+-","",  sentence)
    sentence <- gsub(" @ ","-", sentence)
    sentence <- tolower(sentence)
    sentence <- tolower(sentence)
    sentenceColor <- as.character(sentence)
    sentenceColor <- gsub(l,paste0(" <font color='#cd5c5c'><b> ",l," </b></font> ") , sentenceColor, "<br />" )
    
    
    paste0("<font size='+1'>",sentenceColor,"</font>")
  })
  
  
  
  
  output$translation <- renderText({
    paste0("<i>", input$trans,"</i>")
  })
  
  output$Citation <- renderText({
    input$Edit
    sample <- read.csv("./data/ConcordancesReady.csv", stringsAsFactors = F)
    sample[is.na(sample)] <- ""
    
    lemma <- sample[sample$ref==input$cit,9]
    l <-tolower(lemma)
    sentence <- sample[sample$ref==input$cit,12]
    sentenceColor <- as.character(sentence)
    sentenceColor <- paste0("<font size = '+2'> ",sentenceColor,"</font>")
    sentenceColor <- gsub(l,paste0(" <font color='#cd5c5c'><b> ",l," </b></font> ") , sentenceColor )
    sentenceColor <- gsub(" (\\d+) "," <font color='#4682b4' size='-1'> \\1 </font> ", sentenceColor )
    
    paste0(sentenceColor)
  })
  
  
  
  output$annotatedDepRelTable <- renderTable({
    input$Save
    input$SaveDraft
    input$Edit
    sample <- read.csv("./data/ConcordancesReady.csv", stringsAsFactors = F)
    sample[is.na(sample)] <- ""
    citation <- sample[sample$ref==input$cit,12]
    #sample[sample$ref==input$cit,colnames(sample)=="sem.pros"] <- input$SemPros
    sample[sample$ref==input$cit,colnames(sample)==input$SynDep1] <- str_extract(citation, paste0(input$text1, "\\s([a-z]|[āīūṛḷṇḍñṅḥśṣṭḍṃ])+\\s"))
    sample[sample$ref==input$cit,colnames(sample)==input$SynDep2] <- str_extract(citation, paste0(input$text2, "\\s([a-z]|[āīūṛḷṇḍñṅḥśṣṭḍṃ])+\\s"))
    # sample[sample$ref==input$cit,colnames(sample)=="sense"] <- input$Sense
    # sample[sample$ref==input$cit,colnames(sample)=="subsense"] <- input$Subsense
    # sample[sample$ref==input$cit,colnames(sample)=="domain"] <- input$Dom
    # sample[sample$ref==input$cit,colnames(sample)=="sem.field"] <- input$SemField
    # sample[sample$ref==input$cit,colnames(sample)=="sem.cat"] <- input$SemCat
    # sample[sample$ref==input$cit,colnames(sample)=="transl"] <- input$trans
    # sample[sample$ref==input$cit,colnames(sample)==input$ProsRel] <- str_extract(citation, paste0(input$text3, "\\s([a-z]|[āīūṛḷṇḍñṅḥśṣṭḍṃ])+\\s"))
    # sample[sample$ref==input$cit,colnames(sample)=="uncertainty"] <- input$Uncertainty
    # sample[sample$ref==input$cit,colnames(sample)=="Notes"] <- input$Notes
    sample[sample$ref==input$cit,c(19:22)]
    #sample[sample$ref==input$cit,c(19:22)]
  })
  
  output$annotatedDepRelTable2 <- renderTable({
    input$Save
    input$SaveDraft
    input$Edit
    sample <- read.csv("./data/ConcordancesReady.csv", stringsAsFactors = F)
    sample[is.na(sample)] <- ""
    citation <- sample[sample$ref==input$cit,12]
    #sample[sample$ref==input$cit,colnames(sample)=="sem.pros"] <- input$SemPros
    sample[sample$ref==input$cit,colnames(sample)==input$SynDep1] <- str_extract(citation, paste0(input$text1, "\\s([a-z]|[āīūṛḷṇḍñṅḥśṣṭḍṃ])+\\s"))
    sample[sample$ref==input$cit,colnames(sample)==input$SynDep2] <- str_extract(citation, paste0(input$text2, "\\s([a-z]|[āīūṛḷṇḍñṅḥśṣṭḍṃ])+\\s"))
    # sample[sample$ref==input$cit,colnames(sample)=="sense"] <- input$Sense
    # sample[sample$ref==input$cit,colnames(sample)=="subsense"] <- input$Subsense
    # sample[sample$ref==input$cit,colnames(sample)=="domain"] <- input$Dom
    # sample[sample$ref==input$cit,colnames(sample)=="sem.field"] <- input$SemField
    # sample[sample$ref==input$cit,colnames(sample)=="sem.cat"] <- input$SemCat
    # sample[sample$ref==input$cit,colnames(sample)=="transl"] <- input$trans
    # sample[sample$ref==input$cit,colnames(sample)==input$ProsRel] <- str_extract(citation, paste0(input$text3, "\\s([a-z]|[āīūṛḷṇḍñṅḥśṣṭḍṃ])+\\s"))
    # sample[sample$ref==input$cit,colnames(sample)=="uncertainty"] <- input$Uncertainty
    # sample[sample$ref==input$cit,colnames(sample)=="Notes"] <- input$Notes
    sample[sample$ref==input$cit,c(23:26)]
    #sample[sample$ref==input$cit,c(19:22)]
  })
  
  output$annotatedProsRelTable <- renderTable({
    input$Save
    input$SaveDraft
    input$Edit
    sample <- read.csv("./data/ConcordancesReady.csv", stringsAsFactors = F)
    sample[is.na(sample)] <- ""
    citation <- sample[sample$ref==input$cit,12]
    sample[sample$ref==input$cit,colnames(sample)=="sem.pros"] <- input$SemPros
    sample[sample$ref==input$cit,colnames(sample)=="sense"] <- input$Sense
    sample[sample$ref==input$cit,colnames(sample)=="subsense"] <- input$Subsense
    sample[sample$ref==input$cit,colnames(sample)=="domain"] <- input$Dom
    sample[sample$ref==input$cit,colnames(sample)=="sem.field"] <- input$SemField
    sample[sample$ref==input$cit,colnames(sample)=="sem.cat"] <- input$SemCat
    sample[sample$ref==input$cit,colnames(sample)=="transl"] <- input$trans
    sample[sample$ref==input$cit,colnames(sample)==input$ProsRel] <- str_extract(citation, paste0(input$text3, "\\s([a-z]|[āīūṛḷṇḍñṅḥśṣṭḍṃ])+\\s"))
    
    sample[sample$ref==input$cit,c(27:32)]
  })
  
  output$annotatedConcRelTable <- renderTable({
    input$Save
    input$SaveDraft
    input$Edit
    sample <- read.csv("./data/ConcordancesReady.csv", stringsAsFactors = F)
    sample[is.na(sample)] <- ""
    citation <- sample[sample$ref==input$cit,12]
    sample[sample$ref==input$cit,colnames(sample)==input$ConcRel] <- str_extract(citation, paste0(input$text4, "\\s([a-z]|[āīūṛḷṇḍñṅḥśṣṭḍṃ])+\\s"))
    
    sample[sample$ref==input$cit,c(41:47)]
  })
  
  output$annotatedNotesTable <- renderTable({
    input$Save
    input$SaveDraft
    input$Edit
    sample <- read.csv("./data/ConcordancesReady.csv", stringsAsFactors = F)
    sample[is.na(sample)] <- ""
    citation <- sample[sample$ref==input$cit,12]
    sample[sample$ref==input$cit,colnames(sample)=="uncertainty"] <- input$Uncertainty
    sample[sample$ref==input$cit,colnames(sample)=="Notes"] <- input$Notes
    
    sample[sample$ref==input$cit,c(48:50,33,34)]
  })
  
  output$wordcloudIntro <- renderText({
    paste0("<font size ='+4'> most commonly shared <font color='#4682b4'>cotext</font>")
  })
  
  output$CitationSetIntro <-  renderText({
    paste0("<font size ='+4'color='#4682b4'>Whole Dataset</font><br />
          <font size ='1'>diplay more citations (if available) with the 'show entries' menu below.
           search the dataset with the search box to the right below</font>")
  })
  
  output$CitationSet <- DT::renderDataTable({
    input$Save
    input$SaveDraft
    input$Edit
    sample <- read.csv("./data/ConcordancesReady.csv", stringsAsFactors = F)
    sample[is.na(sample)] <- ""
    sample
    #
    
  },options = list(pageLength = 1))
  
  
  output$SimilarityIntro <- renderText({
    CitCotext <- sample[sample$ref==input$cit,38]
    CitCotextTok <- tokenize_words(CitCotext, lowercase=F)
    CitCotextTok <- unlist(CitCotextTok)
    stopwords <- c("TATR", "TAD","TAD","CET","AYAM","AYAṂ","IDAM","IDAṂ","EDAM","EDAṂ","YATHĀ","TATHĀGAT","BODHISATTV")
    WordsToSearch <- setdiff(CitCotextTok, stopwords)
    
    PotentialSimilar <- c()
    OtherCitsDF <- sample[-(sample$ref==input$cit),]
    for (i in WordsToSearch){
      grepped <- grep(paste0(" ",i," "), sample[,38])
      if(length(grepped)>0){
        greppedCit <- sample[c(grepped),35]
        PotentialSimilar <- c(PotentialSimilar,greppedCit)
      }
    }
    if (!is.null(PotentialSimilar)){
      SharedCotextDF <- data.frame(table(PotentialSimilar))
      if (nrow(SharedCotextDF[SharedCotextDF$Freq >1,])>0){
        SharedCotextDF <- SharedCotextDF[SharedCotextDF$Freq >1,]
        SharedCotextDF <-  SharedCotextDF[order(SharedCotextDF$Freq,decreasing=T),]
        toDisplay <- OtherCitsDF[OtherCitsDF$ref %in% SharedCotextDF$PotentialSimilar, c(35,11)]
        shared <- SharedCotextDF[SharedCotextDF$PotentialSimilar %in% toDisplay$ref,]
        toDisplay <- data.frame(shared[2],toDisplay)
        colnames(toDisplay) <- c("shared","Potentially.Similar","Cit")
        toDisplay$sharedCotext <- ""
        toDisplay$wordCount <- ""
        for (i in 1:length(toDisplay$Cit)){
          testTok <- unlist(tokenize_words(toDisplay$Cit[i], lowercase=F))
          toDisplay$wordCount[[i]] <- length(testTok)
          toDisplay$sharedCotext[[i]] <-  paste(intersect(WordsToSearch, testTok), collapse=" ")
        }
        paste0("<font size ='+4'><font color='#4682b4'> cotext</font> shared with:</font>
               <br /> <font size ='-1'>you can diplay more citations (if available) with the 'show entries' menu below. You can search the dataset use the search box to the right below</font>")
      }else{
        paste0("<font size ='+2' color='#cd5c5c'>no similar citations detected</font>")
        
      }
      
    }
  })
  
  
  
  output$SimilarCit <-  DT::renderDataTable({
    CitCotext <- sample[sample$ref==input$cit,38]
    CitCotextTok <- tokenize_words(CitCotext, lowercase=F)
    CitCotextTok <- unlist(CitCotextTok)
    stopwords <- c("TATR", "TAD","TAD","CET","AYAM","AYAṂ","IDAM","IDAṂ","EDAM","EDAṂ","YATHĀ","TATHĀGAT","BODHISATTV")
    WordsToSearch <- setdiff(CitCotextTok, stopwords)
    
    PotentialSimilar <- c()
    OtherCitsDF <- sample[-(sample$ref==input$cit),]
    for (i in WordsToSearch){
      grepped <- grep(paste0(" ",i," "), sample[,38])
      if(length(grepped)>0){
        greppedCit <- sample[c(grepped),35]
        PotentialSimilar <- c(PotentialSimilar,greppedCit)
      }
    }
    if (!is.null(PotentialSimilar)){
      SharedCotextDF <- data.frame(table(PotentialSimilar))
      if (nrow(SharedCotextDF[SharedCotextDF$Freq >1,])>0){
        SharedCotextDF <- SharedCotextDF[SharedCotextDF$Freq >1,]
        SharedCotextDF <-  SharedCotextDF[order(SharedCotextDF$Freq,decreasing=T),]
        toDisplay <- OtherCitsDF[OtherCitsDF$ref %in% SharedCotextDF$PotentialSimilar, c(35,11)]
        shared <- SharedCotextDF[SharedCotextDF$PotentialSimilar %in% toDisplay$ref,]
        toDisplay <- data.frame(shared[2],toDisplay)
        colnames(toDisplay) <- c("sharedCount","SharedWith","Cit")
        toDisplay$sharedCotext <- ""
        toDisplay$wordCount <- ""
        for (i in 1:length(toDisplay$Cit)){
          testTok <- unlist(tokenize_words(toDisplay$Cit[i], lowercase=F))
          toDisplay$wordCount[[i]] <- length(testTok)
          toDisplay$sharedCotext[[i]] <-  paste(intersect(WordsToSearch, testTok), collapse=" ")
        }
        
        toDisplay$perc <-  as.numeric(toDisplay$sharedCount)/as.numeric(toDisplay$wordCount)
        
        toDisplay
        
      }
    }
  },options = list(pageLength = 1))
  
  output$cotextWordcloud <-  renderPlot({
    CitCotext <- sample[sample$ref==input$cit,38]
    CitCotextTok <- tokenize_words(CitCotext, lowercase=F)
    CitCotextTok <- unlist(CitCotextTok)
    stopwords <- c("TATR", "TAD","TAD","CET","AYAM","AYAṂ","IDAM","IDAṂ","EDAM","EDAṂ","YATHĀ","TATHĀGAT","BODHISATTV")
    WordsToSearch <- setdiff(CitCotextTok, stopwords)
    
    PotentialSimilar <- c()
    OtherCitsDF <- sample[-(sample$ref==input$cit),]
    for (i in WordsToSearch){
      grepped <- grep(paste0(" ",i," "), sample[,38])
      if(length(grepped)>0){
        greppedCit <- sample[c(grepped),35]
        PotentialSimilar <- c(PotentialSimilar,greppedCit)
      }
    }
    if (!is.null(PotentialSimilar)){
      SharedCotextDF <- data.frame(table(PotentialSimilar))
      if (nrow(SharedCotextDF[SharedCotextDF$Freq >1,])>0){
        SharedCotextDF <- SharedCotextDF[SharedCotextDF$Freq >1,]
        SharedCotextDF <-  SharedCotextDF[order(SharedCotextDF$Freq,decreasing=T),]
        toDisplay <- OtherCitsDF[OtherCitsDF$ref %in% SharedCotextDF$PotentialSimilar, c(35,11)]
        shared <- SharedCotextDF[SharedCotextDF$PotentialSimilar %in% toDisplay$ref,]
        toDisplay <- data.frame(shared[2],toDisplay)
        colnames(toDisplay) <- c("shared","Potentially.Similar","Cit")
        toDisplay$sharedCotext <- ""
        toDisplay$wordCount <- ""
        for (i in 1:length(toDisplay$Cit)){
          testTok <- unlist(tokenize_words(toDisplay$Cit[i], lowercase=F))
          toDisplay$wordCount[[i]] <- length(testTok)
          toDisplay$sharedCotext[[i]] <-  paste(intersect(WordsToSearch, testTok), collapse=" ")
        }
        WordFreqs <- DataFrameTokens_CAPS2(toDisplay$sharedCotext)
        wordcloud(WordFreqs$Var1,WordFreqs$Freq, color="steelblue")
      }else{
        WordFreqs <-DataFrameTokens_CAPS2("ZERO ZERO COTEXT COTEXT COTEXT  COMMON COMMON")
        wordcloud(WordFreqs$Var1,WordFreqs$Freq, color="indianred",rot.per=.7)
      }
      
    }
  })
  
  
  output$workSummaryIntro <- renderText({
    print("<font color='#228b22' size='+4'>Work Summary</font>")
  })
  
  output$workSummary <- DT::renderDataTable({
  #  data <- PROGRESS()
    input$Save
    input$Edit
    sample <- read.csv("./data/ConcordancesReady.csv", stringsAsFactors = F)
    sample[is.na(sample)] <- ""
    ProgressDF <- sample[,c(1,7,6,32)]
    ProgressDF$progress <- "done"
    ProgressDF$progress[ProgressDF$sem.pros==""] <- "toDo"
    ProgressDF <- ProgressDF[,c(1,5)]

    zu <- ProgressDF %>%
      gather(title, progress) %>%
      count(title, progress) %>%
      spread(progress, n, fill = 0) %>%
      mutate( "20perc"= ifelse(done/toDo >0.21, "DONE", "doMore")  )
  
   })
  

  
  output$downloadData <- downloadHandler(
    
    
    filename = function() {
      lemma <- sample[sample$ref==input$cit,9]
      paste("~/Desktop/",lemma,"_dataset", Sys.time(), ".csv", sep="")
    },
    
    
    content = function(file) {
      sampleForcsv <- read.csv("./data/ConcordancesReady.csv", stringsAsFactors = F)
      sampleForcsv$is_object_or_patient_of <- gsub("\\d+ ","", sampleForcsv$is_object_or_patient_of)
      sampleForcsv$takes_as_object_or_patient <- gsub("\\d+ ","", sampleForcsv$takes_as_object_or_patient)
      sampleForcsv$is_subject_or_agent_of <- gsub("\\d+ ","", sampleForcsv$is_subject_or_agent_of)
      sampleForcsv$takes_as_subject_or_agent <- gsub("\\d+ ","", sampleForcsv$takes_as_subject_or_agent)
      sampleForcsv$takes_as_oblique <- gsub("\\d+ ","", sampleForcsv$takes_as_oblique)
      sampleForcsv$is_oblique_of <- gsub("\\d+ ","", sampleForcsv$is_oblique_of)
      sampleForcsv$is_modified_by <- gsub("\\d+ ","", sampleForcsv$is_modified_by)
      sampleForcsv$modifies <- gsub("\\d+ ","", sampleForcsv$modifies)
      sampleForcsv$is_listed_with <- gsub("\\d+ ","", sampleForcsv$is_listed_with)
      sampleForcsv$is_contrasted_with <- gsub("\\d+ ","", sampleForcsv$is_contrasted_with)
      sampleForcsv$is_glossed_by <- gsub("\\d+ ","", sampleForcsv$is_glossed_by)
      sampleForcsv$glosses <- gsub("\\d+ ","", sampleForcsv$glosses)
      sampleForcsv$is_juxtaposed_to <- gsub("\\d+ ","", sampleForcsv$is_juxtaposed_to)
      sampleForcsv$is_object_or_patient_of <- gsub("\\s?@\\s?","", sampleForcsv$is_object_or_patient_of)
      sampleForcsv$takes_as_object_or_patient <- gsub("\\s?@\\s?","", sampleForcsv$takes_as_object_or_patient)
      sampleForcsv$is_subject_or_agent_of <- gsub("\\s?@\\s?","", sampleForcsv$is_subject_or_agent_of)
      sampleForcsv$takes_as_subject_or_agent <- gsub("\\s?@\\s?","", sampleForcsv$takes_as_subject_or_agent)
      sampleForcsv$takes_as_oblique <- gsub("\\s?@\\s?","", sampleForcsv$takes_as_oblique)
      sampleForcsv$is_oblique_of <- gsub("\\s?@\\s?","", sampleForcsv$is_oblique_of)
      sampleForcsv$is_modified_by <- gsub("\\s?@\\s?","", sampleForcsv$is_modified_by)
      sampleForcsv$modifies <- gsub("\\s?@\\s?","", sampleForcsv$modifies)
      sampleForcsv$is_listed_with <- gsub("\\s?@\\s?","", sampleForcsv$is_listed_with)
      sampleForcsv$is_contrasted_with <- gsub("\\s?@\\s?","", sampleForcsv$is_contrasted_with)
      sampleForcsv$is_glossed_by <- gsub("\\s?@\\s?","", sampleForcsv$is_glossed_by)
      sampleForcsv$glosses <- gsub("\\s?@\\s?","", sampleForcsv$glosses)
      sampleForcsv$is_juxtaposed_to <- gsub("\\s?@\\s?","", sampleForcsv$is_juxtaposed_to)
      sampleForcsv$is_object_or_patient_of <- gsub("\\s?-.*?$","", sampleForcsv$is_object_or_patient_of)
      sampleForcsv$takes_as_object_or_patient <- gsub("\\s?-.*?$","", sampleForcsv$takes_as_object_or_patient)
      sampleForcsv$is_subject_or_agent_of <- gsub("\\s?-.*?$","", sampleForcsv$is_subject_or_agent_of)
      sampleForcsv$takes_as_subject_or_agent <- gsub("\\s?-.*?$","", sampleForcsv$takes_as_subject_or_agent)
      sampleForcsv$takes_as_oblique <- gsub("\\s?-.*?$","", sampleForcsv$takes_as_oblique)
      sampleForcsv$is_oblique_of <- gsub("\\s?-.*?$","", sampleForcsv$is_oblique_of)
      sampleForcsv$is_modified_by <- gsub("\\s?-.*?$","", sampleForcsv$is_modified_by)
      sampleForcsv$modifies <- gsub("\\s?-.*?$","", sampleForcsv$modifies)
      sampleForcsv$is_listed_with <- gsub("\\s?-.*?$","", sampleForcsv$is_listed_with)
      sampleForcsv$is_contrasted_with <- gsub("\\s?-.*?$","", sampleForcsv$is_contrasted_with)
      sampleForcsv$is_glossed_by <- gsub("\\s?-.*?$","", sampleForcsv$is_glossed_by)
      sampleForcsv$glosses <- gsub("\\s?-.*?$","", sampleForcsv$glosses)
      sampleForcsv$is_juxtaposed_to <- gsub("\\s?-.*?$","", sampleForcsv$is_juxtaposed_to)
      write.csv(sampleForcsv, file)
      
    }
    
  )
  
  
}


shinyApp(ui, server)

## VERSION BELOW WORKS:

# # MEANING MAPPER APP
# 
# # !!!  @Luis and @Bruno !!! => before launching the app:
# # create a folder called "RawConc" in the PARENT DIRECTORY of the MeaningMapper folder
# # save the SketchEngine csv as Conc.txt in the data folder.
# # remove everything else from the data folder.
# 
# ## @Ligeia => TO DO :
# #  version with ConcRel.
# #  When All works publish that verison online and remove previous one
# 
# ## ADD ConcRel :
# # "leadingTo"
# # "causedBy"
# # "possessing"
# # "belongingTo"
# # "locusOf"
# # "locatedIn"
# # "byMeansOf"
# # "achievedThrough"
# # "goalOf"
# # "takesGoal"
# # think of conll export
# 
# 
# library(shiny)
# library(DT)
# library(tidyverse)
# library(tokenizers)
# library(stringr)
# library(ggplot2)
# library(plotly)
# library(data.tree)
# library(collapsibleTree)
# library(DiagrammeR)
# library(wordcloud)
# AddWordID <- function(KwicDF_lemCol_9_KwicCol_11){
#   KwicDF_lemCol_9_KwicCol_11$citWithID
#   for (i in 1:nrow(KwicDF_lemCol_9_KwicCol_11)){
# 
#     lemma <- KwicDF_lemCol_9_KwicCol_11[i,9]
#     sentence <- KwicDF_lemCol_9_KwicCol_11[i,11]
#     lemma <- as.character(lemma)
#     sentence <- as.character(sentence)
#     # sentence <- gsub(" @ "," - ",sentence)
#     sentence <- gsub("\\s+([-|@])","\\1",sentence)# uncomment this to assign a signle ID to STEM+suffix together (this will not take advantage of pseudo lemmatisation afforder by stemming.)
#     #print(sentence)
#     sentencetok <- tokenize_regex(sentence,"\\s+")
#     sentencetok <- unlist(sentencetok)
#     #print(sentencetok)
#     IDs <- seq(1:length(sentencetok))
#     sentencetok <- tolower(sentencetok)
#     sentencetok <- gsub("([-|@])"," \\1",sentencetok) #uncomment this is you uncommnted the line sentence <- gsub("\\s+-","-",sentence)
#     sentenceP <- paste(IDs,sentencetok)
#     sentenceP <- paste(sentenceP,collapse = " ")
#     #  print(sentenceP)
#     KwicDF_lemCol_9_KwicCol_11$citWithID[i]<-  sentenceP
#   }
#   return(KwicDF_lemCol_9_KwicCol_11)
# }
# 
# 
# DataFrameTokens_CAPS2 <- function(Cleaned_Segmented_Text){
#   Textname <- deparse(substitute(Cleaned_Segmented_Text))
#   Text_TokensDF <- as.data.frame(table(unlist(str_extract_all(Cleaned_Segmented_Text," ([A-Z]|[ĀĪŪṚḶṆḌÑṄḤŚṢṬḌṂ])+ "))))
#   #write.table(Text_TokensDF,file= paste(Textname,"TokensDF.csv", sep=""), quote=F,sep=",",row.names=F)
#   return(Text_TokensDF)
# }
# 
# ConcPrepR <- function(filePath){
#   library(tidyverse)
#   library(tokenizers)
#   library(stringr)
#   library(readtext)
#   AddWordID <- function(KwicDF_lemCol_10_KwicCol_12){
#     KwicDF_lemCol_10_KwicCol_12$citWithID
#     for (i in 1:nrow(KwicDF_lemCol_10_KwicCol_12)){
# 
#       lemma <- KwicDF_lemCol_10_KwicCol_12[i,10]
#       sentence <- KwicDF_lemCol_10_KwicCol_12[i,12]
#       lemma <- as.character(lemma)
#       sentence <- as.character(sentence)
#       sentence <- gsub(" @ "," - ",sentence)
#       sentence <- gsub("\\s+-","-",sentence)
#       #print(sentence)
#       sentencetok <- tokenize_regex(sentence,"\\s+")
#       sentencetok <- unlist(sentencetok)
#       #print(sentencetok)
#       IDs <- seq(1:length(sentencetok))
#       sentencetok <- tolower(sentencetok)
#       sentencetok <- gsub("-"," -",sentencetok)
#       sentenceP <- paste(IDs,sentencetok)
#       sentenceP <- paste(sentenceP,collapse = " ")
#       #  print(sentenceP)
#       KwicDF_lemCol_10_KwicCol_12$citWithID[i]<-  sentenceP
#     }
#     return(KwicDF_lemCol_10_KwicCol_12)
#   }
# 
#   SAMPLE <- readtext(filePath)
#   OriginalConc <- SAMPLE$text
#   SAMPLE <- SAMPLE$text
#   ## fix missing starting <s> => this is a problem with sketch engine
#   MissingS <- unlist(regmatches(SAMPLE,gregexpr(".....................................,\\d+\",\".[^s]..........",SAMPLE, perl = T)))
#   for (m in MissingS){
#     fixed <- gsub("(,\\d+\"),\"(.)",'\\1,"<s> \\2',m)
#     SAMPLE <- gsub(m,fixed,SAMPLE)
#   }
#   rm(m)
# 
#   SAMPLE <- gsub("<g/>", "",SAMPLE)
#   SAMPLE <- gsub('"',"",SAMPLE)
#   SAMPLE <- gsub('^corpus.*?\n',"",SAMPLE)
#   SAMPLE <- gsub('^subcorpus.*?\n',"",SAMPLE)
#   SAMPLE <- gsub('^concordance.*?\n',"",SAMPLE)
#   SAMPLE <- gsub('^query.*?\n',"",SAMPLE)
#   SAMPLE <- gsub('^_,',"",SAMPLE)
#   SAMPLE <- gsub('\n_,',"\n",SAMPLE)
#   #find and fill missing page and sID values
#   # fixMissing <- gsub('(([a-z]|[āīūṛḷṇḍñṅḥśṣṭḍṃ])([a-z]|[āīūṛḷṇḍñṅḥśṣṭḍṃ])+,\\d+[vr]?),<s>',"\\1,0000,<s>",SAMPLE) # cases where structure c is missing altogether. remove this line when you fix corpus <s>
#   # fixMissing <- gsub(',,',",0000,",fixMissing)
#   # Missing <- unlist(regmatches(fixMissing,gregexpr("................0000.....................",fixMissing)))
#   # n <- 1
#   # #print("the following missing pages and sID are being filled with placeholders")
#   # print("CHECK that there are only 2 colums containing numerals for page and sID")
#   # for (m in Missing){
#   #   print(m)
#   #  makeUnique <- gsub("0000", paste0("0000",n), b)
#   #
#   #   SAMPLE <- gsub(m, makeUnique, SAMPLE)
#   #   n <- n+1
#   # }
#   # rm(m)
# 
# 
#   HeaderString <- "title,text.type,author,tradition,discourse,period,page,sID,kwicLeft,lemma,kwicRight"
#   SAMPLE <- paste(HeaderString,SAMPLE, sep="\n")
#   write(SAMPLE,"./RawConcordanceToProcess.csv")
#   SAMPLE <- read.csv("./RawConcordanceToProcess.csv", stringsAsFactors = F )
#   SAMPLE$kwicRight <- gsub("\\s+"," ",SAMPLE$kwicRight)
#   SAMPLE$kwicLeft <- gsub("\\s+"," ",SAMPLE$kwicLeft)
#   SAMPLE$Kwic <- paste0(SAMPLE$kwicLeft,SAMPLE$lemma,SAMPLE$kwicRight)
#   SAMPLE$Kwic <- gsub("</?s>","",SAMPLE$Kwic)
#   SAMPLE$Kwic <- gsub("\\s+"," ",SAMPLE$Kwic)
#   SAMPLE$lemma <- gsub("\\s","",SAMPLE$lemma)
#   colnames(SAMPLE)
#   SAMPLE <- AddWordID(SAMPLE)
#   SAMPLE$transl <- ""
#   SAMPLE$sense <- ""
#   SAMPLE$subsense <- ""
#   SAMPLE$domain <- ""
#   SAMPLE$sem.field <- ""
#   SAMPLE$sem.cat <- ""
#   SAMPLE$is_object_or_patient_of <- ""
#   SAMPLE$takes_as_object_or_patient <- ""
#   SAMPLE$is_subject_or_agent_of <- ""
#   SAMPLE$takes_as_subject_or_agent <- ""
#   SAMPLE$takes_as_oblique <- ""
#   SAMPLE$is_oblique_of <- ""
#   SAMPLE$is_modified_by <- ""
#   SAMPLE$modifies <- ""
#   SAMPLE$is_listed_with <- ""
#   SAMPLE$is_contrasted_with <- ""
#   SAMPLE$is_glossed_by <- ""
#   SAMPLE$glosses <- ""
#   SAMPLE$is_juxtaposed_to <- ""
#   SAMPLE$sem.pros <- ""
#   SAMPLE$uncertainty <- ""
#   SAMPLE$Notes <- ""
#   SAMPLE$ref <- paste0(SAMPLE$title,SAMPLE$sID)
#   SAMPLE$lemma1 <- tolower(SAMPLE$lemma)
#   SAMPLE$in_compound <-""
#   for(i in 1:nrow(SAMPLE)){
# 
#     SAMPLE$in_compound[[i]] <- ifelse(length(grep("@\\s?$", SAMPLE$kwicLeft[i]))>0,"YES","NO")
#     SAMPLE$in_compound[[i]] <- ifelse(length(grep("^\\s?-([a-z]|[āīūṛḷṇḍñṅḥśṣṭḍṃ])+ @", SAMPLE$kwicRight[i]))>0,"YES",SAMPLE$in_compound[i])
#   }
#   zu <- paste(str_extract_all(SAMPLE$kwicLeft,"([A-Z]|[ĀĪŪṚḶṆḌÑṄḤŚṢṬḌṂ])+"),str_extract_all(SAMPLE$kwicRight,"([A-Z]|[ĀĪŪṚḶṆḌÑṄḤŚṢṬḌṂ])+"),sep = " ")
#   SAMPLE$cotext <- gsub(',|character\\(0\\)|\\(|\\)|\\c|\"',"", zu)
#   SAMPLE$case_or_voice <- ""
#   SAMPLE$number <- ""
#   ## Add ConcRel:
#   SAMPLE$leadingTo <- ""
#   SAMPLE$causedBy <- ""
#   SAMPLE$possessing <- ""
#   SAMPLE$belongingTo <- ""
#   SAMPLE$locusOf <- ""
#   SAMPLE$locatedIn <- ""
#   SAMPLE$byMeansOf <- ""
#   SAMPLE$achievedThrough <- ""
#   SAMPLE$goalOf <- ""
#   SAMPLE$takesGoal <- ""
# 
# 
#   #print(colnames(SAMPLE))
#   # move author column to the end
#   SAMPLE <- SAMPLE[,c(1,2,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51, 3)]
#   SAMPLE <- SAMPLE[!duplicated(SAMPLE$ref),]
#   # print(colnames(SAMPLE))
#   write.csv(SAMPLE, "./data/ConcordancesReady.csv", row.names = F)
# 
#   FileName <- paste0("../RawConc/", unique(SAMPLE$lemma)[1],"_RawConc",Sys.Date(),".txt")
#   write(OriginalConc, FileName)
#   #write.csv(SAMPLE, "~/Desktop/SAMPLE.csv", row.names = F)
#   #print("your file is ready in the data folder")
# }
# #ConcPrepR("./data/Conc.txt")
# 
# 
# sample <- read.csv("./data/ConcordancesReady.csv", stringsAsFactors = F)
# sample[is.na(sample)] <- ""
# 
# colnames(sample)
# 
# LexicalData <- readRDS("./www/LexicalData_Revised20Nov2019.rds")
# #colnames(LexicalData)
# 
# ui <- fluidPage(
#   titlePanel(HTML("<font color='#3B444C' size='+4'>Meaning Mapper: a lexical annotation tool</font>")),
# 
#   # tags$h1("Meaning Mapper: a lexical annotation tool"),
#   tags$div(HTML("created by <font color = '#4682B4'> Ligeia Lugli</font> as part of <a href='https://btw.mangalamresearch.org/en-us/' target='_blank'>The Buddhist Translators Workbench</a>.")),
#   tags$br(),
#   sidebarLayout(
#     sidebarPanel(
#       tags$div(HTML("<font size='+3'>see <a href='Meaning Mapper App_ draft documentation.html' target='_blank'>documentation</a></font>")),
#       tags$hr(),
#       tags$h3("visualise annotations"),
#       tags$hr(),
#       selectInput(inputId = "DataVis",
#                   label = "Which aspect of the lemma would you like to explore?",
#                   choices = c("sense",
#                               "subsense",
#                               "sem.pros",
#                               "domain",
#                               "sem.field",
#                               "sem.cat",
#                               "text.type",
#                               "period",
#                               "number",
#                               "case_or_voice",
#                               "title"),
#                   selected = "domain"),
# 
#       # Select variable to colour plot by
#       selectInput(inputId = "fill",
#                   label = "By which variable would you like to chart your chosen aspect of the lemma?",
#                   choices = c("sense",
#                               "subsense",
#                               "sem.pros",
#                               "domain",
#                               "sem.field",
#                               "sem.cat",
#                               "text.type",
#                               "period",
#                               "number",
#                               "case_or_voice",
#                               "title"),
#                   selected = "sem.pros"),
# 
#       selectInput(inputId = "barchart_type",
#                   label = "which type of bartchart would you like to view?",
#                   choices = c("side-by-side"="dodge",
#                               "stacked" ="stack",
#                               "percentage"="fill"),
#                   selected = "stack"),
# 
#       tags$hr(),
#       tags$br(),
#       tags$hr(),
#       tags$h3("annotate"),
#       tags$hr(),
#       selectInput("cit",
#                   "select a citation to annotate",
#                   choices = sort(as.character(sample$ref[nchar(sample$sem.pros)<3]))
#                   #choices = setNames(c(1:length(sort(sample$ref[sample$sem.pros==""]))), as.character(sort(sample$ref[sample$sem.pros==""])))
#       ),
#       tags$br(),
#       textInput("trans","input translation here:", value=NULL),
#       # tags$br(),
#       # tags$h4("semantic prosody"),
#       # tags$br(),
#       tags$br(),
#       # tags$h4("syntactic dependencies"),
# 
#       textInput("Grammar","lemma,case,number:", value= NULL),
# 
#       actionButton('SaveDraft', 'Save draft'),
#       tags$br(),
#       selectInput("SynDep1",
#                   "dep.rel",
# 
#                   choices = c("select",colnames(sample[,19:26])),
#                   # selected = "select"#NULL
#                   selected = NULL
#       ),
#       # textInput("text1","Text 1:",value="dep"),
#       textInput("text1","dep.head",value= NULL),
#       tags$br(),
#       selectInput("SynDep2",
#                   "additional dep.rel",
#                   choices=c("select",colnames(sample[,19:26])),
#                   selected=NULL
#       ),
#       # textInput("text1","Text 1:",value="dep"),
#       textInput("text2","additional dep.head",value=NULL),
#       tags$br(),
#       # tags$h4("prosodic relations"),
#       selectInput("ProsRel",
#                   "prosodic relations",
#                   choices=c("select",colnames(sample[,27:31])),
#                   selected=NULL),
#       textInput("text3","pros.head",value=NULL),
# 
#       selectInput("ConcRel",
#                   "conceptual relations",
#                   choices=c("select",colnames(sample[,41:50])),
#                   selected=NULL),
#       textInput("text4","conc.head",value=NULL),
# 
#       actionButton('SaveDraft2', 'Save draft'),
#       tags$h4("Semantics"),
#       selectizeInput("Sense","sense",
# 
#                      choices=c("",levels(factor(sample$sense))),
#                      options = list(create = TRUE),
#                      selected= NULL
# 
#       ),
#       selectizeInput("Subsense","subsense",
# 
#                      choices=c("",levels(factor(sample$subsense))),
#                      options = list(create = TRUE),
#                      selected= NULL
# 
#       ),
#       selectizeInput("Dom","domain",
# 
#                      choices=c("",levels(factor(LexicalData$domain))),
#                      options = list(create = TRUE),
#                      selected= NULL
# 
#       ),
# 
#       selectizeInput("SemField","sem.field",
#                      choices=c("",levels(factor(LexicalData$sem.field))),
#                      options = list(create = TRUE),
#                      selected= NULL
# 
#       ),
#       selectizeInput("SemCat","sem.cat",
#                      choices=c("",levels(factor(LexicalData$sem.cat))),
#                      options = list(create = TRUE),
#                      selected= NULL
# 
#       ),
# 
# 
#       selectInput("SemPros",
#                   "select sem.pros",
#                   choices=c("","neu","neu.neg","neg","pos"),
#                   selected = NULL),
# 
#       selectInput("Uncertainty",
#                   "uncertainty",
#                   choices=c("","vague","disputed","philological","other"),
#                   selected= NULL),
# 
#       textInput("Notes",
#                 "add note",
#                 value=NULL),
#       tags$br(),
#       actionButton('Save', 'Save'),
#       tags$br(),
#       tags$head(
#         tags$style(HTML('#Edit{background-color:red}')),
#         tags$style(HTML('#Save{background-color:lightgreen}'))
#       ),
# 
#       tags$br(),
# 
#       downloadButton('downloadData', 'Download csv'),
#       tags$br(),
#       tags$hr(),
#       tags$div(HTML("<font color='red' size='+2'><b>EDIT</b></font>")),
#       tags$div(HTML("<font color='red'>download csv before editing!</font>")),
#       tags$hr(),
#       textInput("find","find",value=""),
# 
#       textInput("replace","replace",value=""),
# 
#       selectInput("where","in which column?",
# 
#                   choices = c("",colnames(sample)),
#                   selected=""),
#       selectizeInput("whereRow","in which citation?",
# 
#                      choices = c("",paste0(sample$title,sample$sID),"all"),
#                      selected=""),
# 
# 
#       actionButton('Edit', 'EDIT')
# 
# 
# 
# 
# 
#     ),
#     mainPanel(
#       htmlOutput("Intro"),
# 
#       fluidRow(
# 
#         column(width=5, plotlyOutput("progress",height="300")),
#         column(width=2, plotlyOutput("progress0", height="300")),
#         column(width=5, plotlyOutput("SemanticSummary",height="300"))
#       ),
#       collapsibleTreeOutput("SemanticTree"),
#       tags$hr(),
#       # htmlOutput("prova"),
#       tags$hr(),
#       htmlOutput("MetadataIntro"),
#       tags$hr(),
#       tableOutput("Metadata"),
#       #  tags$hr(),
# 
#       htmlOutput("segmented"),
#       htmlOutput("Citation0"),
#       htmlOutput("translation"),
#       tags$hr(),
# 
#       htmlOutput("Citation"),
#       tags$br(),
#       tableOutput("annotatedDepRelTable"),
#       tags$br(),
#       tableOutput("annotatedDepRelTable2"),
#       tags$br(),
#       tableOutput("annotatedProsRelTable"),
#       tags$br(),
#       tableOutput("annotatedConcRelTable"),
#       tags$br(),
# 
#       tags$hr(),
#       htmlOutput("wordcloudIntro"),
#       plotOutput("cotextWordcloud"),
#       tags$hr(),
#       htmlOutput("SimilarityIntro"),
#       tags$hr(),
# 
#       DT::dataTableOutput("SimilarCit"),
#       tags$hr(),
#       htmlOutput("CitationSetIntro"),
#       tags$hr(),
#       DT::dataTableOutput("CitationSet"),
#       tags$hr(),
#       DT::dataTableOutput("workSummary"),
#       tags$br(),
#       tags$br()
#     )
#   )
# )
# 
# server <- function(input, output, session) {
#   sample <- read.csv("./data/ConcordancesReady.csv", stringsAsFactors = F)
#   sample[is.na(sample)] <- ""
#   observeEvent(input$cit,{
#     updateTextInput(session,'trans',
#                     value = #print(sample$Translation[sample$ref==input$cit])
#                       ifelse(
#                         nchar(as.character(sample$transl[sample$ref==input$cit]))>1,
#                         print(sample$transl[sample$ref==input$cit]),
#                         print("translation not available")
#                       )
#     )
#   })
# 
#   observeEvent(input$cit,{
#     updateSelectInput(session,  "SynDep1",
#                       selected  = #print(sample$Translation[sample$ref==input$cit])
#                         ifelse(nchar(as.character(sample$takes_as_object_or_patient[sample$ref==input$cit]))>1, "takes_as_object_or_patient",
#                                ifelse(nchar( as.character(sample$is_subject_or_agent_of[sample$ref==input$cit]))>1,"is_subject_or_agent_of",
#                                       ifelse(nchar( as.character(sample$is_object_or_patient_of[sample$ref==input$cit]))>1,"is_object_or_patient_of",
#                                              ifelse(nchar( as.character(sample$takes_as_subject_or_agent[sample$ref==input$cit]))>1,"takes_as_subject_or_agent",
#                                                     ifelse(nchar( as.character(sample$takes_as_oblique[sample$ref==input$cit]))>1,"takes_as_oblique",
#                                                            ifelse(nchar( as.character(sample$is_oblique_of[sample$ref==input$cit]))>1,"is_oblique_of",
#                                                                   ifelse(nchar( as.character(sample$is_modified_by[sample$ref==input$cit]))>1,"is_modified_by",
#                                                                          ifelse(nchar( as.character(sample$modifies[sample$ref==input$cit]))>1,"modifies", "select")
# 
#                                                                   )
#                                                            )))))))
# 
# 
#   })
# 
#   observeEvent(input$cit,{
#     updateTextInput(session,'text1',
#                     value =
#                       ifelse(nchar(as.character(sample$takes_as_object_or_patient[sample$ref==input$cit]))>1, str_extract(sample$takes_as_object_or_patient[sample$ref==input$cit],"\\d+"),
#                              ifelse(nchar(as.character(sample$is_subject_or_agent_of[sample$ref==input$cit]))>1, str_extract(sample$is_subject_or_agent_of[sample$ref==input$cit],"\\d+"),
#                                     ifelse(nchar(as.character(sample$is_object_or_patient_of[sample$ref==input$cit]))>1, str_extract(sample$is_object_or_patient_of[sample$ref==input$cit],"\\d+"),
#                                            ifelse(nchar(as.character(sample$takes_as_subject_or_agent[sample$ref==input$cit]))>1, str_extract(sample$takes_as_subject_or_agent[sample$ref==input$cit],"\\d+"),
#                                                   ifelse(nchar(as.character(sample$takes_as_oblique[sample$ref==input$cit]))>1, str_extract(sample$takes_as_oblique[sample$ref==input$cit],"\\d+"),
#                                                          ifelse(nchar(as.character(sample$is_oblique_of[sample$ref==input$cit]))>1, str_extract(sample$is_oblique_of[sample$ref==input$cit],"\\d+"),
#                                                                 ifelse(nchar(as.character(sample$is_modified_by[sample$ref==input$cit]))>1, str_extract(sample$is_modified_by[sample$ref==input$cit],"\\d+"),
#                                                                        ifelse(nchar(as.character(sample$modifies[sample$ref==input$cit]))>1, str_extract(sample$modifies[sample$ref==input$cit],"\\d+"),
#                                                                               print("") )
#                                                                 )))))))
# 
# 
# 
#     )
#   })
# 
#   observeEvent(input$cit,{
#     updateTextInput(session,'Grammar',
#                     value = paste(as.character(sample$lemma1[sample$ref==input$cit]),as.character(sample$case_or_voice[sample$ref==input$cit]),as.character(sample$number[sample$ref==input$cit])  ,sep=",")
#     )
#   })
# 
#   observeEvent(input$cit,{
#     updateSelectInput(session,  "SynDep2",
#                       selected  = #print(sample$Translation[sample$ref==input$cit])
#                         ifelse(nchar(as.character(sample$modifies[sample$ref==input$cit]))>1, "modifies",
#                                ifelse(nchar( as.character(sample$is_modified_by[sample$ref==input$cit]))>1,"is_modified_by",
#                                       ifelse(nchar( as.character(sample$is_object_or_patient_of[sample$ref==input$cit]))>1,"is_object_or_patient_of",
#                                              ifelse(nchar( as.character(sample$takes_as_oblique[sample$ref==input$cit]))>1,"takes_as_oblique",
#                                                     ifelse(nchar( as.character(sample$is_oblique_of[sample$ref==input$cit]))>1,"is_oblique_of",
#                                                            ifelse(nchar( as.character(sample$takes_as_subject_or_agent[sample$ref==input$cit]))>1,"takes_as_subject_or_agent",
#                                                                   ifelse(nchar( as.character(sample$is_object_or_patient_of[sample$ref==input$cit]))>1,"is_object_or_patient_of",
#                                                                          ifelse(nchar( as.character(sample$is_subject_or_agent_of[sample$ref==input$cit]))>1,"is_subject_or_agent_of",
#                                                                                 ifelse(nchar( as.character(sample$takes_as_object_or_patient[sample$ref==input$cit]))>1,"takes_as_object_or_patient", "select")
#                                                                          )))))))))
# 
# 
# 
#   })
# 
#   observeEvent(input$cit,{
#     updateTextInput(session,'text2',
#                     value =
#                       ifelse(nchar(as.character(sample$modifies[sample$ref==input$cit]))>1, str_extract(sample$modifies[sample$ref==input$cit],"\\d+"),
#                              ifelse(nchar(as.character(sample$is_modified_by[sample$ref==input$cit]))>1, str_extract(sample$is_modified_by[sample$ref==input$cit],"\\d+"),
#                                     ifelse(nchar(as.character(sample$is_object_or_patient_of[sample$ref==input$cit]))>1, str_extract(sample$is_object_or_patient_of[sample$ref==input$cit],"\\d+"),
#                                            ifelse(nchar(as.character(sample$takes_as_oblique[sample$ref==input$cit]))>1, str_extract(sample$takes_as_oblique[sample$ref==input$cit],"\\d+"),
#                                                   ifelse(nchar(as.character(sample$is_oblique_of[sample$ref==input$cit]))>1, str_extract(sample$is_oblique_of[sample$ref==input$cit],"\\d+"),
#                                                          ifelse(nchar(as.character(sample$takes_as_subject_or_agent[sample$ref==input$cit]))>1, str_extract(sample$takes_as_subject_or_agent[sample$ref==input$cit],"\\d+"),
#                                                                 ifelse(nchar(as.character(sample$is_object_or_patient_of[sample$ref==input$cit]))>1, str_extract(sample$is_object_or_patient_of[sample$ref==input$cit],"\\d+"),
#                                                                        ifelse(nchar(as.character(sample$is_subject_or_agent_of[sample$ref==input$cit]))>1, str_extract(sample$is_subject_or_agent_of[sample$ref==input$cit],"\\d+"),
#                                                                               ifelse(nchar(as.character(sample$takes_as_object_or_patient[sample$ref==input$cit]))>1, str_extract(sample$takes_as_object_or_patient[sample$ref==input$cit],"\\d+"),
# 
#                                                                                      print("") )))
#                                                          ))))))
# 
# 
# 
#     )
#   })
# 
# 
#   observeEvent(input$cit,{
#     updateSelectInput(session,  "SemRel",
#                       selected  = #print(sample$Translation[sample$ref==input$cit])
#                         ifelse(nchar(as.character(sample$is_listed_with[sample$ref==input$cit]))>1, "is_listed_with",
#                                ifelse(nchar( as.character(sample$is_contrasted_with[sample$ref==input$cit]))>1,"is_contrasted_with",
#                                       ifelse(nchar( as.character(sample$is_glossed_by[sample$ref==input$cit]))>1,"is_glossed_by",
#                                              ifelse(nchar( as.character(sample$glosses[sample$ref==input$cit]))>1,"glosses",
#                                                     ifelse(nchar( as.character(sample$is_juxtaposed_to[sample$ref==input$cit]))>1,"is_juxtaposed_to",
#                                                            "select")
#                                              ) ))))
#   })
# 
#   observeEvent(input$cit,{
#     updateTextInput(session,'text3',
#                     value =
#                       ifelse(nchar(as.character(sample$is_listed_with[sample$ref==input$cit]))>1, str_extract(sample$is_listed_with[sample$ref==input$cit],"\\d+"),
#                              ifelse(nchar(as.character(sample$is_contrasted_with[sample$ref==input$cit]))>1, str_extract(sample$is_contrasted_with[sample$ref==input$cit],"\\d+"),
#                                     ifelse(nchar(as.character(sample$is_glossed_by[sample$ref==input$cit]))>1, str_extract(sample$is_glossed_by[sample$ref==input$cit],"\\d+"),
#                                            ifelse(nchar(as.character(sample$glosses[sample$ref==input$cit]))>1, str_extract(sample$glosses[sample$ref==input$cit],"\\d+"),
#                                                   ifelse(nchar(as.character(sample$is_juxtaposed_to[sample$ref==input$cit]))>1, str_extract(sample$is_juxtaposed_to[sample$ref==input$cit],"\\d+"),
# 
#                                                          print("") )   )))))
#   })
# 
#   observeEvent(input$cit,{
#     updateSelectInput(session,  "ConcRel",
#                       selected  = #print(sample$Translation[sample$ref==input$cit])
#                         ifelse(nchar(as.character(sample$leadingTo[sample$ref==input$cit]))>1, "leadingTo",
#                                ifelse(nchar( as.character(sample$causedBy[sample$ref==input$cit]))>1,"causedBy",
#                                       ifelse(nchar( as.character(sample$possessing[sample$ref==input$cit]))>1,"possessing",
#                                              ifelse(nchar( as.character(sample$belongingTo[sample$ref==input$cit]))>1,"belongingTo",
#                                                     ifelse(nchar( as.character(sample$locusOf[sample$ref==input$cit]))>1,"locusOf",
#                                                            ifelse(nchar( as.character(sample$locatedIn[sample$ref==input$cit]))>1,"locatedIn",
#                                                                   ifelse(nchar( as.character(sample$byMeansOf[sample$ref==input$cit]))>1,"byMeansOf",
#                                                                          ifelse(nchar( as.character(sample$achievedThrough[sample$ref==input$cit]))>1,"achievedThrough",
#                                                                                 ifelse(nchar( as.character(sample$goalOf[sample$ref==input$cit]))>1,"goalOf",
#                                                                                        ifelse(nchar( as.character(sample$takesGoal[sample$ref==input$cit]))>1,"takesGoal",
# 
#                                                                                               "select")
#                                                                                 ) )))) )))))
#   })
# 
#   observeEvent(input$cit,{
#     updateTextInput(session,'text4',
#                     value =
#                       ifelse(nchar(as.character(sample$leadingTo[sample$ref==input$cit]))>1, str_extract(sample$leadingTo[sample$ref==input$cit],"\\d+"),
#                              ifelse(nchar(as.character(sample$causedBy[sample$ref==input$cit]))>1, str_extract(sample$causedBy[sample$ref==input$cit],"\\d+"),
#                                     ifelse(nchar(as.character(sample$possessing[sample$ref==input$cit]))>1, str_extract(sample$possessing[sample$ref==input$cit],"\\d+"),
#                                            ifelse(nchar(as.character(sample$belongingTo[sample$ref==input$cit]))>1, str_extract(sample$belongingTo[sample$ref==input$cit],"\\d+"),
#                                                   ifelse(nchar(as.character(sample$locusOf[sample$ref==input$cit]))>1, str_extract(sample$locusOf[sample$ref==input$cit],"\\d+"),
#                                                          ifelse(nchar(as.character(sample$locatedIn[sample$ref==input$cit]))>1, str_extract(sample$locatedIn[sample$ref==input$cit],"\\d+"),
#                                                                 ifelse(nchar(as.character(sample$byMeansOf[sample$ref==input$cit]))>1, str_extract(sample$byMeansOf[sample$ref==input$cit],"\\d+"),
#                                                                        ifelse(nchar(as.character(sample$achievedThrough[sample$ref==input$cit]))>1, str_extract(sample$achievedThrough[sample$ref==input$cit],"\\d+"),
#                                                                               ifelse(nchar(as.character(sample$goalOf[sample$ref==input$cit]))>1, str_extract(sample$goalOf[sample$ref==input$cit],"\\d+"),
#                                                                                      ifelse(nchar(as.character(sample$takesGoal[sample$ref==input$cit]))>1, str_extract(sample$takesGoal[sample$ref==input$cit],"\\d+"),
# 
#                                                                                             print("") )   ))))))))))
#   })
# 
#   observeEvent(input$cit,{
#     updateTextInput(session,  "Sense",
#                     value=
#                       ifelse(nchar(as.character(sample$sense[sample$ref==input$cit]))>1, as.character(sample$sense[sample$ref==input$cit]), "")
#     )
#   })
# 
#   observeEvent(input$cit,{
#     updateTextInput(session,  "Subsense",
#                     value=
#                       ifelse(nchar(as.character(sample$subsense[sample$ref==input$cit]))>1, as.character(sample$subsense[sample$ref==input$cit]), "")
#     )
#   })
# 
#   observeEvent(input$cit,{
#     updateTextInput(session,  "Dom",
#                     value=
#                       ifelse(nchar(as.character(sample$domain[sample$ref==input$cit]))>1, as.character(sample$domain[sample$ref==input$cit]), "")
#     )
#   })
# 
#   observeEvent(input$cit,{
#     updateTextInput(session,  "SemField",
#                     value=
#                       ifelse(nchar(as.character(sample$sem.field[sample$ref==input$cit]))>1, as.character(sample$sem.field[sample$ref==input$cit]), "")
#     )
#   })
# 
#   observeEvent(input$cit,{
#     updateTextInput(session,  "SemCat",
#                     value=
#                       ifelse(nchar(as.character(sample$sem.cat[sample$ref==input$cit]))>1, as.character(sample$sem.cat[sample$ref==input$cit]), "")
#     )
#   })
# 
#   observeEvent(input$cit,{
#     updateSelectInput(session,  "Uncertainty",
#                       selected=
#                         ifelse(nchar(as.character(sample$uncertainty[sample$ref==input$cit]))>1, as.character(sample$uncertainty[sample$ref==input$cit]), "")
#     )
#   })
# 
#   observeEvent(input$cit,{
#     updateSelectInput(session,  "SemPros",
#                       selected=
#                         ifelse(nchar(as.character(sample$sem.pros[sample$ref==input$cit]))>1, as.character(sample$sem.pros[sample$ref==input$cit]), "")
#     )
#   })
# 
#   observeEvent(input$cit,{
#     updateTextInput(session,  "Notes",
#                     value=
#                       ifelse(nchar(as.character(sample$Notes[sample$ref==input$cit]))>1, as.character(sample$Notes[sample$ref==input$cit]), "")
#     )
#   })
# 
#   observeEvent(input$Dom,{
#     updateSelectInput(session,'SemField',
#                       choices=sort(unique(LexicalData$sem.field[LexicalData$domain==input$Dom])))
#   })
# 
#   observeEvent(input$SemField,{
#     updateSelectInput(session,'SemCat',
#                       choices=sort(unique(LexicalData$sem.cat[LexicalData$sem.field==input$SemField])))
#   })
# 
# 
# 
# 
# 
#   observeEvent(input$SaveDraft,{
#     sample <- read.csv("./data/ConcordancesReady.csv", stringsAsFactors = F)
#     sample[is.na(sample)] <- ""
# 
#     sample[sample$ref==input$cit,colnames(sample)=="sem.pros"] <- input$SemPros
#     sample[sample$ref==input$cit,colnames(sample)=="sense"] <- input$Sense
#     sample[sample$ref==input$cit,colnames(sample)=="subsense"] <- input$Subsense
#     sample[sample$ref==input$cit,colnames(sample)=="domain"] <- input$Dom
#     sample[sample$ref==input$cit,colnames(sample)=="sem.field"] <- input$SemField
#     sample[sample$ref==input$cit,colnames(sample)=="sem.cat"] <- input$SemCat
#     sample[sample$ref==input$cit,colnames(sample)=="transl"] <- input$trans
#     sample[sample$ref==input$cit,colnames(sample)=="uncertainty"] <- input$Uncertainty
#     sample[sample$ref==input$cit,colnames(sample)=="Notes"] <- input$Notes
#     sample[sample$ref==input$cit,colnames(sample)=="lemma1"] <- gsub("^(.*?),(.*?),(.*?)$","\\1",input$Grammar)
#     sample[sample$ref==input$cit,colnames(sample)=="case_or_voice"] <- gsub("^(.*?),(.*?),(.*?)$","\\2",input$Grammar)
#     sample[sample$ref==input$cit,colnames(sample)=="number"] <- gsub("^(.*?),(.*?),(.*?)$","\\3",input$Grammar)
# 
#     if (input$SynDep1 != "select"){
#       citation <- sample[sample$ref==input$cit,12]
#       sample[sample$ref==input$cit,colnames(sample)==input$SynDep1] <- str_extract(citation, paste0(as.character(input$text1), "\\s([a-z]|[āīūṛḷṇḍñṅḥśṣṭḍṃ])+\\s"))
#     }
#     if (input$SynDep2 != "select"){
#       citation <- sample[sample$ref==input$cit,12]
#       sample[sample$ref==input$cit,colnames(sample)==input$SynDep2] <-   str_extract(citation, paste0(as.character(input$text2), "\\s([a-z]|[āīūṛḷṇḍñṅḥśṣṭḍṃ])+\\s"))
#     }
#     if (input$ConcRel != "select"){
#       citation <- sample[sample$ref==input$cit,12]
#       sample[sample$ref==input$cit,colnames(sample)==input$ConcRel] <-   str_extract(citation, paste0(as.character(input$text4), "\\s([a-z]|[āīūṛḷṇḍñṅḥśṣṭḍṃ])+\\s"))
#     }
#     if (input$ProsRel != "select"){
#       citation <- sample[sample$ref==input$cit,12]
#       sample[sample$ref==input$cit,colnames(sample)==input$ProsRel] <- str_extract(citation, paste0(as.character(input$text3), "\\s([a-z]|[āīūṛḷṇḍñṅḥśṣṭḍṃ])+\\s"))
#     }
# 
#     write.csv(sample,"./data/ConcordancesReady.csv", row.names=F)
#     # sample <- read.csv("./data/ConcordancesReady.csv", stringsAsFactors = F)
#     # sample[is.na(sample)] <- ""
#   })
# 
#   observeEvent(input$SaveDraft2,{
#     sample <- read.csv("./data/ConcordancesReady.csv", stringsAsFactors = F)
#     sample[is.na(sample)] <- ""
# 
#     sample[sample$ref==input$cit,colnames(sample)=="sem.pros"] <- input$SemPros
#     sample[sample$ref==input$cit,colnames(sample)=="sense"] <- input$Sense
#     sample[sample$ref==input$cit,colnames(sample)=="subsense"] <- input$Subsense
#     sample[sample$ref==input$cit,colnames(sample)=="domain"] <- input$Dom
#     sample[sample$ref==input$cit,colnames(sample)=="sem.field"] <- input$SemField
#     sample[sample$ref==input$cit,colnames(sample)=="sem.cat"] <- input$SemCat
#     sample[sample$ref==input$cit,colnames(sample)=="transl"] <- input$trans
#     sample[sample$ref==input$cit,colnames(sample)=="uncertainty"] <- input$Uncertainty
#     sample[sample$ref==input$cit,colnames(sample)=="Notes"] <- input$Notes
#     sample[sample$ref==input$cit,colnames(sample)=="lemma1"] <- gsub("^(.*?),(.*?),(.*?)$","\\1",input$Grammar)
#     sample[sample$ref==input$cit,colnames(sample)=="case_or_voice"] <- gsub("^(.*?),(.*?),(.*?)$","\\2",input$Grammar)
#     sample[sample$ref==input$cit,colnames(sample)=="number"] <- gsub("^(.*?),(.*?),(.*?)$","\\3",input$Grammar)
# 
#     if (input$SynDep1 != "select"){
#       citation <- sample[sample$ref==input$cit,12]
#       sample[sample$ref==input$cit,colnames(sample)==input$SynDep1] <- str_extract(citation, paste0(as.character(input$text1), "\\s([a-z]|[āīūṛḷṇḍñṅḥśṣṭḍṃ])+\\s"))
#     }
#     if (input$SynDep2 != "select"){
#       citation <- sample[sample$ref==input$cit,12]
#       sample[sample$ref==input$cit,colnames(sample)==input$SynDep2] <-   str_extract(citation, paste0(as.character(input$text2), "\\s([a-z]|[āīūṛḷṇḍñṅḥśṣṭḍṃ])+\\s"))
#     }
#     if (input$ConcRel != "select"){
#       citation <- sample[sample$ref==input$cit,12]
#       sample[sample$ref==input$cit,colnames(sample)==input$ConcRel] <-   str_extract(citation, paste0(as.character(input$text4), "\\s([a-z]|[āīūṛḷṇḍñṅḥśṣṭḍṃ])+\\s"))
#     }
#     if (input$ProsRel != "select"){
#       citation <- sample[sample$ref==input$cit,12]
#       sample[sample$ref==input$cit,colnames(sample)==input$ProsRel] <- str_extract(citation, paste0(as.character(input$text3), "\\s([a-z]|[āīūṛḷṇḍñṅḥśṣṭḍṃ])+\\s"))
#     }
# 
#     write.csv(sample,"./data/ConcordancesReady.csv", row.names=F)
#     # sample <- read.csv("./data/ConcordancesReady.csv", stringsAsFactors = F)
#     # sample[is.na(sample)] <- ""
#   })
# 
#   observeEvent(input$Save,{
#     sample <- read.csv("./data/ConcordancesReady.csv", stringsAsFactors = F)
#     sample[is.na(sample)] <- ""
# 
#     sample[sample$ref==input$cit,colnames(sample)=="sem.pros"] <- input$SemPros
#     sample[sample$ref==input$cit,colnames(sample)=="sense"] <- input$Sense
#     sample[sample$ref==input$cit,colnames(sample)=="subsense"] <- input$Subsense
#     sample[sample$ref==input$cit,colnames(sample)=="domain"] <- input$Dom
#     sample[sample$ref==input$cit,colnames(sample)=="sem.field"] <- input$SemField
#     sample[sample$ref==input$cit,colnames(sample)=="sem.cat"] <- input$SemCat
#     sample[sample$ref==input$cit,colnames(sample)=="transl"] <- input$trans
#     sample[sample$ref==input$cit,colnames(sample)=="uncertainty"] <- input$Uncertainty
#     sample[sample$ref==input$cit,colnames(sample)=="Notes"] <- input$Notes
#     sample[sample$ref==input$cit,colnames(sample)=="lemma1"] <- gsub("^(.*?),(.*?),(.*?)$","\\1",input$Grammar)
#     sample[sample$ref==input$cit,colnames(sample)=="case_or_voice"] <- gsub("^(.*?),(.*?),(.*?)$","\\2",input$Grammar)
#     sample[sample$ref==input$cit,colnames(sample)=="number"] <- gsub("^(.*?),(.*?),(.*?)$","\\3",input$Grammar)
# 
#     if (input$SynDep1 != "select"){
#       citation <- sample[sample$ref==input$cit,12]
#       sample[sample$ref==input$cit,colnames(sample)==input$SynDep1] <- str_extract(citation, paste0(as.character(input$text1), "\\s([a-z]|[āīūṛḷṇḍñṅḥśṣṭḍṃ])+\\s"))
#     }
#     if (input$SynDep2 != "select"){
#       citation <- sample[sample$ref==input$cit,12]
#       sample[sample$ref==input$cit,colnames(sample)==input$SynDep2] <-   str_extract(citation, paste0(as.character(input$text2), "\\s([a-z]|[āīūṛḷṇḍñṅḥśṣṭḍṃ])+\\s"))
#     }
#     if (input$ConcRel != "select"){
#       citation <- sample[sample$ref==input$cit,12]
#       sample[sample$ref==input$cit,colnames(sample)==input$ConcRel] <-   str_extract(citation, paste0(as.character(input$text4), "\\s([a-z]|[āīūṛḷṇḍñṅḥśṣṭḍṃ])+\\s"))
#     }
#     if (input$ProsRel != "select"){
#       citation <- sample[sample$ref==input$cit,12]
#       sample[sample$ref==input$cit,colnames(sample)==input$ProsRel] <- str_extract(citation, paste0(as.character(input$text3), "\\s([a-z]|[āīūṛḷṇḍñṅḥśṣṭḍṃ])+\\s"))
#     }
# 
#     write.csv(sample,"./data/ConcordancesReady.csv", row.names=F)
#     # sample <- read.csv("./data/ConcordancesReady.csv", stringsAsFactors = F)
#     # sample[is.na(sample)] <- ""
#   })
# 
#   observeEvent(input$Edit,{
#     sample <- read.csv("./data/ConcordancesReady.csv", stringsAsFactors = F)
#     sample[is.na(sample)] <- ""
#     if(input$whereRow!="all"){
#       ROWsID <- gsub("^.*?(\\d+)$","\\1", input$whereRow)
#       ROWtitle <- gsub("^(.*?)\\d+$","\\1", input$whereRow)
#       # EditLog <- paste0("replaced: ",as.character(input$find),"\nwith: ",as.character(input$replace), "\ncolumn: ",as.character(input$where), " row: ", as.character(ROWsID), "\non: ", Sys.time())
#       sample[sample$sID==ROWsID & sample$title==ROWtitle,colnames(sample)==input$where] <- gsub(input$find, input$replace,sample[sample$sID==ROWsID & sample$title==ROWtitle,colnames(sample)==input$where])
# 
#       ExistingLog <- read.csv("./www/MeaningMapperEditLog.csv",stringsAsFactors = F)
#       newLog <- c(input$where,input$whereRow,input$find,input$replace,as.character(Sys.time()))
#       EditLog <- rbind(ExistingLog,newLog)
#       write.csv(EditLog, file="./www/MeaningMapperEditLog.csv", row.names = F)
# 
#       if(input$where==colnames(sample)[11]){
#         sample[sample$sID==ROWsID & sample$title==ROWtitle,] <- AddWordID(sample[sample$sID==ROWsID & sample$title==ROWtitle,])
# 
#       }
#       write.csv(sample,"./data/ConcordancesReady.csv", row.names=F)
#       sample <- read.csv("./data/ConcordancesReady.csv", stringsAsFactors = F)
#       sample[is.na(sample)] <- ""
#     }else{
#       sample[,colnames(sample)==input$where] <- gsub(input$find, input$replace,sample[,colnames(sample)==input$where])
#       ExistingLog <- read.csv("./www/MeaningMapperEditLog.csv",stringsAsFactors = F)
# 
#       newLog <- c(input$where,"all rows",input$find,input$replace,as.character(Sys.time()))
#       EditLog <- rbind(ExistingLog,newLog)
#       write.csv(EditLog, file="./www/MeaningMapperEditLog.csv", row.names = F)
#       if(input$where==colnames(sample)[11]){
# 
#         sample[grep(input$find, input$where),] <- AddWordID(sample[grep(input$find, input$where),])
#       }
#       write.csv(sample,"./data/ConcordancesReady.csv", row.names=F)
#       sample <- read.csv("./data/ConcordancesReady.csv", stringsAsFactors = F)
#       sample[is.na(sample)] <- ""
#     }
# 
# 
#   })
# 
# 
#   PROGRESS <- reactive({
#     input$Save
#     input$Edit
#     sample <- read.csv("./data/ConcordancesReady.csv", stringsAsFactors = F)
#     sample[is.na(sample)] <- ""
#     ProgressDF <- sample[,c(1,7,6,32)]
#     ProgressDF$progress <- 1
#     ProgressDF$progress[ProgressDF$sem.pros==""] <- 0
#     # colnames(sample)
#     # ggplot(ProgressDF, aes(title,fill=progress))+geom_bar(position="fill")+coord_flip()+
#     #   scale_fill_manual(values = c("done" = "green", "toDo" = "orange"))+theme_void()
#     Progress20Perc <-ProgressDF %>%
#       group_by(title)%>%
#       mutate(highlight = ifelse(sum(progress>0) & min(prop.table(table(progress)))> 0.2,T,F))
#   })
# 
# 
# 
#   output$Intro<- renderText({
#     # input$Save
#     # input$Edit
#     #
#     # sample <- read.csv("./data/ConcordancesReady.csv", stringsAsFactors = F)
#     # sample[is.na(sample)] <- ""
#     # ProgressDF <- sample[,c(1,7,6,32)]
#     # ProgressDF$progress <- "done"
#     # ProgressDF$progress[ProgressDF$sem.pros==""] <- "toDo"
#     ProgressDF <- PROGRESS()
#     paste0("<font size='+2'><font color='#cd5c5c'> ",unique(sample[,9]),"</font><font color='#708090'> ",nrow(ProgressDF), " citations ",nrow(ProgressDF[ProgressDF$progress=="done",])/nrow(ProgressDF)*100,"% complete</font>" )
#   })
# 
#   output$progress <- renderPlotly({
#     Progress20Perc <- PROGRESS()
#     # input$Save
#     # input$Edit
#     # sample <- read.csv("./data/ConcordancesReady.csv", stringsAsFactors = F)
#     # sample[is.na(sample)] <- ""
#     # ProgressDF <- sample[,c(1,7,6,32)]
#     # ProgressDF$progress <- 1
#     # ProgressDF$progress[ProgressDF$sem.pros==""] <- 0
#     #
#     # Progress20Perc <-ProgressDF %>%
#     #   group_by(title)%>%
#     #   mutate(highlight = ifelse(sum(progress>0) & min(prop.table(table(progress)))> 0.2,T,F))
#     plot <- ggplot(Progress20Perc, aes(title,fill=factor(progress), alpha=factor(highlight)))+geom_bar(position="fill")+coord_flip()+
#       scale_alpha_discrete(range = c(0.8,0.1))+scale_fill_manual(values = c("1" = "forestgreen", "0" = "steelblue")) +
#       theme_void()+theme(legend.position = "none")  #theme_minimal()+theme(legend.position = "none",panel.grid = element_blank(), text=element_text(size=14),axis.title.x = element_blank(), axis.ticks.x = element_blank(),axis.title.y = element_blank(),axis.text.x = element_blank(), axis.ticks.y = element_blank())
#     ggplotly(plot, fill="title")
# 
#   })
# 
#   output$progress0 <- renderPlotly({
#     # input$Save
#     # input$Edit
#     # sample <- read.csv("./data/ConcordancesReady.csv", stringsAsFactors = F)
#     # sample[is.na(sample)] <- ""
#     # ProgressDF <- sample[,c(1,7,6,32)]
#     # ProgressDF$progress <- "done"
#     # ProgressDF$progress[ProgressDF$sem.pros==""] <- "toDo"
#     ProgressDF <- PROGRESS()
#     plot <- ggplot(ProgressDF, aes(title))+geom_bar(alpha=0.2)+coord_flip()+theme_void()
#     ggplotly(plot, fill="title")
#   })
# 
#   output$SemanticSummary <- renderPlotly({
#     input$Save
#     input$SaveDraft
#     input$Edit
#     sample <- read.csv("./data/ConcordancesReady.csv", stringsAsFactors = F)
#     sample[is.na(sample)] <-""
#     plot <- ggplot(sample, aes_string(x = input$DataVis, fill= input$fill))+
#       geom_bar(position = input$barchart_type)+coord_flip()+
#       theme(panel.grid = element_blank(), text=element_text(size=10),axis.title.x = element_blank(), axis.ticks.x = element_blank(),axis.title.y = element_blank(),axis.text.x = element_blank(), axis.ticks.y = element_blank())
# 
#     ggplotly(plot)
#   })
# 
#   output$SemanticTree <-   renderCollapsibleTree({
#     input$Save
#     input$SaveDraft
#     input$Edit
#     sample <- read.csv("./data/ConcordancesReady.csv", stringsAsFactors = F)
#     sample[is.na(sample)] <-""
#     if (is.na(sum(nchar(sample$sense)))){
#       sample %>%
#         group_by(lemma1,genre, period) %>%
#         summarize("Number of citations" = n()) %>%
#         collapsibleTree(
#           hierarchy = c("lemma1","genre","period"),
#           root = "lemma",
#           attribute = "Number of citations",
#           nodeSize = "Number of citations",
#           fontSize= 14,
#           tooltip=TRUE,
#           collapsed = FALSE,
#           zoomable = FALSE
#         )
# 
#     }else{
#       sample %>%
#         group_by(lemma1,domain, sense, subsense) %>%
#         summarize("Number of citations" = n()) %>%
#         collapsibleTree(
#           hierarchy = c("lemma1","domain","sense", "subsense"),
#           root = "lemma",
#           attribute = "Number of citations",
#           nodeSize = "Number of citations",
#           fontSize= 14,
#           tooltip=TRUE,
#           collapsed = FALSE,
#           zoomable = FALSE
#         )
#     }
#   })
# 
# 
# 
#   output$MetadataIntro <- renderText({
# 
#     sample <- read.csv("./data/ConcordancesReady.csv", stringsAsFactors = F)
#     sample[is.na(sample)] <- ""
# 
# 
#     paste0("<font color='#4682b4' size='+2'><b> ",input$cit," </b></font>")
#   })
# 
#   output$Metadata <- renderTable({
# 
#     sample <- read.csv("./data/ConcordancesReady.csv", stringsAsFactors = F)
#     sample[is.na(sample)] <-""
# 
#     sample[sample$ref==input$cit,colnames(sample)=="lemma1"] <- gsub("^(.*?),(.*?),(.*?)$","\\1",input$Grammar)
#     sample[sample$ref==input$cit,colnames(sample)=="case_or_voice"] <- gsub("^(.*?),(.*?),(.*?)$","\\2",input$Grammar)
#     sample[sample$ref==input$cit,colnames(sample)=="number"] <- gsub("^(.*?),(.*?),(.*?)$","\\3",input$Grammar)
# 
# 
#     sample[sample$ref==input$cit,c(1,7,6,36,39,40)]
#   })
# 
#   output$segmented <- renderText({
#     input$Edit
#     sample <- read.csv("./data/ConcordancesReady.csv", stringsAsFactors = F)
#     sample[is.na(sample)] <- ""
#     paste0("<font size='-1' color='#708090'>",sample[sample$ref==input$cit,11],"</font>")
# 
#   })
# 
#   output$Citation0 <- renderText({
#     input$Edit
#     sample <- read.csv("./data/ConcordancesReady.csv", stringsAsFactors = F)
#     sample[is.na(sample)] <- ""
#     lemma <- sample[sample$ref==input$cit,9]
#     l <-tolower(lemma)
#     sentence <- sample[sample$ref==input$cit,11]
#     sentence <- gsub("\\s+-","",  sentence)
#     sentence <- gsub(" @ ","-", sentence)
#     sentence <- tolower(sentence)
#     sentence <- tolower(sentence)
#     sentenceColor <- as.character(sentence)
#     sentenceColor <- gsub(l,paste0(" <font color='#cd5c5c'><b> ",l," </b></font> ") , sentenceColor, "<br />" )
# 
# 
#     paste0("<font size='+1'>",sentenceColor,"</font>")
#   })
# 
# 
# 
# 
#   output$translation <- renderText({
#     paste0("<i>", input$trans,"</i>")
#   })
# 
#   output$Citation <- renderText({
#     input$Edit
#     sample <- read.csv("./data/ConcordancesReady.csv", stringsAsFactors = F)
#     sample[is.na(sample)] <- ""
# 
#     lemma <- sample[sample$ref==input$cit,9]
#     l <-tolower(lemma)
#     sentence <- sample[sample$ref==input$cit,12]
#     sentenceColor <- as.character(sentence)
#     sentenceColor <- paste0("<font size = '+2'> ",sentenceColor,"</font>")
#     sentenceColor <- gsub(l,paste0(" <font color='#cd5c5c'><b> ",l," </b></font> ") , sentenceColor )
#     sentenceColor <- gsub(" (\\d+) "," <font color='#4682b4' size='-1'> \\1 </font> ", sentenceColor )
# 
#     paste0(sentenceColor)
#   })
# 
# 
# 
#   output$annotatedDepRelTable <- renderTable({
#     input$Save
#     input$SaveDraft
#     input$Edit
#     sample <- read.csv("./data/ConcordancesReady.csv", stringsAsFactors = F)
#     sample[is.na(sample)] <- ""
#     citation <- sample[sample$ref==input$cit,12]
#     #sample[sample$ref==input$cit,colnames(sample)=="sem.pros"] <- input$SemPros
#     sample[sample$ref==input$cit,colnames(sample)==input$SynDep1] <- str_extract(citation, paste0(input$text1, "\\s([a-z]|[āīūṛḷṇḍñṅḥśṣṭḍṃ])+\\s"))
#     sample[sample$ref==input$cit,colnames(sample)==input$SynDep2] <- str_extract(citation, paste0(input$text2, "\\s([a-z]|[āīūṛḷṇḍñṅḥśṣṭḍṃ])+\\s"))
#     # sample[sample$ref==input$cit,colnames(sample)=="sense"] <- input$Sense
#     # sample[sample$ref==input$cit,colnames(sample)=="subsense"] <- input$Subsense
#     # sample[sample$ref==input$cit,colnames(sample)=="domain"] <- input$Dom
#     # sample[sample$ref==input$cit,colnames(sample)=="sem.field"] <- input$SemField
#     # sample[sample$ref==input$cit,colnames(sample)=="sem.cat"] <- input$SemCat
#     # sample[sample$ref==input$cit,colnames(sample)=="transl"] <- input$trans
#     # sample[sample$ref==input$cit,colnames(sample)==input$ProsRel] <- str_extract(citation, paste0(input$text3, "\\s([a-z]|[āīūṛḷṇḍñṅḥśṣṭḍṃ])+\\s"))
#     # sample[sample$ref==input$cit,colnames(sample)=="uncertainty"] <- input$Uncertainty
#     # sample[sample$ref==input$cit,colnames(sample)=="Notes"] <- input$Notes
#     sample[sample$ref==input$cit,c(19:22)]
#     #sample[sample$ref==input$cit,c(19:22)]
#   })
# 
#   output$annotatedDepRelTable2 <- renderTable({
#     input$Save
#     input$SaveDraft
#     input$Edit
#     sample <- read.csv("./data/ConcordancesReady.csv", stringsAsFactors = F)
#     sample[is.na(sample)] <- ""
#     citation <- sample[sample$ref==input$cit,12]
#     #sample[sample$ref==input$cit,colnames(sample)=="sem.pros"] <- input$SemPros
#     sample[sample$ref==input$cit,colnames(sample)==input$SynDep1] <- str_extract(citation, paste0(input$text1, "\\s([a-z]|[āīūṛḷṇḍñṅḥśṣṭḍṃ])+\\s"))
#     sample[sample$ref==input$cit,colnames(sample)==input$SynDep2] <- str_extract(citation, paste0(input$text2, "\\s([a-z]|[āīūṛḷṇḍñṅḥśṣṭḍṃ])+\\s"))
#     # sample[sample$ref==input$cit,colnames(sample)=="sense"] <- input$Sense
#     # sample[sample$ref==input$cit,colnames(sample)=="subsense"] <- input$Subsense
#     # sample[sample$ref==input$cit,colnames(sample)=="domain"] <- input$Dom
#     # sample[sample$ref==input$cit,colnames(sample)=="sem.field"] <- input$SemField
#     # sample[sample$ref==input$cit,colnames(sample)=="sem.cat"] <- input$SemCat
#     # sample[sample$ref==input$cit,colnames(sample)=="transl"] <- input$trans
#     # sample[sample$ref==input$cit,colnames(sample)==input$ProsRel] <- str_extract(citation, paste0(input$text3, "\\s([a-z]|[āīūṛḷṇḍñṅḥśṣṭḍṃ])+\\s"))
#     # sample[sample$ref==input$cit,colnames(sample)=="uncertainty"] <- input$Uncertainty
#     # sample[sample$ref==input$cit,colnames(sample)=="Notes"] <- input$Notes
#     sample[sample$ref==input$cit,c(23:26)]
#     #sample[sample$ref==input$cit,c(19:22)]
#   })
# 
#   output$annotatedProsRelTable <- renderTable({
#     input$Save
#     input$SaveDraft
#     input$Edit
#     sample <- read.csv("./data/ConcordancesReady.csv", stringsAsFactors = F)
#     sample[is.na(sample)] <- ""
#     citation <- sample[sample$ref==input$cit,12]
#     sample[sample$ref==input$cit,colnames(sample)=="sem.pros"] <- input$SemPros
#     sample[sample$ref==input$cit,colnames(sample)=="sense"] <- input$Sense
#     sample[sample$ref==input$cit,colnames(sample)=="subsense"] <- input$Subsense
#     sample[sample$ref==input$cit,colnames(sample)=="domain"] <- input$Dom
#     sample[sample$ref==input$cit,colnames(sample)=="sem.field"] <- input$SemField
#     sample[sample$ref==input$cit,colnames(sample)=="sem.cat"] <- input$SemCat
#     sample[sample$ref==input$cit,colnames(sample)=="transl"] <- input$trans
#     sample[sample$ref==input$cit,colnames(sample)==input$ProsRel] <- str_extract(citation, paste0(input$text3, "\\s([a-z]|[āīūṛḷṇḍñṅḥśṣṭḍṃ])+\\s"))
# 
#     sample[sample$ref==input$cit,c(27:32)]
#   })
# 
#   output$annotatedConcRelTable <- renderTable({
#     input$Save
#     input$SaveDraft
#     input$Edit
#     sample <- read.csv("./data/ConcordancesReady.csv", stringsAsFactors = F)
#     sample[is.na(sample)] <- ""
#     citation <- sample[sample$ref==input$cit,12]
#     sample[sample$ref==input$cit,colnames(sample)==input$ConcRel] <- str_extract(citation, paste0(input$text4, "\\s([a-z]|[āīūṛḷṇḍñṅḥśṣṭḍṃ])+\\s"))
# 
#     sample[sample$ref==input$cit,c(41:47)]
#   })
# 
#   output$annotatedNotesTable <- renderTable({
#     input$Save
#     input$SaveDraft
#     input$Edit
#     sample <- read.csv("./data/ConcordancesReady.csv", stringsAsFactors = F)
#     sample[is.na(sample)] <- ""
#     citation <- sample[sample$ref==input$cit,12]
#     sample[sample$ref==input$cit,colnames(sample)=="uncertainty"] <- input$Uncertainty
#     sample[sample$ref==input$cit,colnames(sample)=="Notes"] <- input$Notes
# 
#     sample[sample$ref==input$cit,c(48:50,33,34)]
#   })
# 
#   output$wordcloudIntro <- renderText({
#     paste0("<font size ='+2'> most commenly shared <font color='#4682b4'>cotext items</font> in this citations </font>")
#   })
# 
#   output$CitationSetIntro <-  renderText({
#     paste0("<font size ='+2'> explore the <font color='#4682b4'>whole dataset</font><br />
#           <font size ='1'>diplay more citations (if available) with the 'show entries' menu below.
#            <br />search the dataset with the search box to the right below</font>")
#   })
# 
#   output$CitationSet <- DT::renderDataTable({
#     input$Save
#     input$SaveDraft
#     input$Edit
#     sample <- read.csv("./data/ConcordancesReady.csv", stringsAsFactors = F)
#     sample[is.na(sample)] <- ""
#     sample
#     #
# 
#   },options = list(pageLength = 1))
# 
# 
#   output$SimilarityIntro <- renderText({
#     CitCotext <- sample[sample$ref==input$cit,38]
#     CitCotextTok <- tokenize_words(CitCotext, lowercase=F)
#     CitCotextTok <- unlist(CitCotextTok)
#     stopwords <- c("TATR", "TAD","TAD","CET","AYAM","AYAṂ","IDAM","IDAṂ","EDAM","EDAṂ","YATHĀ","TATHĀGAT","BODHISATTV")
#     WordsToSearch <- setdiff(CitCotextTok, stopwords)
# 
#     PotentialSimilar <- c()
#     OtherCitsDF <- sample[-(sample$ref==input$cit),]
#     for (i in WordsToSearch){
#       grepped <- grep(paste0(" ",i," "), sample[,38])
#       if(length(grepped)>0){
#         greppedCit <- sample[c(grepped),35]
#         PotentialSimilar <- c(PotentialSimilar,greppedCit)
#       }
#     }
#     if (!is.null(PotentialSimilar)){
#       SharedCotextDF <- data.frame(table(PotentialSimilar))
#       if (nrow(SharedCotextDF[SharedCotextDF$Freq >1,])>0){
#         SharedCotextDF <- SharedCotextDF[SharedCotextDF$Freq >1,]
#         SharedCotextDF <-  SharedCotextDF[order(SharedCotextDF$Freq,decreasing=T),]
#         toDisplay <- OtherCitsDF[OtherCitsDF$ref %in% SharedCotextDF$PotentialSimilar, c(35,11)]
#         shared <- SharedCotextDF[SharedCotextDF$PotentialSimilar %in% toDisplay$ref,]
#         toDisplay <- data.frame(shared[2],toDisplay)
#         colnames(toDisplay) <- c("shared","Potentially.Similar","Cit")
#         toDisplay$sharedCotext <- ""
#         toDisplay$wordCount <- ""
#         for (i in 1:length(toDisplay$Cit)){
#           testTok <- unlist(tokenize_words(toDisplay$Cit[i], lowercase=F))
#           toDisplay$wordCount[[i]] <- length(testTok)
#           toDisplay$sharedCotext[[i]] <-  paste(intersect(WordsToSearch, testTok), collapse=" ")
#         }
#         paste0("<font size ='+2'>this citation <font color='#4682b4'>shares cotext</font> items with:</font>
#                <br /> <font size ='-1'>you can diplay more citations (if available) with the 'show entries' menu below. You can search the dataset use the search box to the right below</font>")
#       }else{
#         paste0("<font size ='+2' color='#cd5c5c'>no similar citations detected</font>")
# 
#       }
# 
#     }
#   })
# 
# 
# 
#   output$SimilarCit <-  DT::renderDataTable({
#     CitCotext <- sample[sample$ref==input$cit,38]
#     CitCotextTok <- tokenize_words(CitCotext, lowercase=F)
#     CitCotextTok <- unlist(CitCotextTok)
#     stopwords <- c("TATR", "TAD","TAD","CET","AYAM","AYAṂ","IDAM","IDAṂ","EDAM","EDAṂ","YATHĀ","TATHĀGAT","BODHISATTV")
#     WordsToSearch <- setdiff(CitCotextTok, stopwords)
# 
#     PotentialSimilar <- c()
#     OtherCitsDF <- sample[-(sample$ref==input$cit),]
#     for (i in WordsToSearch){
#       grepped <- grep(paste0(" ",i," "), sample[,38])
#       if(length(grepped)>0){
#         greppedCit <- sample[c(grepped),35]
#         PotentialSimilar <- c(PotentialSimilar,greppedCit)
#       }
#     }
#     if (!is.null(PotentialSimilar)){
#       SharedCotextDF <- data.frame(table(PotentialSimilar))
#       if (nrow(SharedCotextDF[SharedCotextDF$Freq >1,])>0){
#         SharedCotextDF <- SharedCotextDF[SharedCotextDF$Freq >1,]
#         SharedCotextDF <-  SharedCotextDF[order(SharedCotextDF$Freq,decreasing=T),]
#         toDisplay <- OtherCitsDF[OtherCitsDF$ref %in% SharedCotextDF$PotentialSimilar, c(35,11)]
#         shared <- SharedCotextDF[SharedCotextDF$PotentialSimilar %in% toDisplay$ref,]
#         toDisplay <- data.frame(shared[2],toDisplay)
#         colnames(toDisplay) <- c("sharedCount","SharedWith","Cit")
#         toDisplay$sharedCotext <- ""
#         toDisplay$wordCount <- ""
#         for (i in 1:length(toDisplay$Cit)){
#           testTok <- unlist(tokenize_words(toDisplay$Cit[i], lowercase=F))
#           toDisplay$wordCount[[i]] <- length(testTok)
#           toDisplay$sharedCotext[[i]] <-  paste(intersect(WordsToSearch, testTok), collapse=" ")
#         }
# 
#         toDisplay$perc <-  as.numeric(toDisplay$sharedCount)/as.numeric(toDisplay$wordCount)
# 
#         toDisplay
# 
#       }
#     }
#   },options = list(pageLength = 1))
# 
#   output$cotextWordcloud <-  renderPlot({
#     CitCotext <- sample[sample$ref==input$cit,38]
#     CitCotextTok <- tokenize_words(CitCotext, lowercase=F)
#     CitCotextTok <- unlist(CitCotextTok)
#     stopwords <- c("TATR", "TAD","TAD","CET","AYAM","AYAṂ","IDAM","IDAṂ","EDAM","EDAṂ","YATHĀ","TATHĀGAT","BODHISATTV")
#     WordsToSearch <- setdiff(CitCotextTok, stopwords)
# 
#     PotentialSimilar <- c()
#     OtherCitsDF <- sample[-(sample$ref==input$cit),]
#     for (i in WordsToSearch){
#       grepped <- grep(paste0(" ",i," "), sample[,38])
#       if(length(grepped)>0){
#         greppedCit <- sample[c(grepped),35]
#         PotentialSimilar <- c(PotentialSimilar,greppedCit)
#       }
#     }
#     if (!is.null(PotentialSimilar)){
#       SharedCotextDF <- data.frame(table(PotentialSimilar))
#       if (nrow(SharedCotextDF[SharedCotextDF$Freq >1,])>0){
#         SharedCotextDF <- SharedCotextDF[SharedCotextDF$Freq >1,]
#         SharedCotextDF <-  SharedCotextDF[order(SharedCotextDF$Freq,decreasing=T),]
#         toDisplay <- OtherCitsDF[OtherCitsDF$ref %in% SharedCotextDF$PotentialSimilar, c(35,11)]
#         shared <- SharedCotextDF[SharedCotextDF$PotentialSimilar %in% toDisplay$ref,]
#         toDisplay <- data.frame(shared[2],toDisplay)
#         colnames(toDisplay) <- c("shared","Potentially.Similar","Cit")
#         toDisplay$sharedCotext <- ""
#         toDisplay$wordCount <- ""
#         for (i in 1:length(toDisplay$Cit)){
#           testTok <- unlist(tokenize_words(toDisplay$Cit[i], lowercase=F))
#           toDisplay$wordCount[[i]] <- length(testTok)
#           toDisplay$sharedCotext[[i]] <-  paste(intersect(WordsToSearch, testTok), collapse=" ")
#         }
#         WordFreqs <- DataFrameTokens_CAPS2(toDisplay$sharedCotext)
#         wordcloud(WordFreqs$Var1,WordFreqs$Freq, color="steelblue")
#       }else{
#         WordFreqs <-DataFrameTokens_CAPS2("ZERO ZERO COTEXT COTEXT COTEXT  COMMON COMMON")
#         wordcloud(WordFreqs$Var1,WordFreqs$Freq, color="indianred",rot.per=.7)
#       }
# 
#     }
#   })
# 
#   output$workSummary <- DT::renderDataTable({
#     data <- PROGRESS()
# 
#     ProgressDF <- data %>%
#       group_by(title, progress) %>%
#       summarise(count = n() / nrow(.) )
#     ProgressDF %>%
#       mutate("20%done"= ifelse(count >0.2, TRUE, FALSE ))
#   })
# 
#   output$downloadData <- downloadHandler(
# 
# 
#     filename = function() {
#       lemma <- sample[sample$ref==input$cit,9]
#       paste("~/Desktop/",lemma,"_dataset", Sys.time(), ".csv", sep="")
#     },
# 
# 
#     content = function(file) {
#       sampleForcsv <- read.csv("./data/ConcordancesReady.csv", stringsAsFactors = F)
#       sampleForcsv$is_object_or_patient_of <- gsub("\\d+ ","", sampleForcsv$is_object_or_patient_of)
#       sampleForcsv$takes_as_object_or_patient <- gsub("\\d+ ","", sampleForcsv$takes_as_object_or_patient)
#       sampleForcsv$is_subject_or_agent_of <- gsub("\\d+ ","", sampleForcsv$is_subject_or_agent_of)
#       sampleForcsv$takes_as_subject_or_agent <- gsub("\\d+ ","", sampleForcsv$takes_as_subject_or_agent)
#       sampleForcsv$takes_as_oblique <- gsub("\\d+ ","", sampleForcsv$takes_as_oblique)
#       sampleForcsv$is_oblique_of <- gsub("\\d+ ","", sampleForcsv$is_oblique_of)
#       sampleForcsv$is_modified_by <- gsub("\\d+ ","", sampleForcsv$is_modified_by)
#       sampleForcsv$modifies <- gsub("\\d+ ","", sampleForcsv$modifies)
#       sampleForcsv$is_listed_with <- gsub("\\d+ ","", sampleForcsv$is_listed_with)
#       sampleForcsv$is_contrasted_with <- gsub("\\d+ ","", sampleForcsv$is_contrasted_with)
#       sampleForcsv$is_glossed_by <- gsub("\\d+ ","", sampleForcsv$is_glossed_by)
#       sampleForcsv$glosses <- gsub("\\d+ ","", sampleForcsv$glosses)
#       sampleForcsv$is_juxtaposed_to <- gsub("\\d+ ","", sampleForcsv$is_juxtaposed_to)
#       sampleForcsv$is_object_or_patient_of <- gsub("\\s?@\\s?","", sampleForcsv$is_object_or_patient_of)
#       sampleForcsv$takes_as_object_or_patient <- gsub("\\s?@\\s?","", sampleForcsv$takes_as_object_or_patient)
#       sampleForcsv$is_subject_or_agent_of <- gsub("\\s?@\\s?","", sampleForcsv$is_subject_or_agent_of)
#       sampleForcsv$takes_as_subject_or_agent <- gsub("\\s?@\\s?","", sampleForcsv$takes_as_subject_or_agent)
#       sampleForcsv$takes_as_oblique <- gsub("\\s?@\\s?","", sampleForcsv$takes_as_oblique)
#       sampleForcsv$is_oblique_of <- gsub("\\s?@\\s?","", sampleForcsv$is_oblique_of)
#       sampleForcsv$is_modified_by <- gsub("\\s?@\\s?","", sampleForcsv$is_modified_by)
#       sampleForcsv$modifies <- gsub("\\s?@\\s?","", sampleForcsv$modifies)
#       sampleForcsv$is_listed_with <- gsub("\\s?@\\s?","", sampleForcsv$is_listed_with)
#       sampleForcsv$is_contrasted_with <- gsub("\\s?@\\s?","", sampleForcsv$is_contrasted_with)
#       sampleForcsv$is_glossed_by <- gsub("\\s?@\\s?","", sampleForcsv$is_glossed_by)
#       sampleForcsv$glosses <- gsub("\\s?@\\s?","", sampleForcsv$glosses)
#       sampleForcsv$is_juxtaposed_to <- gsub("\\s?@\\s?","", sampleForcsv$is_juxtaposed_to)
#       sampleForcsv$is_object_or_patient_of <- gsub("\\s?-.*?$","", sampleForcsv$is_object_or_patient_of)
#       sampleForcsv$takes_as_object_or_patient <- gsub("\\s?-.*?$","", sampleForcsv$takes_as_object_or_patient)
#       sampleForcsv$is_subject_or_agent_of <- gsub("\\s?-.*?$","", sampleForcsv$is_subject_or_agent_of)
#       sampleForcsv$takes_as_subject_or_agent <- gsub("\\s?-.*?$","", sampleForcsv$takes_as_subject_or_agent)
#       sampleForcsv$takes_as_oblique <- gsub("\\s?-.*?$","", sampleForcsv$takes_as_oblique)
#       sampleForcsv$is_oblique_of <- gsub("\\s?-.*?$","", sampleForcsv$is_oblique_of)
#       sampleForcsv$is_modified_by <- gsub("\\s?-.*?$","", sampleForcsv$is_modified_by)
#       sampleForcsv$modifies <- gsub("\\s?-.*?$","", sampleForcsv$modifies)
#       sampleForcsv$is_listed_with <- gsub("\\s?-.*?$","", sampleForcsv$is_listed_with)
#       sampleForcsv$is_contrasted_with <- gsub("\\s?-.*?$","", sampleForcsv$is_contrasted_with)
#       sampleForcsv$is_glossed_by <- gsub("\\s?-.*?$","", sampleForcsv$is_glossed_by)
#       sampleForcsv$glosses <- gsub("\\s?-.*?$","", sampleForcsv$glosses)
#       sampleForcsv$is_juxtaposed_to <- gsub("\\s?-.*?$","", sampleForcsv$is_juxtaposed_to)
#       write.csv(sampleForcsv, file)
# 
#     }
# 
#   )
# 
# 
# }
# 
# 
# 
# # shinyApp(ui, server)
# 
# 
# shinyApp(ui, server)