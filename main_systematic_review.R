######'SCRIPT DESCRIPTION ----
#'
#'This script is used to analyze the results of a systematic review regarding the occurrence of human viruses on fomites in the environment.
#'
#'There are eight categories for the systematic review results:  
#'1. Sampling location (e.g., Hospital)
#'2. Implement type used to take sample (e.g., Cotton)
#'3. Pre-moisturizer used during sampling (e.g., Lysis buffer)
#'4. Eluent used during sampling (e.g., Lysis buffer)
#'5. Area of fomite swabbed (e.g., >100 cm^2)
#'6. High touch surfaces sampled (Fomite or inanimate object) type sampled (e.g., Electronic accessory)
#'7. Viral target (e.g. Norovirus)
#'8. Fomite results comparison with health of community (e.g., Quantitative)
#'
#'
#'#Location, implement, pre-moist, eluent, area swabbed, high touch surfaces, viral target, outcome classification
#'
#'Four main actions are performed in this script: 
#'
#'#Quantification and visualization
#'  1. Descriptive data: Quantify and visualize the number of datasets in each category. 
#'  2. Positivity rate data: Classify and visualize the positivity rate in each category. 
#'
#'#Statistical Analysis
#'  3. Univariate analysis: Test if positivity rate is associated with the virus type, sampling location, or area of fomite swabbed.
#'  4. Multivariate model: Investigate how virus type, sampling location, and area swabbed are associated with the presence of virus on fomites.
#'
######LOADING LIBRARIES ----
library(here)
library(openxlsx)
library(writexl)
library(stringr) 
library(pROC)
library(plotROC)
library(ResourceSelection)
library(rstatix)
library(conover.test)
library("ggplot2")
library("scales") 
library(EnvStats) 

here::i_am("main_systematic_review.R") 

######FUNCTIONS ----

##Data cleaning functions ---

countSingleOnlyDF<-function(column) {
  
  ##'FUNCTION: countSingleOnlyDF function ---
  #'
  #'This function counts how many occurrences of a category happen in a single column. 
  #'  Args:
  #'    column: A column containing unique occurrences in each row.
  #'    
  #'  Returns:
  #'    CountTableSortedDF: A dataframe with labels and number of times each label appears.
  
  CountTable <- table(column, useNA = 'no')
  CountTableSorted <- CountTable[order(CountTable,decreasing = TRUE)] #sorting
  CountTableSortedDF<-data.frame(CountTableSorted) #convert table to data frame
  colnames(CountTableSortedDF) <- c('labels','values')
  return(CountTableSortedDF)
}


countMultipleOnly <- function(column_diff) {
  
  ##'FUNCTION: countMultipleOnly function ---
  #'
  #'This function separates the words from a row contain a list.
  #'  Args:
  #'    column_diff: A list. 
  #'    
  #'  Returns:
  #'    words: A string of the words separated. 

words = c()

for (i in 1: length(column_diff)) 
{
    if (!is.na(column_diff[i])) 
  {
    temp_list <- as.character(strsplit(column_diff[i], '\\;')[[1]])
      words <- c(words,temp_list) 
  }     
  }
return(words)
}


countMultipleOnlyDF<-function(column_diff) {
  
  ##'FUNCTION: countMultipleOnlyDF function ---
  #'
  #'This function counts how many occurrences of a category happen in a column in which each row contain a list.
  #'  Args:
  #'    column_diff: A column in which each row contains a list. 
  #'    
  #'  Returns:
  #'     CountTableSortedDF: A dataframe with labels and number of times each label appears.
  
  CountString <- countMultipleOnly(column_diff)
  CountTable<-table(unlist(strsplit(CountString, ";")))  #splitting per ";"
  CountTableSorted <- CountTable[order(CountTable,decreasing = TRUE)] #sorting
  CountTableSortedDF<-data.frame(CountTableSorted) #convert table to data frame
  colnames(CountTableSortedDF) <- c('labels','values')
  return(CountTableSortedDF)
}


countMultiple <- function(column,column_diff) { 
  
  ##'FUNCTION: countMultiple function ---
  #'
  #'This function separates the words from a row contain a list or a single word.
  #'  Args:
  #'    column_diff: A list. 
  #'    
  #'  Returns:
  #'    words: A string of the words separated. 
  
  words = c()
  
  for (i in 1: length(column_diff)) 
  {
    
    
    if(is.na(column[i]) || column[i] != c('Multiple')){
      words <- c(words,column[i]) 
    }
    
    if (!is.na(column_diff[i])) 
    {
            temp_list <- as.character(strsplit(column_diff[i], '\\;')[[1]]) #the categories are divided by ";"
      words <- c(words,temp_list) 
      
    }   
    
  }
  return(words)
}

countMultipleDF<-function(column,column_diff) {
  
  ##'FUNCTION: countMultipleDF function ---
  #'
  #'This function counts how many occurrences of a category happen in a column in which each row contain a list or a single word.
  #'  Args:
  #'    column_diff: A column in which each row contains a list. 
  #'    
  #'  Returns:
  #'     CountTableSortedDF: A dataframe with labels and number of times each label appears.
  
  
  CountString <- countMultiple(column, column_diff)
  CountTable<-table(unlist(strsplit(CountString, ";")))  #splitting per ";"
  CountTableSorted <- CountTable[order(CountTable,decreasing = TRUE)] #sorting
  CountTableSortedDF<-data.frame(CountTableSorted) #convert table to data frame
  colnames(CountTableSortedDF) <- c('labels','values')
  return(CountTableSortedDF)
}

##Data classification functions ---

ReClassify <- function(DF, wordToClassify) {
  
  ##'FUNCTION: ReClassify function ---
  #'
  #'This function takes a single word and maps it to the label from a separate DF.
  #'  Args:
  #'    wordToClassify: A single word to be classified.
  #'    DF: Dataframe containing the classification corresponding to the wordToClassify.
  #'    
  #'  Returns:
  #'     wordToClassify: The new word post classification.
  
  wordToClassify <- tolower(wordToClassify)
  
  for (i in 1:nrow(DF)) {
    
    inputs <- tolower(DF[i,2])
    
    temp_list <- as.character(strsplit(inputs, '\\;')[[1]])
   
    if (wordToClassify %in% temp_list) {
     return(DF[i,1]) 
      }         
  } 
  if(is.na(wordToClassify)){
    return(wordToClassify)
  }
  
    return(paste0(wordToClassify, ' WAS NOT CLASSIFIED'))                  
}


ReClassifyList <- function(DF,column) {
  
  ##'FUNCTION: ReClassifyList function ---
  #'
  #'This function takes a list of words word and maps them to the label from a separate DF.
  #'  Args:
  #'    column: A list of words to be classified.
  #'    DF: Dataframe containing the classification corresponding to the list.
  #'    
  #'  Returns:
  #'     labels: The new list of words post classification.
  
  labels = c()
  
  for (i in 1: length(column)) 
  {
    
    #get words presplit
    input_words_presplit = column[i] 
    
    #splits the words into a list
    input_words_list <- as.character(strsplit(input_words_presplit, '\\;')[[1]])
    
    #loop over all the words in our list
    for(j in 1:length(input_words_list)){
      
      #grab each word one-by-one from our list
      input_word = input_words_list[j]
      
      #classify each word
      labled_word = ReClassify(DF,input_word)
      
      #add the class-lable to our main list
      labels <- c(labels,labled_word) 
      
    }
    
  }
  
  return(labels)
}

##Data visualization functions ---

PlotBarGraph<-function(DataFrame,title) {
  
  ##'FUNCTION: PlotBarGraph function ---
  #'
  #'This function plots a bar graph. Use for Descriptive Data section.
  #'
  #'  Args:
  #'    DataFrame: Dataframe containing the labels and values to be plotted.
  #'    title: Title for the bar graph.
  #'    
  #'  Returns:
  #'     barplot: A bar graph.
  

  #Extract color palette
  n1 <- length(unique(DataFrame$labels)) #Amount of default colors
  hex_codes1 <- hue_pal()(n1) #Identify hex codes
  
 barplot<- ggplot(DataFrame, aes(x = labels, y = values, fill = labels)) +  #Plot with values on top
      geom_bar(stat = "identity", colour="black") +
      geom_text(aes(label = values), 
                vjust = -0.3, 
                size =4)+
    scale_fill_manual(values = hex_codes1) +  #Use the custom color palette
   scale_y_continuous(expand = expansion(mult = c(0.05, 0.1))) + #expands y-axis
       theme(legend.title = element_blank(), #Delete legend title
        legend.position="none",  
        panel.background = element_rect(fill= "white", colour = "black", linewidth=0.5),
        text=element_text(size=20),
        axis.title.x= element_blank(), #Delete x axis text
        axis.text.x=element_text(size=12, angle = 45, hjust =1), 
        legend.text = element_text(size=12),
        plot.title = element_text(hjust = 0.5))+ #Plot tittle centered  
        xlab(NULL) + 
      ggtitle(title)
      
  return(barplot)
}

DualVariableGraph<-function(xColumn,yColumn,title) {
  
  ##'FUNCTION: DualVariableGraph function ---
  #'
  #'This function plots a box plot with jitter. Use for Positivity Rate data.
  #'
  #'  Args:
  #'    xColumn: Column containing x-variable.
  #'    yColumn:Column containing y-variable.
  #'    title: Title for the box plot graph.
  #'    
  #'  Returns:
  #'     barplot: A box plot graph.
  
  DataFrame<-data.frame(x= xColumn,y = yColumn)
  
    #extract color palette
  n1 <- length(unique(DataFrame$x))#Amount of default colors
  hex_codes1 <- hue_pal()(n1) #Identify hex codes
  
   DataFrame$x <- factor(DataFrame$x, levels = names(sort(table(DataFrame$x), decreasing = TRUE)))
    
   dualplot<- ggplot(DataFrame, aes(x, y, fill = x)) +
      geom_boxplot(outlier.shape = 21, outlier.size = 1.5, outlier.alpha = 0.5) +
     geom_jitter(aes(fill= x),shape = 21, width = 0.08, alpha = 0.5) +
     scale_fill_manual(values = hex_codes1) +  # Use the custom color palette
        labs(x = NULL, y = "Positivity Rate") +
    labs (x = NULL, y = "Positivity Rate")+ #CHANGE y- axis TITLE 
    theme(text=element_text(size=20),
          legend.text=element_text(size=15),
          axis.title.x=element_blank(),#Deleting x axis text
          axis.text.x=element_text(size=12, angle = 45, hjust =1),
          axis.ticks.x=element_blank(),
          plot.title = element_text(hjust = 0.5))+ #plot tittle centered
     stat_n_text(size=3) + 
    theme(legend.title = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill= "white", colour = "black", linewidth=1))+
    theme(legend.position = "none") +
     ggtitle(title) #graph tittle
  

  return(dualplot)
}

######DATA PREPARATION ----

###Import systematic review data results ---

excel_file_path <- here("my_excel_file.xlsx")  # Replace "my_excel_file.xlsx" with the name of your Excel file

# Read the Excel file
data <- read_xlsx(excel_file_path)

xlsx_source<-"/Users/winni/Documents/Clean_Code/Chapter 3/Fomites_Systematic_Review_SDR_Data_20230511_CLEAN.xlsx" 
data<-read.xlsx(xlsx_source, sheet = "Data")

#Classification Sheet Source
classification_sheet<-"/Users/winni/Documents/Clean_Code/Chapter 3/Fomites_Systematic_Review_SDR_Data_20230511_CLASSIFICATION_CLEAN.xlsx"

######DATA QUANTIFICATION AND VISUALIZATION: DESCRIPTIVE DATA ----

  #1) LOCATION ---
  location_class<-read.xlsx(classification_sheet, sheet = "Location_Classify")
  location_type<- ReClassifyList(location_class,data$Location) #Classification
  location_type_num<- countSingleOnlyDF(location_type) #Quantification
  location_type_plot <-PlotBarGraph(location_type_num,"Sampling Location") #Visualization
  
  #2) IMPLEMENT TYPE ---
  implement_class<-read.xlsx(classification_sheet, sheet = "Implement_Classify")
  implement_type<- ReClassifyList(implement_class,data$Implement) #Classification
  implement_num<-countSingleOnlyDF(implement_type) #Quantification
  implement_plot <-PlotBarGraph(implement_num,"Implement Type") #Visualization
  
  #3) PRE-MOISTURIZER TYPE --
  eluent_prem_class<-read.xlsx(classification_sheet, sheet = "Eluent_Prem_Classify")
  prem_type<- ReClassifyList(eluent_prem_class,data$Pre.Moisturizer) #Classification
  prem_type_num<- countSingleOnlyDF(prem_type) #Quantification
  prem_type_plot <-PlotBarGraph(prem_type_num,"Pre-Moisturizer Type") #Visualization
  
  #4) ELUENT TYPE --
  eluent_class<-read.xlsx(classification_sheet, sheet = "Eluent_Prem_Classify")
  eluent_type<- ReClassifyList(eluent_class,data$Eluent) #Classification
  eluent_type_num<- countSingleOnlyDF(eluent_type) #Quantification
  eluent_type_plot <-PlotBarGraph(eluent_type_num,"Eluent Type") #Visualization
  
  #5) AREA SWABBED---
  area_class<-read.xlsx(classification_sheet, sheet = "Area_Classify")
  area_type<- ReClassifyList(area_class,data$Area.Swabbed) #Classification
  area_type_num<- countSingleOnlyDF(area_type) #Quantification
  area_type_plot <-PlotBarGraph(area_type_num,"Area Swabbed") 
  
  #6) HIGH TOUCH SURFACES (HTS)  --- 
  
  #HTS SAMPLED -
    HTS_class<-read.xlsx(classification_sheet, sheet = "HTS_Classify")
    HTS_type<- ReClassifyList(HTS_class,data$Fomites.Sampled) #Classification
    
    #Quantification
        HTS_num<- countSingleOnlyDF(HTS_type) 
      
        #COUNT NUMER OF HTS SAMPLE TYPE PER DATASET
        HTS_Countperdataset <- data.frame(data$Full.Citation, location_type ,data$Fomites.Sampled) #create a new df
        # Create empty column
        HTS_Countperdataset$Total_HTS_type_count <- 0
        
        # Loop through each row of the data frame and count semicolons in All_HTS column
        for (i in 1:nrow(HTS_Countperdataset)) {
          num_semicolons <- sum(str_count(HTS_Countperdataset$data.Fomites.Sampled[i], ";"))
          HTS_Countperdataset$Total_HTS_type_count[i] <- num_semicolons + 1
        }
        
        #summary 
        HTS_Count_summary <-summary(HTS_Countperdataset$Total_HTS_type_count)
        
        #COUNT THE NUMBER OF DATASETS THAT HAVE AT LEAST ONE SAMPLE TYPE SAMPLED 
        HTS_Count_atleastone <- data.frame(labels= HTS_num$labels, values = c(0))#create a new df
        
        # Loop through each row of the data frame and count if each sample type was sampled at least once
        for (i in 1:nrow(HTS_Countperdataset)) {
          for (j in 1:nrow(HTS_Count_atleastone)) { 
            templist<-ReClassifyList(HTS_class, HTS_Countperdataset$data.Fomites.Sampled[i])
            templist_collapsed <- paste(templist, collapse = ";")
            HTS_Countperdataset$HTS_Reclassified[i] <- templist_collapsed
               if (any(grepl(HTS_Count_atleastone$labels[j], templist))) {
            HTS_Count_atleastone$values[j] <-HTS_Count_atleastone$values[j] + 1
        }
        }
        }
    
      HTS_plot <-PlotBarGraph(HTS_Count_atleastone,"Fomites Sampled") #Visualization
      
      #HTS FOUND POSITIVE - 
      HTS_class<-read.xlsx(classification_sheet, sheet = "HTS_Classify")
      pos_HTS_type<- ReClassifyList(HTS_class,data$Fomites.Positive) #Classification
      
      
      #Quantification
      Pos_HTS_num<- countSingleOnlyDF(pos_HTS_type)
      
        #COUNT THE NUMBER OF DATASETS THAT HAVE AT LEAST ONE SAMPLE TYPE POSITIVE 
        Pos_HTS_Countperdataset <- data.frame(data$Full.Citation, location_type, data$Fomites.Positive) #create a new df
        Pos_HTS_Count_atleastone <- data.frame(labels= Pos_HTS_num$labels, values = c(0))#create a new df
        
        # Loop through each row of the data frame and count if each sample type was sampled at least once
        for (i in 1:nrow(Pos_HTS_Countperdataset)) {
          for (j in 1:nrow(Pos_HTS_Count_atleastone)) { 
            pos_templist<-ReClassifyList(HTS_class, Pos_HTS_Countperdataset$data.Fomites.Positive[i])
            pos_templist_collapsed <- paste(pos_templist, collapse = ";")
            Pos_HTS_Countperdataset$HTS_Reclassified[i] <- pos_templist_collapsed
            if (any(grepl(Pos_HTS_Count_atleastone$labels[j], pos_templist))) {
              Pos_HTS_Count_atleastone$values[j] <-Pos_HTS_Count_atleastone$values[j] + 1
            }
          }
        }
        
      Pos_HTS_plot <-PlotBarGraph(Pos_HTS_Count_atleastone,"Positive Fomites") #Visualization
        
  
  #7) VIRAL TARGET --
  target_num<-countMultipleOnlyDF(data$Virus) #Quantification
  target_plot <-PlotBarGraph(target_num,"Viral Target") #Visualization
  
    #VIRAL TARGET/PER TYPE [Enveloped vs Non-enveloped]--
    Virus_class<-read.xlsx(classification_sheet, sheet = "Virus_Classify")
    target_type<- ReClassifyList(Virus_class,data$Virus) #Classification
    target_type_num<- countSingleOnlyDF(target_type) #Quantification
    target_type_plot <-PlotBarGraph(target_type_num,"Viral Target Type") #Visualization
    
    #VIRAL TARGET/PER FAMILY--
    Virus_fam<-read.xlsx(classification_sheet, sheet = "VirusFamily_Classify")
    target_fam<- ReClassifyList(Virus_fam,data$Virus) #Classification
    target_fam_num<- countSingleOnlyDF(target_fam) #Quantification
    target_fam_plot <-PlotBarGraph(target_fam_num,"Viral Family") #Visualization
    
  #8) OUTCOME CLASSIFICATION -- 
  outcome_class<-read.xlsx(classification_sheet, sheet = "Outcome_Classify")
  outcome_type<- ReClassifyList(outcome_class,data$Comparison.to.health.data) #Classification
  outcome_num<- countSingleOnlyDF(outcome_type) #Quantification
  outcome_plot <-PlotBarGraph(outcome_num,"Health Outcome Comparison") #Visualization

######DATA QUANTIFICATION AND VISUALIZATION: POSITIVITY RATE DATA ----
  
  #1) LOCATION ---
  location_class<-read.xlsx(classification_sheet, sheet = "Location_Classify")
  location_type<- ReClassifyList(location_class,data$Location) #Classification
  location_dualplot <-DualVariableGraph(location_type,data$Positivity.Rate,"Positivity Rate per Sampling Location") #Visualization
  
    #Subclassification 
    sublocation_class<-read.xlsx(classification_sheet, sheet = "Location_Subclassify2")
    sublocation_type<- ReClassifyList(sublocation_class,data$Location) #Classification
    sublocation_dualplot <-DualVariableGraph(sublocation_type,data$Positivity.Rate,"Location") #Visualization
  
  #2) IMPLEMENT TYPE ---
  implement_class<-read.xlsx(classification_sheet, sheet = "Implement_Classify")
  implement_type<- ReClassifyList(implement_class,data$Implement.Category) #Classification

  implement_nomissing<-data.frame(var1 = implement_type, posrate= data$Positivity.Rate)
  implement_nomissing<- subset(implement_nomissing, !(var1 == "Not specified")) #Quantification
  implement_nomissing_dualplot <-DualVariableGraph(implement_nomissing$var1,implement_nomissing$posrate,"Implement Type") #Visualization
  
  #3) PRE-MOISTURIZER TYPE --
  eluent_prem_class<-read.xlsx(classification_sheet, sheet = "Eluent_Prem_Classify")
  prem_type<- ReClassifyList(eluent_prem_class,data$Pre.Moisturizer) #Classification

  prem_nomissing<-data.frame(var1 = prem_type, posrate= data$Positivity.Rate)
  prem_nomissing<- subset(prem_nomissing, !(var1 == "Not specified")) #Quantification
  prem_nomissing_dualplot <-DualVariableGraph(prem_nomissing$var1,prem_nomissing$posrate,"Pre-Moisturizer Type") #Visualization
  
  #4) ELUENT TYPE --
  eluent_class<-read.xlsx(classification_sheet, sheet = "Eluent_Prem_Classify")
  eluent_type<- ReClassifyList(eluent_class,data$Eluent) #Classification

  eluent_nomissing<-data.frame(var1 = eluent_type, posrate= data$Positivity.Rate)
  eluent_nomissing<- subset(eluent_nomissing, !(var1 == "Not specified")) #Quantification
  eluent_nomissing_dualplot <-DualVariableGraph(eluent_nomissing$var1,eluent_nomissing$posrate,"Eluent Type") #Visualization
  
  #5) AREA SWABBED---
  area_class<-read.xlsx(classification_sheet, sheet = "Area_Classify")
  area_type<- ReClassifyList(area_class,data$Area.Swabbed) #Classification

  area_nomissing<-data.frame(var1 = area_type, posrate= data$Positivity.Rate)
  area_nomissing<- subset(area_nomissing, !(var1 == "Not specified")) #Quantification
  area_nomissing_dualplot <-DualVariableGraph(area_nomissing$var1,area_nomissing$posrate,"Area") #Visualization
  
  #7) VIRAL TARGET TYPE --
  target_dualplot <-DualVariableGraph(data$Virus,data$Positivity.Rate,"Positivity Rate per Target") #Visualization
  
    #VIRAL TARGET/PER TYPE --
    Virus_class<-read.xlsx(classification_sheet, sheet = "Virus_Classify")
    target_type<- ReClassifyList(Virus_class,data$Virus) #Classification
    target_dualplot <-DualVariableGraph(target_type,data$Positivity.Rate,"Virus Type") #Visualization
    
    #VIRAL TARGET/PER FAMILY--
    Virus_fam<-read.xlsx(classification_sheet, sheet = "VirusFamily_Classify")
    target_fam<- ReClassifyList(Virus_fam,data$Virus) #Classification
    target_fam_dualplot <-DualVariableGraph(target_fam,data$Positivity.Rate,"Virus Family") #Visualization
 
######STATISTICAL ANALYSIS----
    
##UNIVARIATE ANALYSIS---

#Define data frame
df_summary <- data.frame(Positivity.Rate = data$Positivity.Rate, 
                         target_fam = target_fam,
                         target_type = target_type, 
                         location = location_type,
                         sublocation = sublocation_type, 
                         implement = implement_type,
                         eluent = eluent_type,
                         prem = prem_type,
                         area = area_type, 
                         Number.of.Positive.Samples = data$Number.of.Positive.Samples,
                         Number.of.Samples = data$Number.of.Samples)

#Define the significance level
alpha <- 0.05
number_of_tests <-6 

#Calculate the adjusted significance level using the Bonferroni correction
bonferroni_alpha <- alpha / number_of_tests

#Normality test
all_shapiro<- shapiro.test(df_summary$Positivity.Rate)

#Kruskal-Wallis test ---

  #LOCATION
  location_kruskal <- kruskal.test(Positivity.Rate ~ sublocation, data = df_summary)
  location_kruskal_result<-location_kruskal$p.value
  
  #TARGET TYPE
  target_kruskal<- kruskal.test(Positivity.Rate ~ target_type, data = df_summary)
  target_kruskal_result<-target_kruskal$p.value
  
  #AREA TYPE
  area_nomissing<-data.frame(var1 = df_summary$area, posrate= df_summary$Positivity.Rate)
  area_nomissing<- subset(area_nomissing, !(var1 == "Not specified"))
  
  area_kruskal<- kruskal.test(posrate ~ var1, data = area_nomissing)
  area_kruskal_result<-area_kruskal$p.value

#Conover-Iman test ---

  #LOCATION
  location_conover <- conover.test(df_summary$Positivity.Rate, df_summary$sublocation, method="bonferroni", kw=TRUE, label=TRUE, 
                               wrap=FALSE, table=TRUE, list=FALSE, rmc=FALSE, alpha=0.05, altp=FALSE)
  
  #TARGET TYPE
  target_conover <- conover.test(df_summary$Positivity.Rate, df_summary$target_type, method="bonferroni", kw=TRUE, label=TRUE, 
                                  wrap=FALSE, table=TRUE, list=FALSE, rmc=FALSE, alpha=0.05, altp=FALSE)
  
  #AREA TYPE
  area_nomissing<-data.frame(var1 = df_summary$area, posrate= df_summary$Positivity.Rate)
  area_nomissing<- subset(area_nomissing, !(var1 == "Not specified"))
  
  area_conover <- conover.test(area_nomissing$posrate, area_nomissing$var1, method="bonferroni", kw=TRUE, label=TRUE, 
                                 wrap=FALSE, table=TRUE, list=FALSE, rmc=FALSE, alpha=0.05, altp=FALSE)

##MULTIVARIATE MODEL: Logistic regression ---
  data_logr <- data.frame(y = data$Positivity.Rate, 
                          x1 = sublocation_type, 
                          x2 = target_type,
                          x3 = area_type)
  data_logr$y[data_logr$y > 0] <- 1
  data_logr<- subset(data_logr, !(x3 == "Not specified"))
  
  #Summary of 0s and 1s
  total_pos<-sum(data_logr$y)
  
  #Generate model
  fitx3 <- glm(y ~ x1 + x2 + x3, data = data_logr, family = binomial(link = "logit"))
  summary(fitx3)
  
  #Plot ROC Curve
  invisible(plot(roc(factor(ifelse(data_logr$y == 1, 1, 0)), fitted(fitx3)), print.thres = c(.1, .5), col = "red", print.auc = T))
  
  #Perform the Hosmer-Lemeshow test 
  hoslem.test(data_logr$y, fitted(fitx3))
