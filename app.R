# Early Childcare Center Cost Analysis

## Load packages (and install if necessary)
packages = c("shiny", "dplyr", "tidyr", "plotly",
             "shinyjs", "stringr", "scales", "shinyBS",
             "shinyWidgets")


package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

ui <- fluidPage(
  
  id = "dashboard",
  
  useShinyjs(),
  
  # css to control overflow from drop down
  tags$head(
    tags$style(HTML("

                  @media only screen and (min-width : 500px) {
                      
                      .tabbable {
                        position: fixed;
                        top: 6%;
                      }
                      
                      #dashboard{
                        margin-bottom:100px;
                        -moz-transform: scale(0.75, 0.75); /* Moz-browsers */
                        zoom: 0.75; /* Other non-webkit browsers */
                        zoom: 75%; /* Webkit browsers */
                      }
                      
                      h4{
                        text-align: left;
                        font-size: 17px;
                      }
                      
                      h3{
                        font-weight: 500;
                        font-size: 20px;
                      }
                      
                      .panelsection{
                        margin-left: 10px;
                      }                 
                  }
                  
                  @media only screen and (min-width : 1400px) {

                      .tabbable {
                        position: fixed;
                        top: 6%;
                      }
                      
                      #dashboard{
                        margin-bottom:100px
                        -moz-transform: scale(1, 1); /* Moz-browsers */
                        zoom: 1; /* Other non-webkit browsers */
                        zoom: 100%; /* Webkit browsers */
                      }
                      
                      h4{
                        text-align: left;
                        font-size: 17px;
                      }
                      
                      h3{
                        font-weight: 500;
                        font-size: 20px;
                      }
                      
                      .panelsection{
                        margin-left: 10px;
                      }
                  }
                  
                  p {
                    margin-right: 60px;
                  }
                  
                    "))
  ),
  
  # Application title
  titlePanel("Early Childcare Center Cost Analysis"),
  
  # Sidebar with a dropdowns for center characteristics 
  sidebarLayout (
    
    sidebarPanel(
      width = 3,
      
      id="form", 
      
      h3("Sort By"),
      
      div (
        class="panelsection",
        
        selectInput("sort",
                    NULL,
                    c("Cost per childcare hour" = "ADJ_COST_CCHR",
                      "Instruction and caregiving score" = "MO_scores",
                      "Instructional planning, coordination, and child assessment score" = "EL_scores",
                      "Workforce development score" = "PW_scores",
                      "Child and family support score" = "Sec_C_scores",
                      "Center administration and planning score" = "XAA_CAOC_scores"),
                    selected = "ADJ_COST_CCHR")),
      
      h3("Data Options"),
      
      div (
        class="panelsection",
        
        materialSwitch(inputId = "outlier", label = "Include outlier", status = "primary"),
        
        checkboxGroupInput("quartile", 
                           "Select cost-per-child-care-hour quartile(s)", 
                           c("First" = 1, "Second" = 2, "Third" = 3, "Fourth" = 4),
                           selected = c(1,2,3,4),
                           inline = TRUE)
      ),
      
      h3("Optional Filters"),
      
      div (
        
        class="panelsection", 
        
        bsCollapsePanel( 
          h4(icon("filter"), "Center Characteristics"),
          
          selectInput("hiqris",
                      "QRIS rating",
                      c("All"=-1,
                        "High QRIS"=1,
                        "Low QRIS"=0),
                      "All"
          ),
          
          selectInput("ages",
                      "Age groups served",
                      c("All"=-1,
                        "Ages 0-5 and school-age"="allages",
                        "Ages 0 to 5 years only"="allec",
                        "Age 0-3 or ages 3-5 only"="Itorpsonly"),
                      "All"
          ),
          
          selectInput("size",
                      "Program size",
                      c("All"=-1,
                        "Large center (serving >=75 children)"=0,
                        "Small center"=1),
                      "All"
          ),
          
          selectInput("profit",
                      "Profit status",
                      c("All"=-1,
                        "For-profit"=1,
                        "Not-for-profit"=0),
                      "All"
          ),
          
          selectInput("largeorg",
                      "Embedded in a larger organization or part of a chain",
                      c("All"=-1,
                        "Yes"=1,
                        "No"=0),
                      "All"
          ),
          
          selectInput("fundcat",
                      "Funding",
                      c("All"=-1,
                        "High subsidy"="high subs",
                        "Majority head start or state pre-k"="maj hs or prek",
                        "Majority private tuition"="private",
                        "Mixed public or mixed public/private"="mixed"),
                      "All"
          ),
          selectInput("state",
                      "State",
                      c("All"=-1,
                        "AR" = "AR",
                        "AZ" = "AZ",
                        "PA" = "PA"),
                      "All"
          )
        ),
        
        bsCollapsePanel( 
          h4(icon("filter"), "Implementation Scores"),
          
          selectInput("MO_scores_filter",
                      "Instruction and caregiving score",
                      c("All" = 1,
                        "Below mean (all centers)" = 2,
                        "Above mean (all centers)" = 3),
                      "All"),
          
          selectInput("EL_scores_filter",
                      "Instructional planning, coordination, and child assessment score",
                      c("All" = 1,
                        "Below mean (all centers)" = 2,
                        "Above mean (all centers)" = 3),
                      "All"),
          
          selectInput("PW_scores_filter",
                      "Workforce development score",
                      c("All" = 1,
                        "Below mean (all centers)" = 2,
                        "Above mean (all centers)" = 3),
                      "All"),
          
          selectInput("Sec_C_scores_filter",
                      "Child and family support score",
                      c("All" = 1,
                        "Below mean (all centers)" = 2,
                        "Above mean (all centers)" = 3),
                      "All"),

          selectInput("XAA_CAOC_scores_filter",
                      "Center administration and planning score",
                      c("All" = 1,
                        "Below mean (all centers)" = 2,
                        "Above mean (all centers)" = 3),
                      "All")
          
        )),
      
      actionButton("resetFilters", "Reset All Filters")
    ),
    
    # Show the two key functions plot
    mainPanel(
      
      tabsetPanel(
        tabPanel("ReadMe", 
                 h3("Overview"), 
                 p("This was a dashboard I created at my previous job for a project analyzing costs at early childcare centers.
                   Our client had originally requested a series of static plots displaying the allocation of costs between different combinations of 
                   center functions. Since it sounded like there would be a lot of redundancy in the plots, I proposed that we deliver an interactive dashboard instead. 
                   I designed this dashboard in conjunction with the project researchers who provided insight on what sections of the data would be 
                   most helpful to display for the client.  We went through several iterations of the dashboard, each time adding in additional features 
                   the client requested.  These requested features include being able to exclude one center with outlier values, being able to sort by various measure
                   scores, and having an extra bar with averaged values at the bottom of each plot.  
                   This version of the dashboard uses random fake data since the original data belonged to the project. 
                   As such, the numbers in this version may not necessarily make sense in context, and simply serve as dummy values for display.")),
        tabPanel(HTML("&nbsp &nbsp &nbsp &nbsp Allocations to <br/>individual key functions"), plotlyOutput("distPlot1")),
        tabPanel(HTML(paste("&nbsp Allocations to grouped key functions", "(classroom and other center functions)", sep="<br/>")), plotlyOutput("distPlot2")),
        tabPanel(HTML(paste("Ratio of instruction and caregiving", "&nbsp &nbsp &nbsp to instructional planning", sep="<br/>")), plotlyOutput("distPlot3")),
        tabPanel(HTML(paste("Ratio of classroom functions", "&nbsp &nbsp to other center functions", sep="<br/>")), plotlyOutput("distPlot4"))
      )
    )
  )
)



server <- function(input, output) {
  
  
  data <- reactive({
    
    # Reading in raw data file
    plot_data_orig<-read.csv("analysis_file FAKE.csv")
    
    # Filtering down file based on input filters
    
    if(!input$outlier){
      plot_data_orig <- filter(plot_data_orig, centerid != 52232) %>% filter(quartile_no_outlier %in% input$quartile)
    }
    else{
      plot_data_orig <- filter(plot_data_orig, quartile_all %in% input$quartile)
    }
    
    # Calculate cutoffs
    for (sort.var in c("MO_scores", "EL_scores", "XAA_CAOC_scores", "PW_scores", "Sec_C_scores")){
      cutoff <- plot_data_orig[[sort.var]] %>% mean(na.rm=TRUE)
      assign(paste0(sort.var, "_cutoff"), cutoff)
    }
    
    
    if(input$hiqris == 1 | input$hiqris == 0){
      plot_data_orig <- filter(plot_data_orig, hiqris==input$hiqris)
    }
    
    if(input$ages == "allages"){
      plot_data_orig <- filter(plot_data_orig, allages == 1)
    }
    else if(input$ages == "allec"){
      plot_data_orig <- filter(plot_data_orig, allec == 1)
    }
    else if(input$ages == "Itorpsonly"){
      plot_data_orig <- filter(plot_data_orig, itorpsonly == 1)
    }
    
    if(input$size == 1 | input$size == 0){
      plot_data_orig <- filter(plot_data_orig, small == input$size)
    }
    
    if(input$profit == 1 | input$profit == 0){
      plot_data_orig <- filter(plot_data_orig, forprofit == input$profit)
    }
    
    if(input$largeorg == 1 | input$largeorg == 0){
      plot_data_orig <- filter(plot_data_orig, largeorg == input$largeorg)
    }
    
    if(input$fundcat != -1){
      plot_data_orig <- filter(plot_data_orig, fundcat == input$fundcat)
    }
    
    if (input$state != -1){
      plot_data_orig <- filter(plot_data_orig, State == input$state)
    }
    
    # Add sort variable
    plot_data_orig <- plot_data_orig %>% mutate(sort_var = eval(parse(text=input$sort)))
    
    # Format sort variable for label
    if (input$sort == "ADJ_COST_CCHR"){
      plot_data_orig <- plot_data_orig %>% mutate(sort_var_str = paste0("$", round(ADJ_COST_CCHR, 2)))
    }
    else {
      plot_data_orig <- plot_data_orig %>% mutate(sort_var_str = paste(str_pad(paste("$", round(ADJ_COST_CCHR, 2)), 8, side="right", pad = " "),str_pad(sort_var, 5, side="right", pad="0")))
    }
    
    # Add sort variable value to center label
    plot_data_orig <- plot_data_orig %>% 
      mutate(centerid_new = paste(" ", centerid, ' ', State))
    
    # Converting centerid to a string
    plot_data_orig$centerid <- as.character(plot_data_orig$centerid)
    
    # calculate cutoff means
    for (sort.var in c("MO_scores", "EL_scores", "XAA_CAOC_scores", "PW_scores", "Sec_C_scores")){
      if (input[[paste0(sort.var, "_filter")]] == 2){
        plot_data_orig <- plot_data_orig %>% filter(eval(parse(text = sort.var)) < eval(parse(text = paste0(sort.var, "_cutoff"))))
      } else if (input[[paste0(sort.var, "_filter")]] == 3){
        plot_data_orig <- plot_data_orig %>% filter(eval(parse(text = sort.var)) >= eval(parse(text = paste0(sort.var, "_cutoff"))))
      }
    }
    
    
    
    # Returning filtered data file
    return(plot_data_orig)
    
  })
  
  
  observeEvent(input$resetFilters, {
    shinyjs::reset("form")
  })
  
  
  
  ################################################################
  # Function for creating the plain (i.e. unstacked) bar graphs  #
  ################################################################
  
  add_side_plot <- function(df, main_plot, marg){

    df <- df %>% select(centerid, centerid_new, sort_var) %>% unique()
    
    sortvar_names <- c("ADJ_COST_CCHR", "MO_scores", "EL_scores", "PW_scores", "Sec_C_scores", "XAA_CAOC_scores")
    sortvar_labs <- c("Cost per childcare hour",
                      "Instruction and caregiving score", 
                      "Instructional planning, coordination, and child assessment score",
                      "Workforce development score",
                      "Child and family support score", 
                      "Center administration and planning score")
    sortvar_colors <- c("#004b80", "#543005", "#BF812D", "#C7EAE5", "#35978F", "#003C30")
    sortvar_line <- c(0, 0, 0, 0, 0, 0)
    
    sort_index <- match(input$sort, sortvar_names)
    
    sorttitle <- sortvar_labs[sort_index] %>% str_wrap(width=25)
    
    if (sorttitle == "Cost per childcare hour"){
      df <- mutate(df, text = paste0("Cost per childcare hour: $", round(sort_var,2)))
      axis_params <- list(title=sorttitle, range=c(12, 0))
    } else{
      df <- mutate(df, text = paste("Score:", round(sort_var,3)))  
      axis_params <- list(title=sorttitle, tickvals = list(0, 0.5, 1), range=c(1.2, 0))
    }
    
    side_plot <- plot_ly(df, x=~sort_var, y=~centerid_new, type="bar", orientation="h", showlegend = FALSE, text=~text, 
                         hoverinfo='text', marker = list(color = sortvar_colors[sort_index],
                                                                                                line = list(color = "#00000",
                                                                                                            width = sortvar_line[sort_index])
                         )) %>% 
      layout(yaxis=list(side="right"),
             xaxis=axis_params)
    
    combo <- subplot(side_plot, main_plot, shareY = TRUE, shareX = FALSE, widths = c(0.2, 0.8), titleX = TRUE, titleY = FALSE, margin = marg)
    
    return(combo)
  }
  
  make_bar_plot <- function(ratio_variable, plot_title, xmax){
    
    # Using filtered data
    plot_data_final <- data()
    
    # Error message for when there's no data with all the chosen filters
    validate(
      need(nrow(plot_data_final) != 0, 'No centers matching all these characteristics found')
    )
    
    plot_data_final <- plot_data_final %>% mutate(text = paste("Ratio: ", round(eval(parse(text=ratio_variable)), 2)))
    
    # Calculating average CCHR
    avg_sort_var <- round(mean(plot_data_final$sort_var, na.rm=TRUE),2)
    
    # Format sort variable for label
    if (input$sort == "ADJ_COST_CCHR"){
      avg_sort_var_str <- paste0("$", round(avg_sort_var, 2))
    }
    else {
      avg_sort_var_str <- str_pad(avg_sort_var, 5, side="right", pad="0")
    }
    
    avg_cchr <- plot_data_final$ADJ_COST_CCHR %>% mean() %>% round(2)
    
    # Calculating average row
    avg_row <- plot_data_final %>% 
      mutate(dummy=1) %>% 
      group_by(dummy) %>% 
      summarise(!!ratio_variable := mean(eval(parse(text=ratio_variable)))) %>% 
      mutate(centerid='Average') %>% 
      mutate(sort_var = avg_sort_var) %>% 
      mutate(centerid_new = "      Average") %>% 
      mutate(text = paste("Ratio: ", round(eval(parse(text=ratio_variable)), 2))) %>% 
      select(-dummy)
    
    # Reordering factor levels for centerid so that they appear in the right order
    plot_data_final$centerid_new <- factor(plot_data_final$centerid_new, levels=unique(arrange(plot_data_final, sort_var)$centerid_new))
    
    # Binding on average row
    plot_data_final<- plot_data_final[c("centerid", ratio_variable, "centerid_new", "sort_var", "text")] %>% rbind(avg_row)
    
    plot <- plot_ly(plot_data_final, x=~eval(parse(text=ratio_variable)), 
                    y=~centerid_new, type="bar", 
                    orientation='h', 
                    marker = list(color = "#5AB4AC",
                                  line = list(color = "#01665E",
                                              width = 0)
                    ),
                    text=~text, hoverinfo='text', height = 550, width = 950) %>%
      layout(legend=list(orientation="v", traceorder='normal'), 
             xaxis=list(title=plot_title, hoverformat='.0f', range=c(0, xmax)), 
             yaxis=list(title="Center", autorange= 'reversed'),
             margin = list(b=100)) 
    
    return(add_side_plot(plot_data_final, plot, marg=0.05))
  }
  
  
  # Individual Key Functions Plot 
  output$distPlot1 <- renderPlotly({
    
    # Using filtered data
    plot_data_orig <- data()
    
    # Error message for when there's no data with all the chosen filters
    validate(
      need(nrow(plot_data_orig) != 0, 'No centers matching all these characteristics found')
    )
    
    # Transforming data from wide to long
    plot_data_trans <- gather(plot_data_orig,cost_type,Percent,ADJ_PERCENT_INSTRUCT:ADJ_PERCENT_WFDEV)
    
    # Calculating average CCHR
    avg_sort_var <- round(mean(plot_data_orig$sort_var, na.rm = TRUE),2)
    
    # Format sort variable for label
    if (input$sort == "ADJ_COST_CCHR"){
      avg_sort_var_str <- paste0("$", round(avg_sort_var, 2))
    }
    else {
      avg_sort_var_str <- str_pad(avg_sort_var, 1, side="right", pad="0")
    }
    
    avg_cchr <- plot_data_trans$ADJ_COST_CCHR %>% mean() %>% round(2)
    
    # Creating an average row
    avg_row <- plot_data_trans %>%
      group_by(cost_type) %>%
      summarise(Percent=mean(Percent)) %>%
      mutate(centerid='Average') %>% 
      mutate(sort_var = avg_sort_var) %>% 
      mutate(centerid_new = "      Average")
    
    # Reordering factor levels for centerid so that they appear in the right order
    plot_data_trans$centerid_new <- factor(plot_data_trans$centerid_new, levels=unique(arrange(plot_data_trans,sort_var)$centerid_new))
    
    # Combining average row and plot data file, labeling cost types, creating text variable for the hoverinfo
    plot_data_final<-select(plot_data_trans, centerid, cost_type, Percent, centerid_new, sort_var) %>%
      rbind(avg_row) %>%
      mutate(cost_type=case_when(cost_type=="ADJ_PERCENT_CAP" ~ "Center administration \nand planning" ,
                                 cost_type=="ADJ_PERCENT_CPC" ~ "Instructional planning, \ncoordination, and assessment",
                                 cost_type=="ADJ_PERCENT_FAMILY" ~ "Child and family support",
                                 cost_type=="ADJ_PERCENT_INSTRUCT" ~ "Instruction and caregiving",
                                 cost_type=="ADJ_PERCENT_WFDEV" ~ "Workforce development"
      )) %>%
      mutate(text=paste(paste("Center ID: ",centerid), paste("Cost type: ",cost_type), paste("Percent: ",round(`Percent`)), sep="<br>"))
    
    # Reordering factor levels for cost type so that they appear in the right order
    plot_data_final$cost_type <- factor(plot_data_final$cost_type,
                                        levels = c("Instruction and caregiving",
                                                   "Instructional planning, \ncoordination, and assessment",
                                                   "Workforce development",
                                                   "Child and family support",
                                                   "Center administration \nand planning")
    )
    
    # Creating plot

    plot1 <- plot_ly(plot_data_final, 
                     x=~Percent, 
                     y=~centerid_new, 
                     type="bar", color=~cost_type, orientation='h', colors=c("#543005", "#BF812D", "#C7EAE5", "#35978F", "#003C30"), text=~text, hoverinfo='text', height = 550, width=1050) %>%
      layout(barmode='stack',
            legend=list(orientation="v", traceorder='normal'),
             xaxis=list(title="Allocations to key functions", hoverformat='.0f'),
             yaxis=list(title="Center", autorange= 'reversed'),
            margin = list(b=100))
    
    add_side_plot(plot_data_final, plot1, marg=0.065)
  })
  
  
  
  # Grouped Key Functions Plot 
  output$distPlot2<- renderPlotly({
    
    # Using filtered data
    plot_data_orig <- data()
    
    # Error message for when there's no data with all the chosen filters
    validate(
      need(nrow(plot_data_orig) != 0, 'No centers matching all these characteristics found')
    )
    
    # Calculating average CCHR
    avg_sort_var <- round(mean(plot_data_orig$sort_var, na.rm = TRUE),2)
    
    # Format sort variable for label
    if (input$sort == "ADJ_COST_CCHR"){
      avg_sort_var_str <- paste0("$", round(avg_sort_var, 2))
    }
    else {
      avg_sort_var_str <- str_pad(avg_sort_var, 1, side="right", pad="0")
    }
    
    # Transforming data from wide to long
    plot_data_trans_v2 <- gather(plot_data_orig,cost_type,Percent,classroom_functions:center_functions)
    
    # Reordering factor levels for centerid so that they appear in the right order
    plot_data_trans_v2$centerid_new <- factor(plot_data_trans_v2$centerid_new, levels=unique(arrange(plot_data_trans_v2, sort_var)$centerid_new))
    
    avg_cchr <- plot_data_trans_v2$ADJ_COST_CCHR %>% mean() %>% round(2)
    
    # Creating an average row
    avg_row <- plot_data_trans_v2 %>% 
      group_by(cost_type) %>% 
      summarise(Percent=mean(Percent)) %>% 
      mutate(centerid='Average') %>% 
      mutate(sort_var = avg_sort_var) %>% 
      mutate(centerid_new = "      Average")
    
    # Combining average row and plot data file, labeling cost types, creating text variable for the hoverinfo
    plot_data_final_v2 <- select(plot_data_trans_v2, centerid, cost_type, Percent, centerid_new, sort_var) %>% 
      rbind(avg_row) %>% 
      mutate(cost_type=case_when(cost_type == "classroom_functions" ~ "Classroom functions" ,
                                 cost_type == "center_functions" ~ "Center functions"
      )) %>% 
      mutate(text=paste(paste("Center ID: ",centerid), paste("Cost type: ",cost_type), paste("Percent: ",round(`Percent`)), sep="<br>"))
    
    # Reordering factor levels for center id so that the average row appears last
    plot_data_final_v2$cost_type <- factor(plot_data_final_v2$cost_type,levels(factor(plot_data_final_v2$cost_type))[c(2,1)]) 
    
    # Creating plot
    plot_2 <-plot_ly(plot_data_final_v2, x=~Percent, y=~centerid_new, type="bar", color=~cost_type, orientation='h', colors = c("#80CDC1", "#003C30"), marker = list(line = list(color = "#000000",                                                                                                                                                                                   #654321,
                                                                                                                                               width = 0)), text=~text, hoverinfo='text', height = 550, width=990) %>%
      layout(barmode='stack', 
             legend=list(orientation="v", traceorder='normal'), 
             xaxis=list(title="Allocations to classroom and other center functions", hoverformat='.0f'), 
             yaxis=list(title="Center", autorange= 'reversed'),
             margin = list(b=100))
    
    plot_2
    
    add_side_plot(plot_data_final_v2, plot_2, marg=0.065)
    
  })
  
  
  
  # Ratio Instruct Plot
  output$distPlot3 <- renderPlotly({
    
    make_bar_plot("RATIO_INSTRUCT", "Allocation ratio of instruction and caregiving to instructional planning", 2.2)
    
  })
  
  
  
  # Ratio Classroom Plot
  output$distPlot4 <- renderPlotly({
    
    make_bar_plot("RATIO_CLASSROOM", "Allocation ratio of classroom functions to center functions", 2.2)
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

