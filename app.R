# deqol viz

library(shiny)
library(shinyWidgets)
library(shinyjs)
library(shinycssloaders)
# library(shinyBS)


library(feather)
library(data.table)
library(plotly)
library(viridisLite)
library(RColorBrewer)

library(reshape2)
library(dplyr)

dimGetter <- function(hse_combined,dim = 1, strata = c("imd","sex")){
  
  require(questionr)
  require(data.table)
  
  df <- data.table(hse_combined)
  dim_aggs <- df[,list(
    mo = wtd.table(mo,weights = wt)[dim],
    mo_rel = wtd.table(mo,weights = wt)[dim]/sum(wtd.table(mo,weights = wt)),
    sc = wtd.table(sc,weights = wt)[dim],
    sc_rel = wtd.table(sc,weights = wt)[dim]/sum(wtd.table(sc,weights = wt)),
    ua = wtd.table(ua,weights = wt)[dim],
    ua_rel = wtd.table(ua,weights = wt)[dim]/sum(wtd.table(ua,weights = wt)),
    pd = wtd.table(pd,weights = wt)[dim],
    pd_rel = wtd.table(pd,weights = wt)[dim]/sum(wtd.table(pd,weights = wt)),
    ad = wtd.table(ad,weights = wt)[dim],
    ad_rel = wtd.table(ad,weights = wt)[dim]/sum(wtd.table(ad,weights = wt))
  ),
  by=c("age5", strata)]
  
  names(dim_aggs)[-(1:3)] <- paste0(names(dim_aggs)[-(1:3)],"_",dim)
  dim_aggs$level <- dim
  dim_aggs
}

# colors for imd
cols <- viridis(5)
cols <- substr(cols, 0, 7)
names(cols) = 1:5

# hse data aggregated
hse = data.table(read_feather("hse.feather"))
hse$imd = 6 - hse$imd

# qale estimates 2017-18
qale = read.csv("./qale.csv")
qale$col = cols[qale$imd]

# year choices
years <- list(
  "EQ-5D-3L"  = c(2003,2004,2005,2006,2008,2010,2011,2012,2014), 
  "EQ-5D-5L" = c(2017, 2018)
)

# modal txt ----
modal_txt = list(
  "hrqol" = HTML("
    <p>This chart depicts the mean health-realted quality of life (HRQoL) over the lifecourse by sex and deprivation (imd) quintile.</p> 
    <p>HRQoL was measured by the EQ-5D-3L (2003-2014) or EQ-5D-5L cross-walk (2017-2018) value set.</p>
    Deprivation was measured using the Index of Multiple Deprivation (IMD), which combines multiple dimensions of relative 
    deprivation (e.g. employment, income, education and housing, among other aspects) into a 
    single index value.
    "),
  "rel_ineq" = "
    The figure shows the relative gap in health-related quality of life between 
    deprivation (imd) quintiles 1 and 5 over the lifecourse (i.e. the ratio between the mean 
    EQ-5D utility scores of the most and the least deprived groups).
    ",
  "abs_ineq" = "
    The figure shows the absolute gab in health-realted quality of life between 
    deprivation (imd) quintiles 1 and 5 over the lifecourse (i.e. the difference between the mean 
    EQ-5D utility scores of the most and the least deprived group).
    ",
  "dim" = "
  The figure shows the proportion of participants reporting no, slight, moderate, severe 
  and extreme (5L) or no, some, and severe (3L) problems on each of the five EQ-5D dimensions 
  by deprivation (imd) quintile.
  ",
  "qale_0" = HTML("
  <p>The figure shows the quality-adjusted life expectancy (QALE) at birth by by deprivation (imd) quintile.</p>
  <p>QALE was estimated by combining age-specific life expectancy and health-related quality of life over the lifecourse.</p>
  "),
  "qale_16" = HTML("
  <p>The figure shows the quality-adjusted life expectancy (QALE) at age 16 by by deprivation (imd) quintile.</p>
  <p>QALE was estimated by combining age-specific life expectancy and health-related quality of life over the lifecourse.</p>
  "),
  "qale_40" = HTML("
  <p>The figure shows the quality-adjusted life expectancy (QALE) at  age 40 by by deprivation (imd) quintile.</p>
  <p>QALE was estimated by combining age-specific life expectancy and health-related quality of life over the lifecourse.</p>
  "),
  "qale_65" = HTML("
  <p>The figure shows the quality-adjusted life expectancy (QALE) at  age 65 by by deprivation (imd) quintile.</p>
  <p>QALE was estimated by combining age-specific life expectancy and health-related quality of life over the lifecourse.</p>
  ")
)

# hline helper func
hline <- function(y = 0, color = "black") {
  list(
    type = "line",
    x0 = 0,
    x1 = 1,
    xref = "paper",
    y0 = y,
    y1 = y,
    line = list(color = color, dash = "dot", width = 0.5)
  )
}


# *UI* ------
ui <- fluidPage(
  
  # suppressDependencies("bootstrap"),
  tags$head(HTML('
  <link href="https://cdn.jsdelivr.net/npm/bootstrap@5.0.2/dist/css/bootstrap.min.css" rel="stylesheet" integrity="sha384-EVSTQN3/azprG1Anm3QDgpJLIm9Nao0Yz1ztcQTwFspd3yD65VohhpuuCOmLASjC" crossorigin="anonymous">
    <script src="resizePlot.js"></script>
    <script src="https://cdn.jsdelivr.net/npm/bootstrap@5.0.2/dist/js/bootstrap.bundle.min.js" integrity="sha384-MrcW6ZMFYlzcLA8Nl+NtUVF0sA7MsXsP1UyJoMp4YLEuNSfAP+JcXn/tWtIaxVXM" crossorigin="anonymous"></script>
    <title>QALY Shortfall Calculator</title>
                 ')),
  includeCSS("style.css"),
  useShinyjs(),
  
  title = "DeQol-Life",
  
  
  
  
  
  
  
  # PLOT OUT ------------
  div(
    uiOutput('plot_ui')
  ),
  
  # INFO MODAL ------
  # modal button
  actionButton(
    inputId = "info", 
    icon = icon("question-circle"), 
    label =  "what is this chart  about?",
    style = "position: fixed;  width: 200px; left: 0; right: 0; top:0; margin: 5px auto; top:0 ",
    class = "badge bg-info rounded-3 fs-6 btn-plotinfo shadow"
  ),
  
  div(
    id = "info_modal",
    style = "position: absolute; width: 500px; top:50px; left: 50%; transform: translateX(-50%);",
    class = "shadow hidden card",
    
    div(
      class = "card-header",
      id="info_modalheader", 
      div(
        class = "h5",
        style = "pointer-events: none;",
        "Chart info"
      )
    ),
    div(
      class = "card-body",
      style = "pointer-events: none;",
      uiOutput("plot_info_txt")
      ),
    
    div(
      id = "modal_footer",
      style = "pointer-events: none;",
      class = "d-block d-flex justify-content-end pt-2 pb-3 border-top",
      actionButton("close_modal", "Close", class  = " btn-secondary me-5 mt-1")  
    )
    
  ),
  
  
  
  
  
  
  
  
  
  # menu  ------
  
  # show menu link button
  a(id = "show_controls", class = "", "show controls", style = "margin-top: -150px; visibility: hidden; cursor: pointer; color: #7cb5ec;"),
  
  div(
    id = "menu",
    class = "shadow-lg pt-4 pb-3 border-top",
    style = "
    position: fixed;
    bottom: 0;
    left:0;
    right: 0;
    background-color: white;
    transition: 1s bottom;
    ",
    fluidRow(
      # style = "  white-space: nowrap; flex-wrap: nowrap;",
      column(
        width = 2, 
        
        # hide menu link button
        a(id = "hide_controls",class = "ms-3 ", "hide controls", style = "cursor: pointer; color: #7cb5ec;")
        ),
      column(
        offset = 0, 
        width = 3,
        style = "min-width: 250px;",
        div(
          class = "display-6",
          "DeQoL-Life"
          ),
         div(
           class = " text-secondary",
           "Decomposing Quality of Life over the Lifecourse"
         ),
           
           a(class = "mb-3", 'href'="https://github.com/bitowaqr/deqol-viz", 'target' ="_blank", "data+code", style = "color: #7cb5ec;"),
         # version ----
         selectInput(
           inputId = "version", 
           label = "Select instrument version", 
           choices = c(
             "EQ-5D-3L" = "EQ-5D-3L", 
             "EQ-5D-5L (van Hout et al. crosswalk)" = "EQ-5D-5L"
             ), 
           selected = "EQ-5D-5L"
           ),
         # years -----
         checkboxGroupButtons(
           inputId = "years",
           label = "Use data from:", 
           status = "light", size = "sm", 
           choices = c("2003", "2006","2012","2014" )
         ),
         prettySwitch(
           inputId = "pool_year",
           label = "pool data",
           value = T,fill = T,inline = T,status = "primary"
         )
      ),
      
      column(
        width = 4, offset = 1,
        style = "min-width: 250px;",
        # var -----
        selectInput(
          inputId = "var", 
          label = "Topic", 
          choices = list(
            "Health-related quality of life (HRQoL)" = c(
              "HRQoL over the life course" = "hrqol", 
              "Relative inequality in HRQoL" ="rel_ineq",
              "Absolute inequality  in HRQoL"="abs_ineq"
              ),
            "EQ-5D Dimension scores" = c(
              "Scores" = "dim"
            ),
            "Quality-adjusted life expectancy (QALE)" = c(
            "QALE at birth" = "qale_0",
            "QALE at 16" = "qale_16",
            "QALE at 40" = "qale_40",
            "QALE at 65" = "qale_65"
            )
          )
          ),
        
        # sex -----
        div(class= "d-flex flex-row justify-content-start align-items-center",
          checkboxGroupButtons(
            inputId = "sex",
            size = "normal",# width = "100%",
            label = "Sex",
            choices = c("Male","Female"),
            selected = c("Male","Female"),
            status = "light"
          ),
          div(
            class = "mt-5 ms-5",
            prettySwitch(
              inputId = "pool_sex",
              label = "pool data",
              value = F,fill = T,inline = T,status = "primary"
            )
          )
        ),
        
        # imd -------
        div(class= "d-flex flex-row justify-content-start align-items-center",
            div(class= "d-flex flex-column justify-content-start align-items-start",
                HTML('<label class="control-label" >Index of multiple deprivation (imd)<br>quintile (1 = most, 5 = least deprived)</label>'),
              # "Index of multiple deprivation (imd) quintile (1 = most, 5 = least deprived)",
            checkboxGroupButtons(
              inputId = "imd",
              size = "normal",# width = "100%",
              label = NULL,
              choices = c("1","2","3","4", "5"),
              selected = c("1","2","3","4", "5"),
              status = "light"
            )
          ),
          div(
            class = "mt-5 ms-3",
            prettySwitch(
              inputId = "pool_imd",
              label = "pool data",
              value = F,
              fill = T,inline = T,status = "primary"
            )
          )
        )
      )
    )
  ),
  
  
  # landing page -----
  HTML('
<div id = "intro_page">
  <div 
  class="split right-intro d-flex flex-column justify-content-center text-center"
  style = "
  background-image: url(g2.jpg);
    background-position: center; 
    background-repeat: no-repeat; 
    background-size: cover; 
  "
  >
    
    <div class = "display-6  mx-5 w-100 mx-auto text-secondary mb-5">
        Data visualisation tool / online supplement
    </div> 
     
     
    <div class = "py-5  " 
    style = "
    
      
    ">
      <div class = "intro-title  mt-4  fw-light text-light" 
      style = "font-family: Helvetica"
      >
          DeQoL-LIFE
      </div>
       
      <div 
      class = "intro_sub  display-6 text-secondary mx-5 mx-auto mb-5"
      >
          Decomposing the socioeconomic gradient of health-related quality of life in England
      </div>
      
      <div class = "fs-3 mb-3 text-light fw-lighter">
        Paul Schneider<sup>1</sup>, 
        James Love-Koh<sup>2</sup>, 
        Simon McNamara<sup>1,3</sup>, 
        Tim Doran<sup>4</sup>, 
        Nils Gutacker<sup>2</sup>
      </div>
      
      <div class = "fs-6 text-secondary w-50 mx-auto mb-4  fw-lighter">
      <sup>1</sup>School of Health and Related Research, University of Sheffield, Sheffield, UK. 
<sup>2</sup>Centre for Health Economics, University of York, York, UK.
<sup>3</sup>BresMed, Sheffield, UK.
<sup>4</sup>Department of Health Sciences, University of York, York, UK.
</div>
      
    </div>
    
    <div class = "d-flex justify-content-center mt-5">
      <div id = "close_intro" class="btn btn-start py-2 px-3 shadow-lg fs-2">Start </div>
    </div>
    
  </div>
</div>
')
)


# *server* -----
server <- function(input, output, session){
 

  # update years for version ------
  observeEvent(input$version,{
    updateCheckboxGroupButtons(
      session, "years", 
      choices = years[[input$version]], 
      status = "light", 
      selected = years[[input$version]], 
      disabled = ifelse(isolate(input$var) %in% c("qale_0","qale_16","qale_40","qale_65"), T, F)
    )
  })
  
  
  # btn enabled/disabled logical  ----
  observeEvent(input$var,{
    
    enable("pool_year")
    enable("pool_sex")
    enable("imd")
    enable("pool_imd")
    enable("version")
    # enable("years")
    updateCheckboxGroupButtons(session, "years", choices = years[[input$version]], status = "light", selected = years[[input$version]])
    
    if(input$var %in% c("qale_0","qale_16","qale_40","qale_65")){
      updateSelectInput(session, "version", selected = "EQ-5D-5L")
      disable("version")
      updatePrettySwitch(session, "pool_year", value = T)
      disable("pool_year")
    }
    
    
    if(input$var == "dim"){
      updatePrettySwitch(session, "pool_year", value = T)
      updatePrettySwitch(session,"pool_sex", value = T)
      disable("pool_sex")
    }
      
    if(input$var == "rel_ineq" | input$var == "abs_ineq"){
      disable("imd")
      disable("pool_imd")
    } 
    
  
    
  })
   
  
  observeEvent(input$info,{
    runjs("document.querySelector('#info_modal').classList.remove('hidden')")
    # other js script in resizePlot.js
  })
  observeEvent(input$close_modal,{
    runjs("document.querySelector('#info_modal').classList.add('hidden')")
    # other js script in resizePlot.js
  })
  
  
  # modal info txt -----
  output$plot_info_txt <- renderUI({
    modal_txt[[input$var]]
  })
  outputOptions(output, "plot_info_txt", suspendWhenHidden = FALSE)
  
  
  # render ui  -------
  output$plot_ui <- renderUI({
    set_height = ifelse(is.null(input$set_plot_height), 400,input$set_plot_height)
    
    out <- plotlyOutput(paste0(input$var,"_plot"),height = "100px")
    out[[1]][[2]]$style = paste0("width:100%; height:",set_height,"px;")
    shinycssloaders::withSpinner(out)
    
  })
  
  
  
  # PLOTLY: hrqol ------
  output$hrqol_plot <- renderPlotly({
    
    
    # re-render when window size is changed
    input$set_plot_height
    
    # select sex, imd, years to display
    select_sex = input$sex
    select_imd = as.numeric(input$imd)
    select_years = c(input$years)
    
    # select starta/pooling
    strata = c()
    
    if(!input$pool_sex){
      strata = c(strata, "sex")
    }
    
    if(!input$pool_imd){
      strata = c(strata, "imd")
    }
    
    if(input$pool_year){
      years_subplots = list(select_years)  
    } else {
      years_subplots = as.list(select_years)
      strata = c(strata,"year")
    }
    
    # aggregate data
    df = hse[which(year %in% select_years & sex %in% select_sex & imd %in% select_imd),list(eq5d = weighted.mean(eq5d,wt,na.rm = T)),key=c(c(strata,"age5"))]
    
    if(nrow(df)==0){
      p1 = plot_ly(
        type = "scatter", showlegend = F
        #, height = set_height
        ) %>%
        add_trace(x = 1, y=1, text = "No data to display ", mode = 'text',
                  textfont = list(color = '#000000', size = 36)) %>%
        layout(
          xaxis = list(zerolinecolor = '#ffff',
                       zerolinewidth = 2,
                       # gridcolor = 'ffff',
                       showticklabels=FALSE),
          yaxis = list(zerolinecolor = '#ffff',
                       zerolinewidth = 2,
                       # gridcolor = 'ffff',
                       showticklabels=FALSE),
          legend = list(orientation = 'h')
        )
      return(p1)
    } 
    
    if(input$pool_year){
      df$year = years_subplots[[1]][1]
    }
    
    if(input$pool_sex){
      df$sex = " "
    }
    
    df$line = ifelse(df$sex=="Male", "dash","solid")
    
    if(input$pool_imd){
      df$imd = "Combined"
      df$col = "cadetblue"
    } else {
      df$col = cols[df$imd]
    }
    
    p_list = list()
    
    for(i in seq_along(years_subplots)){
      p_list[[i]] = plot_ly(
        type = "scatter", 
        mode= "lines+markers", 
        # height = input$set_height,
        showlegend = ifelse(i>1,F,T)) %>%
        add_trace(
          data = df[(df$sex == "Female" | df$sex == " ") & df$year %in%  years_subplots[[i]],], 
          legendgroup = 'group2',
          linetype = ~I(line),
          name = ~paste(sex, "imd",imd),
          color = ~I(col),
          x = ~age5, y = ~eq5d, 
          text = ~paste(sex,"IMD",imd),
          hovertemplate = paste(
            "<extra></extra><b>%{text}</b><br>",
            "EQ-5D: %{y:,.2f}<br>",
            "Age: %{x:,.0f}<br>"
          )
          ) %>%
        add_trace(
          data = df[df$sex == "Male" & df$year %in%  years_subplots[[i]],], 
          legendgroup = 'group1', 
          linetype = ~I(line),
          name = ~paste(sex, "imd",imd),
          color = ~I(col),
          x = ~age5, y = ~eq5d, 
          text = ~paste(sex,"IMD",imd),
          hovertemplate = paste(
            "<extra></extra><b>%{text}</b><br>",
            "EQ-5D: %{y:,.2f}<br>",
            "Age: %{x:,.0f}<br>"
          )
        ) %>%
        layout(
          # plot_bgcolor='#e5ecf6',
          yaxis = list(
            title = "Text",
            zerolinecolor = '#ffff',
            zerolinewidth = 2,
            # gridcolor = 'ffff',
            range = list(ifelse(min(df$eq5d)<0.5,0,0.5), 1)
            )
          )
    }
        
    
    res_plot = subplot(p_list,nrows = ceiling(length(p_list)/3)) %>%
      config(
        displaylogo = FALSE,
        modeBarButtonsToRemove = c(
          "zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d",
          "autoScale2d",
          # "resetScale2d",
          "toggleHover","toggleSpikelines",
          "hoverClosestGl2d", "hoverCompareCartesian","hoverClosestPie",
          "hoverClosestCartesian")) %>%
      layout(
        hovermode = 'closest',
        yaxis = list(
          title = "Health-related quality of life <br>(EQ-5D score)"
        ),
        xaxis = list(
          title = "Age"
        )
      )
    
     return(res_plot)
      
    
    
    # return(p1)
  })
  
  
  
  # PLOTLY: rel inequal ------
  output$rel_ineq_plot <- renderPlotly({
    
    # re-render when window size is changed
    input$set_plot_height
    
    # select sex, imd, years to display
    select_sex = input$sex
    select_years = c(input$years)
    
    # select starta/pooling
    strata = c()
    
    if(!input$pool_sex){
      strata = c(strata, "sex")
    }
    
    if(input$pool_year){
      years_subplots = list(select_years)  
    } else {
      years_subplots = as.list(select_years)
      strata = c(strata,"year")
    }
    
    # aggregate data
    df_1 = hse[which(year %in% select_years & sex %in% select_sex & imd == 1),list(eq5d = weighted.mean(eq5d,wt,na.rm = T)),key=c(c(strata,"age5"))]
    df_5 = hse[which(year %in% select_years & sex %in% select_sex & imd == 5),list(eq5d = weighted.mean(eq5d,wt,na.rm = T)),key=c(c(strata,"age5"))]
    
    if(nrow(df_1)==0){
      p1 = plot_ly(
        type = "scatter", showlegend = F
        #, height = set_height
      ) %>%
        add_trace(x = 1, y=1, text = "No data to display ", mode = 'text',
                  textfont = list(color = '#000000', size = 36)) %>%
        layout(
          xaxis = list(zerolinecolor = '#ffff',
                       zerolinewidth = 2,
                       # gridcolor = 'ffff',
                       showticklabels=FALSE),
          yaxis = list(zerolinecolor = '#ffff',
                       zerolinewidth = 2,
                       # gridcolor = 'ffff',
                       showticklabels=FALSE),
          legend = list(orientation = 'h')
        )
      return(p1)
    } 
    
    
    if(input$pool_year){
      df_1$year = df_5$year = years_subplots[[1]][1]
    }
    
    if(input$pool_sex){
      df_1$sex = df_5$sex = " "
    }
    
    eq5d = df_1$eq5d / df_5$eq5d
    df = cbind(eq5d, df_1[,c("sex","year","age5")])
    
    df$line = ifelse(df$sex=="Male", "dash","solid")
    df = data.frame(df)
    df$col = "cadetblue"
    
    p_list = list()
    
    for(i in seq_along(years_subplots)){
      p_list[[i]] = plot_ly(
        type = "scatter", 
        mode= "lines+markers", 
        showlegend = ifelse(i>1,F,T)) %>%
        add_trace(
          data = df[(df$sex == "Female" | df$sex == " ") & df$year %in%  years_subplots[[i]],], 
          legendgroup = 'group2',
          linetype = ~I(line),
          name = ~paste(sex),
          color = ~I(col),
          x = ~age5, y = ~eq5d, 
          text = ~paste(sex),
          hovertemplate = paste(
            "<extra></extra><b>%{text}</b><br>",
            "EQ-5D: %{y:,.2f}<br>",
            "Age: %{x:,.0f}<br>"
          )
        ) %>%
        add_trace(
          data = df[df$sex == "Male" & df$year %in%  years_subplots[[i]],], 
          legendgroup = 'group1', 
          linetype = ~I(line),
          name = ~paste(sex),
          color = ~I(col),
          x = ~age5, y = ~eq5d, 
          text = ~paste(sex),
          hovertemplate = paste(
            "<extra></extra><b>%{text}</b><br>",
            "Relative inequality: %{y:,.2f}<br>",
            "Age: %{x:,.0f}<br>"
          )
        ) %>%
        layout(
          # plot_bgcolor='#e5ecf6',
          shapes = list(hline(1)),
          yaxis = list(
            title = "Text",
            zerolinecolor = '#ffff',
            zerolinewidth = 2,
            # gridcolor = 'ffff',
          range = list(0.5, ifelse(max(df$eq5d)<1.1, 1.1 ,max(df$eq5d)))
          )
        )
    }
    
    
    res_plot = subplot(p_list,nrows = ceiling(length(p_list)/3)) %>%
      config(
        displaylogo = FALSE,
        modeBarButtonsToRemove = c(
          "zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d",
          "autoScale2d",
          # "resetScale2d",
          "toggleHover","toggleSpikelines",
          "hoverClosestGl2d", "hoverCompareCartesian","hoverClosestPie",
          "hoverClosestCartesian")) %>%
      layout(
        hovermode = 'closest',
        yaxis = list(
          title = "HRQoL: inequality ratio <br>(Most/Least deprived)"
        ),
        xaxis = list(
          title = "Age"
        )
      )
    
    return(res_plot)
    
    
    
    # return(p1)
  })
  
  
  
  # PLOTLY: abs inequal ------
  output$abs_ineq_plot <- renderPlotly({
    
    # re-render when window size is changed
    input$set_plot_height
    
    # select sex, imd, years to display
    select_sex = input$sex
    select_years = c(input$years)
    
    # select starta/pooling
    strata = c()
    
    if(!input$pool_sex){
      strata = c(strata, "sex")
    }
    
    if(input$pool_year){
      years_subplots = list(select_years)  
    } else {
      years_subplots = as.list(select_years)
      strata = c(strata,"year")
    }
    
    # aggregate data
    df_1 = hse[which(year %in% select_years & sex %in% select_sex & imd == 1),list(eq5d = weighted.mean(eq5d,wt,na.rm = T)),key=c(c(strata,"age5"))]
    df_5 = hse[which(year %in% select_years & sex %in% select_sex & imd == 5),list(eq5d = weighted.mean(eq5d,wt,na.rm = T)),key=c(c(strata,"age5"))]
    
    if(nrow(df_1)==0){
      p1 = plot_ly(
        type = "scatter", showlegend = F
        #, height = set_height
      ) %>%
        add_trace(x = 1, y=1, text = "No data to display ", mode = 'text',
                  textfont = list(color = '#000000', size = 36)) %>%
        layout(
          xaxis = list(zerolinecolor = '#ffff',
                       zerolinewidth = 2,
                       # gridcolor = 'ffff',
                       showticklabels=FALSE),
          yaxis = list(zerolinecolor = '#ffff',
                       zerolinewidth = 2,
                       # gridcolor = 'ffff',
                       showticklabels=FALSE),
          legend = list(orientation = 'h')
        )
      return(p1)
    } 
    
    
    if(input$pool_year){
      df_1$year = df_5$year = years_subplots[[1]][1]
    }
    
    if(input$pool_sex){
      df_1$sex = df_5$sex = " "
    }
    
    eq5d = df_1$eq5d - df_5$eq5d
    df = cbind(eq5d, df_1[,c("sex","year","age5")])
    
    df$line = ifelse(df$sex=="Male", "dash","solid")
    df = data.frame(df)
    df$col = "cadetblue"
    
    p_list = list()
    
    for(i in seq_along(years_subplots)){
      p_list[[i]] = plot_ly(
        type = "scatter", 
        mode= "lines+markers", 
        showlegend = ifelse(i>1,F,T)) %>%
        add_trace(
          data = df[(df$sex == "Female" | df$sex == " ") & df$year %in%  years_subplots[[i]],], 
          legendgroup = 'group2',
          linetype = ~I(line),
          name = ~paste(sex),
          color = ~I(col),
          x = ~age5, y = ~eq5d, 
          text = ~paste(sex),
          hovertemplate = paste(
            "<extra></extra><b>%{text}</b><br>",
            "Difference: %{y:,.2f}<br>",
            "Age: %{x:,.0f}<br>"
          )
        ) %>%
        add_trace(
          data = df[df$sex == "Male" & df$year %in%  years_subplots[[i]],], 
          legendgroup = 'group1', 
          linetype = ~I(line),
          name = ~paste(sex),
          color = ~I(col),
          x = ~age5, y = ~eq5d, 
          text = ~paste(sex),
          hovertemplate = paste(
            "<extra></extra><b>%{text}</b><br>",
            "EQ-5D: %{y:,.2f}<br>",
            "Age: %{x:,.0f}<br>"
          )
        ) %>%
        layout(
          # plot_bgcolor='#e5ecf6',
          shapes = list(hline(0)),
          yaxis = list(
            title = "Text",
            zerolinecolor = '#ffff',
            zerolinewidth = 2,
            # gridcolor = 'ffff',
            range = list(-0.4, ifelse(max(df$eq5d) > 0.1 ,max(df$eq5d), 0.1))
          )
        )
    }
    
    
    res_plot = subplot(p_list,nrows = ceiling(length(p_list)/3)) %>%
      config(
        displaylogo = FALSE,
        modeBarButtonsToRemove = c(
          "zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d",
          "autoScale2d",
          # "resetScale2d",
          "toggleHover","toggleSpikelines",
          "hoverClosestGl2d", "hoverCompareCartesian","hoverClosestPie",
          "hoverClosestCartesian")) %>%
      layout(
        hovermode = 'closest',
        yaxis = list(
          title = "HRQoL: absolute inequality <br>(Most - Least deprived)"
        ),
        xaxis = list(
          title = "Age"
        )
      )
    
    return(res_plot)
    
    
    
    # return(p1)
  })
  
  
  
  
  # PLOTLY: dim Plot ------
  output$dim_plot <- renderPlotly({
    
    
    
    
    if(input$version == "EQ-5D-3L"){
      dim_lvls <- 1:3
      col_labs <- c("1 No problems","2 Some problems", "3 Severe problems")
    } else {
      dim_lvls <- 1:5
      col_labs <- c("1 No problems","2 Slight problems", "3 Moderate problems","4 Severe problems","5 Extreme problems")
    }
  
    input$set_plot_height
    
    # select sex, imd, years to display
    select_sex = input$sex
    select_imd = as.numeric(input$imd)
    select_years = c(input$years)
    
    
    df = hse[which(year %in% select_years & sex %in% select_sex & imd %in% select_imd),]
    
    
    if(nrow(df)==0){
      p1 = plot_ly(
        type = "scatter", showlegend = F
        #, height = set_height
      ) %>%
        add_trace(x = 1, y=1, text = "No data to display ", mode = 'text',
                  textfont = list(color = '#000000', size = 36)) %>%
        layout(
          xaxis = list(zerolinecolor = '#ffff',
                       zerolinewidth = 2,
                       # gridcolor = 'ffff',
                       showticklabels=FALSE),
          yaxis = list(zerolinecolor = '#ffff',
                       zerolinewidth = 2,
                       # gridcolor = 'ffff',
                       showticklabels=FALSE),
          legend = list(orientation = 'h')
        )
      return(p1)
    } 
    
    
    strata = c("imd")
    if(input$pool_imd){
      df$imd = "pooled"
    }
    
    dim_agg = lapply(dim_lvls, dimGetter, hse_combined = df, strata = strata)
    dim_aggs = Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, all.x = TRUE,all.y = T), dim_agg)
    dim_aggs_long = reshape2::melt(dim_aggs,id.vars = c(strata,"age5","level"))
    dim_aggs_long_rel <- dim_aggs_long[grepl("rel",dim_aggs_long$variable),]
    dim_aggs_long_rel$variable = as.character(dim_aggs_long_rel$variable)
    keep_var <- dim_aggs_long_rel$level == as.numeric( substr(dim_aggs_long_rel$variable,nchar(dim_aggs_long_rel$variable),nchar(dim_aggs_long_rel$variable)))
    dim_aggs_long_rel <- dim_aggs_long_rel[keep_var,]
    dim_aggs_long_rel$dimension = substr(dim_aggs_long_rel$variable,1,2)
    dim_aggs_long_rel$dimension = factor(dim_aggs_long_rel$dimension, 
                                         levels=c("mo","sc","ua","pd","ad")
    )
    
    dim_names = c("mo" = "Mobility",
                  "sc" = "Self-care",
                  "ua" = "Usual activities",
                  "pd" = "Pain/Discomfort",
                  "ad" = "Anxiety/Depression"
                  )
    
    if(length(dim_lvls) == 5){
      col_index =   c(1,3:6)
    } else {
      col_index =   c(1,3,6)
    }
    cols <- RColorBrewer::brewer.pal(6, "Blues")[col_index]
    names(cols) = dim_lvls
    
    fig = list() 
    plot_legend = T
    plot_x_dim = T
    
    if(input$pool_imd){
      dim_aggs_long_rel$imd = 1
    }
    
    for(k in unique(dim_aggs_long_rel$imd)){
      for(j in unique(dim_aggs_long_rel$dimension)){
        
        fig_j = plot_ly( 
          type = 'bar',
          showlegend = ifelse(plot_legend,T,F)
        ) %>% 
          layout(
            yaxis = list(title = ifelse(j == "mo", ifelse(input$pool_imd,"Pooled",paste("IMD",k))," "))
            )
        
        
        
        plot_legend = F
        
        
        for(i in dim_lvls){
          fig_j = fig_j %>%
            add_trace(data = dim_aggs_long_rel[dim_aggs_long_rel$imd==k & 
                                                 dim_aggs_long_rel$dimension == j &
                                                 dim_aggs_long_rel$level == i
                                               ,], 
                      x = ~(age5), y = ~value, 
                      marker = list(color = cols[i]),
                      # color = ~as.factor(6-level),  
                      type = 'bar', name = col_labs[i]
            ) 
          
        }
        
        fig[[paste(j,k)]] = fig_j %>%
          layout(
            
            barmode = 'stack',
            xaxis = list(
              title = list(
                text = ifelse(k == length(unique(dim_aggs_long_rel$imd)), paste("age<br>",dim_names[j]),""),
                standoff = 0
              )
            )
          )
      }  
      
    }
    
    
    

    subplot(fig,nrows = ceiling(length(fig)/5),titleY = TRUE,titleX = TRUE) %>% 
      config(
        displaylogo = FALSE,
        modeBarButtonsToRemove = c(
          "zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d",
          "autoScale2d",
          # "resetScale2d",
          "toggleHover","toggleSpikelines",
          "hoverClosestGl2d", "hoverCompareCartesian","hoverClosestPie",
          "hoverClosestCartesian")) %>%
      layout(
        hovermode = 'closest'
        )
      
  })
 
  
  
  
  # PLOTLY: qale Plot ------
  observe({
    if(grepl("qale", input$var)){
     
      select_var = paste0(isolate(input$var),"_plot")
      select_age = substr(input$var, 6,10)
      
      if(input$pool_sex & length(input$sex) == 2){
        select_sex = "Pooled"
      } else {
        select_sex = input$sex
      }
      
      select_imd = input$imd
      
      output[[select_var]] <- renderPlotly({
        
        
        qale_dat = qale[qale$sex %in% select_sex & qale$age == select_age & qale$imd %in% select_imd,]
        
        if(nrow(qale_dat)==0){
          p1 = plot_ly(
            type = "scatter", showlegend = F
            #, height = set_height
          ) %>%
            add_trace(x = 1, y=1, text = "No data to display ", mode = 'text',
                      textfont = list(color = '#000000', size = 36)) %>%
            layout(
              xaxis = list(zerolinecolor = '#ffff',
                           zerolinewidth = 2,
                           # gridcolor = 'ffff',
                           showticklabels=FALSE),
              yaxis = list(zerolinecolor = '#ffff',
                           zerolinewidth = 2,
                           # gridcolor = 'ffff',
                           showticklabels=FALSE),
              legend = list(orientation = 'h')
            )
          return(p1)
        } 
        
        
        figs = list()
        
        for(s in select_sex){
          
          fig_j = plot_ly( 
            type = 'bar',
          ) %>%
            add_trace(
              data = qale_dat[qale_dat$sex == s,], 
              x = ~(imd), y = ~qale, 
              marker = list(color = ~col),
              text = ~paste("IMD ",imd),
              # color = ~as.factor(6-level),  
              type = 'bar',
              hovertemplate = paste(
                "<extra></extra><b>%{text}</b><br>",
                "QALE: %{y:,.2f}<br>",
                "IMD: %{x:,.0f}<br>"
              )
            ) %>%
            add_annotations(
              text = ~unique(sex),
              x = 0.5,
              y = 1,
              yref = "paper",
              xref = "paper",
              xanchor = "middle",
              yanchor = "top",
              showarrow = FALSE,
              font = list(size = 15, face = "bold")
            )
          
          figs[[s]] = fig_j
        }
        
        subplot(
          figs
        ) %>% 
          layout(
            yaxis = list(title = "Quality-adjusted life expectancy<br>(QALE)"),
            showlegend = FALSE
          ) %>% 
          config(
            displaylogo = FALSE,
            modeBarButtonsToRemove = c(
              "zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d",
              "autoScale2d",
              # "resetScale2d",
              "toggleHover","toggleSpikelines",
              "hoverClosestGl2d", "hoverCompareCartesian","hoverClosestPie",
              "hoverClosestCartesian")) 
        # %>%
        #     layout(hovermode = 'closest')
        
      })
      
      
       
    }
  })
  
    
  
  
   
  
}

shinyApp(ui, server)