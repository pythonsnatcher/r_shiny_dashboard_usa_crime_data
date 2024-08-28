dashboardPage(
  dashboardHeader(
    title = '1973 US Arrest Data', 
    titleWidth = 650,
    
    tags$li(
      class = "dropdown", 
      tags$a(
        href = 'https://www.linkedin.com/in/chris-s-gilmour/', 
        icon('linkedin'), 
        'My Profile', 
        target = '_blank'
      )
    ),
    tags$li(
      class = 'dropdown', 
      tags$a(
        href = 'https://github.com/pythonsnatcher', 
        icon('github'), 
        'My Git Hub', 
        target = '_blank'
      )
    )
  ),
  
  dashboardSidebar(
    # Sidebar menu
    sidebarMenu(
      id = 'sidebar',
      
      # First menu item
      menuItem('Dataset', tabName = 'data', icon = icon('database')),
      menuItem(
        text = 'Visualization', 
        tabName = 'viz', 
        icon = icon('chart-line')
      ),
      conditionalPanel(
        "input.sidebar == 'viz' && input.t2 == 'distro'",
        selectInput(
          inputId = 'var1', 
          label = 'Select the variable', 
          choices = c1, 
          selected = 'Murder'
        )
      ),
      conditionalPanel(
        "input.sidebar == 'viz' && input.t2 == 'trends'",
        selectInput(
          inputId = 'var2', 
          label = 'Select the Arrest type', 
          choices = c2, 
          selected = 'Rape'
        )
      ),
      conditionalPanel(
        "input.sidebar == 'viz' && input.t2 == 'relation'",
        selectInput(
          inputId = 'var3', 
          label = 'Select the X variable', 
          choices = c1, 
          selected = 'Rape'
        )
      ),
      conditionalPanel(
        "input.sidebar == 'viz' && input.t2 == 'relation'",
        selectInput(
          inputId = 'var4', 
          label = 'Select the Y variable', 
          choices = c1, 
          selected = 'Assault'
        )
      ),
      
      menuItem(
        text = 'Choropleth Map', 
        tabName = 'map', 
        icon = icon('map')
      )
    )
  ),
  
  dashboardBody(
    tabItems(
      # First tab item
      tabItem(
        tabName = 'data',
        # Tab box
        tabBox(
          id = 't1', 
          width = 12, 
          tabPanel(
            title = 'About', 
            icon = icon('address-card'), 
            fluidRow(
              column(
                width = 8, 
                tags$img(src = 'crime.png', width = 600, height = 300),
                tags$br(),
                tags$a(''), 
                align = 'center'
              ),
              column(
                width = 4, 
                tags$br(),
                tags$p('This data set is included in Rstudio, the figures are per 100,000 residents')
              )
            )
          ),
          tabPanel(
            title = 'Data', 
            icon = icon('address-card'), 
            dataTableOutput('dataT')
          ),
          tabPanel(
            title = 'Structure', 
            icon = icon('address-card'), 
            verbatimTextOutput('structure')
          ),
          tabPanel(
            title = 'Summary Stats', 
            icon = icon('address-card'), 
            verbatimTextOutput('summary')
          )
        )
      ),
      
      # Second tab item
      tabItem(
        tabName = 'viz',
        tabBox(
          id = 't2', 
          width = 12,
          tabPanel(
            title = 'Crime Trends by State', 
            value = 'trends',
            fluidRow(
              box(
                title = textOutput('head1'), 
                status = 'primary', 
                solidHeader = TRUE, 
                collapsible = TRUE, 
                collapsed = TRUE, 
                width = 12,
                tableOutput('top5')
              ),
              box(
                title = textOutput('head2'), 
                status = 'primary', 
                solidHeader = TRUE, 
                collapsible = TRUE, 
                collapsed = TRUE, 
                width = 12,
                tableOutput('low5')
              ),
              box(
                title = '', 
                status = 'primary', 
                solidHeader = FALSE, 
                collapsible = FALSE, 
                collapsed = FALSE, 
                width = 12,
                withSpinner(plotlyOutput('bar'))
              )
            )
          ),
          tabPanel(
            title = 'Distribution', 
            value = 'distro', 
            plotlyOutput('histplot')
          ),
          tabPanel(
            title = 'Correlation Matrix',  
            plotlyOutput('cor')
          ),
          tabPanel(
            title = 'Relationship of Arrest Types & Urban Population', 
            value = 'relation',
            radioButtons(
              inputId = 'fit', 
              label = 'Select smooth method', 
              choices = c('loess', 'lm'), 
              selected = 'lm', 
              inline = TRUE
            ),
            withSpinner(plotlyOutput('scatter'))
          )
        )
      ),
      
      # Third tab item
      tabItem(
        tabName = 'map',
        box(
          selectInput(
            'crimetype', 
            'Select Arrest Type', 
            choices = c2, 
            selected = 'Assault', 
            width = 250
          )
        ),
        withSpinner(plotOutput('map_plot')), 
        width = 12
      )
    )
  )
)
