library(shiny)
data = read.csv("data//teams.csv",
                header=T,
                sep=";",
                stringsAsFactors = FALSE)

#precalculations

variables = colnames(data[,4:ncol(data)])

leagues=sort(unique(data$league))
countries=c("BR","DE","UK2","NL","SP","FR","US","UK1","RU","IT","TR")
teams=vector("list",11)
outer=1
for (index in leagues){
    teams[[outer]] = sort(data$team[data$league==index])
    outer = outer + 1
}


shinyUI(navbarPage("Soccerlytics",
                   tabPanel("Compare!",
                            fluidPage(
                                fluidRow(wellPanel(
                                    tags$em("Select the teams you want to compare across the multiple
                                      dimensions of the dataset from the menus on the left of 
                                      the plot."),
                                    tags$em("Make sure you select at least 2 teams and no more than 12."),
                                    tags$em("Just refresh your browser page to clear your selection.")
                                    )
                                    
                                    
                                ),
                                
                                fluidRow(
                                    column(3,
                                           verbatimTextOutput("selected"),
                                           radioButtons("lea","League",
                                                        choices=leagues,
                                                        selected="Brasileirao"),
                                           conditionalPanel(
                                               condition="input.lea=='Brasileirao'",
                                               checkboxGroupInput("BR_pick","Brasileirao",
                                                                  choices=teams[[1]])),
                                           conditionalPanel(
                                               condition="input.lea=='Bundesliga'",
                                               checkboxGroupInput("DE_pick","Bundesliga",
                                                                  choices=teams[[2]],
                                                                  selected ="Bayern Munich")),
                                           conditionalPanel(
                                               condition="input.lea=='Championship'",
                                               checkboxGroupInput("UK2_pick","Championship",
                                                                  choices=teams[[3]])),
                                           conditionalPanel(
                                               condition="input.lea=='Eredivisie'",
                                               checkboxGroupInput("NL_pick","Eredivisie",
                                                                  choices=teams[[4]])),
                                           conditionalPanel(
                                               condition="input.lea=='La Liga'",
                                               checkboxGroupInput("SP_pick","La Liga",
                                                                  choices=teams[[5]],
                                                                  selected ="Barcelona")),
                                           conditionalPanel(
                                               condition="input.lea=='Ligue 1'",
                                               checkboxGroupInput("FR_pick","Ligue 1",
                                                                  choices=teams[[6]])),
                                           conditionalPanel(
                                               condition="input.lea=='MLS'",
                                               checkboxGroupInput("US_pick","MLS",
                                                                  choices=teams[[7]])),
                                           conditionalPanel(
                                               condition="input.lea=='Premier League'",
                                               checkboxGroupInput("UK1_pick","Premier League",
                                                                  choices=teams[[8]])),
                                           conditionalPanel(
                                               condition="input.lea=='Russian PL'",
                                               checkboxGroupInput("RU_pick","Russian PL",
                                                                  choices=teams[[9]])),
                                           conditionalPanel(
                                               condition="input.lea=='Serie A'",
                                               checkboxGroupInput("IT_pick","Serie A",
                                                                  choices=teams[[10]])),
                                           conditionalPanel(
                                               condition="input.lea=='Super Lig'",
                                               checkboxGroupInput("TR_pick","Super Lig",
                                                                  choices=teams[[11]]))
                                           ),   
                                    column(9,
                                           # non elegant!!! javascript knowledge required though
                                           conditionalPanel(condition = "input.BR_pick.length +
                                                                input.DE_pick.length +
                                                                input.UK2_pick.length +
                                                                input.NL_pick.length +
                                                                input.SP_pick.length +
                                                                input.FR_pick.length +
                                                                input.US_pick.length +
                                                                input.UK1_pick.length +
                                                                input.RU_pick.length +
                                                                input.IT_pick.length +
                                                                input.TR_pick.length < 2",
                                                            tags$h1("Please select at least 2 teams")),
                                           conditionalPanel(condition = "input.BR_pick.length +
                                                            input.DE_pick.length +
                                                            input.UK2_pick.length +
                                                            input.NL_pick.length +
                                                            input.SP_pick.length +
                                                            input.FR_pick.length +
                                                            input.US_pick.length +
                                                            input.UK1_pick.length +
                                                            input.RU_pick.length +
                                                            input.IT_pick.length +
                                                            input.TR_pick.length > 12",
                                                            tags$h1("Please select no more than 12 teams")),
                                           conditionalPanel(condition = "(input.BR_pick.length +
                                                            input.DE_pick.length +
                                                            input.UK2_pick.length +
                                                            input.NL_pick.length +
                                                            input.SP_pick.length +
                                                            input.FR_pick.length +
                                                            input.US_pick.length +
                                                            input.UK1_pick.length +
                                                            input.RU_pick.length +
                                                            input.IT_pick.length +
                                                            input.TR_pick.length > 1) &&
                                                            (input.BR_pick.length +
                                                            input.DE_pick.length +
                                                            input.UK2_pick.length +
                                                            input.NL_pick.length +
                                                            input.SP_pick.length +
                                                            input.FR_pick.length +
                                                            input.US_pick.length +
                                                            input.UK1_pick.length +
                                                            input.RU_pick.length +
                                                            input.IT_pick.length +
                                                            input.TR_pick.length < 13)",
                                                            plotOutput("facetplot",
                                                            height="900px")
                                                            )
                                           
                                           )
                                )
                            )
                   ),
            
                   
                   tabPanel("Cluster!",
                            fluidPage(
                                fluidRow(column(3,
                                                sliderInput("cl_num",
                                                            "Select number of clusters",
                                                            min=2,
                                                            max=12,
                                                            value=5,
                                                            step=1)),
                                         column(9,
                                                wellPanel(
                                                    em("Select number of clusters from 
                                                  the slider on the left")
                                                )
                                                )
                                ),
                                fluidRow(
                                    column(3,
                                           radioButtons("league_picker",
                                                        "League",
                                                        choices=leagues,
                                                        selected="Brasileirao"
                                           ),
                                           wellPanel(
                                               tags$em("Pick the league from the above menu."),
                                               tags$hr(),
                                               tags$em("Click and drag on plot area to make a 
                                                 selection and double click into selected 
                                                 area to zoom in (works recursively also)."),
                                               tags$em("Double click on any unselected area of 
                                                 the plot to zoom out to original plot view."),
                                               tags$hr(),
                                               tags$em("Cluster profiles below show how
                                                 clusters are differentiated. The table
                                                 reports how many teams from different leagues belong
                                                 to each cluster")
                                           )
                                    ),
                                    column(9,
                                           plotOutput("plotPCA",height="700px",
                                                      dblclick = "plot_dblclick",
                                                      brush = brushOpts(
                                                            id = "plot_brush",
                                                            resetOnNew = TRUE)
                                           )
                                    )
                                ),
    
                                fluidRow(column(6,
                                                plotOutput("heatmap")),
                                         column(6,
                                                dataTableOutput("clust_table"),
                                                align="center")
                                         
                                         
                                         
                                    
                                )
                                
                            )
                   ),
                   
                   
                   
                   
                   
                   tabPanel("Explore!",
                        fluidPage(
                            fluidRow(
                                column(4,
                                       conditionalPanel(
                                           condition = "input.box_sel1 == 'fouls'",
                                           wellPanel(
                                               tags$b("And the fair play award goes to.... UK!!!")
                                               )
                                               
                                           ),
                                       conditionalPanel(
                                           condition = "input.box_sel1 == 'long_balls'",
                                           wellPanel(
                                               tags$b("Do Brasilian tiki taka masters need energy drinks
                                                 to shot that ball any farther?!?")
                                               )
                                           ),
                                       conditionalPanel(
                                           condition = "input.box_sel1 == 'aerials'",
                                           wellPanel(
                                               tags$b("German teams use their heads!!!")
                                               )
                                           )
                                           
                                       ),
                                column(4,
                                       selectInput("box_sel1",
                                                   label="Select dimension",
                                                   choices=variables,
                                                   selected="tackles")
                                       ),
                                column(4,
                                       wellPanel(
                                           tags$em("Use the dropdown menu to pick the
                                             dimension you want to analyze and check how 
                                             it is distributed among teams from different leagues. 
                                             Don't miss the occasional key insights in the top 
                                             left corner!")
                                           
                                        )
                                    
                                       )
                                ),
                            fluidRow(
                                plotOutput("boxplot1")
                            ),
                            
                            
                            
                            
                            
                            fluidRow(
                                column(3,
                                       wellPanel(
                                           tags$em("Pick the dimensions you want to be shown in the table from
                                             the checkbox below."),
                                           tags$hr(),
                                           tags$em("Use table's filtering and ordering features to dig
                                             deeper into data and finally name those unlabeled red dots
                                             in the plot!"),
                                           tags$hr(),
                                           tags$em("Beware that picking or excluding dimensions will force
                                             the table to recalculate so you will lose filtered or ordered
                                             layouts")
                                       ),
                                       checkboxGroupInput("show",
                                                          label="Show dimensions",
                                                          choices=variables,
                                                          selected=c("tackles","dribbles",
                                                                     "long_balls","fouls"))
                                ),
                                column(9,
                                       dataTableOutput("table")
                                       
                                )
                            )
                            )
                        ),
               
               
               
               
               
               
               
               tabPanel("Documentation",
                        fluidRow(
                            wellPanel(
                                tags$h1("Soccerlytics"),
                                tags$p("Soccerlytics application provides an analytic framework to
                                       inspect a soccer related dataset and focuses on finding differences
                                       in playing styles between teams and between leagues. It was inspired by ",
                                       tags$a(href="http://www.optasportspro.com/en/about/optapro-blog/posts/2015/film-square-pegs-for-square-holes/",
                                              "this seminar presentation"),
                                       "by Will Gürpınar-Morgan while extending the exposed principles
                                       to",tags$b("teams"),"rather than players."),
                                tags$h2("Dataset"),
                                tags$p("Data was gathered at",
                                       tags$a(href="http://www.whoscored.com/","www.whoscored.com"),
                                       "(originally collected by",
                                       tags$a(href="http://www.optasports.com/","Opta"),
                                       "), and processed to obtain the final dataset dimensions."),
                                tags$p(tags$b("Observations"),
                                       "in the dataset represent a total of",
                                       tags$b("214"),
                                       "teams participating in",
                                       tags$b("11"),
                                       "different leagues all over the world:",
                                       tags$li("Brasileirao - ",tags$b("Brasil")),
                                       tags$li("Bundesliga - ",tags$b("Germany")),
                                       tags$li("Championship - ",tags$b("UK "),"lesser division"),
                                       tags$li("Eredivise - ",tags$b("Netherlands")),
                                       tags$li("La Liga - ",tags$b("Spain")),
                                       tags$li("MLS - ",tags$b("USA")),
                                       tags$li("Premier League - ",tags$b("UK "),"top division"),
                                       tags$li("Russian Premier League - ",tags$b("Russia")),
                                       tags$li("Serie A - ",tags$b("Italy")),
                                       tags$li("Super Lig - ",tags$b("Turkey"))
                                       ),
                                tags$p(tags$b("Dimensions"),
                                       "in the dataset have been averaged on a ",
                                       tags$b("per game"),
                                       "basis taking into account that teams have played
                                       a different number of games. All data refer to current
                                       seasons up to 21/12/2015 (USA and Brasil regular seasons have already
                                       finished and no extended season matches have been taken into account."),
                                tags$p("Dimensions represent:",
                                       tags$li(tags$b("tackles "),"- average number of successful tackles per game"),
                                       tags$li(tags$b("challenges "),"- average number of attempted tackles per game"),
                                       tags$li(tags$b("interceptions "),"- average number of intercepted passes per game"),
                                       tags$li(tags$b("fouls "),"- average number of committed fouls per game"),
                                       tags$li(tags$b("clerances "),"- average number of clared balls per game"),
                                       tags$li(tags$b("blocked_shots "),"- average number of blocked shots per game"),
                                       tags$li(tags$b("passes "),"- average number of attempted passes per game"),
                                       tags$li(tags$b("chances_openplay "),"- average number of chances in open play situations per game"),
                                       tags$li(tags$b("crosses "),"- average attempted crosses per game"),
                                       tags$li(tags$b("long_balls "),"- average number of long passes per game"),
                                       tags$li(tags$b("through_balls "),"- average numbers of thorugh balls leading to scoring chances per game"),
                                       tags$li(tags$b("shots_outbox "),"- average number of shots taken from outside penalty area per game"),
                                       tags$li(tags$b("shots_inbox "),"- average number of shots taken from inside penalty area per game"),
                                       tags$li(tags$b("dribbles "),"- average number of attempted dribbles per game"),
                                       tags$li(tags$b("aerials "),"- average number of aerial duels per game"),
                                       tags$li(tags$b("fouled "),"- average number of suffered fouls per game")
                                       ),
                                tags$h2("Compare!"),
                                tags$p("This sub-application is meant to compare teams across all the dimensions
                                       in the dataset. Users can pick whichever team from whichever league and compare
                                       them in order to gain insights related to their play styles. It is arbitrarily
                                       meant to work with a range of 2 to 12 teams (2 teams being the minimum number for
                                       a comparison and 12 teams being the upper limit mainly due to using highly 
                                       discriminating color palettes)."),
                                tags$p("In-page instructions will guide you through your comparisons."),
                                tags$h2("Cluster!"),
                                tags$p("This sub-application is heavily inspired by Will Gürpınar-Morgan's work cited above.
                                       It reduces dimensionality of the dataset through",
                                       tags$b("principal component analysis"),
                                       "technique to find inherent structure in underlying data and plots every observation 
                                        (team) in the 2 dimensional space of the first 2 principal components. Upon these components",
                                       tags$b("k-means"),
                                       "clustering algorithm is run and all teams are assigned to a cluster (users can choose the number of
                                        clusters thus determining how many groups to create) and each cluster is 
                                       represented by a different color in the plot."),
                                tags$p("All the teams are represented in the background of the plot and only the teams in the chosen league
                                       are highlighted. Users can zoom in in order to inspect particular regions of the plot."),
                                tags$p("Teams belonging to the same 
                                       cluster share a particular style of play, which is summarised in the",
                                       tags$b("heatmap"),
                                       "plot named",
                                       tags$em("Cluster profiles"),
                                       ": dimensions' values are averaged among teams in each cluster and visually represented
                                       by a color gradient ranging from lower to higher values."
                                       ),
                                tags$p("Users could end up, for instance, with a cluster characterized by high average values of shots,
                                       passes and chances: teams belonging to such cluster are likely to be the ones dominating the game through
                                       ball possession and will eventually result in being the more successful ones."),
                                tags$p("On the other hand, users might have a cluster characterized by high average values of clearances and challenges
                                       and lower average values of shots and chances: teams belonging to this clusters are likely to suffer opponents'
                                       dominance and will eventually result in being the less successful ones."),
                                tags$p("As a last example, a cluster might be characterized by high average number of aerials and long balls, which 
                                       is typical for the play stile of some UK teams"),
                                tags$p("Finally, a table summarises the contribution (expressed as number of teams) of each league to each cluster."),
                                tags$p("In-page instructions will guide you through your clustering operations."),
                                tags$em("Please note that results from k-means algorithm might be inconsistent between different calls, meaning a team could
                                        be assigned to different clusters at different calls even if the same clusters number is
                                        chosen. This is peculiar to k-means clustering."),
                                tags$h2("Explore!"),
                                tags$p("This sub-application give users the chance to explore each dimension of the dataset and
                                       examine their distributions after grouping observations by league, highlighting league-dependent
                                       differences in game styles and anomalies within leagues."),
                                tags$p("It also gives users full access to all informations stored in dataset."),
                                tags$p("In-page instructions will guide you through your explorations."),
                                tags$em("Please note there's a known bug: when only one dimension is picked for table
                                        analysis, column mislabeling occurs.")
                                
                                
                            )
                        )
                        
               )
)
)