library(shiny)
library(ggplot2)
library(dplyr)
library(reshape2)
library(scales)
library(tidyr)

data = read.csv("data//teams.csv",
                header=T,
                sep=";",
                stringsAsFactors = FALSE)

# precalculations for Compare! tab

team_molten = melt(data[,-c(2,3)],id.vars="team")
teamdf = tbl_df(team_molten)

# precalculations for Explore! tab

box = ggplot(data,aes(x=league))

# precalculations for Cluster! tab

hold = 4:ncol(data)
PCAdata = prcomp(data[,hold],center=TRUE,scale.=TRUE)

shinyServer(function(input,output){
    
    # used in Cluster! tab
    
    clus_data = reactive({
        clust = kmeans(PCAdata$x[,1:2],centers=input$cl_num)
        factor(clust$cluster)
    })
    
    bound_data = reactive({
        relevance = ifelse(data$league==input$league_picker,1,0)
        data.frame(data,PCAdata$x[,1:2],
              cluster = clus_data(),
              relevance
        )
    })
    
    # for zooming interaction
    ranges <- reactiveValues(x = NULL, y = NULL)

    output$plotPCA = renderPlot({
        
        ggplot(bound_data(),aes(x=PC1,y=PC2,color=cluster)) + 
            scale_alpha_continuous(range=c(0,1),guide="none") +
            geom_point(aes(alpha=.3+relevance),size=4) +
            theme_minimal() + 
            theme(legend.position="top",
                  panel.grid=element_blank(),
                  axis.ticks=element_blank(),
                  axis.title=element_blank(),
                  axis.text=element_blank()) +
            geom_text(aes(alpha=relevance,
                          label=team),
                      size=4,
                      vjust=1,
                      hjust=1)+
            coord_cartesian(xlim = ranges$x, ylim = ranges$y)
            
        })
    
    # for zooming interaction
    observeEvent(input$plot_dblclick, {
        brush <- input$plot_brush
        if (!is.null(brush)) {
            ranges$x <- c(brush$xmin, brush$xmax)
            ranges$y <- c(brush$ymin, brush$ymax)
            
        } else {
            ranges$x <- NULL
            ranges$y <- NULL
        }
    })
    
    # heatmap
    
    
    output$heatmap = renderPlot({
    
        d = tbl_df(data.frame(data[4:ncol(data)],cluster = clus_data()))
        
        f = d%>%group_by(cluster)%>%summarise_each(funs(mean))
        non_scaled_molten = melt(data.frame(f),id.vars="cluster")
        
        f_scaled = apply(f[,-1],2,rescale)
        f = cbind(cluster=factor(f$cluster),f_scaled)
        molten = melt(data.frame(f),id.vars="cluster")
        
        
        
        q = ggplot(molten,aes(x=factor(cluster),
                              y=variable))
        q + geom_tile(aes(fill=value),color="white") +
            labs(x="cluster",y="",title="Cluster profiles") + 
            scale_fill_continuous(low="white",
                                  high="steelblue",
                                  name="",
                                  breaks=c(0,1),
                                  labels=c("lowest", "highest")) + 
            theme_bw() + theme(panel.grid=element_blank())    
        
        
        
    })
   
    # cluster table
    
    tab_data = reactive({
        data.frame(league = data$league ,cluster = clus_data())
    })
    
    
    output$clust_table = renderDataTable({
       f = tbl_df(data.frame(round(table(tab_data()$league,tab_data()$cluster),2)))
       f%>%spread(Var2,Freq)%>%rename(league=Var1)
        
        
    },options = list(searching=FALSE,paging=FALSE))
    
    
    
    
    
    
    # used in Compare! Tab
    
    filtered = reactive({
        flt = c(input$BR_pick,
                input$DE_pick,
                input$UK2_pick,
                input$NL_pick,
                input$SP_pick,
                input$FR_pick,
                input$US_pick,
                input$UK1_pick,
                input$RU_pick,
                input$IT_pick,
                input$TR_pick
                )
        teamdf%>%filter(team %in% flt)
    })
    
    output$selected = renderText({
        paste("Selected teams: ",length(unique(filtered()$team)))})
    
    output$facetplot = renderPlot({
        
        ggplot(filtered(),aes(x=team,y=value,fill=team)) + 
        geom_bar(stat="identity") + 
        facet_wrap(~variable,scales="free_y") +
        theme_bw() +
        theme(panel.grid=element_blank(),
              axis.text.x = element_blank(),
              axis.ticks.x = element_blank()) +
        labs(x="",y="") + 
        scale_fill_brewer(type="qual",palette=3)
        
    })
    
    # used in Explore! tab
    
    output$table = renderDataTable({
        table_data = cbind(data[,c(1,3)],round(data[,input$show],2))
        table_data
    },options = list(pageLength=25)
    )
  
    output$boxplot1 = renderPlot({
        box + 
            geom_boxplot(aes_string(y=input$box_sel1),
                         outlier.size = 4,
                         outlier.colour = "red") +
            labs(x="",
                 y="")+
            theme_minimal()+
            theme(axis.text.x = element_text(angle=30,size=12),
                  axis.ticks.x = element_blank(),
                  panel.grid = element_blank()) 
        })
    
    
    
    
})

