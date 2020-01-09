
#https://datascience-enthusiast.com/R/shiny_ML.html
#https://github.com/ismyrnow/leaflet-groupedlayercontrol

#########################################################


########################################################
function(input, output, session) {
  
  #result_auth <- secure_server(check_credentials = check_credentials(credentials))
  
  #output$res_auth <- renderPrint({
  #  reactiveValuesToList(result_auth)
  #})
  
  
  '  # Create data table
  output$all.dataset <- DT::renderDataTable({
    DT::datatable(data = ClustereData %>% dplyr::select(1:46),
                  options = list(pageLength = 20), 
                  rownames = FALSE, extensions="Scroller", style="bootstrap", class="compact", width="100%",
                  options=list(deferRender=TRUE, scrollY=300, scroller=TRUE))
  })'
  
  '  output$pivot <- renderRpivotTable({
    rpivotTable(data =   dfpivot ,  
                rows = c("DES_REGIONE_ABI_GEST") , #,"DES_PROVINCIA_ABI_GEST" for this update dfpivot
                cols="c_I1I2Indice",vals = "Freq", aggregatorName = "Count", 
                rendererName = "Table", subtotals = TRUE)#width="200%", height="600px")
  })'
  
  '  output$scatterplot <- renderPlot({
    ggplot(data = ClustereData, aes_string(x = input$x, y = input$y,color=input$z,size=input$z)) +
      geom_point(alpha = input$alpha)
  })'
  
  '  output$histplot.x <- renderPlot({
    ggplot(data = ClustereData, aes_string(x = input$x)) +
      geom_histogram(aes(y = ..density..),bins = 100,col="darkgreen",fill="darkgreen")+
      geom_density(alpha=.2, fill="#FF6666",col = "red")
    # stat_function(fun = dnorm ,args = fun_args.x,  col = "blue") 
     })'
  
  '  #  fun_args.y <- list(mean = mean(ClustereData$y), sd = sd(ClustereData$y))
  output$histplot.y <- renderPlot({
    ggplot(data = ClustereData, aes_string(x= input$y)) +
      geom_histogram(aes(y = ..density..),bins = 100,col="darkgreen",fill="darkgreen")+
      geom_density(col = "red",alpha=.2, fill="#FF6666")
    # stat_function(fun = dnorm,args = fun_args.y, col = "blue") 
  })'
  
  '  output$df.print <- renderTable ({
    ClustereData %>%
      #group_by("c_I1I2Indice")%>%
      summarise(mean.indice =round( mean(INDICE), digits = 5),
                mean.investment =round(  mean(index.oninvestment),digits = 5),
                mean.protection = round( mean(index.on.prodection),digits = 5))
  })'
  
  '  output$mytable1 <- renderTable ({
    ClustereData %>%
      group_by_(input$z) %>%
      summarize(mean.Indice = round( mean(INDICE), digits = 5), 
                mean.Index.on.Investment=round(  mean(index.oninvestment), digits = 5), 
                mean.Index.on.Protection = round( mean(index.on.prodection), digits = 5)) %>%
      arrange(desc(mean.Indice))    
  })'
  
  #output$pivot <- renderRpivotTable({
  #  rpivotTable(data =   dfpivot ,  
  #              rows = c("DES_REGIONE_ABI_GEST") , #,"DES_PROVINCIA_ABI_GEST" for this update dfpivot
  #              cols="c_I1I2Indice",vals = "Freq", aggregatorName = "Count", 
  #              rendererName = "Table", subtotals = TRUE)#width="200%", height="600px")
  #})
  
  '  output$mytable = DT::renderDataTable({
    DT::datatable(data= ClustedMean, options = list(pageLength = 11))
  })' 
  
  '  # Create data table
  output$alldataset <- DT::renderDataTable({
    DT::datatable(data = ClustereData %>% dplyr::select(1:46),
                  options = list(pageLength = 20), 
                  rownames = FALSE)
  })'
  
  '  # Create text output stating the correlation between the two ploted 
  output$correlation <- renderText({
    r <- round(cor(ClustereData[, input$x], ClustereData[, input$y], use = "pairwise"), 3)
    paste0("Correlation = ", r, ". Note: If the relationship between the two variables is not linear,
             the correlation coefficient will not be meaningful.")
  })'
  
  '  # Print data table
  output$moviestable <- DT::renderDataTable({
    brushedPoints(ClustereData,input$plot_brush)%>%
      # nearPoints(ClustereData, coordinfo = input$plot_hover) %>% 
      dplyr::select(COD_CAB_GEST, REGIONE ,DES_REGIONE_ABI_GEST,DES_PROVINCIA_ABI_GEST, c_I1I2Indice)
  })'
  
  '  # Download file Selected Graf
  output$download_data1 <- downloadHandler(
    filename = function() {
      paste0("selecteddata.", input$filetype)
    },
    content = function(file) { 
      if(input$filetype == "csv"){ 
        write_csv(moviestable %>% select(input$plot_brush)  , path = file) 
      }
      if(input$filetype == "tsv"){ 
        write_tsv(moviestable %>% select(input$plot_brush), path = file) 
      }
    }
  )'
  
  
  ' # Download file
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("ClustereData.", input$filetype)
    },
    content = function(file) { 
      if(input$filetype == "csv"){ 
        write_csv(ClustereData %>% dplyr::select(input$selected_var), path = file) 
      }
      if(input$filetype == "tsv"){ 
        write_tsv(ClustereData %>% dplyr::select(input$selected_var), path = file) 
      }
    }
  )
  '
  #  myy.data <- ClustereData %>% dplyr::select(COD_CAB_GEST, REGIONE ,DES_REGIONE_ABI_GEST,DES_PROVINCIA_ABI_GEST, c_I1I2Indice)
  #  thedata <- reactive(myy.data)
  
  # output$Raw <- renderPrint({
  #    #if(input$tabs == 5){
  #    output$alldataset <- DT::renderDataTable(
  #      DT::datatable(thedata(), filter = 'top',escape = FALSE, 
  #                    options = list(pageLength = 15, scrollX='500px',autoWidth = TRUE)))
  #    # }
  #  })
  
  #  output$downLoadFilter <- downloadHandler(
  #    filename = function() {
  #      paste('Filtered data-', Sys.Date(), '.csv', sep = '')
  #    },
  #    content = function(file){
  #      if(input$filetype == "csv"){ 
  #        write.csv(thedata()[input[["alldataset_rows_all"]], ],file)
  #      }
  #      if(input$filetype == "tsv"){ 
  #        write_tsv(thedata()[input[["alldataset_rows_all"]], ],file)
  #      }
  #    }
  #  )
  
  selectedData <- reactive({
    ClustereData[, c(input$xcol, input$ycol, input$zcol )]
  })
  
  clusters <- reactive({
    set.seed(87)
    kmeans(selectedData(), input$clusters)
  })
  
  #output$plot1 <- renderPlot({plotcluster(selectedData(), clusters$cluster) })
  
  output$plot1 <- renderPlot({
    palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
              "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
    #par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
         col = clusters()$cluster,
         pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4, col = input$alpha)
  })
  
  output$plot.elbow <- renderPlot({
    wss <- 0
    # For 1 to 15 cluster centers
    set.seed(87)
    for (i in 1:15) {
      set.seed(87)
      km.out_c_I1I2Indice <- kmeans(selectedData(), centers = i, nstart = 20) #, iter.max = 50
      # Save total within sum of squares to wss variable
      wss[i] <- km.out_c_I1I2Indice$tot.withinss
    }
    # Plot total within sum of squares vs. number of clusters
    plot(1:15, wss, type = "b", 
         xlab = "Number of Clusters", 
         ylab = "Within groups sum of squares")
  })
  
  
  
  
  
  "##add the cluster in the data
combined_AML$c_knn_I1I2Indice <- as.factor(km.out_c_I1I2Indice$cluster) 
head(combined_AML,2)"
  
  
  output$cluster.number <- DT::renderDataTable({
    set.seed(87)
    fit = kmeans(selectedData(),input$clusters, nstart = 20, iter.max = 50)
    #fit = kmeans(selectedData(),input$clusters)
    ClustereData$cluster.number <- as.integer(fit$cluster)
    ClustereData <- ClustereData %>%
      dplyr::select(COD_CAB_GEST, REGIONE, DES_REGIONE_ABI_GEST,
                    DES_PROVINCIA_ABI_GEST, c_I1I2Indice, cluster.number)
    #ClustereData <- arrange(selectedData(), input$xcol)
    ClustereData = DT::datatable(ClustereData, filter = 'top',escape = FALSE, 
                                 options = list(pageLength = 15, scrollX='500px',autoWidth = TRUE))
    return( ClustereData)
  })
  
  
  
  #  output$filtered_row <- 
  #     renderPrint({
  #      input[["cluster.number_rows_all"]]
  #    })
  
  
  output$cluster.downLoadFilter <- 
    downloadHandler(
      filename = "Filtered Data.csv",
      content = function(file){
        write.csv(ClustereData[input[["cluster.number_rows_all"]], ],
                  file)
      }
    )
  

 #  output$cluster.downLoadFilter <- 
#     downloadHandler(
#      filename = function() {
#        paste('Filtered data-', Sys.Date(), '.csv', sep = '')
#      },
#      content = function(file){
#        if(input$filetype == "csv"){ 
#          write.csv(ClustereData[input[["cluster.number_rows_all"]], ],file)
#        }
#        if(input$filetype == "tsv"){ 
#          readr::write_tsv(ClustereData[input[["cluster.number_rows_all"]], ],file)
#        }
#      }
#   )
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ' output$elbow.table <-  renderPrint({  
   kmeans(selectedData(), centers =  clusters()$centers, nstart = 20, iter.max = 50)
   })'#[1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss" "betweenss"    "size"         "iter"         "ifault"  
  
  output$elbow.table  <-  renderPrint({  
    set.seed(87)
    fit = kmeans(selectedData(),input$clusters, nstart = 20, iter.max = 50)
    #fit = kmeans(selectedData(),input$clusters)
    
    #a = as.data.frame(fit$centers)
    #a = a[order(a[,1], decreasing = TRUE),]
    
    Segment.Membership = as.character(fit$cluster)
    
    withinss = fit$withinss
    tot.withinss = fit$tot.withinss
    betweenss = fit$betweenss
    clustmeans = aggregate(selectedData(),by = list(Segment.Membership), FUN = mean)
    
    
    #a <- clustmeans[order(clustmeans$INDICE, decreasing = TRUE),]
    #a$Group.1 <- seq(1:input$clusters)
    #clustmeans.a.size <- aggregate(c_I1I2Indice,by=list(fit$cluster), FUN = length)
    
    Summary = list(#Segment.Membership = table(Segment.Membership), 
      `Mean of Each Cluster` = clustmeans, `Number of Members in Each Cluster` = table(Segment.Membership),
      `Cluster Withinss` = withinss, `Cluster tot.withinss` = tot.withinss,
      `Cluster Betweenss` = betweenss)#, a1 = a, clustmeans.a.size = clustmeans.a.size )
    Summary
  })
  
  output$sum2 <- renderPrint({
    summary(ClustereData[,(input$var)])
  })
  
  output$histplot.x1 <- renderPlot({
    ggplot(data = ClustereData, aes_string(x = input$xcol)) +
      geom_histogram(aes(y = ..density..),bins = 100,col="darkgreen",fill="darkgreen")+
      geom_density(alpha=.2, fill="#FF6666",col = "red")
    # stat_function(fun = dnorm ,args = fun_args.x,  col = "blue") 
  })
  
  # fun_args.y <- list(mean = mean(ClustereData$y), sd = sd(ClustereData$y))
  output$histplot.y1 <- renderPlot({
    ggplot(data = ClustereData, aes_string(x= input$ycol)) +
      geom_histogram(aes(y = ..density..),bins = 100,col="darkgreen",fill="darkgreen")+
      geom_density(col = "red",alpha=.2, fill="#FF6666")
    # stat_function(fun = dnorm,args = fun_args.y, col = "blue") 
  })
  
  output$histplot.z1 <- renderPlot({
    ggplot(data = ClustereData, aes_string(x= input$zcol)) +
      geom_histogram(aes(y = ..density..),bins = 100,col="darkgreen",fill="darkgreen")+
      geom_density(col = "red",alpha=.2, fill="#FF6666")
    # stat_function(fun = dnorm,args = fun_args.y, col = "blue") 
  })
  
  ' # Create text output stating the correlation between the two ploted 
  output$correlation1 <- renderText({
     r1 <- round(cor(ClustereData[, input$xcol], ClustereData[, input$ycol], use = "pairwise"), 4)
     paste0("Correlation = ", r1, ". Note: If the relationship between the xcol and ycol is not linear,
             the correlation coefficient will not be meaningful.")
  })
    
  output$correlation2 <- renderText({
    r2 <- round(cor(ClustereData[, input$xcol], ClustereData[, input$zcol], use = "pairwise"), 4)
    paste0("Correlation = ", r2, ". Note: If the relationship between the xcol and zcol is not linear,
             the correlation coefficient will not be meaningful.")
  })
  
  output$correlation3 <- renderText({
    r3 <- round(cor(ClustereData[, input$ycol], ClustereData[, input$zcol], use = "pairwise"), 4)
    paste0("Correlation = ", r3, ". Note: If the relationship between the ycol and zcol is not linear,
             the correlation coefficient will not be meaningful.")
  })
  '
  
  
  output$contents <- renderTable({
    # load your data
    data <- mydata()
    if(input$disp == "head") {
      return(head(data,5))
    }
  }) # https://shiny.rstudio.com/articles/upload.html
  
  
  RF = reactive({
    RF = caret::train(as.formula(paste(input$yAttr, paste( input$xAttr, collapse = ' + '), sep=" ~ ")),
                      data = mydata(), method = "ranger")
    return(RF)
  })
  
  output$RFsummary = renderPrint({
    RF()
  })
  
  
  LM = reactive({
    LM = caret::train(as.formula(paste(input$yAttr, paste( input$xAttr, collapse = ' + '), sep=" ~ ")),
                      data = mydata(), method="lm")
    return(LM)
  })
  
  output$LMsummary = renderPrint({
    LM()
  }) 
  
  
  output$y.value = renderPrint({
    #input$yAttr 
    # a = (paste(input$yAttr,"~", paste(input$xAttr, collapse = "+") , sep=""))
    a =  as.formula(paste(input$yAttr, paste( input$xAttr, collapse = ' + '), sep=" ~ "))
    return(a)
  })  
  
  
  output$obs <- renderTable({
    
    RMSE.rf <- mean(RF()$resample[[1]]) # RMSE
    RMSE.lm <- mean(LM()$resample[[1]])
    
    RSQUARED.rf <- mean(RF()$resample[[2]]) #RSQUARED
    RSQUARED.lm <- mean(LM()$resample[[2]])
    
    MAE.rf <- mean(RF()$resample[[3]]) # MAE
    MAE.lm <- mean(LM()$resample[[3]])
    
    
    N_metrics <- matrix(c( RMSE.rf, RMSE.lm, RSQUARED.rf, RSQUARED.lm,  MAE.rf,  MAE.lm ), ncol = 3)
    
    colnames(N_metrics) <- c("RMSE", "R-SQUARED", "MAE")
    row.names(N_metrics) <- c ("RF", "LM")
    
    N_metrics
  }, rownames = TRUE)
  
  '  output$obs.text <- renderText({
    paste0("Correlation = ",  mtcars[[1]][1], ". Note: If the relationship between the xcol and ycol is not linear,
             the correlation coefficient will not be meaningful.")
  })'
  
  '  output$sum <- renderTable({
    summary(caret::resamples(list( rf =  RF(), lm =  LM())))$values
  })'
  
  output$sum1 <- renderPrint({
    summary(caret::resamples(list( rf =  RF(), lm =  LM())))
  })
  
  output$plot <- renderPlot({
    dotplot(resamples(list( rf =  RF(), lm =  LM())))
  })
  
  ################
  
  
  Dataset <- reactive({
    if (is.null(input$file)) { return(NULL) }
    else{
      Dataset <- as.data.frame(read.csv(input$file$datapath ,header=TRUE, sep = ","))
      return(Dataset)
    }
  })
  
  # Select variables:
  output$yvarselect <- renderUI({
    if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
    
    selectInput("yAttr", "Select Y variable",
                colnames(Dataset()), colnames(Dataset())[1])
  })
  
  output$xvarselect <- renderUI({
    if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
    
    checkboxGroupInput("xAttr", "Select X variables",            
                       choices = setdiff(colnames(Dataset()),input$yAttr), 
                       selected = FALSE ) #DONT SELECT ALL THE Xs.
    #selected = setdiff(colnames(Dataset()),input$yAttr))
  })
  
  output$fxvarselect <- renderUI({
    if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
    
    checkboxGroupInput("fxAttr", "Select factor variables in X",  
                       setdiff(colnames(Dataset()),input$yAttr),"" )
    
  })
  
  mydata = reactive({
    mydata = Dataset()[,c(input$yAttr,input$xAttr)]
    
    if (length(input$fxAttr) >= 1){
      for (j in 1:length(input$fxAttr)){
        mydata[,input$fxAttr[j]] = factor(mydata[,input$fxAttr[j]])
      }
    }
    return(mydata)
  })
  
  
  
  #my.data <- ClustereData %>% dplyr::select(COD_CAB_GEST, REGIONE ,DES_REGIONE_ABI_GEST,DES_PROVINCIA_ABI_GEST, c_I1I2Indice)
  #thedata <- reactive(my.data)
  #thedata <- reactive(cluster.number)
  # thedata <- mtcars
  
  #  output$cluster.downLoadFilter <- downloadHandler(
  #    filename = function() {
  #      paste('Filtered data-', Sys.Date(), '.csv', sep = '')
  #    },
  #    content = function(file){
  #      if(input$filetype == "csv"){ 
  #        write.csv(thedata[input[["alldataset_rows_all"]], ],file)
  #      }
  #      if(input$filetype == "tsv"){ 
  #        write_tsv(thedata[input[["alldataset_rows_all"]], ],file)
  #      }
  #    }
  # )
  
  
  
  
  
  
  
  
  
  
  
  
  
  '# Reactive expression called whenever inputs change.
  data <- reactive({
    responseRoutine(input$method, input$centerscale, input$trainpercent, input$randomseed)
  })
  
  # Fill-in the tabs with output from caret
  
  output$fit <- renderPrint({
  data()$fit
  })
  
  output$my.resample <- renderPrint({
    data()$my.resample
  })
  
  
  output$confusion <- renderPrint({
    data()$confusion
  })
  
  output$dotplot <- renderPlot({
    dotPlot(varImp(data()$fit), main="Dotplot of variable importance values")
  })
  '
  
  
  
  
  
  
  
  '  data <- reactive({
    responseRoutine(input$method, input$centerscale, input$trainpercent, input$randomseed)
  })
  
  Dataset <- reactive({
    if (is.null(input$file)) { return(NULL) }
    else{
      Dataset <- as.data.frame(read.csv(input$file$datapath ,header=TRUE, sep = ","))
      
      return(Dataset)
    }
  })
  
  mydata <- reactive({
    mydata = Dataset()[,c(input$yAttr,input$xAttr)]
    
    if (length(input$fxAttr) >= 1){
      for (j in 1:length(input$fxAttr)){
        mydata[,input$fxAttr[j]] = factor(mydata[,input$fxAttr[j]])
      }
    }
    return(mydata)
    
  })'
  '
  ols <- reactive({
    rhs = paste(input$xAttr, collapse = "+")
    ols = lm(paste(input$yAttr,"~", rhs , sep=""), data = mydata())
    return(ols)
  })
  
  ols <- reactive({

    rhs = paste(input$xAttr, collapse = "+")
    ols = lm(paste(input$yAttr,"~", rhs , sep=""), data = mydata())

    return(ols)
  })
  
  
  output$olssummary = renderPrint({
    summary(ols())
  })

  # Reactive expression called whenever inputs change.
  #data <- reactive({
  #  responseRoutine(input$method, input$centerscale, input$trainpercent, input$randomseed)
  #})
  
  # Fill-in the tabs with output from caret
  
  output$fit <- renderPrint({
    mydata()$fit
  })
  
  output$confusion <- renderPrint({
    mydata()$confusion
  })
  
  output$dotplot <- renderPlot({
    dotPlot(varImp(mydata()$fit), main="Dotplot of variable importance values")
  })'
  
}

# output$moviestable <- DT::renderDataTable({
#    brushedPoints(ClustereData,input$plot_brush)%>%
#      # nearPoints(ClustereData, coordinfo = input$plot_hover) %>% 
#      dplyr:: select(COD_CAB_GEST, REGIONE ,DES_REGIONE_ABI_GEST,DES_PROVINCIA_ABI_GEST, c_I1I2Indice)
#  })

#####################################

