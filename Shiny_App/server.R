library(shiny)
library(gridExtra)
library(tidyverse)
library(caret)
library(DT)
library(xtable)
library(ranger)
library(e1071)


shinyServer(function(input, output) {
    
    data("mtcars")
    # preProcValues  <- preProcess(
    #     mtcars %>% select(-"mpg"),
    #     method = c("center", # subtract mean from values.
    #                "scale", # divide values by standard deviation.
    #                "zv", # remove attributes with a zero variance (all the same value).
    #                "nzv",# remove attributes with a near zero variance (close to the same value).
    #                "pca"),# transform data to the principal components.
    #     thresh = 1, # the cutoff for the cumulative percent of variance to be retained by PCA
    #     pcaComp = NULL,
    #     na.remove = TRUE,
    #     k = 5,
    #     knnSummary = mean,
    #     outcome = NULL,
    #     fudge = 0.2,
    #     numUnique = 3,
    #     verbose = FALSE,
    #     freqCut = 95 / 5,
    #     uniqueCut = 10,
    #     cutoff = 0.9,
    #     rangeBounds = c(0, 1)
    # )
    
    predict_data <- reactive({
        datatable(data.frame(
        cyl = input$radio_cyl,
        vs = input$radio_vs,
        am = input$radio_am,
        gear = input$radio_gear,
        carb = input$slider_carb,
        disp = input$slider_disp,
        hp = input$slider_hp,
        drat = input$slider_drat,
        wt = input$slider_wt,
        qsec = input$slider_qsec),
        colnames = c("Number of cylinders:","Engine:","Transmission:","Number of forward gears:","Number of carburetors:","Displacement (cu.in.):","Gross horsepower:","Rear axle ratio:","Weight (1000 lbs):","1/4 mile time:"), filter = 'none', rownames = FALSE
            )

    })
    
    
    output$mytable = renderDataTable({
        predict_data()
    })

    data("mtcars")
    # mtcars_transformed  <- predict(preProcValues, mtcars %>% select(-"mpg"))
    # mtcars_transformed$mpg <- mtcars$mpg
    # predict_data_transformed <- reactive({
    #     predict(preProcValues, newdata = predict_data())
    # })
    
    # tr_ctrl <- trainControl(method="cv", number=5, verboseIter=FALSE)
    # rfmodel <- train(mpg~., data=mtcars_transformed, method="ranger",
    #                  trControl = tr_ctrl)
    tr_ctrl <- trainControl(method="cv", number=5, verboseIter=FALSE)
    rfmodel <- train(mpg~., data=mtcars, method="ranger",
                     trControl = tr_ctrl)
    
    # rf_predicion <- predict(rfmodel, newdata=reactive({
    #     predict(preProcValues, newdata = reactive({
    #         predict(preProcValues, newdata = predict_data())
    #     }))
    # }))

    # rf_predicion <- reactive({
    #     predict(rfmodel, newdata = predict_data_transformed)
    # })
    

    
    
    mtcars$mpgsp <- ifelse(mtcars$mpg - 20 > 0, mtcars$mpg - 20, 0)
    model1 <- lm(hp ~ mpg, data = mtcars)
    model2 <- lm(hp ~ mpgsp + mpg, data = mtcars)
    
    
    
    
    rf_predicion <- reactive({
        predict(rfmodel, newdata = data.frame(
            cyl = as.numeric(input$radio_cyl) ,
            vs = as.numeric(input$radio_vs),
            am = as.numeric(input$radio_am),
            gear = as.numeric(input$radio_gear),
            carb = as.numeric(input$slider_carb),
            disp = as.numeric(input$slider_disp),
            hp = as.numeric(input$slider_hp),
            drat = as.numeric(input$slider_drat),
            wt = as.numeric(input$slider_wt),
            qsec = as.numeric(input$slider_qsec))) 
    })  
    
    
    
    model1pred <- reactive({
        predict(model1, newdata = data.frame(mpg = input$sliderMPG))
    })
    
    model2pred <- reactive({
        predict(model2, newdata = 
                    data.frame(mpg = input$sliderMPG,
                               mpgsp = ifelse(input$sliderMPG - 20 > 0,
                                              input$sliderMPG - 20, 0)))
    })
    
    
    output$table <- renderTable(predict_data)
    

    
    output$plot1 <- renderPlot({
        data("mtcars")
        mtcars$carname <- rownames(mtcars)
        mtcars$am <- factor(mtcars$am,levels = c(0,1),labels = c("Automatic", "Manual")) 
        
        grid.arrange(
            ggplot(mtcars,
                   aes(
                       x = reorder(carname, -mpg),
                       y = mpg,
                       group = am,
                       fill = am
                   )) +
                geom_boxplot(alpha = .7) +
                geom_point(aes(color = am), size = 3) +
                coord_flip()  +
                theme_minimal() +
                xlab("") +
                ylab("Miles/(US) gallon") +
                geom_hline(
                    yintercept = rf_predicion(),
                    show.legend = TRUE,
                    size = 1,
                    color = "red"
                ) +
                facet_grid() + geom_text(aes(
                    label = paste(carname, " : " , mpg, " MPG", sep = "")
                ), size = 4, hjust = "inward")
        ) 
        
    })
    
    output$rfpred <- renderText({
        paste("You will get around", round(rf_predicion(), 2), "MPG with this car.")
    })    
    
  
})