library(shiny)
library(DT)
library(shinythemes)
shinyUI(fluidPage(
        theme = shinytheme("united"),
        titlePanel("Predict Miles/(US) gallon"),
        p("André van Zyl"),
        p(paste(Sys.Date())),
        p("Before buying a new car, calculate its expected miles per gallon (MPG) using the random forest algorithm."),
        sidebarLayout(
                sidebarPanel(
                        h1("Enter the motor car details:"),
                        radioButtons(
                                "radio_cyl",
                                label = h3("Number of cylinders:"),
                                choices = list(4, 6, 8),
                                selected = 8
                        ),
                        radioButtons(
                                "radio_vs",
                                label = h3("Engine:"),
                                choices = list("V-shaped" = 0, "Straight" = 1),
                                selected = 0
                        ),  
                        radioButtons(
                                "radio_am",
                                label = h3("Transmission:"),
                                choices = list("Automatic" = 0, "Manual" = 1),
                                selected = 0
                        ),
                        radioButtons(
                                "radio_gear",
                                label = h3("Number of forward gears:"),
                                choices = list(3, 4,5),
                                selected = 3
                        ),
                        sliderInput(
                                "slider_carb",
                                "Number of carburetors:",
                                1,
                                8,
                                value = median(mtcars$carb)
                        ),                         
                        sliderInput(
                                "slider_disp",
                                "Displacement (cu.in.):",
                                40,
                                600,
                                value = mean(mtcars$disp)
                        ),
                        sliderInput(
                                "slider_hp",
                                "Gross horsepower:",
                                20,
                                600,
                                value = mean(mtcars$hp)
                        ),
                        sliderInput(
                                "slider_drat",
                                "Rear axle ratio:",
                                2.0L,
                                8.0L,
                                value = mean(mtcars$drat)
                        ),
                        sliderInput(
                                "slider_wt",
                                "Weight (1000 lbs):",
                                1.0L,
                                8.0L,
                                value = mean(mtcars$wt)
                        ),   
                        sliderInput(
                                "slider_qsec",
                                "1/4 mile time:",
                                5L,
                                30L,
                                value = mean(mtcars$qsec)
                        )
                ),
                mainPanel(
                        h1("Your car details"),
                        p("Here is a table with the car information you entered."),
                        dataTableOutput("mytable"),
                        h1("Predicted Miles/(US) gallon from random forest model:"),
                        h2(textOutput("rfpred")),
                        h1("A plot to compare MPG"),
                        p("This plot compares the predicted MPG for your car (RED LINE) with the known MPGs of other cars."),
                        plotOutput("plot1")


                )
        )
))