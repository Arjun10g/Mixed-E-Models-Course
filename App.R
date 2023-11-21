library(shiny)
library(ggplot2)
library(tidyverse)
library(janitor)
library(shinyjs)
library(papaja)
library(effects)
library(broom)

ui <- fluidPage(
  shinyjs::useShinyjs(),
  tags$head(includeCSS("www/quiz.css")),
  tabsetPanel(
    tabPanel(
      'Regression',
      sidebarLayout(
        sidebarPanel(
          sliderInput('predictors',label = 'Choose the Number of Predictors',min = 1,max = 10,value = 1,step = 1),
          sliderInput('samplesize',label = 'Choose the Sample Size', min = 10, max = 1000, value = 10, step = 5),
          sliderInput('intercept',label = 'Choose Intercept',min = -20,max = 20,value = 0,step = 1),
          textInput('slope',label = 'Enter Slopes (comma separated)',value = ''),
          sliderInput('rse', label = 'Residual Standard Error',min = 1, max = 10,value = 1,step = 1)
          ),
        mainPanel(verbatimTextOutput('out1'),
                  plotOutput('plot1'))
      )
    ), tabPanel(
      'Additivity, Non-Linearity, categorical & Collinearity',
      sidebarLayout(
        sidebarPanel(selectInput('lm',label = 'Choose Type of Model',choices = c('interaction', 'non-linearity', 'categorical', 'categorical-interaction','collinearity'),selected = 'interaction')),
        mainPanel(verbatimTextOutput('out2'),
                  plotOutput('plot2'))
      )
    )
  )
)


server <- function(input, output, session) {
  observeEvent(c(input$predictors, input$samplesize, input$intercept, input$slope, input$rse), {
    output$out1 <- renderPrint({
      # Input validation
      need(nchar(input$slope) > 0, "Enter a value for slope")
      slope_vector <- as.numeric(unlist(strsplit(input$slope, ",")))
      need(length(slope_vector) == input$predictors, "Enter the correct number of slopes")
      
      # Model calculation
      tryCatch({
        model_out <- linear_regression_dat(predictors = input$predictors, samplesize = input$samplesize, 
                                           intercept = input$intercept, slopes = input$slope, rse = input$rse)
        list(model_out$linear_model_summary, model_out$bias, allEffects(model_out$linear_model))
      }, error = function(e) {
        paste("Error in model calculation:", e$message)
      })
      
    })
    
    output$plot1 <- renderPlot({
        model_out <- tryCatch({linear_regression_dat(predictors = input$predictors, samplesize = input$samplesize, 
                                           intercept = input$intercept, slopes = input$slope, rse = input$rse)},error = function(e) NULL)

        if (!is.null(model_out) && !is.null(model_out$linear_model)) {
          # Plot the effects
          plot(allEffects(model_out$linear_model))
        }
    })
  })
  
  observeEvent(input$lm, {
    # First remove all previously added dynamic UI elements
    shiny::removeUI(selector = '.dynamic-ui', multiple = TRUE)
    
    # Then add new UI elements based on the selected model type
    if(input$lm == 'interaction'){
      shiny::insertUI(selector = '#lm', ui = div(class = 'dynamic-ui', sliderInput('interaction_sample', label = 'Choose Sample Size', min = 10, max = 1000, value = 10, step = 5)), where = 'afterEnd')
      shiny::insertUI(selector = '#lm', ui = div(class = 'dynamic-ui', sliderInput('interaction_size', label = 'Choose Interaction Effect Size', min = -10, max = 10, value = 0, step = 0.2)), where = 'afterEnd')
    } else if(input$lm == 'non-linearity'){
      shiny::insertUI(selector = '#lm', ui = div(class = 'dynamic-ui', sliderInput('nl_sample', label = 'Choose Sample Size', min = 10, max = 1000, value = 10, step = 5)), where = 'afterEnd')
      shiny::insertUI(selector = '#lm', ui = div(class = 'dynamic-ui', sliderInput('nl_size', label = 'Choose Non Linear effect Size', min = -10, max = 10, value = 0, step = 0.2)), where = 'afterEnd')
    } else if(input$lm == 'categorical'){
      shiny::insertUI(selector = '#lm', ui = div(class = 'dynamic-ui', sliderInput('categorical_sample', label = 'Choose Sample Size', min = 10, max = 1000, value = 10, step = 5)), where = 'afterEnd')
      shiny::insertUI(selector = '#lm', ui = div(class = 'dynamic-ui', sliderInput('categorical_categories', label = 'Choose The number of levels', min = 2, max = 10, value = 2, step = 1)), where = 'afterEnd')
      shiny::insertUI(selector = '#lm', ui = div(class = 'dynamic-ui', textInput('categorical_effect', label = 'Enter effects for each level (comma separated)', value = '1,1')), where = 'afterEnd')
      shiny::insertUI(selector = '#lm', ui = div(class = 'dynamic-ui', sliderInput('categorical_rse', label = 'Choose RSE', min = 0, max = 20, value = 1, step = 1)), where = 'afterEnd')
    } else if(input$lm == 'categorical-interaction'){
      shiny::insertUI(selector = '#lm', ui = div(class = 'dynamic-ui', sliderInput('categorical_int_sample', label = 'Choose Sample Size', min = 10, max = 1000, value = 10, step = 5)), where = 'afterEnd')
      shiny::insertUI(selector = '#lm', ui = div(class = 'dynamic-ui', sliderInput('categorical_int_categories', label = 'Choose The number of levels', min = 2, max = 10, value = 2, step = 1)), where = 'afterEnd')
      shiny::insertUI(selector = '#lm', ui = div(class = 'dynamic-ui', textInput('categorical_int_effect', label = 'Enter effects for each level (comma separated)', value = '1,1')), where = 'afterEnd')
      shiny::insertUI(selector = '#lm', ui = div(class = 'dynamic-ui', sliderInput('categorical_int_interactioneffect', label = 'Choose The interaction effect size', min = -10, max = 10, value = 0, step = 1)), where = 'afterEnd')
      shiny::insertUI(selector = '#lm', ui = div(class = 'dynamic-ui', sliderInput('categorical_int_rse', label = 'Choose RSE', min = 0, max = 20, value = 1, step = 1)), where = 'afterEnd')
    } else {
      shiny::insertUI(selector = '#lm', ui = div(class = 'dynamic-ui', sliderInput('cl_sample', label = 'Choose Sample Size', min = 10, max = 1000, value = 10, step = 5)), where = 'afterEnd')
      shiny::insertUI(selector = '#lm', ui = div(class = 'dynamic-ui', selectInput('cl_es1', label = 'Choose effect size for predictor 1',choices = c('small','medium','large'),selected = 'small')), where = 'afterEnd')
      shiny::insertUI(selector = '#lm', ui = div(class = 'dynamic-ui', selectInput('cl_es2', label = 'Choose effect size for predictor 2', choices = c('small','medium','large'),selected = 'small')), where = 'afterEnd')
      shiny::insertUI(selector = '#lm', ui = div(class = 'dynamic-ui', sliderInput('cl_corr', label = 'Choose degree of correlation', min = -1, max = 1, value = 0, step = 0.05)), where = 'afterEnd')
      shiny::insertUI(selector = '#lm', ui = div(class = 'dynamic-ui', sliderInput('cl_rse', label = 'Choose RSE', min = 0, max = 20, value = 1, step = 1)), where = 'afterEnd')
    }
  })
  
  observeEvent(c(input$interaction_sample, input$interaction_size), {
    tryCatch({
      m1 <- interaction(input$interaction_sample, input$interaction_size)
      output$out2 <- renderPrint({ list(m1[[1]], m1[[2]]) })
    }, error = function(e) {
      output$out2 <- renderText(paste("Error:", e$message))
    })
  })
  
  # ObserveEvent for the Non-Linearity Model
  observeEvent(c(input$nl_sample, input$nl_size), {
    tryCatch({
      m1 <- demo_non_linearity(input$nl_sample, input$nl_size)
      output$out2 <- renderPrint({ list(m1[[1]], m1[[2]]) })
      output$plot2 <- renderPlot({ m1[[3]] })
    }, error = function(e) {
      output$out2 <- renderText(paste("Error:", e$message))
      output$plot2 <- renderText("Plot not available due to error.")
    })
  })
  
  # ObserveEvent for the Categorical Model
  observeEvent(c(input$categorical_sample, input$categorical_categories, input$categorical_effect, input$categorical_rse), {
    tryCatch({
      m1 <- categorical_pred(input$categorical_sample, input$categorical_categories, input$categorical_effect, input$categorical_rse)
      output$out2 <- renderPrint({ m1[[1]] })
      output$plot2 <- renderPlot({ m1[[2]] })
    }, error = function(e) {
      output$out2 <- renderText(paste("Error:", e$message))
      output$plot2 <- renderText("Plot not available due to error.")
    })
  })
  
  # ObserveEvent for the Categorical Interaction Model
  observeEvent(c(input$categorical_int_sample, input$categorical_int_categories, input$categorical_int_effect, input$categorical_int_interactioneffect, input$categorical_int_rse), {
    tryCatch({
      m1 <- categorical_interaction(input$categorical_int_sample, input$categorical_int_categories, input$categorical_int_effect, input$categorical_int_interactioneffect, input$categorical_int_rse)
      output$out2 <- renderPrint({ m1[[1]] })
      output$plot2 <- renderPlot({ m1[[2]] })
    }, error = function(e) {
      output$out2 <- renderText(paste("Error:", e$message))
      output$plot2 <- renderText("Plot not available due to error.")
    })
  })
  
  # ObserveEvent for the Collinearity Model
  observeEvent(c(input$cl_sample, input$cl_es1, input$cl_es2, input$cl_corr, input$cl_rse), {
    tryCatch({
      m1 <- multi_collinearity(sample = input$cl_sample,es_1 =  input$cl_es1,es_2 =  input$cl_es2,corr =  input$cl_corr,rse =  input$cl_rse)
      output$out2 <- renderPrint({ m1[[1]] })
      output$plot2 <- renderPlot({ m1$plot })
    }, error = function(e) {
      output$out2 <- renderText(paste("Error:", e$message))
      output$plot2 <- renderText("Plot not available due to error.")
    })
  })
}



linear_regression_dat <- function(predictors, samplesize, intercept, slopes, rse){
  x <- replicate(predictors, rnorm(samplesize))
  
  s <- as.numeric(unlist(strsplit(slopes, ",")))
  
  if(length(s) != predictors) {
    stop("Number of slopes must match the number of predictors.")
  }
  
  fslopes <- if(ncol(x) == 1) s else diag(s)
  y <- intercept + rowSums(x %*% fslopes) + rnorm(samplesize, sd = rse)
  
  dat <- as_tibble(x) %>% bind_cols(y = y)
  linear_model <- lm(y ~ ., data = dat)
  linear_model_summary <- summary(linear_model)
  
  list(linear_model = linear_model, 
       linear_model_summary = linear_model_summary,
       bias = c(intercept, s) - linear_model_summary$coefficients[, "Estimate"])
}


interaction <- function(samplesize,interaction_effect) {
  x1 <- rnorm(samplesize)
  x2 <- rnorm(samplesize)
  y <- 1 + 1 * x1 + 1 * x2 + interaction_effect * x1 * x2 + rnorm(samplesize)
  data <- data.frame(x1, x2, y)
  
  interaction_model <- lm(y ~ x1 * x2, data = data)
  linear_model <- lm(y ~ x1 + x2, data = data)
  list(additivity = interaction_model %>% summary, `No Additvity` = linear_model %>% summary)
}

demo_non_linearity <- function(samplesize, cubic_coef) {
  x <- rnorm(samplesize)
  y <- 2 + 1*x + cubic_coef * x^2 + rnorm(samplesize) # Quadratic relationship
  data <- data.frame(x, y)
  
  lm_model <- lm(y~x,data = data) # Linear Model
  nlm_model <- lm(y ~ x + I(x^2), data = data) # Using polynomial terms
  p <- data %>% ggplot(aes(x,y)) +  geom_smooth(method = 'lm', se = F) + geom_smooth(method = 'lm', se = F, formula = y~poly(x,2)) + papaja::theme_apa()
  list(linear_model = summary(lm_model), non_linear_model = summary(nlm_model), plot_comp = p)
}

categorical_pred <- function(sample, categories, effect, rse) {
  effect <- paste0('c(', effect,')') %>% parse(text = . ) %>% eval
  # Create the factor variable
  x <- rbinom(n = sample, size = categories - 1, prob = 0.5) %>% 
    factor(labels = paste0('category ', 1:categories))
  
  # Create a named vector of effects
  effect_sizes <- setNames(effect, levels(x))
  
  # Generate a response variable y influenced by x
  y <- as.numeric(effect_sizes[as.character(x)]) + rnorm(sample, sd = rse)
  x <- x %>% as.factor
  # Fit a linear model
  lm_model <- lm(y ~ x)
  p <- NULL %>% ggplot(aes(x,y)) + geom_boxplot() + papaja::theme_apa()
    # Output the summary of the model
  list(lm_model = lm_model %>% summary, p = p)
}

categorical_interaction <- function(sample, categories, category_effect,interaction_effect, rse) {
  effect <- paste0('c(', category_effect,')') %>% parse(text = . ) %>% eval
  # Create the factor variable
  x <- rbinom(n = sample, size = categories - 1, prob = 0.5) %>% 
    factor(labels = paste0('category ', 1:categories))
  
  # Create a named vector of effects
  effect_sizes <- setNames(effect, levels(x))
  x2 <- rnorm(sample)
  
  # Generate a response variable y influenced by x
  y <- 0 * as.numeric(effect_sizes[as.character(x)]) + 0 * x2 + ((interaction_effect) * as.numeric(effect_sizes[as.character(x)]) * x2) + rnorm(sample, sd = rse)
  
  # Fit a linear model
  lm_model <- lm(y ~ x * x2)
  
  # Plotting
  p <- NULL %>% ggplot(aes(x = x2, y = y, group = x, color = x)) + 
    geom_smooth(method = 'lm', se = FALSE) +
    papaja::theme_apa() + 
    theme(legend.position = c(0.85, 0.85), legend.background = element_rect(fill = 'transparent')) +
    coord_cartesian(ylim = c(-6, 6))
  
  # Output the summary of the model and the plot
  list(lm_model_summary = summary(lm_model), plot = p)
}

multi_collinearity <- function(sample,es_1, es_2,corr, rse){
  x <- MASS::mvrnorm(sample,c(0,0),Sigma = matrix(c(1,corr,corr,1),nrow = 2))
  x1 <- x[,1]
  x2 <- x[,2]
  effect_1 <- switch (es_1,
    'small' = 0,
    'medium' = 1,
    'large' = 2
  )
  effect_2 <- switch (es_2,
                      'small' = 0,
                      'medium' = 1,
                      'large' = 2
  )
  y <- 0 + x1*effect_1 + x2*effect_2 + rnorm(sample, sd = rse)
  dat <- data.frame(x1 = x1, x2 = x2, y = y)
  mod <- lm(y~x1 + x2,data = dat)
  p <- allEffects(mod) %>% plot()
  list(mod_summary = mod %>% summary, plot = p)
  
}


shinyApp(ui = ui,server = server)