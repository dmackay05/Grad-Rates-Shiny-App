library(shiny)


shinyServer(function(input, output) {

        output$lea_selector <- renderUI({

        selectInput(
            inputId = "lea",
            label = "Choose LEA:",
            choices = as.character(unique(lea_info$lea_name)),
            selected = "")

    })

    output$school_selector <- renderUI({

        available <- lea_info[lea_info$lea_name == input$lea, "school_name"]

        selectInput(
            inputId = "school_name",
            label = "Choose School:",
            choices = unique(available),
            selected = unique(available)[1])

    })

    output$group_selector <- renderUI({

        selectInput(
            inputId = 'group',
            label = 'Choose Student Group',
            selected = 'All Students',
            choices = list("All Students", Race = list("Asian", "American Indian", "Caucasian",
                        "Black/African American", "Hispanic/Latino", "Pacific Islander"),
                        StudentGroup = list("Low Income", "English Learner", "Special Ed"))
            )

    })

    output$type_selector <- renderUI({

        selectInput(
            inputId = 'cohort_type',
            label = 'Choose Cohort Type',
            selected = 'FEDERAL4YR',
            choices = c("FEDERAL4YR")
            )

    })

    output$cohort_selector <- renderUI({

        selectInput(
            inputId = 'cohort',
            label = 'Choose Start Year',
            selected = '2009',
            choices = c("2009", "2010", "2011", "2012", "2013",
                        "2014", "2015", "2016", "2017", "2018", "2019", "2020")
            )
    })

    dataset1 <- reactive({
        grad_rates %>%
            mutate(school_name = paste(school_name, school_number, sep = " - ")) %>%
            filter(str_detect(lea_name, input$lea),
                   str_detect(sub_group_code, input$group),
                   str_detect(school_name, input$school_name),
                   cohort_year >= input$cohort,
                   cohort_type == input$cohort_type) %>%
            mutate(cohort_graduation_rate = cohort_graduation_rate / 100,
                   cohort_dropout_rate = cohort_dropout_rate / 100)
    })

    dataset2 <- reactive({
        hs_completion_status %>%
            mutate(school_name = paste(school_name, school_nbr, sep = " - ")) %>%
            filter(cohort_type == 'FEDERAL4YR', cohort_year < 2021,
                   str_detect(lea_name, input$lea),
                   str_detect(school_name, input$school_name),
                   cohort_year >= input$cohort)
    })

    dataset3 <- reactive({
        hs_completion_status %>%
            mutate(school_name = paste(school_name, school_nbr, sep = " - ")) %>%
            filter(cohort_type == 'FEDERAL4YR', cohort_year < 2021,
                   str_detect(lea_name, input$lea),
                   str_detect(school_name, input$school_name),
                   cohort_year >= input$cohort) %>%
            group_by(lea_name, school_name, school_name, cohort_year, outcome_description) %>%
            summarise(student_count = n()) %>%
            mutate(outcome_description = ifelse(is.na(outcome_description), 'Excluded', outcome_description)) %>%
            pivot_wider(id_cols = c(lea_name, school_name, cohort_year),
                        names_from = outcome_description,
                        values_from = student_count,
                        values_fill = 0) %>%
            mutate(Total = sum(across(where(is.numeric)))) %>%
            mutate(Total = Total - Excluded) %>%
            mutate(across(.cols = everything(), .fns = as.character)) %>%
            select(lea_name, school_name, cohort_year, Graduate, Dropout, `Continuing Student`, `Other Completer`, Excluded, Total)
    })

    dataset4 <- reactive({
        hs_completion_status %>%
            mutate(school_name = paste(school_name, school_nbr, sep = " - ")) %>%
            filter(cohort_type == 'FEDERAL4YR', cohort_year < 2021,
                   str_detect(lea_name, input$lea),
                   str_detect(school_name, input$school_name),
                   cohort_year >= input$cohort) %>%
            mutate(outcome_description = if_else(is.na(outcome_description), "Excluded", outcome_description)) %>%
            arrange(outcome_description) %>%
            group_by(lea_name, school_name, cohort_year, hs_completion_status) %>%
            summarise(student_count = n()) %>%
            ungroup() %>%
            mutate(cohort_year = as.character(cohort_year)) %>%
            pivot_wider(names_from = hs_completion_status,
                        values_from = student_count,
                        values_fill = 0) %>%
            select(-c(lea_name, school_name)) %>%
            rowwise(cohort_year) %>%
            mutate(total = sum(across(where(is.numeric))))
    })

    # trend_plot <- eventReactive()

    output$trend <- renderPlot({
        data_name <- dataset1()
        ggplot(data_name) +
            geom_line(aes(x = cohort_year, y = cohort_graduation_rate, group = 1)) +
            geom_point(aes(x = cohort_year, y = cohort_graduation_rate)) +
            scale_y_continuous(labels = scales::percent) +
            expand_limits(y = c(0, 1)) +
            ggthemes::theme_few() +
            labs(title = "Cohort Grad Rate Over Time",
                 x = NULL,
                 y = "Cohort Grad Rate",
                 color = NULL)
    })

    output$dropout <- plotly::renderPlotly({
        data_name <- dataset1()
        ggplot(data_name) +
            geom_col(aes(x = cohort_year, y = dropout_count)) +
            expand_limits(y = 0) +
            ggthemes::theme_few() +
            labs(title = "Cohort Dropout Count Over Time",
                 x = NULL,
                 y = "Cohort Grad Rate",
                 color = NULL)
    })


    output$table <- renderTable({

        data <- dataset1() %>%
            select(LEA = lea_name, School = school_name, District = district_id,
                   SchoolID = school_id, SchoolNumber = school_number,
                   Group = sub_group_code,
                   Cohort = cohort_year, Rate = cohort_graduation_rate,
                   Count = cohort_count, DropOut = cohort_dropout_rate) %>%
            mutate(District = as.character(District),
                   SchoolID = as.character(SchoolID))

    }, width = "900px")

        output$counts <- renderTable({
            validate(need(input$school_name != input$lea , "Please choose a school"))
            data <-dataset2() %>%
                group_by(lea_name, school_name, cohort_year, outcome_description) %>%
                summarise(student_count = n()) %>%
                mutate(outcome_description = ifelse(is.na(outcome_description), 'Excluded', outcome_description)) %>%
                pivot_wider(id_cols = c(lea_name, school_name, cohort_year),
                            names_from = outcome_description,
                            values_from = student_count,
                            values_fill = 0) %>%
                mutate(Total = sum(across(where(is.numeric)))) %>%
                mutate(Total = Total - Excluded) %>%
                mutate(across(.cols = everything(), .fns = as.character)) %>%
                select(lea_name, school_name, cohort_year, Graduate, Dropout, `Continuing Student`, `Other Completer`, Excluded, Total)

        }, width = "900px")


    output$status <- renderTable({
        validate(need(input$school_name != input$lea , ""))
        data <- dataset2() %>%
            mutate(outcome_description = if_else(is.na(outcome_description), "Excluded", outcome_description)) %>%
            arrange(outcome_description) %>%
            group_by(lea_name, school_name, cohort_year, hs_completion_status) %>%
            summarise(student_count = n()) %>%
            ungroup() %>%
            mutate(cohort_year = as.character(cohort_year)) %>%
            pivot_wider(names_from = hs_completion_status,
                        values_from = student_count,
                        values_fill = 0) %>%
            select(-c(lea_name, school_name)) %>%
            rowwise(cohort_year) %>%
            mutate(total = sum(across(where(is.numeric))))

    }, width = "900px")

    output$grad_rates <- downloadHandler(
        filename = function() {
            paste("Grad_Rates_", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
            write_csv(dataset1(), file, na = "")
        })

    output$grad_outcomes <- downloadHandler(
        filename = function() {
            paste("Grad_Counts_", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
            write_csv(dataset4(), file, na = "")
        })

    output$completion <- downloadHandler(
        filename = function() {
            paste0("Completion_Status_", Sys.Date(), ".csv")
        },
        content = function(file) {
            write_csv(dataset4(), file, na = "")
        })

})
