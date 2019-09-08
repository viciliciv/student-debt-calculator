#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(dplyr)
library(plotly)
library(rAmCharts)
library(lubridate)
library(kableExtra)
library(DT)
library(highcharter)
library(shinydashboard)


currency_convert<-function(value){
    if(nchar(round(value,0)) > 6){
        paste('$',
              format(round(value/10^6,2), nsmall=2, big.mark=","),
              'M', sep = '')
    }else{paste('$',format(round(as.numeric(value), 0), nsmall=0, big.mark=","), sep = '')
    }}

equiv_rate <- function (rate, from.freq = 1, to.freq = 1){
    cc.rate <- ifelse(from.freq == Inf, rate, log(1 + rate/from.freq) * 
                          from.freq)
    if (to.freq == Inf) 
        cc.rate
    else 
        (exp(cc.rate/to.freq) - 1) * to.freq
}

annuity_installment<-function (rate, n.periods = Inf, pv = if (missing(fv)) 1 else 0, 
                               fv = 0, terminal.payment = 0, immediate.start = FALSE, cf.freq = 1, 
                               comp.freq = 1){
    r = equiv_rate(rate, comp.freq, cf.freq)/cf.freq
    df <- (1 + r)^-n.periods
    annuity.pv = pv + (fv - terminal.payment) * df * if (immediate.start) 
        1 + r
    else 1
    if (rate == 0) 
        return(annuity.pv/n.periods)
    adjust <- if (immediate.start) 
        1 + r
    else 1
    r * annuity.pv/(adjust * (1 - df))
}

annuity_period<- function (rate, instalment = 1, pv = if (missing(fv)) 1 else 0, 
                           fv = 0, terminal.payment = 0, immediate.start = FALSE, cf.freq = 1, 
                           comp.freq = 1, round2int.digits = 3){
    if (rate == 0) 
        return((pv + fv - terminal.payment)/instalment)
    r = equiv.rate(rate, comp.freq, cf.freq)/cf.freq
    pv = pv - if (immediate.start) 
        instalment
    else 0
    df <- (instalment/r - pv)/(fv - terminal.payment + instalment/r)
    n <- -log(df)/log(1 + r) + if (immediate.start) 
        1
    else 0
    as.integer(n) + zapsmall(n - as.integer(n), round2int.digits)
}

calculator_without_contribution<-function(rate, debt, loan_term, min_monthly, employer_contribution, maximum_benefit){
    rate = rate/100
    total_interest = 0
    total_payment = 0
    total_contribution = 0
    ending_balance <- 1
    summary_list <- data.frame()
    remaining_cont = maximum_benefit
    for (i in 1:100000){
        if (i == 1){
            beginning_balance = debt
        }else{
            beginning_balance = ending_balance
            if (remaining_cont > 0){
                remaining_cont = remaining_cont - employer_contribution
            }else{
                remaining_cont = 0
            }}
        
        if (beginning_balance < min_monthly){
            min_monthly_pay = round((1+rate/12)*beginning_balance,2)
        }else{
            min_monthly_pay = min_monthly}
        
        interest_pay = round((rate/12)*beginning_balance,2)
        base_principal_pay = min_monthly_pay - interest_pay
        
        
        if (remaining_cont >= employer_contribution) {
            if ((beginning_balance - base_principal_pay) > employer_contribution){
                pay_contribution = employer_contribution
            }else{
                pay_contribution = beginning_balance - base_principal_pay}
            # }else if (employer_contribution == maximum_benefit){
            #     pay_contribution = employer_contribution
        }else{
            pay_contribution = 0}
        
        ending_balance = round(beginning_balance - base_principal_pay - pay_contribution,2)
        
        total_interest = total_interest + interest_pay
        total_payment = total_payment + min_monthly_pay
        total_contribution = total_contribution + pay_contribution
        
        if (i>0){
            year = round(i/12,0)
            row_list <- c(as.integer(i), total_interest, total_payment, total_contribution, ending_balance)
            summary_list <- rbind(summary_list, row_list)
            colnames(summary_list)<-c('Month', 'Total Interest', 'Total Payment', 'Total Contribution', 'Ending Balance')}
        
        if (ending_balance <= 0){
            break
        }
        
        
    }
    return(summary_list)
}




library(shinydashboard)

ui <- dashboardPage(skin = 'purple',
    dashboardHeader(title = "Student Loan Calculator"),
    dashboardSidebar(skin = 'purple',
        numericInput('debt', 'Student Loan Debt', min = 0, value =  25732),
        numericInput('rate', 'Interest Rate', min = 0, value = 6.52),
        numericInput('loan_term', 'Loan Term (Years)', min = 0, value = 10),
        numericInput('additional_pay', 'Additional Payment', min = 0, value = 50),
        uiOutput('min'),
        span(textOutput("warn"), style="color:red"),
        actionButton('calc', 'Recalculate Min. Monthly'),
        numericInput('employer_cont', 'Employer Monthly Contribution', min = 0, value = 50),
        numericInput('max_benefit', 'Employer Maximum Benefit', min = 0, value = 2400),
        actionButton('submit', 'Submit')
    ),
    dashboardBody(
        # fluidRow(valueBoxOutput("credit_cnt"),
        fluidRow(
            valueBoxOutput(width = 4, "months_saved_emp"),
            valueBoxOutput(width = 4, "int_saved_emp"),
            valueBoxOutput(width = 4, "tot_saved_emp"),
            valueBoxOutput(width = 4, "months_saved_emp_extra"),
            valueBoxOutput(width = 4, "int_saved_emp_extra"),
            valueBoxOutput(width = 4, "tot_saved_emp_extra")
        ),
        fluidRow(
            tabBox(
                title = "Summary",
                # The id lets us use input$tabset1 on the server to find the current tab
                id = "tabset1", height = NULL, width = 12,
                tabPanel("Chart", highchartOutput('summ_chart')),
                tabPanel("Table", tableOutput('summ_table'))
            )
        )
    )
)


server <- function(input, output, session) {
    
    int_rate<-eventReactive(input$calc,{input$rate})
    loan_term<-eventReactive(input$calc, {input$loan_term})
    debt<-eventReactive(input$calc, {input$debt})
    
    min_warning<-reactive({
        round(input$debt * ((input$rate/100))/12,2)
    })
    
    calc_min_monthly<-eventReactive(input$calc, {
        x<-annuity_installment(rate =int_rate()/100, n.periods = loan_term()*12, pv = debt(),
                               fv = 0, terminal.payment = 0, immediate.start = FALSE, cf.freq = 12,
                               comp.freq = 12)
        # paste('Minimum Monthly Should be: ',round(x,2))
        round(x,2)
    })
    
    output$min<-renderUI({
        numericInput('min_monthly', 'Min. Monthly Payment', min = min_warning(), value = 238.56)
    })
    
    output$warn<-renderText({str_wrap(paste('Warning: The lowest Min. Monthly that can be used is: ',
                                min_warning()), width = 10)})
    
    observeEvent(input$calc,{
        updateNumericInput(session, 'min_monthly', min = min_warning(), value = calc_min_monthly())
    })
    
    table<-eventReactive(input$submit,{
        table<-calculator_without_contribution(rate = input$rate, debt = input$debt, loan_term = input$loan_term, 
                                               min_monthly = input$min_monthly,employer_contribution = 0, 
                                               maximum_benefit = 0) 
        table<-table%>%
            mutate(Type = 'Without Contribution')
    })
    
    table_cont<-eventReactive(input$submit,{
        table_cont<-calculator_without_contribution(rate = input$rate, debt = input$debt, loan_term = input$loan_term, 
                                                    min_monthly = input$min_monthly,employer_contribution = input$employer_cont, 
                                                    maximum_benefit = input$max_benefit) 
        table_cont<-table_cont%>%
            mutate(Type = 'With Contribution')
        
    })
    
    table_cont_extra_payments<-eventReactive(input$submit,{
        table_cont<-calculator_without_contribution(rate = input$rate, debt = input$debt, loan_term = input$loan_term,
                                                    min_monthly = input$min_monthly + input$additional_pay,employer_contribution = input$employer_cont,
                                                    maximum_benefit = input$max_benefit)
        table_cont<-table_cont%>%
            mutate(Type = 'With Contribution and Additional Payment')
        
    })
    
    output$months_saved_emp<-renderValueBox({
        x<-rbind(tail(table(),1), tail(table_cont(),1))%>%
            select(-Type) %>%
            mutate(`Month Saved` = -(Month - lag(Month)),
                   `Interest Saved` = -(`Total Interest` - lag(`Total Interest`)),
                   `Total Payment Saved` = -(`Total Payment` - lag(`Total Payment`)))%>%
            select(`Month Saved`)%>%
            filter(!is.na(`Month Saved`))

        months<-as.character(x)
        valueBox(value = months, subtitle = 'Months Saved with Employer Contribution', color = 'purple')
    })
    
    output$int_saved_emp<-renderValueBox({
        x<-rbind(tail(table(),1), tail(table_cont(),1))%>%
            select(-Type) %>%
            mutate(`Month Saved` = -(Month - lag(Month)),
                   `Interest Saved` = -(`Total Interest` - lag(`Total Interest`)),
                   `Total Payment Saved` = -(`Total Payment` - lag(`Total Payment`)))%>%
            filter(!is.na(`Month Saved`))%>%
            select(`Interest Saved`)

        interest<-paste('$ ',format(round(as.numeric(x), 2), nsmall=1, big.mark=","))
        valueBox(value = interest, subtitle = 'Interest Saved with Employer Contribution', color = 'purple')
    })
    
    output$tot_saved_emp<-renderValueBox({
        x<-rbind(tail(table(),1), tail(table_cont(),1))%>%
            select(-Type) %>%
            mutate(`Month Saved` = -(Month - lag(Month)),
                   `Interest Saved` = -(`Total Interest` - lag(`Total Interest`)),
                   `Total Payment Saved` = -(`Total Payment` - lag(`Total Payment`)))%>%
            filter(!is.na(`Month Saved`))%>%
            select(`Total Payment Saved`)
   
        total_pay<- paste('$ ',format(round(as.numeric(x), 2), nsmall=1, big.mark=","))
        valueBox(value = total_pay, subtitle = 'Total Payment Saved With Employer Contributions', color = 'purple')
    })
    
    output$months_saved_emp_extra<-renderValueBox({
        x<-rbind(tail(table(),1), tail(table_cont_extra_payments(),1))%>%
            select(-Type) %>%
            mutate(`Month Saved` = -(Month - lag(Month)),
                   `Interest Saved` = -(`Total Interest` - lag(`Total Interest`)),
                   `Total Payment Saved` = -(`Total Payment` - lag(`Total Payment`)))%>%
            select(`Month Saved`)%>%
            filter(!is.na(`Month Saved`))
        #        ,`Interest Saved`,`Total Payment Saved`)%>%
        # filter(!is.na(`Month Saved`))
        #        
        months<-as.character(x)
        valueBox(value = months, subtitle = 'Months Saved With Employer Contribution + Extra Payments',
                 color = 'purple')
    })
    
    output$int_saved_emp_extra<-renderValueBox({
        x<-rbind(tail(table(),1), tail(table_cont_extra_payments(),1))%>%
            select(-Type) %>%
            mutate(`Month Saved` = -(Month - lag(Month)),
                   `Interest Saved` = -(`Total Interest` - lag(`Total Interest`)),
                   `Total Payment Saved` = -(`Total Payment` - lag(`Total Payment`)))%>%
            filter(!is.na(`Month Saved`))%>%
            select(`Interest Saved`)
   
        interest<-paste('$ ',format(round(as.numeric(x), 2), nsmall=1, big.mark=","))
        valueBox(value = interest, 
                 subtitle = 'Interest Saved With Employer Contributions + Extra Payments',
                 color = 'purple')
    })
    
    output$tot_saved_emp_extra<-renderValueBox({
        x<-rbind(tail(table(),1), tail(table_cont_extra_payments(),1))%>%
            select(-Type) %>%
            mutate(`Month Saved` = -(Month - lag(Month)),
                   `Interest Saved` = -(`Total Interest` - lag(`Total Interest`)),
                   `Total Payment Saved` = -(`Total Payment` - lag(`Total Payment`)))%>%
            filter(!is.na(`Month Saved`))%>%
            select(`Total Payment Saved`)
 
        total_pay<- paste('$ ',format(round(as.numeric(x), 2), nsmall=1, big.mark=","))
        valueBox(value = total_pay, 
                 subtitle = 'Total Payment Saved With Employer Contributions + Extra Payments',
                 color = 'purple')
    })
    
    output$summ_chart<-renderHighchart({
        menu_obj <- list(list(format = "JPG", label ="Save as JPG", title = "Export chart to JPG"), 
                         list(format = "PNG", label ="Save as PNG", title = "Export chart to PNG"))
        
        db_cont<-table_cont()%>%
            select(Month, `Balance w/ Contribution` =`Ending Balance`)
        db<-table()%>%
            select(Month, `Balance w/o Contribution` =`Ending Balance`)
        db_cont_payments<-table_cont_extra_payments()%>%
            select(Month, `Balance w/ Contribution + Payments` =`Ending Balance`)
        
        db_merge<-merge(db, db_cont, by = 'Month', all.x = TRUE)%>%
            left_join(db_cont_payments, by = 'Month')%>%
            mutate(cont_ending = if_else(is.na(`Balance w/ Contribution`), 0, `Balance w/ Contribution`))%>%
            mutate(cont_pay_ending = if_else(is.na(`Balance w/ Contribution + Payments`), 0, `Balance w/ Contribution + Payments`))%>%
            mutate(monthsss= (ymd(Sys.Date()) %m+% months(Month)))
        
        #'Balance w/ Contribution', 'Balance w/o Contribution', 'Balance w/ Contribution + Payments'
        
        # hchart(db_merge, 'line', hcaes(monthsss, `Balance w/o Contribution`))
        
        highchart()%>%
            hc_xAxis(categories = year(db_merge$monthsss),
                     tickInterval = 12,
                     showFirstLabel = TRUE,
                     showLastLabel = TRUE)%>%
            hc_add_series(name = 'Balance w/o Contribution', data = db_merge$`Balance w/o Contribution`)%>%
            hc_add_series(name = 'Balance w/ Contribution', data = db_merge$`Balance w/ Contribution`)%>%
            hc_add_series(name = 'Balance w/ Contribution + Payments', data = db_merge$`Balance w/ Contribution + Payments`)%>%
            hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
                       shared = TRUE, borderWidth = 5)
    })
    
    output$summ_table<-renderTable({
        rbind(tail(table(),1), tail(table_cont(),1), tail(table_cont_extra_payments(),1))%>%
            mutate(`Total Interest Paid ($)` = format(round(as.numeric(`Total Interest`), 2), nsmall=1, big.mark=","),
                   `Total Payment Made ($)` = format(round(as.numeric(`Total Payment`), 2), nsmall=1, big.mark=","),
                   `Total Contribution Received` = format(round(as.numeric(`Total Contribution`), 2), nsmall=1, big.mark=","))%>%
            select(Type, Month,`Total Contribution Received`, `Total Interest Paid ($)`, `Total Payment Made ($)`)
    })
    
}

shinyApp(ui, server)