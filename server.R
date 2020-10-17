# PortoDash - Investment Portfolio Dashboard
# Dorsa Arezooji -- https://Github.com/Dorsa-Arezooji/PortoDash

library(shiny)
library(ggplot2)
library(reshape2)
library(cowplot)
library(ggcorrplot)
library(scales)
        
theme_set(theme_half_open())

shinyServer(function(input, output) {
    
    readPortfolio = eventReactive(input$go,{
        validate(need(input$inputFile, "You need to upload the assets file!"),
                 need(input$dateFormat, "Enter date format!"))
        if(input$hasMarket) validate(need(input$market, "Enter the market index!"))
        fname = input$inputFile
        path = input$inputFile$datapath
        if(grepl(".csv", fname) || grepl(".CSV", fname)) {}
            else{stop("file is not csv!")}
        portfolio = read.csv(path)
        df = data.frame(portfolio)
        df['Date'] = as.Date(df$Date, format = input$dateFormat)
        if(input$hasMarket){
            if(! input$market %in% colnames(df)){
                stop("The entered market index does not exist")}
        }
        if(ncol(df)<3) stop("Portfolio can not have less than 2 assets!")
        #df = na.omit(df)
        return(df)
    })
    
    calculateDailyReturns = reactive({
        data = readPortfolio()
        df = data[, !(names(data) == 'Date')]
        df0 = df
        df0[nrow(df)+1,] = rep(0, ncol(df))
        df0 = df0[-1,]
        returns_df  = 100*(df0/df-1)
        returns_df = returns_df[1:nrow(df)-1,]
        returns_df[['Date']] = data[2:nrow(df), 1]
        return(returns_df)
    })
    
    MeanReturns = reactive({
        dailyReturns = calculateDailyReturns()
        if(input$hasMarket){
            dailyReturns = dailyReturns[, names(dailyReturns) != input$market]}
        returns_df = dailyReturns[, !(names(dailyReturns) %in% c('Date'))]
        returns = colMeans(returns_df)
        return(returns)
    })
    
    ExpMeanReturns = reactive({
        dailyReturns = calculateDailyReturns()
        if(input$hasMarket){
            dailyReturns = dailyReturns[, names(dailyReturns) != input$market]}
        returns_df = dailyReturns[, !(names(dailyReturns) %in% c('Date'))]
        expReturns = returns_df * (1:nrow(returns_df))
        returns = colSums(expReturns) / sum(1:nrow(returns_df))
        return(returns)
    })
    
    CAPMReturns = reactive({
        validate(need(input$hasMarket, "Market index is needed for CAPM!"),
                 need(input$rf,"Enter the risk free return rate!"))
        dailyReturns = calculateDailyReturns()
        returns_df = dailyReturns[, names(dailyReturns) != 'Date']
        marketReturns = returns_df[[input$market]]
        returns_df = returns_df[, names(returns_df) != input$market]
        betas = cov(returns_df, marketReturns)/var(marketReturns)
        riskFree = as.numeric(input$rf)
        returns = riskFree + betas * (mean(marketReturns) - riskFree)
        return(returns)
    })
    
    calculateReturns = eventReactive(input$go,{
        switch(input$returnModel,
               "MH" = MeanReturns(),
               "EWMH" = ExpMeanReturns(),
               "CAPM" = CAPMReturns())
        })
    
    covRisk = reactive({
        df = calculateDailyReturns()
        r = c('Date')
        if (input$hasMarket) r = c('Date', input$market)
        df = df[, !(names(df) %in% r)]
        return(cov(df))
    })
    
    ExpCovRisk = reactive({
        df = calculateDailyReturns()
        r = c('Date')
        if (input$hasMarket) r = c('Date', input$market)
        df = df[, !(names(df) %in% r)]
        weights = (1/sum(1:nrow(df)))*(1:nrow(df))
        WC = cov.wt(df, weights)
        return(WC[[1]])
    })
    
    calculateRisks = eventReactive(input$go,{
        switch(input$riskModel,
               "Cov" = covRisk(),
               "ExpCov" = ExpCovRisk())
    })
    
    visRisk = eventReactive(input$go,{
        Risks = calculateRisks()
        l = c(min(Risks)-0.02, max(Risks)+0.02)
        ggcorrplot(Risks, hc.order = FALSE, outline.col = "white", lab = TRUE,
                   colors = c("#6D9EC1", "white", "orange"), legend.title = '') + 
            scale_fill_gradient2(breaks = l, limit = l, low = "#6D9EC1", high = "#E46726")
    })
    
    visReturn = eventReactive(input$go,{
        Returns = data.frame(calculateReturns())
        names(Returns) = c('returns')
        ggplot(data=Returns, aes(x=returns, y=rownames(Returns))) + 
            geom_bar(colour="white", stat="identity", width = 0.8, fill="#E46726") +
            geom_text(aes(label = sprintf("%.4f", returns), y=rownames(Returns)),
                      size = 5, hjust = 1.2) +
            guides(fill=FALSE) + labs(title='', x='Returns (%)', y='')
    })
    
    output$plotRisk = renderPlot({
        visRisk()
    })
    
    output$plotReturn = renderPlot({
        visReturn()
    })
    
    output$assetCheckBox = renderUI({
        df = readPortfolio()
        assets = colnames(df[, !(names(df) %in% c('Date', input$market))])
        checkboxGroupInput('assetsGroup', 'Select the assets to display:', 
                           choices = assets, selected = c(assets[1], assets[2]))
    })
    
    output$plots_P = renderPlot({
        df_P = readPortfolio()
        d_P = df_P[, names(df_P) %in% c(input$assetsGroup, 'Date')]
        df_melt = melt(d_P, id.vars="Date", value.name="Price", variable.name='Assets')
        ggplot(df_melt, aes(x=Date, y=Price, group=Assets, color=Assets)) + geom_line() + 
            labs(title='', x='Date', y='Price') + theme(legend.position="bottom",
                                                        legend.title = element_blank()) +
            scale_x_date(breaks = breaks_pretty(10))
    })
    
    output$plotInfo_P = renderText({
        xy_str <- function(e) {
            if(is.null(e)) return("NULL\n")
            paste0("Date = ", as.Date(e$x, origin = "1970-01-01"), "\t", "Price = ", 
                   round(e$y, 4), "\n")
        }
        xy_range_str <- function(e) {
            if(is.null(e)) return("NULL\n")
            paste0("Min Date = ", as.Date(e$xmin, origin = "1970-01-01"), "\t",
                   "Min Price = ", round(e$ymin, 4), "\n",
                   "               Max Date = ", as.Date(e$xmax, origin = "1970-01-01"), "\t",
                   "Max Price = ", round(e$ymax, 4))
        }
        
        paste0(
            "Click:         ", xy_str(input$plot_click_P),
            "Double Click:  ", xy_str(input$plot_dblclick_P),
            "Hover:         ", xy_str(input$plot_hover_P),
            "Area:          ", xy_range_str(input$plot_brush_P)
        )
    })
    
    output$plots_R_daily = renderPlot({
        df_R = calculateDailyReturns()
        d_R = df_R[, names(df_R) %in% c(input$assetsGroup, 'Date')]
        d_Rmelt = melt(d_R, id.vars="Date", value.name="Returns", variable.name='Assets')
        ggplot(d_Rmelt, aes(x=Date, y=Returns, group=Assets, color=Assets)) + 
            geom_line() + labs(title='', x='Date', y='Returns (%)') + 
            theme(legend.position="bottom", legend.title = element_blank()) +
            scale_x_date(breaks = breaks_pretty(10))
    })
    
    output$plotInfo_R_daily = renderText({
        xy_str <- function(e) {
            if(is.null(e)) return("NULL\n")
            paste0("Date = ", as.Date(e$x, origin = "1970-01-01"), "\t", "Return(%) = ", 
                   round(e$y, 4), "\n")
        }
        xy_range_str <- function(e) {
            if(is.null(e)) return("NULL\n")
            paste0("Min Date = ", as.Date(e$xmin, origin = "1970-01-01"), "\t",
                   "Min Return(%) = ", round(e$ymin, 4), "\n",
                   "               Max Date = ", as.Date(e$xmax, origin = "1970-01-01"), "\t",
                   "Max Return(%) = ", round(e$ymax, 4))
        }
        
        paste0(
            "Click:         ", xy_str(input$plot_click_R_daily),
            "Double Click:  ", xy_str(input$plot_dblclick_R_daily),
            "Hover:         ", xy_str(input$plot_hover_R_daily),
            "Area:          ", xy_range_str(input$plot_brush_R_daily)
        )
    })
    
    output$plots_R_hist = renderPlot({
        df_R = calculateDailyReturns()
        d_R = df_R[, names(df_R) %in% c(input$assetsGroup, 'Date')]
        d_Rmelt = melt(d_R, id.vars="Date", value.name="Returns", variable.name='Assets')
        ggplot(d_Rmelt, aes(Returns, fill=Assets)) + geom_histogram(binwidth=0.25) +
            theme(legend.position = 'bottom', legend.title = element_blank()) +
            labs(title='', x='Daily Returns (%)', y='Frequency')
    })
    
    output$plotInfo_R_hist = renderText({
        xy_str <- function(e) {
            if(is.null(e)) return("NULL\n")
            paste0("Return(%) = ", round(e$x, 4), "\t", "Frequency = ", round(e$y),"\n")
        }
        xy_range_str <- function(e) {
            if(is.null(e)) return("NULL\n")
            paste0("Min Return(%) = ", round(e$xmin, 4), "\t",
                   "Min Frequency = ", round(e$ymin), "\n",
                   "               Max Return(%) = ", round(e$xmax, 4),"\t",
                   "Max Frequency = ", round(e$ymax))
        }
        
        paste0(
            "Click:         ", xy_str(input$plot_click_R_hist),
            "Double Click:  ", xy_str(input$plot_dblclick_R_hist),
            "Hover:         ", xy_str(input$plot_hover_R_hist),
            "Area:          ", xy_range_str(input$plot_brush_R_hist)
        )
    })
    
    output$efficientFrontier = renderPlot({
        returns = calculateReturns()
        risks_cov = calculateRisks()
        risks_cov_inv = solve(risks_cov)
        vec1 = matrix(1, nrow = length(returns))
        
        alpha = as.numeric(t(vec1) %*% risks_cov_inv %*% vec1)
        beta = as.numeric(t(vec1) %*% risks_cov_inv %*% returns)
        gamma = as.numeric(t(returns) %*% risks_cov_inv %*% returns)
        delta = alpha * gamma - beta * beta
        
        risks = sqrt(diag(risks_cov))
        
        p1 = stat_function(fun=function(z) 
            beta/alpha+sqrt((beta/alpha)^2-(gamma-delta*z^2)/(alpha)), color='#E46726', size=2)
        p2 = stat_function(fun=function(z) 
            beta/alpha-sqrt((beta/alpha)^2-(gamma-delta*z^2)/(alpha)), color='#f0eeed', size=2)
        
        p = switch (input$FrontierChoice, 
                'eff' = p1,
                'ieff' = p2)
        
        if (input$returnModel == 'CAPM') {
            ggtext = geom_text(aes(label=rownames(returns)) ,hjust=0, vjust=2, size = 4.5)
        }
            else {ggtext = geom_text(aes(label=names(returns)) ,hjust=0, vjust=2, size = 4.5)}
        
        P = ggplot(data.frame(x = risks, y = returns), aes(x, y)) +
            geom_point(size = 3, color = '#1efadb') + ggtext +
            scale_x_continuous(limits = c(0, max(risks)*1.2)) +
            scale_y_continuous(limits = c(-max(returns), max(returns)*2)) +
            labs(title='', x='Risk (%)', y='Return (%)')
        
        if (input$portfolioGO) {
            p_user_xy = userPortfolioPoint()
            p_user = geom_point(aes(x = p_user_xy[1], y = p_user_xy[2]),
                                color='#8742f5', pch=23, size = 4, fill='#8742f5')}
        
        if (input$portfolioGO & input$FrontierChoice=='both') return(P+p1+p2+p_user)
        else if (input$portfolioGO & input$FrontierChoice!='both') return(P+p+p_user)
        else if (input$portfolioGO==FALSE & input$FrontierChoice=='both') return(P+p1+p2)
        else {return(P+p)}
    })
    
    output$plotInfo_EF = renderText({
        xy_str <- function(e) {
            if(is.null(e)) return("NULL\n")
            paste0("Risk(%) = ", round(e$x, 2), "\t", "\t", "Return(%) = ", round(e$y, 2),"\n")
        }
        xy_range_str <- function(e) {
            if(is.null(e)) return("NULL\n")
            paste0("Min Risk(%) = ", round(e$xmin, 2), "\t",
                   "Min Return(%) = ", round(e$ymin, 2), "\n",
                   "               Max Risk(%) = ", round(e$xmax, 2),"\t",
                   "Max Return(%) = ", round(e$ymax, 2))
        }
        
        paste0(
            "Click:         ", xy_str(input$plot_click_EF),
            "Double Click:  ", xy_str(input$plot_dblclick_EF),
            "Hover:         ", xy_str(input$plot_hover_EF),
            "Area:          ", xy_range_str(input$plot_brush_EF)
        )
    })
    
    output$userWeights = renderUI({
        asset_names = colnames(readPortfolio())
        returns = calculateReturns()
        r = c('Date')
        if (input$hasMarket) {
            r = c('Date', input$market)
            asset_names = asset_names[!(asset_names %in% r)]}
        else {asset_names = names(returns)}
        n_assets = length(asset_names)
        n_half1 = round(n_assets/2)+1
        n_half2 = n_half1+1
        fluidRow(
            column(6, lapply(1:n_half1, function(i) {
            numericInput(paste0('w_', i), asset_names[i], 0)
                })),
            column(6, lapply(n_half2:n_assets, function(i) {
                numericInput(paste0('w_', i), asset_names[i], 0)
            }))
            )
        })
    
     userPortfolioPoint = eventReactive(input$portfolioGO,{
        returns = calculateReturns()
        risks = calculateRisks()
        n_assets = length(returns)
        w_assets = unlist(lapply(1:n_assets, function(i) {
            eval(parse(text = paste0('input$w_', i)))}))
        w_assets = w_assets/sum(w_assets)
        portfolio_return = sum(returns*w_assets)
        sig = 0
        for (i in 1:n_assets) {
            for (j in 1:n_assets) {
                sig = sig + w_assets[i] * w_assets[j] * risks[i, j]
                }
            }
        portfolio_risk = sqrt(sig)
        portfolio = c(portfolio_risk, portfolio_return)
        return(portfolio)
    })
    
    output$userPortfolio = renderText({
        x = userPortfolioPoint()
        paste0('Portfolio Return: ', round(x[2], 4), '\n',
               'Portfolio Risk: ', round(x[1], 4))
    })

})

