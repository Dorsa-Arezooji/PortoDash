# PortoDash - Investment Portfolio Dashboard
# Dorsa Arezooji -- https://Github.com/Dorsa-Arezooji/PortoDash

library(shiny)
library(shinythemes)

shinyUI(navbarPage("PortoDash", collapsible = TRUE, 
                   footer = HTML("<h5>&nbsp &nbsp &nbsp &#169;<a 
                                 href='https://Dorsa-Arezooji.github.io'>
                                 Dorsa Arezooji</a> 2020</h5>"),
    theme = shinythemes::shinytheme("united"),

    tabPanel("Data & Models",
             sidebarLayout(
                 sidebarPanel(id = 'sidebar',
                     fileInput("inputFile", "Upload assets .csv file:",
                               multiple = FALSE, accept = c('.csv')),
                     textInput("dateFormat", "Date Format:", 
                               placeholder = "e.g. %Y-%m-%d"),
                     hr(),
                     checkboxInput("hasMarket", "File contains market index", TRUE),
                     fluidRow(
                         column(6, textInput("market", "Market index:", 
                                             placeholder = "e.g. SPX, S&P500")),
                         column(6, textInput("rf", "Risk-free return (%):", 
                                             placeholder = "enter without %"))),
                     hr(),
                     radioButtons("returnModel", "Select return model:",
                                  c("Mean Historical"="MH", 
                                    "Exponentially Weighted Mean Historical"="EWMH", 
                                    "Capital Asset Pricing Model (CAPM)"="CAPM")),
                     hr(),
                     radioButtons("riskModel", "Select risk model:",
                                  c("Sample Covariance"="Cov", 
                                    "Exponentially Weighted Covariance"="ExpCov")),
                     HTML('<h2><br></h2>'),
                     actionButton("go", "Done", class = "btn-primary")
                 ),
                 mainPanel(
                     tabsetPanel(
                         tabPanel('Returns', plotOutput('plotReturn')),
                         tabPanel('Risk Matrix', plotOutput('plotRisk'))
                         )
                     )
                 )
             ),
    
    tabPanel("Explore Assets",
             sidebarLayout(
                 sidebarPanel(id = 'sidebar',
                     uiOutput("assetCheckBox")
                 ),
                 mainPanel(
                     tabsetPanel(
                         tabPanel('Daily Prices', 
                                  plotOutput("plots_P",
                                             click = "plot_click_P",
                                             dblclick = "plot_dblclick_P",
                                             hover = "plot_hover_P",
                                             brush = "plot_brush_P"), 
                                  h1(''),
                                  verbatimTextOutput("plotInfo_P")),
                         tabPanel('Daily Returns', 
                                  plotOutput("plots_R_daily",
                                             click = "plot_click_R_daily",
                                             dblclick = "plot_dblclick_R_daily",
                                             hover = "plot_hover_R_daily",
                                             brush = "plot_brush_R_daily"), 
                                  h1(''),
                                  verbatimTextOutput("plotInfo_R_daily")),
                         tabPanel('Returns Histogram', 
                                  plotOutput("plots_R_hist",
                                             click = "plot_click_R_hist",
                                             dblclick = "plot_dblclick_R_hist",
                                             hover = "plot_hover_R_hist",
                                             brush = "plot_brush_R_hist"),
                                  h1(''),
                                  verbatimTextOutput("plotInfo_R_hist"))
                         #column(10, div(plotOutput("plots_P"), style='height:400px'))
                         )
                     )
                 )
             ),
    
    tabPanel("Portfolio",
             sidebarLayout(
                 sidebarPanel(id = 'sidebar',
                     radioButtons("FrontierChoice", "Select frontier(s) to display:", 
                                  c("Efficient"="eff",
                                    "Inefficient"="ieff",
                                    "both"="both")),
                     hr(),
                     h5('Enter the weights (can be zero) for each asset:'),
                     uiOutput('userWeights'),
                     HTML('<h2><br></h2>'),
                     actionButton('portfolioGO','Show My Portfolio',class='btn-primary')
                 ),
                 mainPanel(
                     plotOutput('efficientFrontier',
                                click = "plot_click_EF",
                                dblclick = "plot_dblclick_EF",
                                hover = "plot_hover_EF",
                                brush = "plot_brush_EF"),
                     h1(''),
                     verbatimTextOutput("plotInfo_EF"),
                     h1(''),
                     verbatimTextOutput('userPortfolio')
                     )
                 )
             ),
    tabPanel("Documentation",
             navlistPanel('PortoDash',
                 tabPanel('About',
                          HTML("<img src='PortoDash_logo.png' width='400px'>
                          <p style='font-size:16px'><br><br>PortoDash
                          is a cloud based, easy to use portfolio dashboard!<br> It visualizes 
                          your assets and portfolio, provides risk/return metrics, 
                          and more to help you choose which asssts to invest in.<br><br>
                          <img src='overview.png' width='60%'>
                               <blockquote><i>Disclaimer!<br>While PortoDash offers
                               insights, it does not provide any financial advice!
                               </i></blockquote></p><p style='font-size:16px'>
                               PortoDash is developed and maintained by 
                               <a href='https://Dorsa-Arezooji.github.io'> Dorsa Arezooji</a>.
                               <br>You can find me on <a 
                               href='https://www.linkedin.com/in/dorsa-arezooji/'>
                               <img src='https://i.stack.imgur.com/gVE0j.png'/></a> or 
                               check out my other projects on 
                               <a href='https://github.com/dorsa-arezooji/'>
                               <img src='https://i.stack.imgur.com/tskMh.png'/></a>
                               .</p>")),
                 tabPanel('What does it do?', 
                          HTML("<h1>What does it do?</h1><hr/> <ul style='font-size:16px'>
                          <li>PortoDash offers a number of return and risk models to
                          choose from!</li><li>It calculates the returns and risks of
                          your portfolio's assets to visualize your investment portfolio.
                          </li><li>It also offers interactive visualizations of your 
                          assets and portfolio to help you gain insight.</li></ul>
                               <br>")),
                 '1. Data & Models',
                 tabPanel('1.1. Input File',
                          HTML("<h1>1.1. Input File</h1><hr/>
                               <p style='font-size:16px'>The only thing you need to upload is 
                               your dataset. It needs to:<br><ul style='font-size:16px'>
                               <li>be in <b>.csv</b> format</li><li>have a date column named
                               <b>Date</b> (doesn't need to be the first column)</li><li>
                               include a column containing the <b>market</b>'s daily data
                               (if you are going to use the CAPM model)</li></ul></p>
                               <p style='font-size:16px'>More information on where to find 
                               datasets is included in the <i>Resources</i> section at
                               the end.<br><br>A sample dataset would look like this:<br><br><img 
                               src='dataset.png' width='90%'><br><br>After uploading your dataset,
                               You'll need to specify the <i>date format</i>.<br>Here are some
                               examples for reference:<br><table style='width:90%; font-size:16px;
                               background-color:#f0f0f0'>
                               <tr><th>Format</th><th>Example</th></tr><tr><td>%Y/%m/%d</td>
                               <td>2020/10/15</td></tr><tr><td>%y-%m-%d</td><td>20-10-15</td></tr>
                               <tr><td>%Y.%b.%d</td><td>2020.Oct.15</td></tr><tr><td>%Y %B %d</td>
                               <td>2020 October 15</td></tr></table></p>")),
                 tabPanel('1.2. Market Index & Risk-free Return Rate',
                          HTML("<h1>1.2. Market Index & Risk-free Return Rate</h1><hr/>
                               <p style='font-size:16px'>If you want to choose the CAPM model, 
                               you need to include the market's daily data. In this case<br>
                               <ul style='font-size:16px'><li>first, check the box indicating that
                               <i>File contains market index</i></li><li>next, enter the market index
                               as it appears in the dataset (for instance <i>SPX</i> or <i>S&P500</i>)</li>
                               <li>finally, enter the risk-free return rate without the % sign</li></ul><br>
                               <img src='market.png' width='600px'>
                               <p style='font-size:16px'><br>You can find more information about the risk-free 
                               return rate, and how to calculate it <a href='https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=&cad=rja&uact=8&ved=2ahUKEwjpm7bc17bsAhXnUhUIHTciAW4QFjABegQIARAC&url=https%3A%2F%2Fwww.investopedia.com%2Fterms%2Fr%2Frisk-freerate.asp&usg=AOvVaw3iL4FH3LES44xQ-ByHjcQP'>
                               here</a>.</p>")),
                 tabPanel('1.3. Return Models',
                          HTML("<h1>1.3. Return Models</h1><hr/>
                               <p style='font-size:16px'>The daily returns for all assets are calculated and the
                               overall return rate can be calculated using different models:<br><br></p>
                               <h4>1. Mean Historical</h4><p style='font-size:16px'>takes the average of daily 
                               returns<br><br></p><h4>2. Exponentially Weighted Mean Historical</h4>
                               <p style='font-size:16px'>takes the weighted average of daily returns, assigning higher weights to more
                               recent observations<br><br></p><h4>3. Capital Asset Pricing Model (CAPM)</h4>
                               <p style='font-size:16px'>based on the CAPM model.<br>You can find more information
                               about the CAPM model and its benefits/drawbacks <a hreft='https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=&cad=rja&uact=8&ved=2ahUKEwie3eTv27bsAhUXRxUIHalPDksQFjAHegQIAxAC&url=https%3A%2F%2Fwww.investopedia.com%2Fterms%2Fc%2Fcapm.asp&usg=AOvVaw157MRELY7vMqRaIT2jVAqK'>
                               here</a>.<br><br></p><img src='Returns1.png' width='90%'>")),
                 tabPanel('1.4. Risk Models',
                          HTML("<h1>1.4. Risk Models</h1><hr/>
                               <p style='font-size:16px'>The concept of risk is defined using the spread of an
                               asset's returns.<br>In other words the standard deviation of the return distribution.<br><br>
                               Also, in order to reduce risk, it is recommended to diversify the investment portfolio
                                by choosing assets that are not strongly correlated.<br>In other words, they have (ideally)
                               a correlation close to -1.<br><br>The risk can be measured using different models:<br></p>
                               <h4>1. Sample Covariance</h4><p style='font-size:16px'>this is just your regular covariance
                               matrix<br><br></p><h4>2. Exponentially Weighted Covariance</h4><p style='font-size:16px'>
                               returnd the weighted covariance matrix<br><br></p><img src='Risks.png' width='90%'>")),
                 tabPanel('Important Notes!',
                          HTML("<h1>Important Notes!</h1><hr/>
                               <ul style='font-size:16px'><li>After uploading the dataset and specifying the date format, 
                               market index, risk-free rate, return and risk model, you need to click on the <b>Done</b>
                                button!<br>Otherwise, you might get weird errors when you open other tabs.<br><br></li><li>Also, 
                               make sure to enter the <i>Date format</i> correctly.<br><br></li><li>If you enter the market index 
                               incorrectly (such column does not exist in your file), you'll get an error saying so.<br><br>
                               </li><li>You need to enter both the market index and the risk-free rate for the CAPM model.</li></ul>")),
                 '2. Explore Assets',
                 tabPanel('2.1. Daily Prices',
                          HTML("<h1>2.1. Daily Prices</h1><hr/>
                               <p style='font-size:16px'>In this tab, all your assets are loaded, processed,
                               and displayed.<br>You can select the assets you want to explore on the left panel.
                               <br><br><img src='PanelAssets.png' width='20%'><br><br>You can see the daily prices
                               of the selected assets on the right-hand side panel.<br>This plot, and all other 
                               plots are interactive. Meaning you can click, double click, or hover your pointer 
                               on any part of the plot and get the corresponding coordinates below the plot.<br><br>
                               <img src='DailyPrices.png' width='80%'><br><br>You can also swipe an area and see the maximum and 
                               minimum values for the x and y axes.</p>")),
                 tabPanel('2.2. Daily Returns',
                          HTML("<h1>2.2. Daily Returns</h1><hr/>
                               <p style='font-size:16px'>In the <i>Daily Returns</i> tab, you can see the daily 
                               variations of return rates for the selected assets.<br><br><img src='DailyReturns.png' 
                               width='80%'><br><br>You can use the swiping area as a window by dragging the 
                               window.</p>")),
                 tabPanel('2.3. Returns Histogram',
                          HTML("<h1>2.3. Returns Histogram</h1><hr/>
                               <p style='font-size:16px'>In the <i>Returns Histogram</i> tab, you can see
                               the histogram of daily returns for each selected asset.<br><br>
                               <img src='ReturnsHist.png' width='80%'></p>")),
                 '3. Portfolio',
                 tabPanel('3.1. Frontiers',
                          HTML("<h1>3.1. Frontiers & Asset Weights</h1><hr/>
                               <p style='font-size:16px'>In the <i>Portfolio</i> tab, you can select
                               which frontiers to display along with the assets.<br><br>Next, you 
                               can buil your own portfolio by assigning weights to the assets.<br>
                               If based on the information you arrived at from the previous tabs, 
                               you decide not to invest in a specific asset, you can assign a zero
                               weight to that asset.<br><br><img src='PanelPortfolio.png' width='30%'>
                               </p>")),
                 tabPanel('3.2. Portfolio',
                          HTML("<h1>Portfolio</h1><hr/>
                               <p style='font-size:16px'>After setting the weights, you need to click
                                on the <b>Show My Portfolio</b> button.<br>Your portfolio will be 
                                displayed as the purple diamond shape point on the plot.<br>The overall 
                                return and risk of your portfolio will also be displayed below the 
                                interactive panel.<br><br><img src='Portfolio.png' width='80%'></p>")),
                 'Resources',
                 tabPanel('Dataset',
                          HTML("<h1>Dataset</h1><hr/>
                               <p style='font-size:16px'>I have included a 
                               <a href='https://github.com/Dorsa-Arezooji/PortoDash/blob/master/testdata.csv'>
                               sample dataset</a> in the PortoDash github repo. However, there are 
                               several websites where you can get historical price data.<br>Here 
                               are a few for reference:<br></p><ul style='font-size:16px'><li>
                               <a href='www.nasdaq.com'>www.nasdaq.com</a></li><li>
                               <a href='www.finance.yahoo.com'>www.finance.yahoo.com</a></li>
                               <li><a href='www.macrotrends.net'>www.macrotrends.net</a></li></ul>")),
                 tabPanel('Recommended Readings',
                          HTML("<h1>Recommended Readings</h1><hr/>
                               <p style='font-size:16px'>There are a number of excellent online resources 
                               on investment portfolio management.<br><a href='www.investopedia.com'>
                               Investopedia</a> is a good place to start with.</p>"))
             )
    ),
    tags$head(tags$style(
        HTML('
         #sidebar {
            height: 800px;
        }

        body, label, input, button, select { 
          font-family: "Arial";
        }')
    ))
))
#includeMarkdown("documentation.md")