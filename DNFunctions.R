#-------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------
# Utility functions for transforming data and producing charts in R
# 
# Version:  October 2019 - v1.0
# Authors: Edgar Scrase
# 
# Copyright © 2021 UNHCR Global Data Service, Statistics and Demographics Section
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#-------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------

# The default x axis style for e.g. a chart with facets and totals.
defaultXAxisStyle <- element_text(family=fontsForCharts, colour="#aaaaaa", size=7)


#-------------------------------------------------------------------------------------------------------
# Worker functions ...
#-------------------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------------------------------------------------------------------
#----- Generates a percentage
GetPercent <- 
  function(enum, denom, rounding) {
    pc <- 0
    rounding <- as.numeric(rounding)
    enum <- as.numeric(enum)
    denom <- as.numeric(denom)
    
    if(enum > 0 && denom > 0) {
      pc <- round(enum/denom*100,rounding)
    }
    
    returnValue <- as.numeric(pc)
  }

#-------------------------------------------------------------------------------------------------------------------------------------------------------
#----- Percent label creator for the stacked bar chart
GetPercentLabel <- 
  function(pc, threshold) {
    
    pc <- as.numeric(pc)
    threshold <- as.numeric(threshold)
    
    percStr <- ifelse(
      (is.na(pc) == FALSE && pc >= threshold),
      str_c(as.character(pc), "%"),
      ""
    )
    
    #    message(str_c("\nEnum: ", enum, " Denom: ", denom, " Threshold:", threshold, " Percent:", pc, " Str:", percStr))
    returnValue <- percStr
  }  


#-------------------------------------------------------------------------------------------------------------------------------------------------------
#----- Calculates a column with a percentage and another with the PercentLabel...
CalculatePercentageWithLabel <- 
  function(df, level1Col1, level1Col2, level2Col, countCol, labelThreshold) {

    # Include a suitable default for the label threshold (i.e. the % at which to start showing labels)
    if ( labelThreshold == 0) {
      labelThreshold <- 10.0
    }
    
    # Reset both the percent and the label
    df$PercentLabel <- ""
    df$Percent <- 0
    
    # So in order to use the column names provided by string values, we need to convert them to names and then use the !! to unquote them
    # https://stackoverflow.com/questions/47081564/replacing-group-by-with-group-by-when-the-argument-is-a-string-in-dplyr
    level1Col1 <- as.name(level1Col1)
    level1Col2 <- as.name(level1Col2)
    level2Col <- as.name(level2Col)
    countCol <- as.name(countCol)
    
    # Then lets calculate the percentages ...
    # This creates the percent and the percent label columns - it looks a little intense, 
    df <- df %>% 
      group_by(!!level1Col1, !!level1Col2) %>% 
      # okay, we've got the total, now we can do some math with mutate
      # And round the data too to the nearest integer
      mutate( GroupTotal=sum(!!countCol, na.rm=T)) %>%    
      ungroup() %>%    
      group_by(!!level1Col1, !!level1Col2, !!level2Col) %>% 
      # okay, now lets also create a label for all columns
      mutate(
        Percent=GetPercent(!!countCol, GroupTotal, 0), 
        PercentLabel=GetPercentLabel(Percent, labelThreshold)) %>% 
      ungroup()
    

    returnValue <- df
}


#-------------------------------------------------------------------------------------------------------------------------------------------------------
# Summarise the data by year and month given specific citizen and geo values
SummariseByCitizenAndGeo <-
  function(df, citizenVal, geoVal, outputColName) {
    
    # lets try just simple to begin with
    dfSummary <- df %>% 
      filter(citizen == citizenVal) %>%
      filter(geo == geoVal) 
    
    # Remove the last month as the data is incomplete
    dfSummary <- dfSummary[-nrow(dfSummary),]
    
    # Filter the data down to what we want
    dfSummary <- dfSummary %>% 
      group_by(Year, Month)  %>% 
      summarise(TotalCount=sum(Count)) %>% 
      select(Year, Month, TotalCount)
    
    names(dfSummary)[names(dfSummary) == "TotalCount"] <- outputColName
    
    returnValue <- dfSummary
  }


#-------------------------------------------------------------------------------------------------------------------------------------------------------
# Generate folded data for the Eurostat pending applications (broken down by others, latin americans and venezuelans)
GenerateFoldedData <-
  function(dfTotal, dfCat1, dfCat2, totalColName, cat1ColName, cat2ColName, dfWithTitles, countCat2exCat1ColName, countTotalexCat2ColName) {

    # Group the data ensuring we get the latest value of the count
    df <- dfTotal %>% 
      group_by(Year)  %>% 
      summarise(Month=max(Month)) %>% 
      select(Year, Month)
    
    df$YearMonth <- paste(df$Year, "-", ifelse(df$Month < 10, "0", ""), df$Month, sep="")
    
    # join, to pull accross the totals
    df <- left_join(df, dfTotal, by=c("Year"="Year", "Month"="Month") )
    
    # join to pull accross the cat 1 totals (e.g. Venezuelans)
    df <- left_join(df, dfCat1, by=c("Year"="Year", "Month"="Month") )
    # join to pull accross the cat 2 totals (e.g. Latin americans)
    df <- left_join(df, dfCat2, by=c("Year"="Year", "Month"="Month") )
    
    # Calculate Cat2 excluding Cat1 (e.g. Latin Americans except Venezuelans)
    df[[countCat2exCat1ColName]] <- df[[cat2ColName]] - df[[cat1ColName]]
    # Others except Latin Americans
    df[[countTotalexCat2ColName]] <- df[[totalColName]] - df[[cat2ColName]]
    
    # Gather the last four columns
    df <- df %>% gather(5:8, key="PopGroup", value="Count")
    # Filter out the Cat2
    df <- df %>% filter(PopGroup != cat2ColName)
    
    
    # Create the count and percent labels
    df$CountLabel = comma(df$CountTotal)
    df <- CalculatePercentageWithLabel(df, "Year", "Year", "PopGroup", "Count", 3)


    # Factor the pending applications and include the "real" titles for the pending population groups.
    df <- left_join(df, dfWithTitles, by=c("PopGroup"="Key") )

  }

#-------------------------------------------------------------------------------------------------------------------------------------------------------
# Generate folded data for the Eurostat pending applications (broken down by others, latin americans and venezuelans)
GenerateFoldedDataSingleCategory <-
  function(dfTotal, dfCat1, totalColName, cat1ColName, dfWithTitles, countTotalexCat1ColName) {
    
    # Group the data ensuring we get the latest value of the count
    df <- dfTotal %>% 
      group_by(Year)  %>% 
      summarise(Month=max(Month)) %>% 
      select(Year, Month)
    
    df$YearMonth <- paste(df$Year, "-", ifelse(df$Month < 10, "0", ""), df$Month, sep="")
    
    # join, to pull accross the totals
    df <- left_join(df, dfTotal, by=c("Year"="Year", "Month"="Month") )
    
    # join to pull accross the cat 1 totals (e.g. Venezuelans)
    df <- left_join(df, dfCat1, by=c("Year"="Year", "Month"="Month") )
    
    # Others except Latin Americans
    df[[countTotalexCat1ColName]] <- df[[totalColName]] - df[[cat1ColName]]
    
    # Gather the last two columns
    df <- df %>% gather(5:6, key="PopGroup", value="Count")
    
    # Create the count and percent labels
    df$CountLabel = comma(df$CountTotal)
    df <- CalculatePercentageWithLabel(df, "Year", "Year", "PopGroup", "Count", 3)
    
    # Factor the pending applications and include the "real" titles for the pending population groups.
    df <- left_join(df, dfWithTitles, by=c("PopGroup"="Key") )
    
  }

#-------------------------------------------------------------------------------------------------------------------------------------------------------
GenerateChartWithTotalsAndFacets <-
  function(df, xColName, yColName, yColLabel, facetColName, facetCols, titleText, captionText) {
  
    xColName = as.name(xColName)
    yColName = as.name(yColName)
    yColLabel = as.name(yColLabel)
    facetColName = as.name(facetColName)
    
    # Do the visualisation
    # Note that we use get() to get the column object from the string name that is passed in
    # See https://stackoverflow.com/questions/34097133/passing-data-and-column-names-to-ggplot-via-another-function for more info
    plot <- ggplot(df, 
               aes(x=!!xColName,
                    y=!!yColName,
                    label=!!yColLabel,
                    family=fontsForCharts
                )
            ) + # Make a comma separated label for here ...
      geom_bar(stat="identity") +
      # And this is to present the labels ...
      geom_text(hjust=-0.1, size=4, colour="#505050") + 
      # Reverse categorical plot for the x axis (that will become the y axis) - see https://gist.github.com/jennybc/6f3fa527b915b920fdd5
      scale_x_discrete(limits = rev(levels(df[[xColName]]))) +
      scale_y_continuous(limits=c(0,max(df[[yColName]])*1.7)) +
      coord_flip() +
      labs(
        title=paste(titleText), 
        y="", 
        x="", 
        caption=paste(captionText),
        family=fontsForCharts) +
      # set a very minimal theme
      theme_minimal(base_family=fontsForCharts, base_size=20) +   
      # Tweak the axis text
      theme(
        axis.text.x=defaultXAxisStyle, #element_text(family=fontsForCharts, colour="#aaaaaa", size=7), 
        axis.text.y=element_text(family=fontsForCharts, size=20),
        strip.text.x=element_text(family=fontsForCharts, size=24),
        panel.grid.major = element_line(colour = "#efefef", size=0.6),
        panel.grid.minor = element_line(colour = "#efefef", size=0.4)
      ) +
      facet_wrap(df[[facetColName]], ncol=facetCols)
  
    returnValue <- plot

}

#-------------------------------------------------------------------------------------------------------------------------------------------------------
GenerateChartWithTotals <-
  function(df, xColName, yColName, yColLabel, titleText, captionText) {
    
    xColName = as.name(xColName)
    yColName = as.name(yColName)
    yColLabel = as.name(yColLabel)

    # Do the visualisation
    # Note that we use get() to get the column object from the string name that is passed in
    # See https://stackoverflow.com/questions/34097133/passing-data-and-column-names-to-ggplot-via-another-function for more info
    plot <- ggplot(df, 
                   aes(x=!!xColName,
                       y=!!yColName,
                       label=!!yColLabel,
                       family=fontsForCharts
                   )
    ) + # Make a comma separated label for here ...
      geom_bar(stat="identity") +
      # And this is to present the labels ...
      geom_text(hjust=-0.1, size=4, colour="#505050") + 
      # Reverse categorical plot for the x axis (that will become the y axis) - see https://gist.github.com/jennybc/6f3fa527b915b920fdd5
      scale_x_discrete(limits = rev(levels(df[[xColName]]))) +
      scale_y_continuous(limits=c(0,max(df[[yColName]])*1.7)) +
      coord_flip() +
      labs(
        title=paste(titleText), 
        y="", 
        x="", 
        caption=paste(captionText),
        family=fontsForCharts) +
      # set a very minimal theme
      theme_minimal(base_family=fontsForCharts, base_size=24) +   
      # Tweak the axis text
      theme(
        axis.text.x=element_text(family=fontsForCharts, colour="#aaaaaa", size=7), 
        axis.text.y=element_text(family=fontsForCharts, size=20),
        strip.text.x=element_text(family=fontsForCharts, size=24),
        panel.grid.major = element_line(colour = "#efefef", size=0.6),
        panel.grid.minor = element_line(colour = "#efefef", size=0.4)
      ) 
    
    returnValue <- plot
    
  }


#-------------------------------------------------------------------------------------------------------------------------------------------------------
# fillValues is a vector
GenerateChartWithStackedBarChartAndFacets <-
  function(df, xColName, yColName, yColLabel, fillColName, fillValues, facetColName, facetCols, titleText, captionText) {

    pos <- position_fill(vjust=0.47)
    
    plot <- ggplot(df,
                    # The x axis is the names of the countries ordered by the overall count                
                    #aes(x=fct_reorder(Year, Percent, desc=TRUE),
                    aes(x=get(xColName),
                        # and the y axis is the percentage based on the variable (with the zeros removed)
                        y=get(yColName),
                        # and the labels are the percentage label strings
                        label=get(yColLabel),    
                        # and the fill is the decisions
                        fill=get(fillColName), 
                        na.rm=TRUE,
                        family=fontsForCharts)
                   ) +
      
      geom_bar(position=pos, stat="identity") +
      geom_text(position=pos, size = 3, colour="#ffffff") + 
      
      scale_x_discrete(limits = rev(levels(df[[xColName]]))) +
      # Then set our colours and legend labels using the parameters of scale_fill_manual
      # note that we strim as needed to avoid the total count
      scale_fill_manual(values=fillValues) +
      # flip the coordinates
      coord_flip() +
      # set the labels
      labs(title=titleText, 
           y="", 
           x="", 
           fill="", 
           caption=captionText, 
           family=fontsForCharts) +
      # set a very minimal theme
      theme_minimal(base_family=fontsForCharts) +   
      # These two lines tweak the positioning of the legend and hide the x axis ticks need to go AFTER the call to theme_minimal
      theme(
        axis.text.x=element_blank(), 
        axis.text.y=element_text(family=fontsForCharts, size=20),
        strip.text.x=element_text(family=fontsForCharts, size=20),
        legend.position="bottom",
        legend.text=element_text(size=14)
      ) +
      #  facet_wrap(. ~Citizen, labeller=as_labeller(LabelGetCoOName), ncol=5)
      facet_wrap(df[[facetColName]], ncol=facetCols)  
    
    
    returnValue <- plot
}


#-------------------------------------------------------------------------------------------------------------------------------------------------------
# fillValues is a vector
GenerateChartWithStackedBarChart <-
  function(df, xColName, yColName, yColLabel, fillColName, fillValues, titleText, captionText) {
    
    pos <- position_fill(vjust=0.47)
    
    plot <- ggplot(df,
                   # The x axis is the names of the countries ordered by the overall count                
                   #aes(x=fct_reorder(Year, Percent, desc=TRUE),
                   aes(x=get(xColName),
                       # and the y axis is the percentage based on the variable (with the zeros removed)
                       y=get(yColName),
                       # and the labels are the percentage label strings
                       label=get(yColLabel),    
                       # and the fill is the decisions
                       fill=get(fillColName), 
                       na.rm=TRUE,
                       family=fontsForCharts)
    ) +
      
      geom_bar(position=pos, stat="identity") +
      geom_text(position=pos, size = 3, colour="#ffffff") + 
      
      scale_x_discrete(limits = rev(levels(df[[xColName]]))) +
      # Then set our colours and legend labels using the parameters of scale_fill_manual
      # note that we strim as needed to avoid the total count
      scale_fill_manual(values=fillValues) +
      # flip the coordinates
      coord_flip() +
      # set the labels
      labs(title=titleText, 
           y="", 
           x="", 
           fill="", 
           caption=captionText, 
           family=fontsForCharts) +
      # set a very minimal theme
      theme_minimal(base_family=fontsForCharts) +   
      # These two lines tweak the positioning of the legend and hide the x axis ticks need to go AFTER the call to theme_minimal
      theme(
        axis.text.x=element_blank(), 
        axis.text.y=element_text(family=fontsForCharts, size=20),
        strip.text.x=element_text(family=fontsForCharts, size=20),
        legend.position="bottom",
        legend.text=element_text(size=14)
      ) 
    
    returnValue <- plot
  }

#-------------------------------------------------------------------------------------------------------------------------------------------------------
#----- Produce a population pyramid; takes a data frame with the "Age" and "Sex" factors, and the values as "Percent"
# Adapted from https://stackoverflow.com/questions/14680075/simpler-population-pyramid-in-ggplot2
# Sex has to be Male, Female ...
GeneratePopulationPyramid <- 
  function(popPyDF) {
    popPy1 <- ggplot(data = popPyDF, 
                     mapping = aes(
                       x = Age, 
                       y = ifelse(test = Sex == "Male",  yes = -Percent, no = Percent), 
                       fill = Sex,
                       label=paste(round(Percent*100, 0), "%", sep=""),
                       family=fontsForCharts
                     )) +
      geom_bar(stat = "identity") +
      #geom_text( aes(label = TotalCount, TotalCount = TotalCount + 0.05)) +
      geom_text(hjust=ifelse(test = popPyDF$Sex == "Male",  yes = 1.1, no = -0.1), size=6, colour="#505050") +
      #  scale_y_continuous(limits=c(0,max(appArr$Count)*1.7)) +
      # The 1.1 at the end is a buffer so there is space for the labels on each side
      scale_y_continuous(labels = abs, limits = max(popPyDF$Percent) * c(-1,1) * 1.1) +
      # Custom colours
      scale_fill_manual(values=as.vector(c("#d23f67","#505050"))) +
      # Remove the axis labels and the fill label from the legend - these are unnecessary for a Population Pyramid
      labs(
        x = "",
        y = "",
        fill="", 
        family=fontsForCharts
      ) +
      theme_minimal(base_family=fontsForCharts, base_size=20) +   
      coord_flip() +
      # Remove the grid and the scale
      theme( 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x=element_blank(), 
        axis.text.y=element_text(family=fontsForCharts, size=20),
        strip.text.x=element_text(family=fontsForCharts, size=24),
        legend.position="bottom",
        legend.text=element_text(size=20)
      )
    
    returnValue <- popPy1
  }

#-------------------------------------------------------------------------------------------------------------------------------------------------------
#----- Summarise data for a population pyramid
# cooCode - two character ISO code e.g. VE
# Year - 4 digit e.g. 2019
SummarisePopulationPyramidData <- 
  function(df, ageDF, cooCode, year) {
    
    # Filter the data to the specific CoO and Year
    summaryDF <- df %>% filter(citizen == cooCode) %>% filter(Year == year)
    
    # Summarise this data (typically it maybe monthly)
    summaryDF <- summaryDF %>% 
      group_by(Year, sex, age)  %>%
      summarise(TotalCount=sum(Count)) %>% 
      select(Year, sex, age, TotalCount)
    
    # lets get the total and create the percentages ....
    summaryTotal <- sum(summaryDF$TotalCount)
    summaryDF$Percent <- summaryDF$TotalCount / summaryTotal
    
    # Standardise the sex to use Male and Female
    summaryDF$Sex <- ifelse(test = summaryDF$sex == "M",  yes = "Male", no = "Female")
    
    # Join the summary to the age data frame
    summaryDF <- left_join(summaryDF, ageDF, by=c("age"="Code") )
    
    
    returnValue <- summaryDF
    
  }




#-------------------------------------------------------------------------------------------------------------------------------------------------------
GenerateColumnChartWithTotals <-
  function(df, xColName, yColName, yColLabel, titleText, captionText, doRotateXAxisLabels) {
    
    xColName = as.name(xColName)
    yColName = as.name(yColName)
    yColLabel = as.name(yColLabel)
    
    
    xAxisLabelFormat <- element_text(family=fontsForCharts, colour="#505050", size=14)
    if( doRotateXAxisLabels==TRUE) {
      xAxisLabelFormat <- element_text(family=fontsForCharts, colour="#505050", size=14, angle=45, hjust=1, vjust=1) 
    }
        
    
    # Do the visualisation
    # Note that we use get() to get the column object from the string name that is passed in
    # See https://stackoverflow.com/questions/34097133/passing-data-and-column-names-to-ggplot-via-another-function for more info
    plot <- ggplot(df, 
                   aes(x=!!xColName,
                       y=!!yColName,
                       label=!!yColLabel,
                       family=fontsForCharts
                   )
    ) + # Make a comma separated label for here ...
      geom_bar(stat="identity") +
      # And this is to present the labels ...
      geom_text(hjust=0.5, vjust=-0.7, size=4, colour="#505050") + 
      # Reverse categorical plot for the x axis (that will become the y axis) - see https://gist.github.com/jennybc/6f3fa527b915b920fdd5
#      scale_x_continuous("ID", labels = as.character(ID), breaks = ID)
###      scale_x_discrete(labels=as.character(df[[xColName]]), breaks=df[[xColName]]) +
#      scale_x_discrete(labels=as.character(df[[xColName]]), breaks=df[[xColName]]) +
      scale_x_discrete(limits = rev(levels(df[[xColName]]))) +
#      scale_x_discrete(limits = rev(levels(df[[xColName]]))) +
      scale_y_continuous(limits=c(0,max(df[[yColName]])*1.7)) +
      labs(
        title=paste(titleText), 
        y="", 
        x="", 
        caption=paste(captionText),
        family=fontsForCharts) +
      # set a very minimal theme
      theme_minimal(base_family=fontsForCharts, base_size=20) +   
      # Tweak the axis text
      theme(
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
#        axis.text.x=element_text(family=fontsForCharts, colour="#505050", angle = 90, vjust = 0.5, size=14),
        axis.text.x=xAxisLabelFormat,
        axis.text.y=element_blank(), 
        strip.text.x=element_text(family=fontsForCharts, size=12)
      ) 
    
    returnValue <- plot
    
  }


#-------------------------------------------------------------------------------------------------------------------------------------------------------
# Generates a stacked column chart.  Adapted from this tutorial: https://www.datanovia.com/en/blog/how-to-create-a-ggplot-stacked-bar-chart-2/
# fillValues is a vector
GenerateStackedColumnChart <-
  function(df, xColName, yColName, yColFillLabel, fillColName, fillValues, yTotalColName, yTotalColLabel, titleText, captionText) {
    
    pos <- position_fill(vjust=0.6)
    
    xColName = as.name(xColName)
    yColName = as.name(yColName)
    yColFillLabel = as.name(yColFillLabel)
    fillColName = as.name(fillColName)
    yTotalColName = as.name(yTotalColName)
    yTotalColLabel = as.name(yTotalColLabel)
    
    
    # Generate the label positions here - mid way up each segment of the stack
    df <- df %>%
      group_by(!!xColName) %>%
      arrange(!!xColName, desc(!!fillColName)) %>%
      mutate(LabYPos = cumsum(!!yColName) - 0.5 * !!yColName) 
    
    # Generate the chart
    plot <- ggplot(df,
                   # The x axis is the names of the countries ordered by the overall count                
                   #aes(x=fct_reorder(Year, Percent, desc=TRUE),
                   aes(x=!!xColName,
                       # and the y axis is the percentage based on the variable (with the zeros removed)
                       y=!!yColName,
                       # and the labels are the percentage label strings
                       #label=!!yColLabel,    
                       # and the fill is the decisions
                       fill=!!fillColName, 
                       na.rm=TRUE,
                       family=fontsForCharts)
    ) +
      
      # This line generates the stacking: https://www.datanovia.com/en/blog/how-to-create-a-ggplot-stacked-bar-chart-2/
      geom_col(aes(fill = !!fillColName), width = 0.7) +
      
      #      geom_bar(position=pos, stat="identity") +
      
      # Label for the total for each bar
      geom_text(position="identity", aes(y=df[[yTotalColName]], label=df[[yTotalColLabel]], vjust=-0.6), size = 3, colour="#505050" ) +
      
      #geom_text(position=pos, size = 3, colour="#ffffff", y=df[[yColName]], group=df[[fillColName]]) +
      #geom_text(position="identity", size = 3, colour="#ffffff", y=df[[yColName]], group=df[[fillColName]]) +
      geom_text(aes(y=df$LabYPos, group=!!fillColName, label=!!yColFillLabel), size = 3, colour="#ffffff") + 
      
      
      scale_x_discrete(limits = rev(levels(df[[xColName]]))) +
      # Then set our colours and legend labels using the parameters of scale_fill_manual
      # note that we strim as needed to avoid the total count
      scale_fill_manual(values=fillValues) +
      #      scale_y_continuous(limits=c(0,max(df[[yColName]])*1.7)) +
      # set the labels
      labs(title=titleText, 
           y="", 
           x="", 
           fill="", 
           caption=captionText, 
           family=fontsForCharts) +
      # set a very minimal theme
      theme_minimal(base_family=fontsForCharts) +   
      # These two lines tweak the positioning of the legend and hide the x axis ticks need to go AFTER the call to theme_minimal
      theme(
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x=element_text(family=fontsForCharts, size=14), 
        axis.text.y=element_blank(),
        strip.text.x=element_text(family=fontsForCharts, size=20),
        legend.position="bottom",
        legend.text=element_text(size=14)
      ) 
    
    returnValue <- plot
  }


#-------------------------------------------------------------------------------------------------------
# Draws a donut pie chart, by generating percentages from the given yCol and using the attributes in xCol to present the info
# The fill labels will always be %'s
GenerateDonutChart <-
  function(df, xColName, yColName, fillValueColName, titleText, captionText) {
    
    xColName = as.name(xColName)
    
    # Compute percentages
    df$Fraction <- df[[yColName]] / sum(df[[yColName]])
    df$FractionLabel <- round(df$Fraction*100, 0)
    
    # Compute the cumulative percentages (top of each rectangle)
    df$ymax <- cumsum(df$Fraction)
    
    # Compute the bottom of each rectangle
    df$ymin <- c(0, head(df$ymax, n=-1))
    
    # Compute label position
    df$labelPosition <- (df$ymax + df$ymin) / 2
    
    # Compute a good label
    df$Label <- paste0(df[[xColName]], "\n", df$FractionLabel, "%")
    # Strip out the labels of small segments - it would be good to parameterise this ...
    df$Label[df$Fraction < 0.03] <- ""
    
    # Make the plot
    ggplot(df, aes(ymax=ymax, ymin=ymin, xmax=2.5, xmin=1.5, fill=!!xColName)) +
      geom_rect() +
      # Then set our colours and legend labels using the parameters of scale_fill_manual
      # note that we strim as needed to avoid the total count
      scale_fill_manual(values=levels(df[[fillValueColName]])) +
      geom_text( x=3.5, aes(y=labelPosition, label=Label, family=fontsForCharts), colour="#505050", size=8) + # x here controls label position (inner / outer)      
      #      scale_fill_brewer(palette=2) +
      #      scale_color_brewer(palette=2) +
      coord_polar(theta="y") +
      xlim(c(-1, 4)) +
      theme_void(base_family=fontsForCharts) +
      theme(
        legend.position = "none",
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        strip.text.x=element_blank()
      )
  }


#-------------------------------------------------------------------------------------------------------------------------------------------------------
GenerateBarChartWithTotals <-
  function(df, xColName, yColName, yColLabel, titleText, captionText) {
    
    xColName = as.name(xColName)
    yColName = as.name(yColName)
    yColLabel = as.name(yColLabel)
    
    
    yAxisLabelFormat <- element_text(family=fontsForCharts, colour="#505050", size=14)
    
    
    # Do the visualisation
    # Note that we use get() to get the column object from the string name that is passed in
    # See https://stackoverflow.com/questions/34097133/passing-data-and-column-names-to-ggplot-via-another-function for more info
    plot <- ggplot(df, 
                   aes(x=!!xColName,
                       y=!!yColName,
                       label=!!yColLabel,
                       family=fontsForCharts
                   )
    ) + # Make a comma separated label for here ...
      geom_bar(stat="identity") +
      # And this is to present the labels ...
      geom_text(hjust=-0.2, vjust=0.2, size=4, colour="#505050") + 
      # Reverse categorical plot for the x axis (that will become the y axis) - see https://gist.github.com/jennybc/6f3fa527b915b920fdd5
      #      scale_x_continuous("ID", labels = as.character(ID), breaks = ID)
      ###      scale_x_discrete(labels=as.character(df[[xColName]]), breaks=df[[xColName]]) +
      #      scale_x_discrete(labels=as.character(df[[xColName]]), breaks=df[[xColName]]) +
      scale_x_discrete(limits = rev(levels(df[[xColName]]))) +
      #      scale_x_discrete(limits = rev(levels(df[[xColName]]))) +
      scale_y_continuous(limits=c(0,max(df[[yColName]])*1.7)) +
      
      coord_flip()+
      labs(
        title=paste(titleText), 
        y="", 
        x="", 
        caption=paste(captionText),
        family=fontsForCharts) +
      
      # set a very minimal theme
      theme_minimal(base_family=fontsForCharts, base_size=16) +   
      # Tweak the axis text
      theme(
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        #        axis.text.x=element_text(family=fontsForCharts, colour="#505050", angle = 90, vjust = 0.5, size=14),
        axis.text.x=element_blank(), 
        axis.text.y=yAxisLabelFormat,
        strip.text.x=element_text(family=fontsForCharts, size=12)
      ) 
    
    returnValue <- plot
    
  }

#-------------------------------------------------------------------------------------------------------------------------------------------------------
#----- Save charts
# The full path should include the directory and file name, using forward slashes for sub folders
SaveChart <- 
  function(thePlot, widthMM, heightMM, fullPath) {
    
    ggsave(thePlot, 
           width=widthMM, 
           height=heightMM, 
           units="mm", 
           limitsize = FALSE,
           filename = fullPath, 
           bg = "transparent"
    )
  }





#-------------------------------------------------------------------------------------------------------------------------------------------------------
# Creates a scatter chart for each year of projections based on 
GeneratePlanningFigureScatterChartWrapper <-
  function(df, ptList, width, height, outputDirectory, outputSubDirectory, outputPTDescription) {
    
    xColName <- "Actual"
    yColName <- "Planned"
    
    xColName = as.name(xColName)
    yColName = as.name(yColName)
    
    ##### a ##### Remove the zeros ...
    df <- df %>% filter(!!xColName > 0 & !!yColName > 0 & PT %in% ptList)
    
    
    ##### b ##### Calculate the max and min
    
    # Ensure the data columns are numeric
    df[[ xColName ]] <- as.numeric(df[[ xColName ]])
    df[[ yColName ]] <- as.numeric(df[[ yColName ]])
    
    #minX <- minY <- 10^0# min(df[[ xColName ]], na.rm=TRUE)
    minX <- min(df[[ xColName ]], na.rm=TRUE)
    maxX <- max(df[[ xColName ]], na.rm=TRUE)
    minY <- min(df[[ yColName ]], na.rm=TRUE)
    maxY <- max(df[[ yColName ]], na.rm=TRUE)
    
print(paste(minX, minY, maxX, maxY), na.print = "NA")            
    
    # Calculate the lower bounds - note that we want the lowest of the two so that the X and Y scales are consistent
    minX = round(minX, -1 * (floor(log10(abs(minX)))))
    minY = round(minY, -1 * (floor(log10(abs(minY)))))
    
    lowerLimit = min(c(minX, minY))
    
    # Calculate the upper bounds
    #maxX = round(maxX, -1 * (floor(log10(abs(maxX))) + 1))
    maxX = round(maxX, -1 * (floor(log10(abs(maxX)))))
    #maxY = round(maxY, -1 * (floor(log10(abs(maxY))) + 1))
    maxY = round(maxY, -1 * (floor(log10(abs(maxY)))))
    
    upperLimit = max(c(maxX, maxY))    
    
print(paste(minX, minY, maxX, maxY), na.print = "NA")        
    
    # & Planned > 0 & Actual > 0

    # Produce the year zero chart
    plotY0 <- GenerateScatterChart(df %>% filter(Projection == 0), 
                                   xColName, yColName, xColName, yColName, lowerLimit, upperLimit,
                                   "Planned versus actual (current year)")
    print(plotY0)
    SaveChart(plotY0, width, height, paste(outputDirectory, outputSubDirectory, "Scatter_", outputPTDescription, "_y0.png", sep="") )
    
    # Produce the year one chart
    plotY1 <- GenerateScatterChart(df %>% filter(Projection == 1), 
                                   xColName, yColName, xColName, yColName, lowerLimit, upperLimit,
                                   "Planned versus actual (next year)")
    print(plotY1)
    SaveChart(plotY1, width, height, paste(outputDirectory, outputSubDirectory, "Scatter_", outputPTDescription, "_y1.png", sep="") )
    
    # Produce the year two chart
    plotY2 <- GenerateScatterChart(df %>% filter(Projection == 2), 
                                   xColName, yColName, xColName, yColName, lowerLimit, upperLimit,
                                   "Planned versus actual (in two years)")
    print(plotY2)
    SaveChart(plotY2, width, height, paste(outputDirectory, outputSubDirectory, "Scatter_", outputPTDescription, "_y2.png", sep="") )
    
    # Return the charts in a vector so they can be consumed if needed
    #returnValue <- c(plotY0, plotY1, plotY2)
  }

#-------------------------------------------------------------------------------------------------------------------------------------------------------
GenerateScatterChart <-
  function(df, xColName, yColName, xColLabel, yColLabel, lowerLimit, upperLimit, titleText) {
    #, gridSize) {
    
    ##### a ##### Setup the column names
    xColName = as.name(xColName)
    yColName = as.name(yColName)
    yColLabel = as.name(yColLabel)
    
    ##### c ##### Produce the caption text (with the average error margin)
    # And calculate the average margins of error for each projection year
    # And use that to produce the caption text.
    captionText <- "Average error margin:"

    # Calculate the absolute difference between the two
    df$DiffAbs <- abs(df$Planned - df$Actual)
    # And the error margin      
    errorMargin <- sum(df$DiffAbs) / sum(df$Actual, na.rm=TRUE) * 100
    # And then add this to the caption text
    captionText <- paste(captionText, " ", round(errorMargin, 0), "%", sep="") 

  

    ##### d ##### Do the visualisation
    # Note that we use get() to get the column object from the string name that is passed in
    # See https://stackoverflow.com/questions/34097133/passing-data-and-column-names-to-ggplot-via-another-function for more info
    plot <- ggplot(df, 
                   aes(x=!!xColName,
                       y=!!yColName
                    )
    ) + # Make a comma separated label for here ...
      geom_point(color="#0072BC") +
      geom_abline(intercept = 0, color="#999999") +
      theme_minimal(base_family=fontsForCharts, base_size=10) +
      
      scale_y_log10(
#      scale_y_continuous(
#        #name="Actual", 
        labels = comma,
        expand = expansion(mult = 0, add = c(0, 0.3)),
        limits=c(lowerLimit, upperLimit)
        #, 
        #breaks=10^(0:7)
#        ,
#        limits = c(minY, maxY)
        
#        , 
#        breaks = seq(minY, maxY, by = gridSize), 
#        expand = c(minY, minY),
#        limits = c(minY, maxY)
      ) +
      scale_x_log10(
        labels = comma, 
        expand = expansion(mult = 0, add = c(0, 0.3)),
        limits = c(lowerLimit, upperLimit)
      ) +
#      expand_limits(x = minX, y = minY) +
#      scale_x_continuous(
#        #name="Planned", 
#        labels = comma, 
#        breaks = seq(minX, maxX, by = gridSize),
#        expand = c(minX, minX),
#        limits = c(minX, maxX),
#        trans='log10'
#        ) +
      
      labs(
        title=paste(titleText), 
        y=yColLabel, 
        x=xColLabel, 
        caption=paste(captionText)
      ) +
      
      theme(
        axis.text.x = element_text(colour="#aaaaaa", family=fontsForCharts, size=12), 
        axis.line.x = element_line(colour="#dddddd"), 
        axis.text.y = element_text(colour="#aaaaaa",  family=fontsForCharts, size=12),
        axis.line.y = element_line(colour="#dddddd"), 
        panel.grid = element_line(colour="#dddddd"),
        plot.caption = element_text(colour="#aaaaaa",  family=fontsForCharts, size=12),
        plot.title = element_text(colour="#505050",  family=fontsForCharts, size=18),
        axis.title.x = element_text(colour="#505050", family=fontsForCharts, size=14),
        axis.title.y = element_text(colour="#505050", family=fontsForCharts, size=14)
      ) 
      
    
    returnValue <- plot
    
  }


#-------------------------------------------------------------------------------------------------------------------------------------------------------
GeneratePlanningFigureSummaryWrapper <-
  function(df, titleText, asyFilters, ptFilters, directoryName, subDirectoryName, width, height) {

    ##### a ##### Check for the last four arguments which are optional
    if (missing(directoryName)) {
      directoryName <- pfDirectoryName
    }
    if (missing(subDirectoryName)) {
      subDirectoryName <- pfChartsDirName
    }
    if (missing(width)) {
      width <- widthStandard
    }
    if (missing(height)) {
      height <- heightStandard
    }
    
    
    ##### b ##### Generate the fileName snippet - the default is the title text
    # But if only one country is specified, we use that, and we append the population type if only one is listed... 
    fileNameSnippet <- titleText
    
    if(length(asyFilters) == 1 & asyFilters[1] != "") {
      fileNameSnippet <- toString(asyFilters)  
    } else if (length(asyFilters) == 1 & asyFilters[1] == "") {
      fileNameSnippet <- "All"  
    }
    
    if(length(ptFilters) == 1 & ptFilters[1] != "") {
      fileNameSnippet <- paste(fileNameSnippet, "_", toString(ptFilters), sep="")
    
      # Append the pt to the title if there is only one  
      titleText <- paste(titleText, " - ", toString(ptFilters), sep="")
    } else if(length(ptFilters) == 1 & ptFilters[1] == "") {
      fileNameSnippet <- paste(fileNameSnippet, "_All", sep="")
    }
    
    
    
    ##### c ##### Generate the plot
    plot <- GeneratePlanningFigureSummary(df, titleText, asyFilters, ptFilters)
    
    # Show it
    print(plot)
    
    
    ##### d ##### Save it
    SaveChart(plot, width, height, paste(directoryName, subDirectoryName, "PF_Line_", fileNameSnippet, ".png", sep="") )
    
  }

#-------------------------------------------------------------------------------------------------------------------------------------------------------
GeneratePlanningFigureSummary <-
  function(df, titleText, asyFilters, ptFilters) {
  
    # ensure the DataYears are integers
    df$DataYear <- as.integer(df$DataYear)
    
    ##### 1 ##### Do the filtering by CoA and one or more population type filters
    if (length(asyFilters) > 0 & asyFilters[1] != "") {
    #if (asyFilter != "") {
      df <- df %>% filter(asylum %in% asyFilters)
    }
    if (length(ptFilters) > 0 & ptFilters[1] != "") {
      df <- df %>% filter(PT %in% ptFilters)  
    }
    
    
    ##### 2 ##### Group by to summarise the data so that we only display one actual and one planned line.
    #df <- df %>% group_by(asylum, PT, Year, DataType) %>%
    df <- df %>% group_by(Year, DataYear, DataType) %>%
      summarise(
        Planned = sum(Planned),
        Actual = sum(Actual)
        ##--##
        #,
        #Projected = sum(Projected)
      ) %>%
      ungroup()

#    View(df)
        
    # Now iterate through and if we find an ASR try to apply it to others and then remove it
    # Get the ASR records
    dfASR <- df %>% filter(DataType == "ASR")
    if( length( dfASR$DataYear ) > 0 ) {
      print(length(dfASR$DataYear))
      for( i in 1 : length(dfASR$DataYear)) {
        # See how many records match - it could be more than one as e.g. 2013 could be the start 
        # of one planning cycle and the middle of another one; 
        # if more than one, then apply the total of the ASR record for the selected DataYear to the others 
        # and then we will remove them
        
        print(dfASR$DataYear[i])
        print(length(df$DataYear[df$DataYear == dfASR$DataYear[i] ]))
        
        if(length(df$DataYear[df$DataYear == dfASR$DataYear[i] ]) > 1) {
          df$Actual[df$DataYear == dfASR$DataYear[i] ] <- 
            df$Actual[df$DataYear == dfASR$DataYear[i] ] + dfASR$Actual[i]
          df$DataType[df$DataYear == dfASR$DataYear[i] & df$DataType == "ASR" ] <- "ASRUsed"
        }
      }
      
      df <- df %>% filter (DataType != "ASRUsed")
    }
    
    
    
    ##### 3 ##### And add the projection, data year, and the labels
    # Calculate the data year
    #df$DataYear <- as.integer(str_extract(df$DataType, "[0-9]{4}"))
    df$Projection <- df$DataYear - df$Year
    
    yColLabel <- "Population"
    
    y1ColName <- "Actual"
    y1ColLabel <- "Actual"
    y2ColName <- "Planned"
    y2ColLabel <- "Planned"
    xColName <- "DataYear"
    xColLabel <- "Year"
    
    titleText <- paste("Planned and actual populations -", titleText)
    
    planningColour <- "#EF4A60"
    asrColour <- "#0072BC"
    ##--##
    #projectedColour <- "#00B398"
    
    ##### 4 ##### Find the negative or zero steps in the data and ignore them colour wise
    df$ProjectionChange <- c(diff(df$Projection, 1), NA)
    df$YearChange <- c(diff(df$DataYear, 1), NA)
    
    # Append the Planning colour to the cube
    df$ColourP <- planningColour
    # Set the links between planning cycles to be NA
    df$ColourP[df$ProjectionChange <= 0] <- NA 
    
    # And do the same for the actual data
    df$ColourA <- asrColour
    df$ColourA[df$ProjectionChange <= 0 & df$YearChange <= 0 ] <- NA 
    
    ##--##
    #df$ColourPr <- projectedColour
    #df$ColourPr[df$ProjectionChange <= 0 & df$YearChange <= 0 ] <- NA 
    

    #View(df)
    
    #View(df)
    
    
    # And calculate the average margins of error for each projection year
    # And use that to produce the caption text.
    captionText <- "Average error margins:"
    for( i in 0:2) {
      dfTemp <- df %>% filter(Projection == i)
      
      #errorMargin <- RMSE(dfTemp$Planned, dfTemp$Actual)  / sum(dfTemp$Actual) * 100
      #errorMargin <- RMSE(dfTemp$Planned, dfTemp$Actual)
      #errorMargin <- RMSE(dfTemp$Planned, dfTemp$Actual) / mean(dfTemp$Actual) * 100
      
      # Calculate the absolute difference between the two
      dfTemp$DiffAbs <- abs(dfTemp$Planned - dfTemp$Actual)
      
      errorMargin <- sum(dfTemp$DiffAbs) / sum(dfTemp$Actual, na.rm=TRUE) * 100
      
      #print(paste(sum(dfTemp$Planned, na.rm=TRUE), sum(dfTemp$Actual, na.rm=TRUE)))
      
      termSeparator <- ifelse( i == 0, "", "; ")
      captionText <- paste(captionText, termSeparator, "Y", i, " ", round(errorMargin, 0), "%", sep="") 
    }
    
    
    
    ##### 5 ##### Calculate the max and min
    minX <- min(df[[ xColName ]], na.rm = TRUE)
    maxX <- max(df[[ xColName ]], na.rm = TRUE)
    minY <- min( c(min(df[[ y1ColName ]]), min(df[[ y2ColName ]])))
    maxY <- max( c(max(df[[ y1ColName ]]), max(df[[ y2ColName ]])))
    
  print(maxY)  
    
    if(minY > 0) {
      minY = 0
    }  
    
    # Round to the nearest largest round number
    maxYTemp <- round(maxY, -1 * (floor(log10(abs(maxY))) + 1))  
    if(round(maxYTemp, 0) == 0) {
      print("MaxY was zero, using alternative formula")
      
      level <- floor(log10(abs(maxY)))
      maxY <- round(maxY, -1 * level)  
      maxY <- maxY + as.integer(toString(paste("1e+0",level, sep="")))
      
    } else {
      maxY <- maxYTemp
    }
    
    print(paste(minX, maxX))
    print(paste(comma(minY),comma(maxY), maxY))
    
    gridSizeX <- 1 # i.e. every year...
    gridSizeY <- maxX / 5
    
    
    ##### 6 ##### And draw the plot
    plot <- ggplot(df, aes(x = DataYear)) +
      # add two lines as paths so that the points are processed in the order supplied
      geom_path(aes(y = Actual), colour=df$ColourA) +
      geom_path(aes(y = Planned), colour=df$ColourP) +
      
      ##--##
      #geom_path(aes(y = Projected), colour=df$ColourPr) +
      
      # Set a minimal theme
      theme_minimal(base_family=fontsForCharts, base_size=10) +
      
      # Specify the limits and label format of the scale y
      scale_y_continuous(
        #name="Actual", 
        labels = comma, 
        #breaks = seq(minY, maxY, by = gridSizeY), 
        expand = c(minY, minY),
        limits = c(minY, maxY)) +
      
      # Specify the limits and label format of the scale x
      scale_x_continuous(
        #name="Planned", 
        #labels = comma, 
        breaks = seq(minX, maxX, by = gridSizeX),
        expand = c(0, 0.5),
        limits = c(minX, maxX)
       ) +
      
      # Add the labels
      labs(
        title=paste(titleText), 
        y=yColLabel, 
        x=xColLabel, 
        caption=paste(captionText)
      ) +
      
      # Detailed colours for specific chart elements
      theme(
        axis.text.x = element_text(colour="#aaaaaa", family=fontsForCharts, size=12), 
        axis.line.x = element_line(colour="#eeeeee"), 
        axis.text.y = element_text(colour="#aaaaaa",  family=fontsForCharts, size=12),
        axis.line.y = element_line(colour="#eeeeee"), 
        panel.grid = element_line(colour="#eeeeee"),
        plot.caption = element_text(colour="#aaaaaa",  family=fontsForCharts, size=12),
        plot.title = element_text(colour="#505050",  family=fontsForCharts, size=16),
        axis.title.x = element_text(colour="#505050", family=fontsForCharts, size=14),
        axis.title.y = element_text(colour="#505050", family=fontsForCharts, size=14)
      ) 


    returnValue <- plot

}



#-------------------------------------------------------------------------------------------------------------------------------------------------------
# Classifies the planning figure data into 9 types based on whether there was an increase, decrease or the figures were equal
# between Year 0 and Year 1, and then Year 1 and Year 2
# There are 9 permuations in total:
#   Decrease-Decrease, Decrease-Equal, Decrease-Increase, 
#   Equal-Decrease, Equal-Equal, Equal-Increase,
#   Increase-Decrease, Increase-Equal, Increase-Increase
# Requires the data to be structured as in the dataPFASR data frame in PlanningFiguresVersusASR
SummarisePlanningFigureShapes <-
  function(df, ptList, colToSummarise) {
    
    ##### a ##### Setup the column name
    colToSummarise = as.name(colToSummarise)
    
    ##### b ##### Filter the data
    df <- df %>% filter(PT %in% ptList & DataType != "ASR")
    
    ##### c ##### Get a subset of the relevant columns
    df <- df %>% select(asylum, PT, Year, Projection, !!colToSummarise)
    
    ##### d ##### Spread the data, so that each row is a planning set of Y0, Y1 and Y2 for a combination of CoA, Year and PT
    # Prepend Y to the Projection, so that when they become column names, they are easier to use...
    df$Projection <- paste("Y", df$Projection, sep="")
    # Spread it
    dfSummary <- spread(df, key="Projection", value=!!colToSummarise )
    # Zero out NA's
    dfSummary[is.na(dfSummary)] <- 0
    
    ##### e ##### Then apply the logic for generating the 9 permuations, including the Jam Tomorrow Curve
    dfSummary$Shape <- ""
    
    dfSummary$Shape <- ifelse((dfSummary$Y1 < dfSummary$Y0 & dfSummary$Y2 < dfSummary$Y1), "1. Decrease-Decrease", dfSummary$Shape)
    dfSummary$Shape <- ifelse((dfSummary$Y1 < dfSummary$Y0 & dfSummary$Y2 == dfSummary$Y1), "2. Decrease-Equal", dfSummary$Shape)
    dfSummary$Shape <- ifelse((dfSummary$Y1 < dfSummary$Y0 & dfSummary$Y2 > dfSummary$Y1), "3. Decrease-Increase", dfSummary$Shape)
    
    dfSummary$Shape <- ifelse((dfSummary$Y1 == dfSummary$Y0 & dfSummary$Y2 < dfSummary$Y1), "4. Equal-Decrease", dfSummary$Shape)
    dfSummary$Shape <- ifelse((dfSummary$Y1 == dfSummary$Y0 & dfSummary$Y2 == dfSummary$Y1), "5. Equal-Equal", dfSummary$Shape)
    dfSummary$Shape <- ifelse((dfSummary$Y1 == dfSummary$Y0 & dfSummary$Y2 > dfSummary$Y1), "6. Equal-Increase", dfSummary$Shape)
    
    dfSummary$Shape <- ifelse((dfSummary$Y1 > dfSummary$Y0 & dfSummary$Y2 < dfSummary$Y1), "7. Increase-Decrease", dfSummary$Shape)
    dfSummary$Shape <- ifelse((dfSummary$Y1 > dfSummary$Y0 & dfSummary$Y2 == dfSummary$Y1), "8. Increase-Equal", dfSummary$Shape)
    dfSummary$Shape <- ifelse((dfSummary$Y1 > dfSummary$Y0 & dfSummary$Y2 > dfSummary$Y1), "9. Increase-Increase", dfSummary$Shape)
    
    ##### f ##### Create a key so we can follow the data (with the CoA, Year and PopulationType)
    dfSummary$Key <- paste(dfSummary$asylum, "_", dfSummary$Year, "_", dfSummary$PT, sep="")
    
    
    returnValue <- dfSummary
    
  }


#-------------------------------------------------------------------------------------------------------------------------------------------------------
# Generates Chart with 9 facets with the shapes of the normalised data over the three year periods
# Requires the dataFrame as structured by DNFunctions.SummarisePlanningFigureShapes
GeneratePlanningFigureShapeFacets <-
  function(dfSummary) {
    
    ##### a ##### Try normalising to visualise
    # Use the parallel maxima function to get the max by row of Y0, Y1 and Y2 (it is totally sweet as)
    # https://statisticsglobe.com/pmax-pmin-r-function-example/
    dfSummary$Max <- pmax(dfSummary$Y0, dfSummary$Y1, dfSummary$Y2)
    
    dfSummary$Y0 <- dfSummary$Y0 / dfSummary$Max
    dfSummary$Y1 <- dfSummary$Y1 / dfSummary$Max
    dfSummary$Y2 <- dfSummary$Y2 / dfSummary$Max
    
    ##### b ##### lets add the observation count to the shapes
    dfSummary <- dfSummary %>% 
      group_by(Shape) %>%
      mutate(ShapeCount = n()) %>%
      ungroup() %>%
      mutate(ShapeWithCount = paste0(Shape, " (n=", ShapeCount, ")"))
    
    
    ##### c ##### Gather the data to make it easier to visualise
    # Remove the unnecessary columns by reselecting the data
    dfSummary <- dfSummary %>% select(Key, Y0, Y1, Y2, ShapeWithCount)
    
    # Gather the data so we can plot it...
    dfSummary <- gather(dfSummary, key="Timeline", value="NormalisedCount", -Key, -ShapeWithCount) %>% 
      arrange(Key, Timeline)
    

    ##### d ##### Generate the plot
    thePlot <- ggplot(dfSummary, aes(x = Timeline, y = NormalisedCount, color = ShapeWithCount, group = Key)) + 
      geom_line() +
      # The custom colours - note that colour and fill are different aesthetics - so we need to use
      # scale_fill_brewer or scale_colour_brewer respectively
      #scale_fill_brewer(palette="Dark2")
      scale_colour_manual(values=c(
        "#00B398", "#00B398", "#00B398", 
        "#0072BC", "#0072BC", "#0072BC", 
        "#EF4A60", "#EF4A60", "#EF4A60")) +      

      theme_minimal(base_family=fontsForCharts) +
      guides(color = FALSE) +

      
      facet_wrap(vars(ShapeWithCount), ncol=3)    +
      
      # Detailed fonts for specific chart elements
      theme(
        axis.text.x = element_text(colour="#aaaaaa", family=fontsForCharts, size=10), 
        axis.text.y = element_text(colour="#aaaaaa", family=fontsForCharts, size=10),
        panel.grid = element_line(colour="#eeeeee"),
        #plot.title = element_text(family=fontsForCharts, size=18),
        axis.title.x = element_text(colour="#505050", family=fontsForCharts, size=12),
        axis.title.y = element_text(colour="#505050", family=fontsForCharts, size=12),
        # For the facets
        strip.text.x = element_text(color="#505050", family=fontsForCharts, size=10)

      ) 
      
    
    # Return the plot
    returnValue <- thePlot
}


##arrivalsToSpain[, paste("Y",maxYear, "",sep="")]
#as.list(arrivalsToSpain[, "Y2018"])
#
#arrivalsToSpain[["2018"]]
#arrivalsToSpain[[as.character(maxYear)]]
#arrivalsToSpain[[paste("`", maxYear, "`", sep="")]]
#
#arrivalsToSpain$`Y2018`
#
#
## ensure it is sorted by the latest year - 
##see https://stackoverflow.com/questions/18222286/dynamically-select-data-frame-columns-using-and-a-vector-of-column-names for the correct way to address the cell
#arrivalsToSpain <- arrivalsToSpain %>% arrange(desc(arrivalsToSpain[[as.character(maxYear)]]))
#arrivalsToSpain <- arrivalsToSpain %>% arrange(desc(Y2018))



