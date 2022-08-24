library(glue)
library(boot)
library(ggplot2)
library(reshape2)
library(stringr)
library(dplyr)
library(data.table)
library(pracma)

options(width=320);

#'Generate the mean value of a list
#'
#'@param x the original dataset
#'@param d the indice or list of indices to subsample 'x'
#'
#'@return mean(x[d])
meanFunc = function(x, d)
{
    return(mean(x[d]))
}

#'Generate the mean value of a list using the log anti log method (geometrical mean)
#'
#'@param x the original dataset
#'@param d the indice or list of indices to subsample 'x'c
#'
#'@return exp(mean(log(x[d])))
geomMeanFunc = function(x, d)
{
    return(exp(mean(log(x[d]))))
}

#'Compute the standard deviation of a log-transformed log-normal distribution (like TCT)
#'See https://en.wikipedia.org/wiki/Standard_deviation
computeLogNormalSD = function(x)
{
    mu       = mean(x)
    sigmaSq  = var(x)
    E        = exp(2*mu + sigmaSq)
    variance = E*(exp(sigmaSq)-1)
    
    return(sqrt(variance))
}

plotStackedBarchart = function(d, maxAxis=NA, minAxis=NA, legendName="",
                               xLabel=value, yLabel=name, fillLabel=id, facetLabel=grid)
{
    xLabel     = deparse(substitute(xLabel))
    yLabel     = deparse(substitute(yLabel))
    fillLabel  = deparse(substitute(fillLabel))
    facetLabel = deparse(substitute(facetLabel))

    d[[fillLabel]] = factor(d[[fillLabel]])

    g = ggplot(d, aes(x = get(xLabel), y = get(yLabel), fill = factor(get(fillLabel), levels=unique(.data[[fillLabel]]), ordered=TRUE))) + 
        geom_bar(stat = 'identity', position = 'stack') + facet_grid(~ factor(get(facetLabel), levels=unique(.data[[facetLabel]]), ordered=TRUE))

    if(is.na(maxAxis) && is.na(minAxis))
        g = g + expand_limits(y = 0)
    else
    {
        if(is.na(maxAxis))
            maxAxis = 0
        if(is.na(minAxis))
            minAxis = 0
        g = g + expand_limits(y = c(minAxis, maxAxis))
    }

    g = g + labs(x = "", y = "", col=legendName, fill=legendName) + 
        scale_y_continuous(labels = scales::percent_format()) +
        scale_fill_brewer(palette="Dark2") +
        theme(plot.background = element_rect(fill = 'transparent', colour = 'transparent'),
              panel.background = element_rect(fill = 'transparent', colour = 'transparent'),
              plot.margin=grid::unit(c(0,0,0,0), "mm"),
              axis.title = element_text(size = rel(2.0), colour = "black"),
              axis.text  = element_text(size = rel(2.0), colour = "black"),
              panel.grid.major   = element_line(colour = "#DDDDDD"),
              panel.grid.major.y = element_blank(), 
              panel.grid.minor.y = element_blank(),
              strip.text.x = element_text(size=22, colour = "black"),
              aspect.ratio = 3/4,
              legend.title = element_text(size=24, face="bold"), 
              legend.text  = element_text(size=24),
              legend.position = c(0.5, -0.4),
              legend.direction = "horizontal")
    
    return(g)
}

#'Plot a barchart
#'
#'@param d the dataframe to plat as a bar chart. Require a column "name", "x" and "y"
#'@param maxAxis the maximum axis value
#'
#'@return the plot object
plotBarChart = function(d, maxAxis=NA, xLabel=value, yLabel=name, fillLabel=id, legendName="", stack=FALSE)
{
    xLabel     = deparse(substitute(xLabel))
    yLabel     = deparse(substitute(yLabel))
    fillLabel  = deparse(substitute(fillLabel))
    d[[fillLabel]] = factor(d[[fillLabel]])

    print(d)
    g = ggplot(d, aes(x = get(xLabel), y = get(yLabel), fill = factor(get(fillLabel), levels=unique(.data[[fillLabel]]), ordered=TRUE)))
   
    if(stack == TRUE)
        g = g + geom_bar(stat="identity", position='stack', width=0.75)
    else 
        g = g + geom_bar(stat="identity", position=position_dodge(0.5), width=0.25)

    if(is.na(maxAxis))
        g = g + expand_limits(y = 0)
    else
        g = g + expand_limits(y = c(0, maxAxis))


    g = g + labs(x = "", y = "", col=legendName, fill=legendName) + 
        scale_y_continuous(labels = scales::percent_format()) +
        scale_x_discrete(name="", expand=c(0,0)) +
        scale_fill_brewer(palette="Dark2") +
        theme(plot.background = element_rect(fill = 'transparent', colour = 'transparent'),
              panel.background = element_rect(fill = 'transparent', colour = 'transparent'),
              plot.margin=grid::unit(c(0,0,0,0), "mm"),
              axis.title = element_text(size = rel(2.0), colour = "black"),
              axis.text  = element_text(size = rel(2.0), colour = "black"),
              panel.grid.major   = element_line(colour = "#DDDDDD"),
              panel.grid.major.y = element_blank(), 
              panel.grid.minor.y = element_blank(),
              aspect.ratio = 1/3,
              legend.title = element_text(size=24, face="bold"), 
              legend.text  = element_text(size=24),
              legend.position = c(0.5, -0.3),
              legend.direction = "horizontal")
    
    #g = g + scale_fill_brewer(palette="Dark2") +
    #    scale_y_continuous(labels = scales::percent_format()) +
    #    theme(plot.background = element_rect(fill = 'transparent', colour = 'transparent'),
    #          panel.background = element_rect(fill = 'transparent', colour = 'transparent'),
    #          plot.margin=grid::unit(c(0,0,0,0), "mm"),
    #          axis.title = element_text(size = rel(1.2), colour = "black"),
    #          axis.text  = element_text(size = rel(1.2), colour = "black"),
    #          panel.grid.major   = element_line(colour = "#AAAAAA"),
    #          panel.grid.minor   = element_blank(),
    #          panel.grid.major.x = element_blank(),
    #          aspect.ratio = length(d$x)/30,
    #          legend.title = element_text(size=14, face="bold"), 
    #          legend.text  = element_text(size=12))
    return(g)
}


#'Plot a list of bootstrap data
#'
#'@param d the list of data.frame object with each containing: a column mean, lower, and upper. A name column permits to name the bar chart (each name is a metric). An id column allows to gather objects with different ids under the same metric. 
#'         You can change the column names with the corresponding label
#'@param maxAxis the maximum axis value
#'@param isBarChart is the graph a bar chart? (default: FALSE)
#'
#'@return the plot object
plotListBootstrap = function(d, maxAxis=NA, minAxis=NA, isBarChart=FALSE, legendName=" ",
                             meanLabel=mean, lowerLabel=lower, upperLabel=upper,
                             nameLabel=name, idLabel=id)
{
    lowerLabel = deparse(substitute(lowerLabel))
    upperLabel = deparse(substitute(upperLabel))
    meanLabel  = deparse(substitute(meanLabel))

    tr = data.frame(d)
    tr = tr %>% group_by(!!ensym(idLabel)) %>% mutate(id = seq.int(1, nrow(cur_data()), 1))
#    tr = tr[with(tr,order(-id)),] ## Sorting
    tr$id = factor(tr$id, ordered=TRUE)
    g = ggplot(tr, aes(fill=factor(.data[[substitute(idLabel)]], levels=unique(.data[[substitute(idLabel)]]), ordered=TRUE), x=id, y=get(meanLabel), colour=factor(.data[[substitute(idLabel)]], unique(.data[[substitute(idLabel)]], ordered=TRUE))))
    
    if(isBarChart)
        g = g + geom_bar(width=0.5, stat="identity", position=position_dodge())
    if(is.na(maxAxis) && is.na(minAxis))
        g = g + expand_limits(y = 0)
    else
    {
        if(is.na(maxAxis))
            maxAxis = 0
        if(is.na(minAxis))
            minAxis = 0
        g = g + expand_limits(y = c(minAxis, maxAxis))
    }
    
    g = g + scale_x_discrete(name="", labels=unique(tr[[substitute(nameLabel)]])) +
        scale_y_continuous(name="") + 
        geom_errorbar(aes(ymin=get(lowerLabel), ymax=get(upperLabel)), width = 0, size = 0.7, position=position_dodge(width=0.66), stat="identity") +
        labs(x = "", y = "", col=legendName, fill=legendName) + 
        coord_flip() +
        scale_color_brewer(palette="Dark2") +
        theme(plot.background = element_rect(fill = 'transparent', colour = 'transparent'),
              panel.background = element_rect(fill = 'transparent', colour = 'transparent'),
              plot.margin=grid::unit(c(0,0,0,0), "mm"),
              axis.title = element_text(size = rel(1.4), colour = "black"),
              axis.text  = element_text(size = rel(1.2), colour = "black"),
              panel.grid.major   = element_line(colour = "#DDDDDD"),
              panel.grid.major.y = element_blank(), 
              panel.grid.minor.y = element_blank(),
              legend.title = element_text(size=16, face="bold"), 
              legend.text  = element_text(size=14),
              aspect.ratio = length(unique(tr$id))/25) +
        geom_point(size=1.7, position=position_dodge(width=0.66))         # dots

    return(g)
}

#'Plot the bootstrap data
#'
#'@param d the data.frame object containing: a column meanLabel, lowerLabel and upperLabel to define
#'@param maxAxis the maximum axis value
#'@param isBarChart is the graph a bar chart? (default: FALSE)
#'@param brewerX adapt the color of the label
#'@param useWithoutOutliers should we take into account the withoutOutliers data? If yes, consider setting meanWOLabel", lowerWOLabel, and upperWOLabel
#'@return the plot object
plotBootstrap = function(d, maxAxis=NA, isBarChart=FALSE, brewerX=FALSE, useWithoutOutliers=FALSE, 
                         meanLabel=mean, lowerLabel=lower, upperLabel=upper, 
                         meanWOLabel=meanWO, lowerWOLabel=lowerWO, upperWOLabel=upperWO)
{
    lowerLabel = deparse(substitute(lowerLabel))
    upperLabel = deparse(substitute(upperLabel))
    meanLabel  = deparse(substitute(meanLabel))

    lowerWOLabel = deparse(substitute(lowerLabel))
    upperWOLabel = deparse(substitute(upperLabel))
    meanWOLabel  = deparse(substitute(meanLabel))

    tr = data.frame(d)
    tr$id = factor(seq.int(1, nrow(tr), 1), ordered=TRUE)
    
    g = NULL
    
    if(useWithoutOutliers)
    {
        if(brewerX)
            g = ggplot(tr, aes(x=id, y=get(meanLabelWO), colour=id))
        else
            g = ggplot(tr, aes(x=id, y=get(meanLabelWO)))
    }
    else
    {
        if(brewerX)
            g = ggplot(tr, aes(x=id, y=get(meanLabel), colour=id))
        else
            g = ggplot(tr, aes(x=id, y=get(meanLabel)))
    }

    g = g + scale_x_discrete(name="", breaks=tr$id, labels=rownames(tr))

    
    if(isBarChart)
        g = g + geom_bar(stat="identity")
        
    
    if(useWithoutOutliers)
    {
        g = g + geom_errorbar(aes(ymin=get(lowerLabelWO), ymax=get(upperLabelWO)), width = 0, size = 0.5) +
                geom_errorbar(aes(ymin=get(lowerLabel),   ymax=get(upperLabel)), position = position_nudge(x = -0.35), width=0, size=0.5);
    }
    else
    {
        g = g + geom_errorbar(aes(ymin=get(lowerLabel), ymax=get(upperLabel)), width = 0, size = 0.5)
    }
    
    
    g = g + geom_point(size=1.5, colour="black", fill="white")         # dots
    if(useWithoutOutliers)
        g = g + geom_point(aes(y=get(meanLabel)), colour="black", size=1.5, position = position_nudge(x = -0.35));
    
    g = g + labs(x = "", y = "") + coord_flip();
    if(is.na(maxAxis))
        g = g + expand_limits(y = 0)
    else
        g = g + expand_limits(y = c(0, maxAxis))

    if(brewerX)
        g = g + scale_colour_brewer(palette="Dark2") 
    else
        g = g + scale_fill_brewer(palette="Dark2")

    g = g + theme(aspect.ratio = nrow(d)/70,
                  plot.margin=grid::unit(c(0,0,0,0), "mm"),
                  plot.background = element_rect(fill = 'transparent', colour = 'transparent'),
                  panel.background = element_rect(fill = 'transparent', colour = 'transparent'),
                  axis.title = element_text(size = rel(1.0), colour = "black"),
                  axis.text  = element_text(size = rel(1.0), colour = "black"),
                  legend.title = element_text(size=14, face="bold"), 
                  legend.text  = element_text(size=12),
                  panel.grid.major   = element_line(colour = "#DDDDDD"),
                  panel.grid.major.y = element_blank(), 
                  panel.grid.minor.y = element_blank())
    
    return(g)
}


#'Generate the 95% CI generated via bootstrapping
#
#'@param data the data to use during bootstrapping
#'@param statistic the statistical function to use
#'
#'@return an array containing (defaultStats, lowerBound, upperBound)
bootstrapCI = function(data, statistic)
{
    b   = boot(data, statistic=statistic, R=5000)
    cis = boot.ci(b, conf=0.95, type="bca")
    
    return(c(statistic(data), cis$bca[4], cis$bca[5], sd(data)))
}

#'Compute the overall bootstrap of x containing all the data to bootstrap from
#'All the data to bootstrap from. Column names are based on groupedData from parseLog
#
#'@param x the data to analyse
#
#'Return a one row data frame containing the acc, tct, and nbOp bootstrap results.
#
#'Column Format: "mcc", "mccMin", "mccMax", "f1", f1Min", f1Max","tct", "tctMin", "tctMax", "nbOp", "nbOpMin", and "nbOpMax"
computeOverallBootstrap = function(x)
{
    colNames = c("mcc" ,       "mccMin" ,       "mccMax", "mccSD",
                 "f1"  ,       "f1Min"  ,       "f1Max",  "f1SD",
                 "tct" ,       "tctMin" ,       "tctMax", "tctSD",
                 "nbOp",       "nbOpMin",       "nbOpMax", "nbOpSD",
                 "constraint", "constraintMin", "constraintMax", "constraintSD")

    #Perform the analysis
    mccBootstrap        = bootstrapCI(x$meanMcc, meanFunc)
    f1Bootstrap         = bootstrapCI(x$meanF1, meanFunc)
    tctBootstrap        = t.test(x$meanLogTCT, conf.level = 0.95)
    tctBootstrap        = c(exp(mean(x$meanLogTCT)), exp(tctBootstrap$conf.int[1]), exp(tctBootstrap$conf.int[2]), computeLogNormalSD(x$meanLogTCT))
    
    if(length(unique(x$meanInter + x$meanUnion + x$meanMinus)) == 1)
        opBootstrap     = c(mean(x$meanInter + x$meanUnion + x$meanMinus), mean(x$meanInter + x$meanUnion + x$meanMinus), mean(x$meanInter + x$meanUnion + x$meanMinus), 0)
    else
        opBootstrap     = bootstrapCI(x$meanInter + x$meanUnion + x$meanMinus, meanFunc)
    if(length(unique(x$meanConstraintRatio)) == 1)
        constraintBootstrap = c(mean(x$meanConstraintRatio), mean(x$meanConstraintRatio), mean(x$meanConstraintRatio), 0)
    else
        constraintBootstrap = bootstrapCI(x$meanConstraintRatio, meanFunc)
    
    #Transform the analysis as data frame
    res = data.frame(cbind(t(mccBootstrap), t(f1Bootstrap), t(tctBootstrap), t(opBootstrap), t(constraintBootstrap)))
    colnames(res) = colNames

    return(res)
}

#'Compute the PairWise bootstrap CI data
#
#'@param x the input data as defined in parseLog
#
#'@return a one row data frame containing mcc, f1, and tct data for each T0T1
#'Column format follow <name><tech> <name>Min<tech> <name>Max<tech> where <name> is mcc, f1, or tct, and tech is T0T1
computePWBootstrap = function(x)
{
    #The input column name (without TXTY)
    subDataNames = c("pairedMcc", "pairedF1", "pairedLogTCT")

    #Result metadata
    subResNames  = c("mcc" , "mccMin" , "mccMax", "mccSD",
                    "f1"  , "f1Min"  , "f1Max",   "f1SD",
                    "tct" , "tctMin" , "tctMax",  "tctSD",
                    "constraint", "constraintMin", "constraintMax", "constraintSD",
                    "totalConstraint", "totalConstraintMin", "totalConstraintMax", "totalConstraintSD")

    colNames     = NULL
    res          = NULL

    #Compute the bootstrap for each comparison
    for(tech in c("T0T1"))
    {
        colNames     = cbind(colNames, paste(subResNames, tech, sep=""))
        mccBootstrap = bootstrapCI(x[[paste("pairedMcc",  tech, sep="")]], meanFunc)
        f1Bootstrap  = bootstrapCI(x[[paste("pairedF1",   tech, sep="")]], geomMeanFunc)
        tctBootstrap = t.test(x[[paste("pairedLogTCT",    tech, sep="")]], conf.level = 0.95)
        tctBootstrap = c(exp(mean(x[[paste("pairedLogTCT", tech, sep="")]])), exp(tctBootstrap$conf.int[[1]]), exp(tctBootstrap$conf.int[[2]]), computeLogNormalSD(x[[paste("pairedLogTCT", tech, sep="")]]))

        pairedConstraint = paste("pairedConstraint", tech, sep="")
        pairedTotalConstraint = paste("pairedTotalConstraint", tech, sep="")
        if(length(unique(x[[pairedConstraint]])) == 1)
            constraintBootstrap = c(mean(x[[pairedConstraint]]), mean(x[[pairedConstraint]]), mean(x[[pairedConstraint]]), 0)
        else
            constraintBootstrap = bootstrapCI(x[[pairedConstraint]], meanFunc)
        if(length(unique(x[[pairedTotalConstraint]])) == 1)
            totalConstraintBootstrap = c(mean(x[[pairedTotalConstraint]]), mean(x[[pairedTotalConstraint]]), mean(x[[pairedTotalConstraint]]), 0)
        else
            totalConstraintBootstrap = bootstrapCI(x[[pairedTotalConstraint]], meanFunc)
        
        res = cbind(res, t(mccBootstrap), t(f1Bootstrap), t(tctBootstrap), t(constraintBootstrap), t(totalConstraintBootstrap))
    }

    res = data.frame(res)
    colnames(res) = colNames
    return (res)
}

computeOverallBootstrapTimePerTechniqueOrder = function(x)
{
    colNames = c("techniqueID", "tct", "tctMin", "tctMax", "tctSD")

    tctFirstBootstrap = t.test(x$First, conf.level = 0.95)
    tctFirstBootstrap = c(exp(mean(x$First)), exp(tctFirstBootstrap$conf.int[1]), exp(tctFirstBootstrap$conf.int[2]), computeLogNormalSD(x$First))
    
    tctSecondBootstrap = t.test(x$Second, conf.level = 0.95)
    tctSecondBootstrap = c(exp(mean(x$Second)), exp(tctSecondBootstrap$conf.int[1]), exp(tctSecondBootstrap$conf.int[2]), computeLogNormalSD(x$Second))
    
    res = data.frame(cbind(c("First", "Second"), t(cbind(tctFirstBootstrap, tctSecondBootstrap))))
    colnames(res) = colNames
    return (res)
}


computePWBootstrapTimePerTechniqueOrder = function(x)
{
    subResNames  = c("tct" , "tctMin" , "tctMax", "tctSD")
    colNames = NULL
    res      = NULL
    
    #Compute the bootstrap for each comparison
    for(tech in c("T0T1"))
    {
        colNames     = cbind(colNames, paste(subResNames, tech, sep=""))
        
        tctBootstrap = t.test(x[[tech]], conf.level = 0.95)
        tctBootstrap = c(exp(mean(x[[tech]])), exp(tctBootstrap$conf.int[[1]]), exp(tctBootstrap$conf.int[[2]]), computeLogNormalSD(x[[tech]]))
        
        res = cbind(res, t(tctBootstrap))
    }
    res = data.frame(res)
    colnames(res) = colNames
    return (res)
}

computeTimePerTechnique = function(x, key)
{
    res = c(mean(log(x$tct[x$techniqueID==(key$pID%%2)]    *1e-6)), 
            mean(log(x$tct[x$techniqueID==((key$pID+1)%%2)]*1e-6)))
    
    res = data.frame(t(res))
    colnames(res) = c("First", "Second")
    return (res);
}

computeLogTCT = function(groupData, tct)
{
    #if(groupData$pID%%2 == groupData$techniqueID)
    #    return(log(min(tct)*1e-6))
    return (mean(log(tct*1e-6)))
}

#'Function parsing the log data of the users study
#'Input must be in data/log
#'Results are saved in ROutput
parseLog = function()
{
    outputDir = "ROutput"
    dir       = "data/log"
    data      = NULL

    #Read the csv files and concatenate them.
    #They should have the same header
    for(fileName in list.files(path=dir, pattern="*.csv"))
    {
        #Parse
        csvData = read.csv(glue("{dir}/{fileName}"), sep=',', header=TRUE)

        #Concatenate
        if(is.null(data))
            data = csvData
        else if(names(data) == names(csvData)) #This should be true
            data = rbind(data, csvData)
        else #If csv files are not consistent: exit the script
        {
            print("Error: csv files are not consistent. Exiting...")
            stop()
        }
    }

    #Group all the data by our foreign key (pID, techniqueID, datasetID)
    print("aggregate data...")
    groupedData = data %>% group_by(pID, techniqueID, datasetID) %>% 
                           summarise(meanUnion           = mean(nbUnion),
                                     meanInter           = mean(nbInter),
                                     meanMinus           = mean(nbMinus),
                                     meanNbReset         = mean(nbReset),
                                     meanNbCancel        = mean(nbCancel),
                                     meanMcc             = mean(mcc),
                                     meanF1              = mean(f1),
                                     meanConstraintRatio = mean(nbConstraint / (nbConstraint + nbUnconstraint)),
                                     meanTotalConstraintRatio = mean(nbTotalConstraint / (nbTotalConstraint + nbTotalUnconstraint)),
                                     meanLogTCT          = computeLogTCT(cur_group(), tct)) 

    timePerTechniqueOrder = data %>% group_by(pID, datasetID) %>%
                                     summarise(computeTimePerTechnique(cur_data(), cur_group()))
    
    overallTimePerTechniqueOrder = data %>% group_by(pID) %>%
                                            summarise(computeTimePerTechnique(cur_data(), cur_group()))

    #Perform the pair-wise comparison for given datasets
    print("Perform pair-wise comparison...")
    pairedWiseData = groupedData %>% group_by(pID, datasetID) %>% 
                                     summarise(#MCC Accuracy
                                               pairedMccT0T1    = rep(filter(cur_data(), techniqueID == 0)$meanMcc - filter(cur_data(), techniqueID == 1)$meanMcc, n()),

                                               #F1 Accuracy
                                               pairedF1T0T1     = rep(filter(cur_data(), techniqueID == 0)$meanF1  / filter(cur_data(), techniqueID == 1)$meanF1, n()),
                                 
                                               #Time (in log formarep(t)
                                               pairedLogTCTT0T1 = rep(filter(cur_data(), techniqueID == 0)$meanLogTCT - filter(cur_data(), techniqueID == 1)$meanLogTCT, n()),

                                               #Constraint
                                               pairedConstraintT0T1 = rep(filter(cur_data(), techniqueID == 0)$meanConstraintRatio  - filter(cur_data(), techniqueID == 1)$meanConstraintRatio, n()),
                                               
                                               #Constraint
                                               pairedTotalConstraintT0T1 = rep(filter(cur_data(), techniqueID == 0)$meanTotalConstraintRatio  - filter(cur_data(), techniqueID == 1)$meanTotalConstraintRatio, n())) %>%
                                    distinct(pID, datasetID, .keep_all=TRUE)

    #Add paired-wise comparisons for learning effect
    pairedWiseTimePerTechniqueOrder        = timePerTechniqueOrder %>% group_by(pID, datasetID) %>%
                                                                       summarise(T0T1 = First - Second)
                                                        
    pairedWiseOverallTimePerTechniqueOrder = overallTimePerTechniqueOrder %>% group_by(pID) %>%
                                                                              summarise(T0T1 = First - Second)

    #Perform the overall bootstrap
    print("perform the overall bootstrap...")
    perDatasetBootstrapData = groupedData %>% group_by(datasetID, techniqueID) %>%
                                              summarise(computeOverallBootstrap(cur_data()))
    overallBootstrapData = groupedData %>% group_by(techniqueID) %>%
                                           summarise(computeOverallBootstrap(cur_data()))

    overallBootstrapData = overallBootstrapData %>% mutate(datasetID = -1)
    simpleBootstrapData = rbind(perDatasetBootstrapData, overallBootstrapData)

    simpleBootstrapData$techniqueID[simpleBootstrapData$techniqueID==0] = "AR"
    simpleBootstrapData$techniqueID[simpleBootstrapData$techniqueID==1] = "2D"
    simpleBootstrapData$datasetID[simpleBootstrapData$datasetID==-1] = "Overall"
    simpleBootstrapData$datasetID[simpleBootstrapData$datasetID==0]  = "Spring"
    simpleBootstrapData$datasetID[simpleBootstrapData$datasetID==1]  = "Cylinder"
    simpleBootstrapData$datasetID[simpleBootstrapData$datasetID==2]  = "Galaxies"

    print(simpleBootstrapData)

    perDatasetBootstrapTechniqueOrderData = timePerTechniqueOrder %>% group_by(datasetID) %>% summarise(computeOverallBootstrapTimePerTechniqueOrder(cur_data()))
    overallBootstrapTechniqueOrderData = overallTimePerTechniqueOrder %>% summarise(computeOverallBootstrapTimePerTechniqueOrder(cur_data()))
    overallBootstrapTechniqueOrderData$datasetID = -1

    simpleBootstrapTechniqueOrderData = rbind(perDatasetBootstrapTechniqueOrderData, overallBootstrapTechniqueOrderData)
    simpleBootstrapTechniqueOrderData$datasetID[simpleBootstrapTechniqueOrderData$datasetID==-1] = "Overall"
    simpleBootstrapTechniqueOrderData$datasetID[simpleBootstrapTechniqueOrderData$datasetID==0]  = "Spring"
    simpleBootstrapTechniqueOrderData$datasetID[simpleBootstrapTechniqueOrderData$datasetID==1]  = "Cylinder"
    simpleBootstrapTechniqueOrderData$datasetID[simpleBootstrapTechniqueOrderData$datasetID==2]  = "Galaxies"
    
    simpleBootstrapTechniqueOrderData$tct    = as.double(simpleBootstrapTechniqueOrderData$tct)
    simpleBootstrapTechniqueOrderData$tctMin = as.double(simpleBootstrapTechniqueOrderData$tctMin)
    simpleBootstrapTechniqueOrderData$tctMax = as.double(simpleBootstrapTechniqueOrderData$tctMax)

    print(simpleBootstrapTechniqueOrderData)

    #Perform the pair-wise bootstrap comparison
    print("perform the pair-wise bootstrap comparison...")
    pwDatasetBootstrapData = pairedWiseData %>% group_by(datasetID) %>%
                                                summarise(computePWBootstrap(cur_data()))
    pwOverallBootstrapData = computePWBootstrap(pairedWiseData) %>% mutate(datasetID = -1)

    pwBootstrapData         = rbind(pwDatasetBootstrapData, pwOverallBootstrapData)

    pwBootstrapData = mapply(c, pwBootstrapData %>% select(datasetID, mccT0T1, mccMinT0T1, mccMaxT0T1, mccSDT0T1,
                                                                      f1T0T1,  f1MinT0T1,  f1MaxT0T1,  f1SDT0T1,
                                                                      tctT0T1, tctMinT0T1, tctMaxT0T1, tctSDT0T1,
                                                                      constraintT0T1, constraintMinT0T1, constraintMaxT0T1, constraintSDT0T1,
                                                                      totalConstraintT0T1, totalConstraintMinT0T1, totalConstraintMaxT0T1, totalConstraintSDT0T1) %>% mutate(ratioTechniqueID="AR / 2D", differenceTechniqueID="AR - 2D"))

    pwBootstrapData = data.frame(pwBootstrapData)
    colnames(pwBootstrapData)[colnames(pwBootstrapData) == "mccT0T1"]    = "mcc"
    colnames(pwBootstrapData)[colnames(pwBootstrapData) == "mccMinT0T1"] = "mccMin"
    colnames(pwBootstrapData)[colnames(pwBootstrapData) == "mccMaxT0T1"] = "mccMax"

    colnames(pwBootstrapData)[colnames(pwBootstrapData) == "f1T0T1"]     = "f1"
    colnames(pwBootstrapData)[colnames(pwBootstrapData) == "f1MinT0T1"]  = "f1Min"
    colnames(pwBootstrapData)[colnames(pwBootstrapData) == "f1MaxT0T1"]  = "f1Max"

    colnames(pwBootstrapData)[colnames(pwBootstrapData) == "tctT0T1"]    = "tct"
    colnames(pwBootstrapData)[colnames(pwBootstrapData) == "tctMinT0T1"] = "tctMin"
    colnames(pwBootstrapData)[colnames(pwBootstrapData) == "tctMaxT0T1"] = "tctMax"

    colnames(pwBootstrapData)[colnames(pwBootstrapData) == "constraintT0T1"]    = "constraint"
    colnames(pwBootstrapData)[colnames(pwBootstrapData) == "constraintMinT0T1"] = "constraintMin"
    colnames(pwBootstrapData)[colnames(pwBootstrapData) == "constraintMaxT0T1"] = "constraintMax"
    
    colnames(pwBootstrapData)[colnames(pwBootstrapData) == "totalConstraintT0T1"]    = "totalConstraint"
    colnames(pwBootstrapData)[colnames(pwBootstrapData) == "totalConstraintMinT0T1"] = "totalConstraintMin"
    colnames(pwBootstrapData)[colnames(pwBootstrapData) == "totalConstraintMaxT0T1"] = "totalConstraintMax"

    pwBootstrapData$mcc    = as.double(pwBootstrapData$mcc)
    pwBootstrapData$mccMin = as.double(pwBootstrapData$mccMin)
    pwBootstrapData$mccMax = as.double(pwBootstrapData$mccMax)

    pwBootstrapData$f1     = as.double(pwBootstrapData$f1)
    pwBootstrapData$f1Min  = as.double(pwBootstrapData$f1Min)
    pwBootstrapData$f1Max  = as.double(pwBootstrapData$f1Max)

    pwBootstrapData$tct    = as.double(pwBootstrapData$tct)
    pwBootstrapData$tctMin = as.double(pwBootstrapData$tctMin)
    pwBootstrapData$tctMax = as.double(pwBootstrapData$tctMax)

    pwBootstrapData$constraint    = as.double(pwBootstrapData$constraint)
    pwBootstrapData$constraintMin = as.double(pwBootstrapData$constraintMin)
    pwBootstrapData$constraintMax = as.double(pwBootstrapData$constraintMax)
    
    pwBootstrapData$totalConstraint    = as.double(pwBootstrapData$totalConstraint)
    pwBootstrapData$totalConstraintMin = as.double(pwBootstrapData$totalConstraintMin)
    pwBootstrapData$totalConstraintMax = as.double(pwBootstrapData$totalConstraintMax)

    pwBootstrapData$datasetID[pwBootstrapData$datasetID==-1] = "Overall"
    pwBootstrapData$datasetID[pwBootstrapData$datasetID==0]  = "Spring"
    pwBootstrapData$datasetID[pwBootstrapData$datasetID==1]  = "Cylinder"
    pwBootstrapData$datasetID[pwBootstrapData$datasetID==2]  = "Galaxies"

    print(pwBootstrapData)

    #Bootstrap learning effect as well
    
    pwDatasetTimePerTechniqueOrderBootstrap = pairedWiseTimePerTechniqueOrder %>% group_by(datasetID) %>%
                                                                  summarise(computePWBootstrapTimePerTechniqueOrder(cur_data()))
    
    pwOverallTimePerTechniqueOrderBootstrap = computePWBootstrapTimePerTechniqueOrder(pairedWiseOverallTimePerTechniqueOrder)
    pwOverallTimePerTechniqueOrderBootstrap$datasetID = -1
    
    pwTimePerTechniqueOrderBootstrap        = rbind(pwDatasetTimePerTechniqueOrderBootstrap, pwOverallTimePerTechniqueOrderBootstrap)
    
    
    pwTimePerTechniqueOrderBootstrap = mapply(c, pwTimePerTechniqueOrderBootstrap %>% select(datasetID, tctT0T1, tctMinT0T1, tctMaxT0T1, tctSDT0T1) %>% mutate(ratioTechniqueID="First / Second", differenceTechniqueID="First - Second"))
    
    pwTimePerTechniqueOrderBootstrap = data.frame(pwTimePerTechniqueOrderBootstrap)
    
    colnames(pwTimePerTechniqueOrderBootstrap)[colnames(pwTimePerTechniqueOrderBootstrap) == "tctT0T1"]    = "tct"
    colnames(pwTimePerTechniqueOrderBootstrap)[colnames(pwTimePerTechniqueOrderBootstrap) == "tctMinT0T1"] = "tctMin"
    colnames(pwTimePerTechniqueOrderBootstrap)[colnames(pwTimePerTechniqueOrderBootstrap) == "tctMaxT0T1"] = "tctMax"
    pwTimePerTechniqueOrderBootstrap$tct    = as.double(pwTimePerTechniqueOrderBootstrap$tct)
    pwTimePerTechniqueOrderBootstrap$tctMin = as.double(pwTimePerTechniqueOrderBootstrap$tctMin)
    pwTimePerTechniqueOrderBootstrap$tctMax = as.double(pwTimePerTechniqueOrderBootstrap$tctMax)
    pwTimePerTechniqueOrderBootstrap$datasetID[pwTimePerTechniqueOrderBootstrap$datasetID==-1] = "Overall"
    pwTimePerTechniqueOrderBootstrap$datasetID[pwTimePerTechniqueOrderBootstrap$datasetID==0]  = "Spring"
    pwTimePerTechniqueOrderBootstrap$datasetID[pwTimePerTechniqueOrderBootstrap$datasetID==1]  = "Cylinder"
    pwTimePerTechniqueOrderBootstrap$datasetID[pwTimePerTechniqueOrderBootstrap$datasetID==2]  = "Galaxies"
    
    print(pwTimePerTechniqueOrderBootstrap)

    # ------------------------------------------------------------------------------
    # -----------------------------------OVERALL------------------------------------
    # ------------------------------------------------------------------------------

    #MCC
    print(glue("Generating {outputDir}/mcc.pdf"))
    g = plotListBootstrap(simpleBootstrapData, nameLabel=datasetID, idLabel=techniqueID, meanLabel=mcc, lowerLabel=mccMin, upperLabel=mccMax)
    ggsave(glue("{outputDir}/mcc.pdf"), plot=g, device="pdf")

    #F1
    print(glue("Generating {outputDir}/f1.pdf"))
    g = plotListBootstrap(simpleBootstrapData, nameLabel=datasetID, idLabel=techniqueID, meanLabel=f1, lowerLabel=f1Min, upperLabel=f1Max)
    ggsave(glue("{outputDir}/f1.pdf"), plot=g, device="pdf")

    #TCT
    print(glue("Generating {outputDir}/tct.pdf"))
    g = plotListBootstrap(simpleBootstrapData, nameLabel=datasetID, idLabel=techniqueID, meanLabel=tct, lowerLabel=tctMin, upperLabel=tctMax)
    ggsave(glue("{outputDir}/tct.pdf"), plot=g, device="pdf")
    
    print(simpleBootstrapTechniqueOrderData)
    #Learning Effect
    print(glue("Generating {outputDir}/learningEffect.pdf"))
    g = plotListBootstrap(simpleBootstrapTechniqueOrderData, nameLabel=datasetID, idLabel=techniqueID, meanLabel=tct, lowerLabel=tctMin, upperLabel=tctMax)
    ggsave(glue("{outputDir}/learningEffect.pdf"), plot=g, device="pdf")

    #ConstraintRatio
    print(glue("Generating {outputDir}/constraint.pdf"))
    g = plotListBootstrap(simpleBootstrapData, nameLabel=datasetID, idLabel=techniqueID, meanLabel=constraint, lowerLabel=constraintMin, upperLabel=constraintMax)
    ggsave(glue("{outputDir}/constraint.pdf"), plot=g, device="pdf")

    # ------------------------------------------------------------------------------
    # --------------------------------PW COMPARISONS--------------------------------
    # ------------------------------------------------------------------------------

    #MCC
    print(glue("Generating {outputDir}/PWmcc.pdf"))
    g = plotListBootstrap(pwBootstrapData, minAxis = -.25, maxAxis = .25, legendName = "Difference", nameLabel=datasetID, idLabel=differenceTechniqueID, meanLabel=mcc, lowerLabel=mccMin, upperLabel=mccMax) + geom_hline(yintercept = 0.0)
    ggsave(glue("{outputDir}/PWmcc.pdf"), plot=g, device="pdf")

    #F1
    print(glue("Generating {outputDir}/PWf1.pdf"))
    g = plotListBootstrap(pwBootstrapData, minAxis = 0.75, maxAxis = 1.25, legendName = "Ratio", nameLabel=datasetID, idLabel=ratioTechniqueID, meanLabel=f1, lowerLabel=f1Min, upperLabel=f1Max) + geom_hline(yintercept = 1.0)
    ggsave(glue("{outputDir}/PWf1.pdf"), plot=g, device="pdf")

    #MCC
    print(glue("Generating {outputDir}/PWtct.pdf"))
    g = plotListBootstrap(pwBootstrapData, legendName = "Ratio", nameLabel=datasetID, idLabel=ratioTechniqueID, meanLabel=tct, lowerLabel=tctMin, upperLabel=tctMax) + geom_hline(yintercept = 1.0)
    ggsave(glue("{outputDir}/PWtct.pdf"), plot=g, device="pdf")

    #TCT -- Learning Effect
    print(glue("Generating {outputDir}/PWLearningEffect.pdf"))
    g = plotListBootstrap(pwTimePerTechniqueOrderBootstrap, legendName = "Ratio", nameLabel=datasetID, idLabel=ratioTechniqueID, meanLabel=tct, lowerLabel=tctMin, upperLabel=tctMax) + geom_hline(yintercept = 1.0)
    ggsave(glue("{outputDir}/PWLearningEffect.pdf"), plot=g, device="pdf")
    

    #Constraint
    print(glue("Generating {outputDir}/PWconstraint.pdf"))
    g = plotListBootstrap(pwBootstrapData, legendName = "Difference", nameLabel=datasetID, idLabel=differenceTechniqueID, meanLabel=constraint, lowerLabel=constraintMin, upperLabel=constraintMax) + geom_hline(yintercept = 0.0)
    ggsave(glue("{outputDir}/PWconstraint.pdf"), plot=g, device="pdf")

    #Save overall data
    #MCC
#    print(glue("generating {outputDir}/mcc.pdf"))
#    g = plotBootstrap(overallBootstrapData, maxAxis=1, meanLabel=mcc, lowerLabel=mccMin, upperLabel=mccMax)
#    ggsave(glue("{outputDir}/mcc.pdf"), plot=g, device="pdf")
#
#    #F1
#    print(glue("generating {outputDir}/f1.pdf"))
#    g = plotBootstrap(overallBootstrapData, maxAxis=1, meanLabel=f1, lowerLabel=f1Min, upperLabel=f1Max)
#    ggsave(glue("{outputDir}/f1.pdf"), plot=g, device="pdf")
#
#    #TCT
#    print(glue("generating {outputDir}/tct.pdf"))
#    g = plotBootstrap(overallBootstrapData, maxAxis=1, meanLabel=tct, lowerLabel=tctMin, upperLabel=tctMax)
#    ggsave(glue("{outputDir}/tct.pdf"), plot=g, device="pdf")
#
#    #NBOperation
#    print(glue("generating {outputDir}/nbOp.pdf"))
#    g = plotBootstrap(overallBootstrapData, maxAxis=1, meanLabel=nbOp, lowerLabel=nbOpMin, upperLabel=nbOpMax)
#    ggsave(glue("{outputDir}//nbOp.pdf"), plot=g, device="pdf")
#
#
#    #Save paired-wise data
#    rownames(pwBootstrapData) = c("Dataset 1", "Dataset 2", "Dataset 3")
#    #MCC
#    print(glue("generating {outputDir}/pw_mccT0T1.pdf"))
#    g = plotBootstrap(pwBootstrapData, maxAxis=1, meanLabel=mccT0T1, lowerLabel=mccMinT0T1, upperLabel=mccMaxT0T1)
#    ggsave(glue("{outputDir}/pw_mccT0T1.pdf"), plot=g, device="pdf")
#
#    print(glue("generating {outputDir}/pw_mccT0T2.pdf"))
#    g = plotBootstrap(pwBootstrapData, maxAxis=1, meanLabel=mccT0T2, lowerLabel=mccMinT0T2, upperLabel=mccMaxT0T2)
#    ggsave(glue("{outputDir}/pw_mccT0T2.pdf"), plot=g, device="pdf")
#
#    print(glue("generating {outputDir}/pw_mccT1T2.pdf"))
#    g = plotBootstrap(pwBootstrapData, maxAxis=1, meanLabel=mccT1T2, lowerLabel=mccMinT1T2, upperLabel=mccMaxT1T2)
#    ggsave(glue("{outputDir}/pw_mccT1T2.pdf"), plot=g, device="pdf")
#
#    #F1
#    print(glue("generating {outputDir}/pw_f1T0T1.pdf"))
#    g = plotBootstrap(pwBootstrapData, maxAxis=1, meanLabel=f1T0T1, lowerLabel=f1MinT0T1, upperLabel=f1MaxT0T1)
#    ggsave(glue("{outputDir}/pw_f1T0T1.pdf"), plot=g, device="pdf")
#
#    print(glue("generating {outputDir}/pw_f1T0T2.pdf"))
#    g = plotBootstrap(pwBootstrapData, maxAxis=1, meanLabel=f1T0T2, lowerLabel=f1MinT0T2, upperLabel=f1MaxT0T2)
#    ggsave(glue("{outputDir}/pw_f1T0T2.pdf"), plot=g, device="pdf")
#
#    print(glue("generating {outputDir}/pw_f1T1T2.pdf"))
#    g = plotBootstrap(pwBootstrapData, maxAxis=1, meanLabel=f1T1T2, lowerLabel=f1MinT1T2, upperLabel=f1MaxT1T2)
#    ggsave(glue("{outputDir}/pw_f1T1T2.pdf"), plot=g, device="pdf")
#
#    #TCT
#    print(glue("generating {outputDir}/pw_tctT0T1.pdf"))
#    g = plotBootstrap(pwBootstrapData, maxAxis=1, meanLabel=tctT0T1, lowerLabel=tctMinT0T1, upperLabel=tctMaxT0T1)
#    ggsave(glue("{outputDir}/pw_tctT0T1.pdf"), plot=g, device="pdf")
#
#    print(glue("generating {outputDir}/pw_tctT0T2.pdf"))
#    g = plotBootstrap(pwBootstrapData, maxAxis=1, meanLabel=tctT0T2, lowerLabel=tctMinT0T2, upperLabel=tctMaxT0T2)
#    ggsave(glue("{outputDir}/pw_tctT0T2.pdf"), plot=g, device="pdf")
#
#    print(glue("generating {outputDir}/pw_tctT1T2.pdf"))
#    g = plotBootstrap(pwBootstrapData, maxAxis=1, meanLabel=tctT1T2, lowerLabel=tctMinT1T2, upperLabel=tctMaxT1T2)
#    ggsave(glue("{outputDir}/pw_tctT1T2.pdf"), plot=g, device="pdf")
}

#'Function parsing the questionnaire data of the users study
#'Input files must be in data/questionnaire 
#'Results are saved in ROutput
parseQuestionnaire = function()
{
    outputDir = "ROutput"
    dir       = "data/questionnaire"
    data      = NULL

    #Read the csv files and concatenate them.
    #They should have the same header
    for(fileName in list.files(path=dir, pattern="*.csv"))
    {
        glue("Parsing {fileName}...")
        #Parse
        csvData = read.csv(glue("{dir}/{fileName}"), sep=',', header=TRUE)

        #Concatenate
        if(is.null(data))
            data = csvData
        else if(names(data) == names(csvData)) #This should be true
            data = rbind(data, csvData)
        else #If csv files are not consistent: exit the script
        {
            print("Error: csv files are not consistent. Exiting...")
            stop()
        }
    }

    print(data)

    #Bootstrap tlx data
    #Format (edit: the format is now transposed):        
    #       Var1   Var2   Var3 ...
    # mean
    # lower
    # upper
    # varName
    # tech
    print("Bootstraping TLX data")
    tlxBootstrap   = NULL
    techIDNames    = c("ar", "d2") #ID as stored in the CSV
    techLabelNames = c("AR", "2D")        #Label to use for rendering

    for(i in 1:length(techIDNames))
    {
        techID    = techIDNames[i]
        techLabel = techLabelNames[i]
        metrics   = c("Performance", "Mental", "Physical", "Temporal", "Frustration", "Effort")
        varNames  = paste(techID, metrics, sep='') 
        
        for(j in 1:length(metrics))
        {
            var    = varNames[j]
            metric = metrics[j]

            #Perform the bootstrap
            print(glue("bootstraping var {var}"))
            bsData           = data.frame(t(c(bootstrapCI(data[[var]], meanFunc), var, metric, techLabel)))
            bsData[1:3]      = as.double(bsData[1:3])
            rownames(bsData) = c(var)
            if(is.null(tlxBootstrap))
                tlxBootstrap = bsData
            else
                tlxBootstrap = rbind(tlxBootstrap, bsData)
        }
    }
    colnames(tlxBootstrap) = c("mean", "lower", "upper", "sd", "varName", "metric", "tech")
    print(tlxBootstrap)

    #Bootstrap PW 
    #Format: (edit: the format is now transposed       
    #       absraVar1   ... absrfVar1 ...   rarfVar1 ...
    # mean
    # lower
    # upper
    # varName
    # tech
    print("Bootstraping Paired-Wise TLX data")
    pwTLXBootstrap = NULL
    techIDPairedNames = data.frame(c("ar", "d2"))  #Name as stored in the CSV
    techLabelPairedNames = data.frame(c("AR", "2D")) #Label to use for rendering
    for(i in 1:length(techIDPairedNames))
    {
        idPairedNames    = t(techIDPairedNames[i])
        labelPairedNames = t(techLabelPairedNames[i])
        varNames = c("Performance", "Mental", "Physical", "Temporal", "Frustration", "Effort")
        for(var in varNames)
        {
            print(glue("Bootstraping var {var}"))
            tech1  = paste(idPairedNames[1], var, sep='')
            tech2  = paste(idPairedNames[2], var, sep='')

            d1 = data[[tech1]] + 1 #Discard infinite values
            d2 = data[[tech2]] + 1 #Discard infinite values
            ratio = d1/d2
            bsData = data.frame(t(c(bootstrapCI(ratio, geomMeanFunc), var, paste(labelPairedNames[1], labelPairedNames[2], sep=" / "))))
            bsData[1:3]      = as.double(bsData[1:3])
            rownames(bsData) = paste(labelPairedNames[1], labelPairedNames[2], var)

            if(is.null(pwTLXBootstrap))
                pwTLXBootstrap = bsData
            else
                pwTLXBootstrap = rbind(pwTLXBootstrap, bsData)
        }
    }
    colnames(pwTLXBootstrap) = c("mean", "lower", "upper", "sd", "varName", "tech")
    print(pwTLXBootstrap)

    #Count the focus: Either Tablet (0) or External Screen (1)
    #Format: (edit: the format is now transposed) 
    #     ar d2
    # Tab
    # External Screen
    print("Counting the focus for each technique...")
    focusData = NULL

    for(i in 1:length(techIDNames))
    {
        techName = techIDNames[i] 
        print(glue("Counting for tech {techName}"))
        ar   = sum(data[[paste(techName, "Focus", sep='')]] == 0)
        tab  = sum(data[[paste(techName, "Focus", sep='')]] == 1)
        both = sum(data[[paste(techName, "Focus", sep='')]] == 2)
        
        subData = data.frame(               t(c(ar,   "External Screen",   techLabelNames[i])))
        subData = rbind(subData, data.frame(t(c(tab,  "Tab",  techLabelNames[i]))))
        subData = rbind(subData, data.frame(t(c(both, "Both", techLabelNames[i]))))

        if(is.null(focusData))
            focusData = subData
        else
            focusData = rbind(focusData, subData)
    }
    focusData = data.frame(focusData)
    colnames(focusData) = c("value", "focus", "tech")
    focusData$value = as.numeric(focusData$value) / nrow(data)
    
    
    #Check the usefulness of external
    print("Counting the usefulness of external screens")
    usefulness = NULL
    
    for(i in 1:length(techIDNames))
    {
        techName  = techIDNames[i]
        techLabel = techLabelNames[i] 
        
        bsData           = data.frame(t(c(bootstrapCI(data[[paste(techName, "External", sep='')]], meanFunc), "External", techLabel)))
        bsData[1:3]      = as.double(bsData[1:3])
        if(is.null(usefulness))
            usefulness = bsData
        else
            usefulness = rbind(usefulness, bsData)
    }
    colnames(usefulness) = c("mean", "lower", "upper", "sd", "varName", "tech")
    print(usefulness)
    
    usefulnessPW = NULL
    
    for(i in 1:length(techIDPairedNames))
    {
        idPairedNames    = t(techIDPairedNames[i])
        labelPairedNames = t(techLabelPairedNames[i])
        tech1  = idPairedNames[1]
        tech2  = idPairedNames[2]

        pwRatio = data[[paste(tech1, "External", sep='')]] / data[[paste(tech2, "External", sep='')]]
        
        bsData           = data.frame(t(c(bootstrapCI(pwRatio, geomMeanFunc), "External", paste(labelPairedNames[1], '/', labelPairedNames[2]))))
        bsData[1:3]      = as.double(bsData[1:3])
        
        if(is.null(usefulness))
            usefulnessPW = bsData
        else
            usefulnessPW = rbind(usefulnessPW, bsData)
    }
    colnames(usefulnessPW) = c("mean", "lower", "upper", "sd", "varName", "tech")
    print(usefulnessPW)

    #Count the ranks
    #Format: (edit: the format is now transposed) 
    #     ar d2
    # 1
    # 2
    # 3
    print("Counting the ranking results...")
    rank = NULL

    for(i in 1:length(techIDNames))
    {
        techName  = techIDNames[i]
        techLabel = techLabelNames[i] 

        print(glue("Counting for tech {techName}"))
        #Names
        galaxyColName   = glue("galaxy{techName}Rank")
        cylinderColName = glue("cylinder{techName}Rank")
        springColName   = glue("spring{techName}Rank")

        #Count
        subGalaxy   = t(data.frame(c(sum(data[[galaxyColName]] == 1), "1st", "Galaxies", techLabel),
                                   c(sum(data[[galaxyColName]] == 2), "2nd", "Galaxies", techLabel)))

        subCylinder = t(data.frame(c(sum(data[[cylinderColName]] == 1), "1st", "Cylinder", techLabel),
                                   c(sum(data[[cylinderColName]] == 2), "2nd", "Cylinder", techLabel)))

        subSpring   = t(data.frame(c(sum(data[[springColName]] == 1), "1st", "Spring", techLabel),
                                   c(sum(data[[springColName]] == 2), "2nd", "Spring", techLabel)))

        subRank = rbind(subSpring, subCylinder, subGalaxy)
        if(is.null(rank))
            rank = subRank
        else
            rank = rbind(rank, subRank)
    }

    rank = data.frame(rank)
    colnames(rank) = c("value", "rank", "metric", "tech")
    rownames(rank) = NULL
    rank$value = as.numeric(rank$value) / nrow(data)

    print(rank)

    #------------------------------------------------------------------------------
    #---------------------------Plot questionnaire data----------------------------
    #------------------------------------------------------------------------------

    #Overall TLX
    print(glue("Generating {outputDir}/tlx.pdf"))
    g = plotListBootstrap(tlxBootstrap, maxAxis=20, nameLabel=metric, idLabel=tech)
    ggsave(glue("{outputDir}/tlx.pdf"), plot=g, device="pdf")

    #PW comparisons
    print(glue("Generating {outputDir}/PWtlx.pdf"))
    g = plotListBootstrap(pwTLXBootstrap, nameLabel=varName, idLabel=tech) + geom_hline(yintercept = 1.0)
    ggsave(glue("{outputDir}/PWtlx.pdf"), plot=g, device="pdf")

    print(focusData)

    #Counting Focus
    print(glue("Generating {outputDir}/focus.pdf"))
    g = plotBarChart(focusData, xLabel=tech, yLabel=value, fillLabel=focus, stack=TRUE)
    ggsave(glue("{outputDir}/focus.pdf"), plot=g, device="pdf")

    #Overall External
    print(glue("Generating {outputDir}/external"))
    g = plotListBootstrap(usefulness, maxAxis=7, nameLabel=varName, idLabel=tech)
    ggsave(glue("{outputDir}/external.pdf"), plot=g, device="pdf")

    #PW comparisons
    print(glue("Generating {outputDir}/PWexternal.pdf"))
    g = plotListBootstrap(usefulnessPW, nameLabel=varName, idLabel=tech) + geom_hline(yintercept = 1.0)
    ggsave(glue("{outputDir}/PWexternal.pdf"), plot=g, device="pdf")

    #Counting Rank
    print(rank)
    print(glue("Generating {outputDir}/rank.pdf"))
    g = plotStackedBarchart(rank, xLabel=tech, yLabel=value, fillLabel=rank, facetLabel=metric)
    ggsave(glue("{outputDir}/rank.pdf"), plot=g, device="pdf")
}

options(tibble.width = Inf)

if(!dir.exists("ROutput"))
    dir.create("ROutput/")

parseLog()
parseQuestionnaire()
