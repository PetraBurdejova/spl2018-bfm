
###############################################################################     
####    MOEexploratory.R    ###################################################    
###############################################################################     
#
# Creates flowchart of MOE quantlet structure.
#
# Output:  MOEchart_qlet.tex       - flowchart in .tex format
#          MOEchart_qlet.pdf       - flowchart in .pdf format
#
###############################################################################

# Clear all variables.
rm(list = ls(all = TRUE))
graphics.off()

# Install and load libraries.
libraries = c("DiagrammeR")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)



###############################################################################
####    0.  SET WORKING DIRECTORY    ##########################################
###############################################################################

####    ATTENTION: Working directory is assumed to be the root of the MOE 
####    repository, not the MOEmergedata Quantlet subdirectory!!!


# If needed, set working directory accordingly:
#setwd("path/to/MOE_repository")



###############################################################################
####    1.  CREATE FLOWCHART    ###############################################
###############################################################################


chart_qlet = "
    
digraph qlets {

    graph [layout = neato,
           overlap = true,
           outputorder = edgesfirst]
    
    # add note statements
    node [shape = box]
    A [pos = '-3,0!', label = ' MOErawdata ']
    B [pos = '-1,0!', label = ' MOEinterpolation ']
    C [pos = '1,0!', label = ' MOEmergedata ']
    D [pos = '3,0!', label = ' MOEregression ']
    F [pos = '3,1!', label = ' MOEtimedummies ']
    P1 [pos = '-3,1!', label = ' MOEplotNAs ']
    P2 [pos = '1,1!', label = ' MOEexploratory ']

    node [shape = ellipse]
    O [pos = '5,0!', label = 'Regression\nOutput']
    O1 [pos = '-3,2!', label = ' MOEplot_na ']
    O2 [pos = '0,2!', label = ' MOEplot_expl ']
    O3 [pos = '2,2!', label = ' MOEplot_corr ']
        
    # add edge statements
    edge [arrowhead = diamond, headport = 'w', tailport = 'e']
    A -> B;
    B -> C;
    C -> D;

    edge [arrowhead = diamond, headport = 's', tailport = 'n']
    F -> D [headport = 'n', tailport = 's'];
    A -> P1;
    C -> P2;
    
    edge [arrowhead = arrow, headport = 'w', tailport = 'e']
    D -> O;

    edge [arrowhead = arrow, headport = 's', tailport = 'n']
    P1 -> O1;
    P2 -> O2;
    P2 -> O3;

}
"

grViz(chart_qlet)
