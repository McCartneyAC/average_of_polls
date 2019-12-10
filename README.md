# average_of_polls
Average of Polls pulled from RCP but displayed with error
[The Shiny App Version is Live Here.](https://acm9q.shinyapps.io/average_of_polls/)


Eventual goals: 
* use mixed-effects to correct for "House Effect" on polling error.
* [or use GAM with a covariate of the house effect for each poll result](https://stats.idre.ucla.edu/r/faq/how-can-i-explore-different-smooths-in-ggplot2/)
* ~~linear distance calculating module for "Whom should I vote for?"~~
* ~~redraw compass map to place user and create a voronoi diagram to go with it?~~


TODO: 


Note that excel doc is structured to be copy/pasted into a jupyter notebook, not actually used for analysis directly becuase no IDE is available on a work computer. 

Here, the House Effect of each firm is taken from `fivethirtyeight`'s dataset (see their github repo [here](https://github.com/fivethirtyeight/data/tree/master/pollster-ratings) ). For Harris polling, the house effect is for both HarrisX and for Harvard Harris until I can figure out a discernment between the two. This is not meant to be a perfect statistical adjustment, just some additional variance to control for with the hope of quieting these effects. In this case, + is |+D| and - is |+R|. 


![Polling Average as of September 18, 2019](https://github.com/McCartneyAC/average_of_polls/blob/master/average_sept_18.png?raw=true)
