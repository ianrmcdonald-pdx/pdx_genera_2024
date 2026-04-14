# PSU Data Science: 2024 Portland Ranked Choice Elections

This repository contains data files and code support for the Spring 2026 Portland Rank Choice Voting project, using data from the November 2024 general election.

A cast vote record file can be found in the data directory for five different races: the mayoral race and the four district city council races.

I have added shapefiles for GIS analysis for precincts and district boundaries.

### How did Portland change its mayoral and council elections in 2024?

In 2022, Portland voters approved comprehensive changes to the city charter. The city government now elects a mayor independent of the city council, while the council size increased from 5 to 12 members, chosen from four districts, each electing three members.

The mayor and council are now elected using a single transferable vote method, where each voter ranks one to six candidates. Importantly, the process eliminates primary elections. Voters choose their preferred candidates from a single, unlimited list, and winners are chosen from a ranked choice method. Information about ranked choice voting is published widely, and the Portland process is summarized on [the city's web site.](https://www.portland.gov/vote/questions)

### What is a cast vote record file?

To provide transparent validation of election results, most ranked choice municipalities publish data sets with unidentified records that contain the individual ballot rankings. The cast vote record files for Portland's six 2024 municipal elections have been published in this way, with records for more than 300,000 ballots, containing candidate preferences and precinct information, The six elections include:

-   citywide mayoral and auditor elections

-   four district based council elections

The published election results can be reproduced (closely, but not perfectly) using and R or Python packages. My preference is the R vote package but that's not the only choice, and a package is helpful but not essential.

Access to individual ballot records gives us unique research opportunities. A few of the research questions you might consider:

1.  **Does ranked choice voting change the result?** Some critics complain that ranked choice voting increases voters' cognitive load without changing outcomes, but we know, at minimum, that the elimination of primaries has an enormous effect on municipal elections.

    Specifically, we can ask whether Portland four district, multi-member council elections created a result that couldn't be reproduced using 12 smaller districts. Is the support for specific winning candidates concentrated geographically within the four districts?

2.  **Did endorsements (positive and negative) by the Democratic Socialists of America change the outcomes?** DSA members comprise a large minority faction of the newly elected city council, though it's not apparent that most voters would have endorsed them. Did the ranked choice voting method facilitate their election? And did the campaign to withhold a ranking of conservative mayoral candidate Rene Gonzalez reduce his chances of winning?

3.  **Was support for specific mayoral candidates linked to support of specific council candidates?**

This report from the [Data and Democracy Lab at the University of Chicago](https://mggg.org/portland-stv-24) investigates other questions and could inspire your own investigation. In the coming days I'll expand on these ideas and suggest some techniques, including links to US Census data and GIS packages available in R.

Project Deliverables and Ideas: Applying Data Science to CVR Voting

More broadly, CVR data is unusually rich because of the individual level ballot content.

1.  CVR data can help address questions about political strategy in a ranked choice voting system. For example, does a campaign to reject ranking a particular candidate diminish that candidate's performance?\
    \
    In Portland, a prominent mayoral candidate, Rene Gonzales, was the target of a "don't rank" rampaign, along with a handful of city counci candidates. Did those candidates appear on fewer ballots, and can we see endorsed candidates combine successfully into electoral blocs?\
2.  Were candidates supported by electoral minorities more successful than they would have been had their support come from smaller geographic areas within council districts?\
3.  We can see the results of individual ballots for multiple city races. Specifically, we can connect ballot records of mayoral candidates to council races. Portland races are non-partisan, but did support for specific mayoral candidates match support for specific council candidates?\
4.  The greatest data science opportunity is the ability to connect ballot content, and voring precincts, to GIS tools. We can connect precinct information to census block information using spatial joins that can analyze precinct characteristics to electoral outcomes.\
    \
    I wonder if the candidates endorsed by the DSA succeeded in areas with economic characteristics such as home ownership and income.

The deliverables can include the following:

A dashboard that regenerates ranked choice voting results for specific subsets of the city and council districts, and depicts maps that show results.

A dashboard that generates results of candidates associated with DSA endorsement blocs, and more generally, effect of moving specific candidates out of the candidate lists.

The effect of using ballot information in alternative forms of voting, such as simple plurality voting, approval voting, and Condorcet voting (all of which we can explain in our early meetings).

irm 4/13/2026
