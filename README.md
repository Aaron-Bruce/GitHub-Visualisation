# github-visualisation
For my visualisation project, I aimed to graph the commits of a specified user on Github. The visualisation would be a scatterplot graph with the x-axis being time in months and the y-axis being the size of the commit in lines of code. If possible, I would have also liked to have put the name of the repository that was commited to on each point of the graph. However, I didn't get near to this desired solution. I struggled massively trying to get a list of each commit as the 'list commits' path in the API documentation doesn't contain any information about the lines of code written for a given commit. In order to get this information, I would have to access each commit throught the 'get commit' path, which could be done by firstly listing all the commits and gathering up a list of the commit ids ('sha'). Nextly, I would have to use this id to 'get' each commit, which from there I could then gather information about the lines of code written in each commit. However, this was not to be as my incompetence with Haskell proved detrimental and despite hours upon hours of effort, I was left with a hardly impressive amount of work done. This also impacted heavily on the visualisation aspect of the project, or lack there of it, as I was holding out on being able to gather the information I wanted before starting into actually visualising the data. Nonetheless, this project has proved an important lesson to me, and in future I will take further steps to become more competent in a language before endeavouring to do anything remotely complex with it.
