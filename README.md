This repository is a Digital Humanities project on the estate of the wealthy 16th-century merchant Jan della Faille de Oude.

Jan della Faille de Oude, or the Elder, (c. 1515–182) was a wealthy merchant from Antwerp. Though he came from a peasant family, when Jan de Oude died in 1582, he was one of the wealthiest merchants in all of Europe due to his trade in textiles between Italy, the Low Countries, and England. Shortly after his death, his estate was estimated to have a value of over £68,000 Flemish, consisting almost entirely of mercantile capital. According to the laws of Antwerp, Jan de Oude's wealth had to be perfectly evenly divided between his nine heirs, while his testament also gave bequests to eighty-two individuals. The gargantuan task of managing the capital in the estate and evenly dividing it among the heir fell to the three sons Jan de Oude chose as executors.

This digital humanities project uses the testament and the existence of two account books from the estate that list all of the accounts and transactions of the estate from the date of Jan de Oude's death on 8 November 1582 until 31 December 1594. The two books were written in the form of [double-entry bookkeeping](https://en.wikipedia.org/wiki/Double-entry_bookkeeping_system) and contain 209 accounts and over 2,000 transactions between the accounts over the twelve year period. This project uses the [R programming language](https://www.r-project.org) to track and analyze the movement of capital within the estate and the distribution of the inheritance to the nine heirs. The analysis demonstrates the slow and difficult process of the transfer of mercantile capital from one generation to the next. This was particularly the case among the heirs of Jan de Oude, who bickered over the inheritance until the end of their lives into the 1610s, over thirty years after their father's death.

The project involves both network analysis and the calculation of the movement of capital in the non-decimal system of the Flemish pound. The creation of network graphs and analysis is done through a number of packages, including [ggraph](https://cran.r-project.org/package=ggraph) and [igraph](http://igraph.org) among others. I have also written a number of functions to deal with the non-decimal system of the Flemish point. Like the pound Sterling, one pound Flemish was equivalent to 20 shillings, and 1 shilling was equivalent to 12 pence, making addition and subtraction of values non trivial.