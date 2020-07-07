##Cargar y utilizar función IPAK
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("bibliometrix","dplyr","fpc","NbClust","cluster","factoextra","tidyr", "RColorBrewer")
ipak(packages)
#file
file <- c("1.bib", "2.bib", "3.bib", "4.bib", "5.bib") 
M <- convert2df(file, dbsource = "scopus", format = "bibtex")
write.csv(M, "M.csv")
#head(M["TC"])
results <- biblioAnalysis(M, sep = ";")
options(width=100)
S <- summary(object = results, k = 10, pause = FALSE)
S$MostProdCountries
S$MostRelSources
#Some basic plots can be drawn using the generic function :
plot(x = results, k = 10, pause = FALSE)
#To obtain the most frequent cited manuscripts:
CR <- citations(M, field = "article", sep = ";")
cited<- cbind(CR$Cited[1:10])
cited
CR <- localCitations(M, sep = ";")
#Dominance factor
DF <- dominance(results, k = 10)
DF
#Authors’ h-index
#indices <- Hindex(M, field = "author", elements="KIM J", sep = ";", years = 10)
# Bornmann's impact indices:
#indices$H
# Bornmann's citations
#indices$CitationList
#To calculate the h-index of the first 10 most productive authors (in this collection):
authors=gsub(","," ",names(results$Authors)[1:10])
indices <- Hindex(M, field = "author", elements=authors, sep = ";", years = 50)
indices$H
## Table: Author's productivity per year
topAU <- authorProdOverTime(M, k = 10, graph = TRUE)
#Bradford law
BR <- bradford(M)
#Lotka law
L <- lotka(results)
# Author Productivity. Empirical Distribution
L$AuthorProd
# Beta coefficient estimate
L$Beta
# Constant
L$C
# Goodness of fit
L$R2
# P-value of K-S two sample test
L$p.value
# Observed distribution
Observed=L$AuthorProd[,3]
# Theoretical distribution with Beta = 2
Theoretical=10^(log10(L$C)-2*log10(L$AuthorProd[,1]))
#PLot
plot(L$AuthorProd[,1],Theoretical,type="l",col="red",ylim=c(0, 1), xlab="Articles",ylab="Freq. of Authors",main="Scientific Productivity")
lines(L$AuthorProd[,1],Observed,col="blue")
legend(x="topright",c("Theoretical (B=2)","Observed"),col=c("red","blue"),lty = c(1,1,1),cex=0.6,bty="n")
#Bipartite networks
#A <- cocMatrix(M, Field = "SO", sep = ";")
#sort(Matrix::colSums(A), decreasing = TRUE)[1:5]
#Citation network
#A <- cocMatrix(M, Field = "CR", sep = ".  ")
#Author network
#A <- cocMatrix(M, Field = "AU", sep = ";")
#Country network
#Authors’ Countries is not a standard attribute of the bibliographic data frame. You need to extract this information from affiliation attribute using the function metaTagExtraction.
M <- metaTagExtraction(M, Field = "AU_CO", sep = ";")
# A <- cocMatrix(M, Field = "AU_CO", sep = ";")
#Author keyword network
A <- cocMatrix(M, Field = "DE", sep = ";")
#Keyword Plus network
A <- cocMatrix(M, Field = "ID", sep = ";")
NetMatrix <- biblioNetwork(M, analysis = "coupling", network = "references", sep = ".  ")
net=networkPlot(NetMatrix,  normalize = "salton", weighted=NULL, n = 30, Title = "references", type = "fruchterman", size=5,size.cex=T,remove.multiple=TRUE,labelsize=0.8,label.n=10,label.cex=F)
NetMatrix <- biblioNetwork(M, analysis = "coupling", network = "authors", sep = ";")
net=networkPlot(NetMatrix,  normalize = "salton", weighted=NULL, n = 30, Title = "Authors' Coupling", type = "fruchterman", size=5,size.cex=T,remove.multiple=TRUE,labelsize=0.8,label.n=10,label.cex=F)
#Co-Citation Network
# Create a co-citation network
NetMatrix <- biblioNetwork(M, analysis = "co-citation", network = "references", sep = ";")
# Plot the network
net=networkPlot(NetMatrix, n = 30, Title = "Co-Citation Network", type = "fruchterman", size=T, remove.multiple=FALSE, labelsize=0.7,edgesize = 5)
# Create keyword co-occurrences network
NetMatrix <- biblioNetwork(M, analysis = "co-occurrences", network = "keywords", sep = ";")
# Plot the network
net=networkPlot(NetMatrix, normalize="association", weighted=T, n = 30, Title = "Keyword Co-occurrences", type = "fruchterman", size=T,edgesize = 5,labelsize=0.7)
# Create a co-citation network sources
NetMatrix <- biblioNetwork(M, analysis = "coupling", network = "sources", sep = ";")
net=networkPlot(NetMatrix, n = 30, Title = "Revistas", type = "fruchterman", size=T, remove.multiple=FALSE, labelsize=0.7,edgesize = 5)
##### Collaboration Networks ############
NetMatrix <- biblioNetwork(M, analysis = "collaboration", network = "universities")
NetMatrix <- biblioNetwork(M, analysis = "co-occurrences", network = "universities")
net=networkPlot(NetMatrix, n = 30, Title = "Affiliation", type = "fruchterman", size=T, remove.multiple=FALSE, labelsize=0.7,edgesize = 5)
# Conceptual Structure using keywords (method="MCA")
CS <- conceptualStructure(M,field="ID", method="MCA", minDegree=4, clust=4 ,k.max=5, stemming=FALSE, labelsize=10, documents=10)
