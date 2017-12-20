# learning draw network with networkD3 package

# official website  -------------------------------------------------------

# http://christophergandrud.github.io/networkD3/


# install -----------------------------------------------------------------

install.packages("networkD3")
library(networkD3)

# example 1 ## simpleNetwork---------------------------------------------------------------

# Create fake data
src <- c("A", "A", "A", "A",
         "B", "B", "C", "C", "D")
target <- c("B", "C", "D", "J",
            "E", "F", "G", "H", "I")
networkData <- data.frame(src, target)

# Plot
simpleNetwork(networkData)

# example 2 ## forceNetwork---------------------------------------------------------------
# Load data
data(MisLinks)
data(MisNodes)

# Plot
forceNetwork(Links = MisLinks, Nodes = MisNodes,
             Source = "source", Target = "target",
             Value = "value", NodeID = "name",
             Group = "group", opacity = 1,fontSize = 10)

# zoom set by TURE, pic can be zoom. double click.
forceNetwork(Links = MisLinks, Nodes = MisNodes, Source = "source",
             Target = "target", Value = "value", NodeID = "name",
             Group = "group", opacity = 0.4, zoom = TRUE)
