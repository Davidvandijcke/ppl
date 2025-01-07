
dt <- fread(file.path(dataOut, 'BFI_network_data.csv.gz'))
edge_list <- data.frame(from = dt$COMPANY_NAME, to = dt$COMPANY_NAME_next, weight = dt$cnt)



# Create a graph object from the edge list
graph <- graph_from_data_frame(edge_list, directed = TRUE)

# Calculate vertex sizes based on 'cnt' values (you may need to adjust this)
vertex_sizes<- c(0, dt$cnt)
names(vertex_sizes) <- V(graph)$name


# Generate an initial layout
layout <- layout_with_fr(graph, area=vcount(graph)^1)

# Identify the central vertex
central_vertex_name <- "microsoft" # replace with the actual name of your central vertex
central_vertex_id <- which(V(graph)$name == central_vertex_name)

# Center the layout on the central vertex
layout <- layout - layout[central_vertex_id, ]
layout[central_vertex_id, ] <- c(0, 0) # Fix the central vertex at (0,0)

# Scale vertices based on their size
for(i in seq_len(vcount(graph))) {
  if(i != central_vertex_id) { # Avoid moving the central vertex
    distance_factor <- sqrt(vertex_sizes[i]) # Scale factor based on size (sqrt for less dramatic effect)
    layout[i, ] <- layout[i, ] * distance_factor
  }
}
V(graph)$frame.color <- "white"
V(graph)$color <-  adjustcolor("dodgerblue", alpha.f = 0.5)
#V(graph)$label <- ""
size_threshold <- quantile(vertex_sizes, 0.85)
# Apply labels to vertices that are larger than the threshold
V(graph)$label[V(graph)$name %in% names(vertex_sizes[vertex_sizes > size_threshold])] <- V(graph)$name[V(graph)$name %in% names(vertex_sizes[vertex_sizes > size_threshold])]
E(graph)$arrow.mode <- 0

# Plot the graph
par(mar = c(0, 0, 0, 0)+0.1)
pdf(file=file.path(figs, "network_pre.pdf"))
plot(graph, layout=layout, vertex.size=vertex_sizes,
     vertex.label.color="black",
     vertex.label.dist=1.5, margin=0)
dev.off()