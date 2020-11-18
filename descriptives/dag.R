library(ggdag)

theme_set(theme_dag_blank())

coords <- list(
  x = c(TPP = 1, FP = 2, Strategy = 1.5, Incumbent = 3, Closemargin = 3),
  y = c(TPP = 2, FP = 2, Strategy = 1.5, Incumbent = 2, Closemargin = 2.75)
)
ie_dag <- dagify(TPP ~ FP + Strategy,
                         FP ~ Incumbent + Strategy,
                         Incumbent ~ Closemargin,
                         labels = c("TPP" = "Two-åparty\n preferences\n (t+1)", 
                                    "FP" = "First preferences\n (t+1)", 
                                    "Incumbent" = "Incumbent",
                                    "Closemargin" = "Close\n Margin (t)",
                                    "Strategy" = "Party\n Strategy"),
                         latent = "Strategy",
                         exposure = "Incumbent",
                         outcome = "TPP", 
                 coords = coords)

ggdag(ie_dag, text = FALSE, use_labels = "label") +
  geom_dag_node(color = c(rep("#3f9d7f", 3), "#c26683", "#3f9d7f")) +
  geom_dag_edges(edge_alpha = 0.25) +
  scale_y_continuous(limits = c(0.5, 3.5))

ggdag_dseparated(ie_dag, text = FALSE, use_labels = "label")

