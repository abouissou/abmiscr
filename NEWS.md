# abmiscr 0.1.0

* `calc_dist_matrix()` and `calc_mds()` wrap `vegan::vegdist()` and `vegan::metaMDS()`, respectively, with commonly-used arguments.
* Includes a suite of functions to help with a workflow that involves comparing ordination runs each with a different number of dimensions. `plot_stress_dim()` visualizes ordination stress along a sequence of dimensions.
* `add_text_npc()` provides an NPC-based specification to put text within a plot.
