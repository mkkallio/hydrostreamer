

# hydrostreamer 1.0.1

This update mostly fixes a few bugs from the previous version 1.0.0 


* Routing algorithm input parameter name changed from *method* to *routing_method* to avoid a conflict with method parameter in `stats::optim()` in case user-defined function was used for optimisation with `hydrostreamer::optimise_region()`.
* Fixed logic error in `optimise_region()` which was caused by mishandling of dates and resulted in poor performance in a cascade of optimisation points
* fixed `observations()` to correctly fetch the observation timeseries.
* updated github README appropriately to the current version

This update also adds a few new functions

* Muskingum-Cunge routing algorithm
* `hydrostreamer::compute_hydrological_signatures()` for the ability to compute hydrological signatures from the timeseries data
* `hydrostreamer::compute_upstream_aggregate()` for the ability to compute e.g. means, maximum, minimum for the values of all upstream segments.
* `hydrostreamer::compute_network_length()` for the ability to estimate the maximum length of the network.
* `evaluate_instant_routing()` for the ability to assess whether instantaneous routing is a fitting solution for the network and timestep.