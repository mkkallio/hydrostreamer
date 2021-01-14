

# hydrostreamer 1.0.1

This update mostly fixes a few bugs from the previous version 1.0.0

* Routing algorithm input parameter name changed from *method* to *routing_method* to avoid a conflict with method parameter in `stats::optim()` in case user-defined function was used for optimisation with `hydrostreamer::optimise_region()`.
* Fixed logic error in `optimise_region()` which was caused by mishandling of dates and resulted in poor performance in a cascade of optimisation points
* fixed `observations()` to correctly fetch the observation timeseries.
* updated github README appropriately to the current version