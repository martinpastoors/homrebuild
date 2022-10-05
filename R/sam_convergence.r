### Basic checks for convergebce SAM fit ###

# You always see the convergence status when printing the fit:
print(fit) # or just "fit", should be "Convergence OK"
# This is the basic check:
fit$opt$convergence # should be 0 if the model has converged
# But if we want to go a bit deeper in how good is the fit:
fit$sdrep # This is important but tricky, the standard errors (second column) should all be estimated (no NaN), but ideally they should also be small (e.g. <1) meaning the parameter is well estimated
# You can extract the standard errors of the fixed parameters with:
unlist(fit$plsd[which(names(fit$plsd)%in%names(fit$sdrep$par.fixed))])
# The max gradient should be closed to 0, the threshold is a bit subjective but < 1e-4 is already good:
max(fit$sdrep$gradient.fixed) 
# Of course there is more we could look at like parameter correlation etc., but it is not relevant here.

