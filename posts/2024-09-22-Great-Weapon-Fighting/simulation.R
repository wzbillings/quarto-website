###
# Simulating GWF damage
# Zane
# 2024-09-22
###

N_sims <- 1e6

validate_args_as_integer <- function(...) {
	mc <- match.call(expand.dots = FALSE)
	dots <- list(...)
	names(dots) <- mc$...
	
	for (i in seq_along(dots)) {
		it <- dots[[i]]
		it_name <- names(dots)[[i]]
		if (!is.numeric(it) || (it < 1) || (it %% 1 != 0)) {
			stop(it_name, " should be an integer ≥ 1")
		}
	}
	
	invisible(TRUE)
}

# Old method
simulation_old_gwf <- function(n, k, n_sims) {
	
	validate_args_as_integer(n, k, n_sims)
	
	one_die <- function() {
		result <- sample.int(n, size = n_sims, replace = TRUE)
		rerolls <- (result %in% c(1, 2))
		result[rerolls] <- sample.int(n, size = sum(rerolls), replace = TRUE)
		return(result)
	}
	
	sim_dice <-
		lapply(1:k, \(x) one_die()) |> 
		simplify2array(except = NULL)
	
	damage <- rowSums(sim_dice)
	
	return(damage)
}

set.seed(375)
old_sim <- simulation_old_gwf(6, 2, 1e6)
old_sim_counts <- old_sim |> table()
old_sim_props <- old_sim_counts |> prop.table()
barplot(old_sim_props)

# New method

simulation_new_gwf <- function(n, k, n_sims) {
	
	validate_args_as_integer(n, k, n_sims)
	
	one_die <- function() {
		result <- sample.int(n, size = n_sims, replace = TRUE)
		result[result < 3] <- 3
		return(result)
	}
	
	sim_dice <-
		lapply(1:k, \(x) one_die()) |> 
		simplify2array(except = NULL)
	
	damage <- rowSums(sim_dice)
	
	return(damage)
}

set.seed(375)
new_sim <- simulation_new_gwf(6, 2, 1e6)
new_sim_counts <- new_sim |> table()
new_sim_props <- new_sim_counts |> prop.table()
barplot(new_sim_props)

# END OF FILE ####
