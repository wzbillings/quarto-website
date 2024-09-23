###
# Probabilities of GWF damage by enumeration
# Zane
# 2024-09-22
###

# Probabilities for old GWF
create_die_matrix <- function(values, k) {
	perms <-
		rep(values, times = k) |>
		matrix(nrow = length(values), ncol = k) |>
		as.data.frame() |>
		expand.grid()
	
	colnames(perms) <- paste0("X", 1:ncol(perms))
	
	return(perms)
}

die_from_method <- function(which_gwf, n) {
	if (which_gwf == "none") {
		die <- seq(1, n, 1)
	} else if (which_gwf == "old") {
		die <- c(
			rep(c(1, 2), each = 2),
			rep(seq(3, n), each = n + 2)
		)
	} else if (which_gwf == "new") {
		die <- c(3, 3, seq(3, n, 1))
	} else {
		stop("'which_gwf' should be one of: 'none', 'old', or 'new'.")
	}
	
	return(die)
}

get_distribution <- function(
		n, k, which_gwf, return_args = TRUE, digits = 4, pct = FALSE
	) {
	die <- die_from_method(which_gwf, n)
	die_matrix <- create_die_matrix(die, k)
	damage_values <- rowSums(die_matrix)
	damage_distribution <- table(damage_values, dnn = NULL)
	damage_distribution_rel <- prop.table(damage_distribution)
	
	tidy_output <- data.frame(
		value = as.integer(names(damage_distribution)),
		freq = as.integer(damage_distribution)
	)
	
	mult <- ifelse(isTRUE(pct), 100, 1)
	tidy_output$rel_freq <- round(
		as.numeric(damage_distribution_rel) * mult,
		digits = ifelse(isTRUE(pct), digits - 2, digits)
	)
	
	if (isTRUE(return_args)) {
		tidy_output$n <- n
		tidy_output$k <- k
		tidy_output$which_gwf <- which_gwf
	}
	
	return(tidy_output)
}

mean_sd_from_dist <- function(dist_res, format = TRUE, pct_input = FALSE) {
	mult <- ifelse(isTRUE(pct_input), 0.01, 1)
	x <- dist_res$value
	wt <- dist_res$rel_freq * mult
	wm <- sum(x * wt)
	wv <- sum(wt * (x - wm)^2)
	wsd <- sqrt(wv)
	
	if (isTRUE(format)) {
		out <- sprintf("%.2f ± %.2f", wm, wsd)
	} else {
		out <- c("mean" = wm, "sd" = wsd)
	}
	
	return(out)
}

