

# Times for each individual
t_vec <- seq(1, 10, 1)

# Number individuals
N <- 10

test <-
	t_vec |>
	replicate(n = N) |>
	`colnames<-`(stringr::str_pad(1:N, width = 2, pad = "0")) |>
	tibble::as_tibble() |>
	tidyr::expand_grid() |>
	tidyr::pivot_longer(
		cols = dplyr::everything(),
		names_to = "id",
		values_to = "t"
	) |>
	dplyr::group_by(id) |>
	# Sample random parameters at subject level
	dplyr::mutate(
		# Random intercept
		log_a = rnorm(1),
		# Random slope
		k = rnorm(1, -1, 0.1)
	) |>
	dplyr::ungroup() |>
	# Calculate means at observation level
	dplyr::mutate(
		# Random noise -- same for both models to better see differences
		noise = rnorm(1),
		# Exponential decay model
		exp_mu = log_a + k * t,
		exp_y = exp(exp_mu + noise),
		# Power law decay model
		pow_mu = log_a + k * log(t),
		pow_y = exp(pow_mu + noise)
	) |>
	# I did this in a shitty way and now I have to pay for it by pivoting twice
	tidyr::pivot_longer(
		cols = c(exp_mu, exp_y, pow_mu, pow_y),
		names_to = "model_var"
	) |>
	tidyr::separate(model_var, into = c("model", "variable"), sep = "_") |>
	tidyr::pivot_wider(
		names_from = variable,
		values_from = value
	)

library(ggplot2)
ggplot2::theme_set(zlib::theme_ms())

ggplot(test) +
	aes(x = t, y = y, color = id) +
	geom_line(show.legend = FALSE) +
	geom_point(show.legend = FALSE) +
	#scale_y_continuous(trans = "log") +
	facet_wrap(~model, scales = "free_y")
