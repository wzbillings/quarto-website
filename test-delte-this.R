d <-
	iris |>
	dplyr::transmute(
		petal_length = Petal.Length,
		sepal_width = Sepal.Width,
		species = as.numeric(Species)
	)

retk_fit <-
	rethinking::ulam(
		flist = alist(
			petal_length ~ normal(mu, sigma),
			mu <- alpha[species] + beta * sepal_width,
			alpha[species] ~ normal(bar_alpha, gamma),
			bar_alpha ~ normal(0, 2),
			beta ~ normal(0, 2),
			gamma ~ exponential(1),
			sigma ~ exponential(1)
		),
		data = as.list(d),
		chains = 4,
		cores = 4,
		iter = 5000,
		warmup = 2500,
		cmdstan = TRUE, # Not sure this is actually working
		control = list(adapt_delta = 0.95, seed = 370)
	)

brms_priors <- c(
	prior(normal(0, 2), class = b),
	# Prior for alpha_bar
	prior(normal(0, 2), class = Intercept),
	prior(exponential(1), class = sd),
	prior(exponential(1), class = sigma)
)

brms_fit <-
	brms::brm(
		formula = petal_length ~ (1 | species) + sepal_width,
		prior = brms_priors,
		family = gaussian(link = "identity"),
		data = d,
		seed = 370,
		iter = 5000,
		# I like cmdstanr more but this is what we used for the rethinking fit
		# so keep it the same just in case
		backend = "rstan",
		cores = 4
	)

retk_post_raw <- extract.samples(retk_fit, n = Inf)
retk_post <-
	data.frame(
		alpha1 = retk_post_raw$alpha[,1],
		alpha2 = retk_post_raw$alpha[,2],
		alpha3 = retk_post_raw$alpha[,3],
		bar_alpha = retk_post_raw$bar_alpha,
		beta = retk_post_raw$beta,
		gamma = retk_post_raw$gamma,
		sigma = retk_post_raw$sigma,
		sample = 1:10000,
		fit = rep("rethinking", 10000)
	)

suppressWarnings(
brms_post <-
	posterior_samples(brms_fit) |>
	dplyr::mutate(
		across(
			starts_with("r_species"),
			~.x + b_Intercept
		)
	) |>
	dplyr::transmute(
		alpha1 = `r_species[1,Intercept]`,
		alpha2 = `r_species[2,Intercept]`,
		alpha3 = `r_species[3,Intercept]`,
		bar_alpha = b_Intercept,
		beta = b_sepal_width,
		gamma = sd_species__Intercept,
		sigma = sigma,
		sample = 1:10000,
		fit = "brms"
	)
)

post <- rbind(retk_post, brms_post)

post |>
	tidyr::pivot_longer(
		cols = -c(fit, sample),
		names_to = "parameter",
		values_to = "value"
	) |>
	ggplot() +
	aes(x = value, fill = fit) +
	geom_histogram(
		alpha = 0.5,
		position = position_identity(),
		color = "black",
		bins = 20,
		boundary = 0
	) +
	labs(x = NULL, y = NULL, fill = NULL) +
	facet_wrap(vars(parameter), scales = "free", nrow = 2) +
	scale_fill_manual(values = c("#E69F00", "#56B4E9")) +
	zlib::theme_ms() +
	theme(
		strip.text = element_text(margin = margin(t = 4, b = 4)),
		axis.text = element_text(size = 10)
	)
