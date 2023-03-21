# Portugese Larks example from slides

# Run larks-example.R script before this one for graph at the end

# packages
library("readr")
library("dplyr")
library("mgcv")
library("gratia")
library("patchwork")
library("ggplot2")

# read in the data
larks_url <- "https://bit.ly/gam-larks"
larks <-  read_csv(larks_url, col_types = "ccdddd")

# Or as per the slides:
# larks <-  read_csv(here("data", "larks.csv"),
#                   col_types = "ccdddd")

# convert some variables to factors (two bird species), and scale the
# easting and northing variables to be in KM
# numerically we prefer smaller valued variables
linnets <- larks %>%
	mutate(crestlark = factor(crestlark),
				 linnet = factor(linnet),
				 e = x / 1000,
				 n = y / 1000)
linnets

# plot the data
linnets %>%
	ggplot(aes(x = e, y = n, colour = linnet)) +
	geom_point(size = 0.5) + # add a point layer
	coord_fixed() +
	scale_colour_discrete(na.value = "#bbbbbb33") +
	labs(x = NULL, y = NULL)

# fit our model
# isotropic thin plate regression spline smooth of spatial coordinates
lin <- gam(linnet ~ s(e, n, k = 100),
						 data = linnets,
						 family = binomial,
						 method = "REML")

# model summary
summary(lin)

# visualise the estimate smooth
# a grid of 75x75 is a bit more efficient than the default of 100x100
draw(lin, rug = FALSE, n = 75)

# model checking is a pain for 0/1 data, so aggregate to binomial counts
linnets2 <- linnets %>%
	mutate(crestlark = as.numeric(as.character(crestlark)),  # to numeric
				 linnet = as.numeric(as.character(linnet)),
				 tet_n = rep(1, nrow(linnets)), # counter for how many grid cells we sum
				 N = rep(1, nrow(linnets)),     # number of obs, 1 per row currently
				 N = if_else(is.na(linnet), NA_real_, N)) %>% # set N to NA if no obs
	group_by(QUADRICULA) %>%                     # group by the larger grid square
	summarise(across(c(N, crestlark, linnet, tet_n, e, n),
									 ~ sum(., na.rm = TRUE))) %>%  # sum all needed variables
	mutate(e = e / tet_n, n = n / tet_n) # rescale to get avg E,N coords

# fit binomial GAM
lin2 <- gam(cbind(linnet, N - linnet) ~ s(e, n, k = 100),
							data = linnets2,
							family = binomial,
							method = "REML")

# model summary
summary(lin2)

# compare the fits
draw(lin, n = 75, rug = FALSE) +
	draw(lin2, n = 75, rug = FALSE) +
	plot_layout(ncol = 2)
# look pretty similar

# let's look at some model diagnostics
appraise(lin2, method = "simulate")

# hmmm, some overdispersion - refit as a quasi binomial
lin3 <- gam(cbind(linnet, N - linnet) ~ s(e, n, k = 100),
							data = linnets2,
							family = quasibinomial,
							method = "REML")

# look at the estimate dispersion parameter - this should be 1 for true
# binomial counts
lin3$scale # should be == 1

# model summary
summary(lin3)

# let's look at some model diagnostics
appraise(lin3, method = "simulate")

# compare the fits
draw(lin, n = 75, rug = FALSE) +
	draw(lin3, n = 75, rug = FALSE) +
	plot_layout(ncol = 2)

# generate fitted values
ds <- data_slice(lin3, e = evenly(e, n = 75), n = evenly(n, n = 75))
fv <- fitted_values(lin3, data = ds, scale = "response")

fv %>%
	ggplot(aes(x = e, y = n, fill = fitted)) +
	geom_tile() +
	scale_fill_viridis_c(option = "plasma") +
	coord_equal()

