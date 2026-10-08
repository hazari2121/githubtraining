# Figures. Colours: diverging blue <-> grey <-> red for correlations,
# single-hue blue ramp for magnitudes, fixed categorical slots for guild/host.

pal <- list(
  surface = "#fcfcfb", ink = "#0b0b0b", ink2 = "#52514e", na = "#e6e5e1",
  div_low = "#1c5cab", div_mid = "#f0efec", div_high = "#c23b3a",
  seq = c("#cde2fb", "#86b6ef", "#3987e5", "#1c5cab", "#0d366b"),
  guild = c(borer = "#2a78d6", defoliator = "#eb6834", other = "#1baf7a",
            `bark borer` = "#2a78d6", `wood borer` = "#4a3aa7"),
  host = c(conifer = "#008300", broadleaf = "#eda100", both = "#e87ba4")
)

theme_eu <- function(base = 9) {
  ggplot2::theme_minimal(base_size = base) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = pal$surface, colour = NA),
      panel.grid = ggplot2::element_blank(),
      text = ggplot2::element_text(colour = pal$ink),
      axis.text = ggplot2::element_text(colour = pal$ink2),
      plot.title = ggplot2::element_text(face = "bold"),
      plot.caption = ggplot2::element_text(colour = pal$ink2, hjust = 0)
    )
}

save_fig <- function(p, path, w, h) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(path, p, width = w, height = h, dpi = 200, bg = pal$surface)
  path
}

plot_coverage <- function(cov_cs) {
  sp_order <- cov_cs |> dplyr::group_by(species) |>
    dplyr::summarise(k = sum(pct_observed > 0)) |> dplyr::arrange(dplyr::desc(k), species) |>
    dplyr::pull(species)
  ct_order <- cov_cs |> dplyr::group_by(country) |>
    dplyr::summarise(k = sum(pct_observed > 0)) |> dplyr::arrange(k) |> dplyr::pull(country)
  d <- cov_cs |>
    dplyr::mutate(species = factor(species, sp_order), country = factor(country, ct_order),
                  pct_observed = dplyr::na_if(pct_observed, 0))
  ggplot2::ggplot(d, ggplot2::aes(species, country, fill = pct_observed)) +
    ggplot2::geom_tile(colour = pal$surface, linewidth = 0.5) +
    ggplot2::scale_fill_gradientn(colours = pal$seq, limits = c(0, 100), na.value = pal$na,
                                  name = "% region-years\nobserved") +
    ggplot2::labs(x = NULL, y = NULL, title = "Monitoring coverage: country x species",
                  caption = "Grey = not monitored. Observed = positive or zero value reported, 2000-2022.") +
    theme_eu() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5))
}

# Heatmap of a species x species matrix in clustering order, with guild/host strips
plot_matrix <- function(M, order, lookup, fill_name, title, caption, type = c("r", "n")) {
  type <- match.arg(type)
  d <- as.data.frame(as.table(M[order, order]), stringsAsFactors = FALSE) |>
    stats::setNames(c("a", "b", "value")) |>
    dplyr::mutate(a = factor(a, order), b = factor(b, rev(order)))
  if (type == "n") d$value[d$value == 0] <- NA

  ann <- lookup |> dplyr::filter(species %in% order) |>
    dplyr::mutate(a = factor(species, order))
  n <- length(order)

  p <- ggplot2::ggplot(d, ggplot2::aes(a, b)) +
    ggplot2::geom_tile(ggplot2::aes(fill = value), colour = pal$surface, linewidth = 0.3)
  p <- if (type == "r") {
    # mark missing cells explicitly: the NA grey is too close to the r = 0 midpoint
    p + ggplot2::geom_point(data = dplyr::filter(d, is.na(value), as.character(a) != as.character(b)),
                            shape = 4, size = 1, colour = pal$ink2) +
      ggplot2::scale_fill_gradient2(low = pal$div_low, mid = pal$div_mid, high = pal$div_high,
                                      midpoint = 0, limits = c(-1, 1), na.value = pal$na, name = fill_name)
  } else {
    p + ggplot2::scale_fill_gradientn(colours = pal$seq, trans = "log10", na.value = pal$na, name = fill_name)
  }
  p +
    ggnewscale_guild(ann, n) +
    ggplot2::coord_equal(clip = "off", ylim = c(0.5, n + 2.6), expand = FALSE) +
    ggplot2::labs(x = NULL, y = NULL, title = title, caption = caption) +
    theme_eu(8) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5))
}

# Guild and host strips above the matrix drawn as coloured points with a legend
# encoded by shape, so identity never relies on colour alone.
ggnewscale_guild <- function(ann, n) {
  list(
    ggplot2::geom_point(data = ann, ggplot2::aes(a, n + 1.2, colour = guild, shape = guild),
                        size = 2.2, inherit.aes = FALSE),
    ggplot2::geom_point(data = ann, ggplot2::aes(a, n + 2.1, colour = host, shape = host),
                        size = 2.2, inherit.aes = FALSE),
    ggplot2::scale_colour_manual(values = c(pal$guild, pal$host), na.value = pal$ink2,
                                 name = "Guild (row 1) / host (row 2)"),
    ggplot2::scale_shape_manual(values = c(borer = 15, defoliator = 17, other = 18,
                                           `bark borer` = 15, `wood borer` = 3,
                                           conifer = 16, broadleaf = 1, both = 10),
                                na.value = 4, name = "Guild (row 1) / host (row 2)"),
    ggplot2::annotate("text", x = 0.3, y = c(n + 1.2, n + 2.1), label = c("guild", "host"),
                      hjust = 1, size = 2.5, colour = pal$ink2)
  )
}

# Country-level r for the strongest pairs (with >= 2 countries)
plot_top_pairs <- function(ts, pc, k = 15, version = "z_within") {
  top <- ts |>
    dplyr::filter(n_countries >= 2, !is.na(r_pooled)) |>
    dplyr::arrange(dplyr::desc(abs(r_pooled))) |>
    dplyr::slice_head(n = k) |>
    dplyr::mutate(pair = paste(species_a, "x", species_b))
  lev <- rev(top$pair)
  d <- pc |> dplyr::filter(version == !!version) |>
    dplyr::inner_join(dplyr::select(top, species_a, species_b, pair, transferability),
                      by = c("species_a", "species_b")) |>
    dplyr::mutate(pair = factor(pair, lev)) |>
    # alternate labels above/below the dot so neighbouring countries stay legible
    dplyr::group_by(pair) |> dplyr::arrange(r_pearson, .by_group = TRUE) |>
    dplyr::mutate(lab_v = dplyr::if_else(dplyr::row_number() %% 2 == 1, -1.1, 2.1)) |>
    dplyr::ungroup()
  top <- top |> dplyr::mutate(pair = factor(pair, lev))
  ggplot2::ggplot(d, ggplot2::aes(r_pearson, pair)) +
    ggplot2::geom_vline(xintercept = 0, colour = pal$ink2, linewidth = 0.3) +
    ggplot2::geom_point(ggplot2::aes(size = n), colour = pal$seq[3], alpha = 0.8) +
    ggplot2::geom_text(ggplot2::aes(label = country, vjust = lab_v), size = 2.3, colour = pal$ink2) +
    ggplot2::geom_point(data = top, ggplot2::aes(r_pooled, pair), shape = 124, size = 6,
                        colour = pal$ink, inherit.aes = FALSE) +
    ggplot2::geom_text(data = top, ggplot2::aes(1.02, pair, label = transferability),
                       hjust = 0, size = 2.5, colour = pal$ink2, inherit.aes = FALSE) +
    ggplot2::scale_x_continuous(limits = c(-1, 1.45), breaks = seq(-1, 1, 0.5)) +
    ggplot2::scale_size_area(max_size = 5, name = "n region-years") +
    ggplot2::labs(x = paste0("Pearson r (", version, ")"), y = NULL,
                  title = "Strongest pairs: correlation within each country",
                  caption = "Dots = countries (n >= 10); black bar = pooled r; right = transferability flag.") +
    theme_eu() +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_line(colour = "#ecebe7", linewidth = 0.3))
}
