# Step 7: guild structure.
#
# The guild/host lookup is to be supplied by SH in data/lookup/species_guild.csv
# (columns: species, guild, host). Until it exists, the analysis uses a
# PROVISIONAL lookup derived from the file's indicator columns:
#   guild: "borer" (bark and wood borers not separable from the file) or "defoliator"
#   host : TREETYPE con -> conifer, dec -> broadleaf

guild_template <- function(species_meta) {
  species_meta |>
    dplyr::transmute(
      species,
      guild = "",   # bark borer / wood borer / defoliator / other
      host  = "",   # conifer / broadleaf / both
      suggested_guild = guild_from_file,
      suggested_host  = host_from_file,
      host_tree_in_file = host_tree,
      is_aggregate, is_genus_level,
      flags_in_file = flags
    )
}

load_guild_lookup <- function(path, species_meta) {
  if (file.exists(path)) {
    user <- readr::read_csv(path, show_col_types = FALSE) |>
      dplyr::mutate(dplyr::across(c(guild, host), ~ dplyr::na_if(trimws(.x), "")))
    if (any(!is.na(user$guild))) {
      return(user |> dplyr::select(species, guild, host) |>
               dplyr::mutate(lookup_source = "user"))
    }
  }
  species_meta |>
    dplyr::transmute(species, guild = guild_from_file, host = host_from_file,
                     lookup_source = "provisional (from file)")
}

# Symmetric matrix of pooled Pearson r (version given); cells with n < 10 are NA
cor_matrix <- function(pw, version = "z_within", stat = "r_pearson") {
  d <- pw |> dplyr::filter(version == !!version)
  sp <- sort(unique(c(d$species_a, d$species_b)))
  M <- matrix(NA_real_, length(sp), length(sp), dimnames = list(sp, sp))
  val <- if (stat == "n") d$n else dplyr::if_else(d$low_n, NA_real_, d[[stat]])
  M[cbind(d$species_a, d$species_b)] <- val
  M[cbind(d$species_b, d$species_a)] <- val
  diag(M) <- if (stat == "n") NA else 1
  M
}

# Hierarchical clustering (average linkage, distance 1 - r).
# Species with fewer than `min_partners` interpretable pairs are left out.
# Remaining missing r (never co-observed) are set to 0 (distance 1) for
# clustering only; this is a pragmatic choice and is noted in the report.
cluster_species <- function(pw, version = "z_within", min_partners = 3) {
  M <- cor_matrix(pw, version)
  keep <- rowSums(!is.na(M)) - 1 >= min_partners
  M <- M[keep, keep]
  Mf <- M; Mf[is.na(Mf)] <- 0
  hc <- stats::hclust(stats::as.dist(1 - Mf), method = "average")
  list(order = rownames(M)[hc$order], hclust = hc, matrix = M,
       n_missing_set_to_zero = sum(is.na(M[upper.tri(M)])))
}

# Permutation test: mean r within groups minus mean r between groups.
# Uses interpretable pairs (n >= 10). Labels are permuted across species.
group_permutation_test <- function(pw, lookup, group_var, version = "z_within",
                                   n_perm = 9999, seed = 20261008) {
  d <- pw |>
    dplyr::filter(version == !!version, !low_n, !is.na(r_pearson)) |>
    dplyr::select(species_a, species_b, r = r_pearson)
  lab <- stats::setNames(lookup[[group_var]], lookup$species)
  d <- d |> dplyr::filter(!is.na(lab[species_a]), !is.na(lab[species_b]))
  sp <- sort(unique(c(d$species_a, d$species_b)))
  ia <- match(d$species_a, sp); ib <- match(d$species_b, sp)
  labs <- unname(lab[sp])

  stat_fun <- function(l) {
    same <- l[ia] == l[ib]
    mean(d$r[same]) - mean(d$r[!same])
  }
  obs <- stat_fun(labs)
  set.seed(seed)
  perm <- replicate(n_perm, stat_fun(sample(labs)))
  same <- labs[ia] == labs[ib]
  tibble::tibble(
    grouping = group_var, version = version,
    n_species = length(sp), n_pairs = nrow(d),
    n_within = sum(same), n_between = sum(!same),
    mean_r_within = mean(d$r[same]), mean_r_between = mean(d$r[!same]),
    median_r_within = stats::median(d$r[same]), median_r_between = stats::median(d$r[!same]),
    diff_observed = obs,
    p_perm_one_sided = (1 + sum(perm >= obs)) / (n_perm + 1),
    n_perm = n_perm, seed = seed
  )
}
