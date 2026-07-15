abc_acl |>
  dplyr::filter(Time == 2024) |>
  dplyr::distinct() |>
  tidyr::separate(Var, into = c("species", "var"), sep = "_") |>
  tidyr::pivot_wider(names_from = var, values_from = Value) |>
  dplyr::mutate(frac = Catch / Quota) |>
  View()

abc_acl |>
  dplyr::filter(Time == 2023) |>
  dplyr::distinct() |>
  tidyr::separate(Var, into = c("species", "var"), sep = "_") |>
  tidyr::pivot_wider(names_from = var, values_from = Value) |>
  dplyr::mutate(frac = Catch / Quota) |>
  View()
