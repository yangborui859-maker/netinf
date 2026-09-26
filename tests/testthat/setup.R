shared_data <- make_test_data(n = 100, seed = 1)
shared_data_small <- make_test_data(n = 40, seed = 11)
set.seed(1)
shared_boot <- bootnet_with_indices(shared_data, nBoots = 1000,
                                    default = "EBICglasso")
set.seed(1)
shared_boot_small <- bootnet_with_indices(shared_data_small, nBoots = 100,
                                          default = "EBICglasso")

shared_empic <- calculate_empirical_influence(shared_boot)

shared_empic_small <- calculate_empirical_influence(shared_boot_small)

shared_gcd_all <- calculate_centrality_gCD(data = shared_data,
                                           boot_result = shared_boot,
                                           case_ids = NULL,
                                           metric = c('Strength','Closeness', 'Betweenness'),
                                           nCores = 1,
                                           default = "EBICglasso"
                                             )

shared_loo <- leave_one_out_analysis(shared_empic_small, shared_data_small,
                                           top_n = 1,
                                           default = "EBICglasso",
                                           verbose = FALSE
                                             )

shared_loo_multi <- leave_one_out_analysis(shared_empic_small, shared_data_small,
                                           remove_cases = c(1, 2, 3),
                                           default = "EBICglasso",
                                           verbose = FALSE
                                             )

shared_diag <- influence_diagnostic(data = shared_boot_small,
                                           top_n = 1,
                                           centrality_metrics = c("Strength","Closeness"),
                                           verbose = FALSE
                                             )

vec_influence <- as.numeric(shared_empic_small)
named_vec <- setNames(vec_influence, seq_along(vec_influence))
mat_influence <- matrix(vec_influence, ncol = 1,
                        dimnames = list(NULL, "influence"))