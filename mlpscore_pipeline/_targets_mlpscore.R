library(targets)
library(tarchetypes)
library(dplyr)
library(purrr)
library(ggplot2)
library(magrittr)

# Propagate active parameter column from environment variable to background workers
val_col_env <- Sys.getenv("MLPSCORE_VALUE_COL", unset = "")
if (val_col_env != "") {
    options(MLPScore.value_col = val_col_env)
}


# Set target options:
tar_option_set(
    packages = c("dplyr", "tidyr", "ggplot2", "purrr", "tibble", "stringr",
                 "readr", "forcats", "tidymodels", "parsnip", "recipes",
                 "workflows", "rsample", "tune", "yardstick", "lme4", "gbm",
                 "twang", "readxl", "writexl", "xgboost", "ranger", "here",
                 "crew", "MLPScore"),
    format = "rds",
    controller = if (Sys.getenv("MLPSCORE_NO_CREW", unset = "") != "") NULL else crew::crew_controller_local(workers = max(1L, min(parallelly::availableCores() - 1L, 4L)))
)

# Source functions
tar_source()

# Define scenario values (Expanded for Figs 4-7)
rho_vals <- c(0, 0.2, 0.5, 0.8)
int_vals <- seq(-4, 4, by = 1)

# Define scenarios dataframe
scenarios <- expand.grid(
    rho_uv = rho_vals,
    adherence_intercept = int_vals
) %>%
    mutate(scenario_id = paste0(
        "rho", gsub("\\.", "_", as.character(rho_uv)),
        "_int", gsub("\\.", "_", gsub("-", "neg", as.character(adherence_intercept)))
    ))

# Define the mapping
scenario_map <- tar_map(
    values = scenarios,
    names = scenario_id,
    tar_target(
        sim_data,
        {
            p <- base_params
            p[["rho_uv"]] <- as.numeric(rho_uv)
            p[["adherence_intercept"]] <- as.numeric(adherence_intercept)
            # Fixed: Ensure default effects are passed if not in Excel
            p$adherence_severity <- 0.5
            p$adherence_comorbidity <- 0.2
            p$outcome_severity <- 1.0
            p$outcome_comorbidity <- 0.5

            # Use deterministic but unique seed per scenario
            # Simple hash-like numeric value: (rho_uv * 10) + (adherence_intercept * 100)
            # But let's use something robust
            s_seed <- as.integer(abs(rho_uv * 1000 + adherence_intercept * 100)) + 12345
            run_simulation(p, seed = s_seed)
        }
    ),
    tar_target(
        weighted_data_obs,
        estimate_ps_and_weights(sim_data, level = "obs", compliance_threshold = as.numeric(base_params$compliance_threshold %||% 0.80))
    ),
    tar_target(
        pops_obs,
        get_comparison_populations(weighted_data_obs, threshold_pt = as.numeric(base_params$compliance_threshold %||% 0.80), threshold_obs = as.numeric(base_params$compliance_threshold %||% 0.80))
    ),
    tar_target(
        summary_obs,
        summarize_comparisons(weighted_data_obs, pops_obs, level = "obs") %>%
            mutate(rho_uv = rho_uv, adherence_intercept = adherence_intercept)
    ),
    tar_target(
        weighted_data_pt,
        estimate_ps_and_weights(sim_data, level = "pt", compliance_threshold = as.numeric(base_params$compliance_threshold %||% 0.80))
    ),
    tar_target(
        pops_pt,
        get_comparison_populations(weighted_data_pt, threshold_pt = as.numeric(base_params$compliance_threshold %||% 0.80), threshold_obs = as.numeric(base_params$compliance_threshold %||% 0.80))
    ),
    tar_target(
        summary_pt,
        summarize_comparisons(weighted_data_pt, pops_pt, level = "pt") %>%
            mutate(rho_uv = rho_uv, adherence_intercept = adherence_intercept)
    )
)

list(
    # Track the input Excel file
    tar_target(
        sim_params_file,
        "simulation_params.xlsx",
        format = "file"
    ),
    # Read base parameters
    tar_target(
        base_params,
        read_sim_params(sim_params_file)
    ),

    # The map itself
    scenario_map,

    # Combine results
    tar_combine(
        all_results_obs,
        scenario_map$summary_obs,
        command = bind_rows(!!!.x)
    ),
    tar_combine(
        all_results_pt,
        scenario_map$summary_pt,
        command = bind_rows(!!!.x)
    ),
    tar_target(
        all_results,
        bind_rows(all_results_obs, all_results_pt)
    ),

    # Define true effects for bias calculation
    tar_target(
        true_effects,
        {
            # Dynamically compute true effects relative to the reference arm using get_true_effects
            effects <- MLPScore::get_true_effects(base_params)
            # Remove the first cohort since it is the reference (relative effect = 0)
            effects[-1]
        }
    ),

    # Representative scenario for Figures 1-3 (Moderate confounding, middle adherence)
    tar_target(
        rep_results,
        all_results %>% filter(rho_uv == 0.5, adherence_intercept == 0)
    ),
    tar_target(
        fig1,
        plot_itt_vs_ps_pt(rep_results %>% filter(Level == "pt"), true_effects) +
            labs(title = "Figure 1: Comparison of PS estimators (Pt Level, Rho=0.5)")
    ),
    tar_target(
        fig2,
        plot_pp_vs_ps_pt(rep_results %>% filter(Level == "pt"), true_effects) +
            labs(title = "Figure 2: ITT vs Naive vs PS (Pt Level, Rho=0.5)")
    ),
    tar_target(
        fig3,
        plot_pp_vs_ps_obs(rep_results %>% filter(Level == "obs"), true_effects) +
            labs(title = "Figure 3: PP(obs) vs PS(obs) (Obs Level, Rho=0.5)")
    ),

    # Render Figures 4-7 using descriptive wrapper functions
    tar_target(
        fig4,
        plot_bias_vs_confounding_pt(all_results_pt, true_effects) +
            labs(title = "Figure 4: Bias vs Confounding Strength (Pt Level)")
    ),
    tar_target(
        fig5,
        plot_bias_vs_confounding_obs(all_results_obs, true_effects) +
            labs(title = "Figure 5: Bias vs Confounding Strength (Obs Level)")
    ),
    tar_target(
        fig6,
        plot_bias_vs_adherence_pt(all_results_pt, true_effects) +
            labs(
                title = "Figure 6: Bias vs Observed Adherence Rate (Pt Level)",
                x = "Observed Adherence Rate"
            )
    ),
    tar_target(
        fig7,
        plot_bias_vs_adherence_obs(all_results_obs, true_effects) +
            labs(
                title = "Figure 7: Bias vs Observed Adherence Rate (Obs Level)",
                x = "Observed Adherence Rate"
            )
    ),

    # Generic bias sweep utility (used by descriptive wrappers above)
    tar_target(
        fig_bias_sweep_pt_rho,
        plot_bias_sweep(all_results_pt, true_effects, x_var = "rho_uv") +
            labs(title = "Bias Sweep vs Confounding (Pt Level)")
    ),

    tar_target(
        analysis_report,
        {
            # Explicitly depend on upstream targets to establish build order
            force(base_params)
            force(all_results)
            force(fig1)
            force(fig2)
            force(fig3)
            force(fig4)
            force(fig5)
            force(fig6)
            force(fig7)

            if (Sys.which("quarto") == "") {
                stop("quarto CLI not found on PATH. Install from https://quarto.org")
            }
            # Render in the analysis/ folder so relative store and figures resolve correctly
            old_wd <- setwd("analysis")
            on.exit(setwd(old_wd), add = TRUE)
            
            # Run quarto CLI to render to PDF
            status <- system2("quarto", c("render", "analysis.qmd"), stdout = TRUE, stderr = TRUE)
            if (!is.null(attr(status, "status")) && attr(status, "status") != 0) {
                stop("quarto render failed:\n", paste(status, collapse = "\n"))
            }
            
            "analysis/analysis.pdf"
        },
        format = "file",
        deployment = "main"
    )
    # NOTE: the package tutorial and trial-analysis vignette now cover these workflows.
    # See vignettes/mlpscore-tutorial.Rmd and vignettes/vignette-1-trial-analysis.Rmd
)
