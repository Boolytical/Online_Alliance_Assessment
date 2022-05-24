# changes:
# - renamed "personal" to "Personal" and "goals" to "Goals"
# - renamed "ML" and "DWLS" to "ML" and "DWLS" since both are variance constrained now
# - sum score calculation dependent on t.test between raw sum scores and summed factor scores


###########################################
######## Create correlation matrix ########
###########################################

correlation_matrix = function(){
        # Read data
        data = read.csv("separated_counselor_client.sav", header = TRUE)
        
        # Make a table of the upper side of a correlation matrix
        cormat = round(cor(data[3:13], method = "spearman"), 2) 
        cormat[lower.tri(cormat)] = NA
        melted_cormat = melt(cormat, na.rm = TRUE)
        
        # Create the correlation plot
        ggheatmap = ggplot(melted_cormat, aes(Var2, Var1, fill = value)) +
                geom_tile(color = "white")+
                scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                                     midpoint = 0, limit = c(-1,1), space = "Lab", 
                                     name="Spearman\nCorrelation") +
                theme_minimal() + 
                theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                                 size = 12, hjust = 1))+
                coord_fixed()

        ggresult = ggheatmap + 
                geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
                theme(axis.title.x = element_blank(),
                        axis.title.y = element_blank(),
                        panel.grid.major = element_blank(),
                        panel.border = element_blank(),
                        panel.background = element_blank(),
                        axis.ticks = element_blank(),
                        legend.justification = c(1, 0),
                        legend.position = c(0.6, 0.7),
                        legend.direction = "horizontal")+
                guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                                             title.position = "top", title.hjust = 0.5))
        return(ggresult)
}

############################################
# Multi-group confirmatory factor analysis #
############################################

MGCFA = function(selection, ID, type, analysis, plot){
        cat(analysis, 'invariance analysis using', type, 'settings on data;', selection, "\n", "\n")
        names = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q7", "Q8", "Q9", "Q6", "Q10", "Q11", "Personal", "  Goals  ")
        
        # Read the data
        data_wide = read.csv("counselor_client_op_ID.sav", header = TRUE)
        data_long = read.csv("separated_counselor_client.sav", header = TRUE)
        
        if (selection == "all"){
                data = data_long
        } 
        
        else if (selection == "organization"){ 
                if (is.character(ID) == TRUE){
                        data = data_long[data_long[['Organization']] == ID, ]
                } 
                else {
                        stop("No valid ID option. If the selection is organization, the ID should be a string with the organization name.")
                }
        }
        
        else if (selection == "individual"){
                data = data_long 
                
                if (is.numeric(ID) == TRUE & is.na(ID) == FALSE){
                        data_ID = data_wide[data_wide[['ID']] == ID, ]
                }
                
                else if(ID != "all"){
                        stop("No valid ID option. If the selection is individual, the ID should be an integer with the pairing code. For an overview of the client-counselor pairs, set ID = \"all\". ")
                }
        }
        
        else { 
                        stop('No valid data selection option. Specify "all", "organization", or "individual".')
        }
        if (ID == "all") {
                if(selection != "individual"){
                        stop("For an overview of the client-counselor pairs, set selection = \"individual\". \n  To analyse all data, set ID = NaN.")
                }
        }
        
        # Run CFA using ML loss function
        if (type == "ML"){ 
                
                cfa_factors =  '# Set first factor loading to NA so that it will be estimated
                                Personal =~ NA*Q1 + Q2 + Q3 + Q4 + Q5 + Q7 + Q8 + Q9
                                Goals =~ NA*Q6 + Q10 + Q11

                                # Fix variances to 1
                                Personal ~~ c(1,1)*Personal
                                Goals ~~ c(1,1)*Goals'
        
                # Run Configural Invariance
                configural_fit = cfa(model = cfa_factors,
                                     data = data,
                                     meanstructure = TRUE,
                                     std.lv = FALSE,
                                     group = 'Role')

                # Run Metric Invariance
                metric_fit = cfa(model = cfa_factors,
                                 data = data,
                                 meanstructure = TRUE,
                                 std.lv = FALSE,
                                 group = 'Role',
                                 group.equal = c("loadings"))

                # Run Scalar Invariance
                scalar_fit = cfa(model = cfa_factors,
                                 data = data,
                                 meanstructure = TRUE,
                                 std.lv = FALSE,
                                 group = 'Role',
                                 group.equal = c("loadings", "intercepts"))

                # Run Strict Invariance
                strict_fit = cfa(model = cfa_factors,
                                 data = data,
                                 meanstructure = TRUE,
                                 std.lv = FALSE,
                                 group = 'Role',
                                 group.equal = c("loadings", "intercepts", "residuals"))
                
                
                # Return the results of the analysis
                if (analysis == "configural"){
                        if (plot == TRUE){
                                png('ML_configural_plot.png', height = 1000, width = 1000, res = 100)
                                par(mfrow = c(1, 2))
                                semPaths(configural_fit, whatLabels = 'std', layout = 'tree2', edge.label.cex = 1.2,
                                         intercepts = FALSE, residuals = FALSE, nodeLabels = names,
                                         sizeMan = 7, sizeLat = 15, sizeInt = 4, rotation = 2, ask = FALSE)
                                dev.off()
                        }
                        
                        # Save factor scores for individual feedback 
                        factor_scores = lavPredict(configural_fit, type = "ov", assemble = TRUE)
                        
                        if(selection != "individual"){
                                return(summary(configural_fit,
                                               standardized = TRUE,
                                               rsquare = TRUE,
                                               fit.measure = TRUE))
                        }
                }
                else if (analysis == "metric"){
                        if (plot == TRUE){
                                png('ML_metric_plot.png', height = 1000, width = 1000, res = 100)
                                par(mfrow = c(1, 2))
                                semPaths(metric_fit, whatLabels = 'std', layout = 'tree2', edge.label.cex = 1.2,
                                         intercepts = FALSE, residuals = FALSE, nodeLabels = names,
                                         sizeMan = 7, sizeLat = 15, sizeInt = 4, rotation = 2, ask = FALSE)
                                dev.off()
                        }
                        
                        # Save factor scores for individual feedback
                        factor_scores = lavPredict(metric_fit, type = "ov", assemble = TRUE)
                        
                        if(selection != "individual"){
                                return(summary(metric_fit,
                                               standardized = TRUE,
                                               rsquare = TRUE,
                                               fit.measure = TRUE))
                        }

                }
                else if (analysis == "scalar"){
                        if (plot == TRUE){
                                png('ML_scalar_plot.png', height = 1000, width = 1000, res = 100)
                                par(mfrow = c(1, 2))
                                semPaths(scalar_fit, whatLabels = 'std', layout = 'tree2', edge.label.cex = 1.2,
                                         intercepts = FALSE, residuals = FALSE, nodeLabels = names,
                                         sizeMan = 7, sizeLat = 15, sizeInt = 4, rotation = 2, ask = FALSE)
                                dev.off()
                        }
                        
                        # Save factor score for individual feedback                         
                        factor_scores = lavPredict(scalar_fit, type = "ov", assemble = TRUE)
                        
                        if(selection != "individual"){
                                return(summary(scalar_fit,
                                               standardized = TRUE,
                                               rsquare = TRUE,
                                               fit.measure = TRUE))
                        }
                }
                else if (analysis == "strict"){
                        if (plot == TRUE){
                        png('ML_strict_plot.png', height = 1000, width = 1000, res = 100)
                        par(mfrow = c(1, 2))
                        semPaths(strict_fit, whatLabels = 'std', layout = 'tree2', edge.label.cex = 1.2,
                                 intercepts = FALSE, residuals = FALSE, nodeLabels = names,
                                 sizeMan = 7, sizeLat = 15, sizeInt = 4, rotation = 2, ask = FALSE)
                        dev.off()
                        }
                        
                        # Save factor scores for individual feedback                         
                        factor_scores = lavPredict(strict_fit, type = "ov", assemble = TRUE)

                        if(selection != "individual"){
                                return(summary(strict_fit,
                                               standardized = TRUE,
                                               rsquare = TRUE,
                                               fit.measure = TRUE))
                        }
                }
                else if (analysis == "table_all" & selection != "individual"){
                        # Do Chi-square tests between nested models
                        p_metric = anova(configural_fit, metric_fit)
                        p_scalar = anova(metric_fit, scalar_fit)
                        p_strict = anova(scalar_fit, strict_fit)

                        # Compare all the models
                        table_fit = matrix(NA, nrow = 4, ncol = 7)
                        colnames(table_fit) = c("Model", "X2", "df", "CFI", "RMSEA", "SRMR", "p-values")
                        table_fit[1, ] = c("Configural model", round(fitmeasures(configural_fit, c('chisq', 'df', 'cfi', 'cfq', 'rmsea', 'srmr')), 3),
                                           round(fitMeasures(configural_fit, 'pvalue'), 3))
                        table_fit[2, ] = c("Metric model", round(fitmeasures(metric_fit, c('chisq', 'df', 'cfi', 'cfq', 'rmsea', 'srmr')), 3),
                                           round(p_metric$`Pr(>Chisq)`[2], 3))
                        table_fit[3, ] = c("Scalar model", round(fitmeasures(scalar_fit, c('chisq', 'df', 'cfi', 'cfq', 'rmsea', 'srmr')), 3),
                                           round(p_scalar$`Pr(>Chisq)`[2], 3))
                        table_fit[4, ] = c("Strict Error model", round(fitmeasures(strict_fit, c('chisq', 'df', 'cfi', 'cfq', 'rmsea', 'srmr')), 3),
                                           round(p_strict$`Pr(>Chisq)`[2], 3))
                        return(kable(table_fit))
                }
                else if (analysis == "table_all" & selection == "individual"){
                        stop('For an individual ID, no table of all methods can be generated. Specify "configural", "metric", "scalar", "strict".')
                }
                
                else {
                        stop('No valid model was picked. Specify "configural", "metric", "scalar", "strict" or "table_all".')
                }

        } 
        
        # Run CFA with variance scaling using DWLS loss function
        else if (type == "DWLS"){

                ordered_factors = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10", "Q11")
                
                DWLS_cfa_factors =  'Personal =~ NA*Q1 + Q2 + Q3 + Q4 + Q5 + Q7 + Q8 + Q9
                                     Goals =~ NA*Q6 + Q10 + Q11
                                          
                                     Personal ~~ c(1,1)*Personal
                                     Goals ~~ c(1,1)*Goals'

                #### Alteration on Lavaan error ####
                # Remove Q3 == 1 for MGCFA
                print(paste('Number of rows that will be removed: ', nrow(data) - nrow(data[data$Q3 != 1, ])))
                data = data[data$Q3 != 1, ]

                # Run Configural Invariance
                ordered_configural_fit = cfa(model = DWLS_cfa_factors,
                                             data = data,
                                             meanstructure = TRUE,
                                             group = 'Role',
                                             ordered = ordered_factors)

                # Run Metric Invariance
                ordered_metric_fit = cfa(model = DWLS_cfa_factors,
                                         data = data,
                                         meanstructure = TRUE,
                                         group = 'Role',
                                         group.equal = c("loadings"),
                                         ordered = ordered_factors)

                # Run Scalar Invariance
                ordered_scalar_fit = cfa(model = DWLS_cfa_factors,
                                         data = data,
                                         meanstructure = TRUE,
                                         group = 'Role',
                                         group.equal = c("loadings", "intercepts"),
                                         ordered = ordered_factors)

                # Run Strict Invariance
                ordered_strict_fit = cfa(model = DWLS_cfa_factors,
                                         data = data,
                                         meanstructure = TRUE,
                                         group = 'Role',
                                         group.equal = c("loadings", "intercepts", "residuals"),
                                         ordered = ordered_factors)

                # Return the results of the analysis
                if (analysis == "configural"){
                        if (plot == TRUE){
                                png('DWLS_configural_plot.png', height = 1000, width = 1000, res = 100)
                                par(mfrow = c(1, 2))
                                semPaths(ordered_configural_fit, whatLabels = 'std', layout = 'tree2', edge.label.cex = 1.2,
                                         intercepts = FALSE, residuals = FALSE, nodeLabels = names,
                                         sizeMan = 7, sizeLat = 15, sizeInt = 4, rotation = 2, ask = FALSE)
                                dev.off()
                        }
                        
                        # Save factor loadings for individual feedback                         
                        factor_scores = lavPredict(ordered_configural_fit, type = "ov", assemble = TRUE)
                        
                        if(selection != "individual"){
                                return(summary(ordered_configural_fit,
                                               standardized = TRUE,
                                               rsquare = TRUE,
                                               fit.measure = TRUE))
                        }
                }
                else if (analysis == "metric"){
                        if (plot == TRUE){
                                png('DWLS_metric_plot.png', height = 1000, width = 1000, res = 100)
                                par(mfrow = c(1, 2))
                                semPaths(ordered_metric_fit, whatLabels = 'std', layout = 'tree2', edge.label.cex = 1.2,
                                         intercepts = FALSE, residuals = FALSE, nodeLabels = names,
                                         sizeMan = 7, sizeLat = 15, sizeInt = 4, rotation = 2, ask = FALSE)
                                dev.off()
                        }
                        
                        # Save factor scores for individual feedback                         
                        factor_scores = lavPredict(ordered_metric_fit, type = "ov", assemble = TRUE)
                        
                        
                        if(selection != "individual"){
                                return(summary(ordered_metric_fit,
                                               standardized = TRUE,
                                               rsquare = TRUE,
                                               fit.measure = TRUE))
                        }
                }
                else if (analysis == "scalar"){
                        if (plot == TRUE){
                                png('DWLS_scalar_plot.png', height = 1000, width = 1000, res = 100)
                                par(mfrow = c(1, 2))
                                semPaths(ordered_scalar_fit, whatLabels = 'std', layout = 'tree2', edge.label.cex = 1.2,
                                         intercepts = FALSE, residuals = FALSE, nodeLabels = names,
                                         sizeMan = 7, sizeLat = 15, sizeInt = 4, rotation = 2, ask = FALSE)
                                dev.off()
                        }
                        
                        # Save factor scores for individual feedback                         
                        factor_scores = lavPredict(ordered_scalar_fit, type = "ov", assemble = TRUE)
                        
                        if(selection != "individual"){
                                return(summary(ordered_scalar_fit,
                                               standardized = TRUE,
                                               rsquare = TRUE,
                                               fit.measure = TRUE))
                        }
                }
                else if (analysis == "strict"){
                        if (plot == TRUE){
                                png('DWLS_strict_plot.png', height = 1000, width = 1000, res = 100)
                                par(mfrow = c(1, 2))
                                semPaths(ordered_strict_fit, whatLabels = 'std', layout = 'tree2', edge.label.cex = 1.2,
                                         intercepts = FALSE, residuals = FALSE, nodeLabels = names,
                                         sizeMan = 7, sizeLat = 15, sizeInt = 4, rotation = 2, ask = FALSE)
                                dev.off()
                        }
                        
                        # Save factor scores for individual feedback                        
                        factor_scores = lavPredict(ordered_strict_fit, type = "ov", assemble = TRUE)
                        
                        if(selection != "individual"){
                                return(summary(ordered_strict_fit,
                                               standardized = TRUE,
                                               rsquare = TRUE,
                                               fit.measure = TRUE))
                        }
                }
                else if (analysis == "table_all" & selection != "individual"){
                        # Do Chi-square tests between nested models
                        ordered_p_metric = anova(ordered_configural_fit, ordered_metric_fit)
                        ordered_p_scalar = anova(ordered_metric_fit, ordered_scalar_fit)
                        ordered_p_strict = anova(ordered_scalar_fit, ordered_strict_fit)

                        # Compare all the models
                        ordered_table_fit = matrix(NA, nrow = 4, ncol = 7)
                        colnames(ordered_table_fit) = c("Model", "X2", "df", "CFI", "RMSEA", "SRMR", "p-value")
                        ordered_table_fit[1, ] = c("Configural model", round(fitmeasures(ordered_configural_fit, c('chisq', 'df', 'cfi', 'cfq', 'rmsea', 'srmr')), 3),
                                                   round(fitMeasures(ordered_configural_fit, 'pvalue'), 3))
                        ordered_table_fit[2, ] = c("Metric model", round(fitmeasures(ordered_metric_fit, c('chisq', 'df', 'cfi', 'cfq', 'rmsea', 'srmr')), 3),
                                                   round(ordered_p_metric$`Pr(>Chisq)`[2], 3))
                        ordered_table_fit[3, ] = c("Scalar model", round(fitmeasures(ordered_scalar_fit, c('chisq', 'df', 'cfi', 'cfq', 'rmsea', 'srmr')), 3),
                                                   round(ordered_p_scalar$`Pr(>Chisq)`[2], 3))
                        ordered_table_fit[4, ] = c("Strict Error model", round(fitmeasures(ordered_strict_fit, c('chisq', 'df', 'cfi', 'cfq', 'rmsea', 'srmr')), 3),
                                                   round(ordered_p_strict$`Pr(>Chisq)`[2], 3))
                        return(kable(ordered_table_fit))
                }
                else if (analysis == "table_all" & selection == "individual"){
                        stop('For an individual ID, no table of all methods can be generated. Specify "configural", "metric", "scalar", "strict".')
                }
                else {
                        stop('No valid model was picked. Specify "configural", "metric", "scalar", "strict" or "table_all".')
                }
        } 
        # Run no CFA analysis
        else {
                stop('No valid analysis type was selected. Specify "ML" or "DWLS".')
        }
        
        # Individual feedback
        if (selection == "individual"){
                        cat("Client and counselor ratings per question for counselor ID:", ID, "\n \n")
                
                        # Raw scores
                        client_scores = data_wide[, c(paste0("Q", 1:11, ".client"), "ID")]
                        counselor_scores = data_wide[, c(paste0("Q", 1:11, ".counselor"), "ID")]
                        
                        # Calculate raw sum scores and summed factor scores for comparison by t-test
                        Personal_sum_raw = rowSums(data[, c(3:7, 9:11)])
                        factor_sum_Personal = rowSums(factor_scores[, 1:8])
                        Goals_sum_raw = rowSums(data[, c(8, 12:13)])
                        factor_sum_Goals = rowSums(factor_scores[, 9:11])
                        
                        
                        # Check for significant difference between raw sum scores and summed factor scores
                        # If statistically the same: use raw sum scores for easier interpretation
                        
                        if(t.test(Personal_sum_raw, factor_sum_Personal)$p.value > 0.05 &
                           t.test(Goals_sum_raw, factor_sum_Goals)$p.value > 0.05){
                                
                                if(ID != "all"){
                                        # Create table only for one specific pair using raw scores
                                        individual_descriptives = data.frame(c(paste0("Q", 1:11), "", 
                                                                               "Sum Personal", "Sum Goals",
                                                                               "Total", 
                                                                               " ",
                                                                               "% rating Personal",
                                                                               "% rating Goals",
                                                                               "% rating Total"),
                                                                             row.names = 1)
                                        client_scores = client_scores[client_scores$ID == ID, -12]
                                        counselor_scores = counselor_scores[counselor_scores$ID == ID, -12]
                                } 
                                
                                # Use raw sum scores
                                sum_Personal_client = rowSums(client_scores[c(1:5, 7, 8:9)])
                                sum_Personal_counselor = rowSums(counselor_scores[c(1:5, 7, 8:9)])
                                satisfaction_Personal_client = round(((sum_Personal_client / (8*3)) * 100), 2)
                                satisfaction_Personal_counselor = round(((sum_Personal_counselor / (8*3)) * 100), 2)
                                
                                sum_Goals_client = rowSums(client_scores[c(6, 10:11)])
                                sum_Goals_counselor = rowSums(counselor_scores[c(6, 10:11)])
                                satisfaction_Goals_client = round(((sum_Goals_client / (3*3)) * 100), 2)
                                satisfaction_Goals_counselor = round(((sum_Goals_counselor / (3*3)) * 100), 2)
                                
                                sum_total_client = rowSums(client_scores[-12])
                                sum_total_counselor = rowSums(counselor_scores[-12])
                                satisfaction_total_client = round(((sum_total_client / (11*3)) * 100), 2)
                                satisfaction_total_counselor = round(((sum_total_counselor / (11*3)) * 100), 2)
                                
                                
        
                        } else{
                                cat("Sum scores are calculated using rounded summed factor scores. \n")
                                
                                factor_scores = cbind(factor_scores, ID = data$ID)

                                # Only consider factor scores for complete and unique client-counselor pairs 
                                factor_scores = factor_scores %>%
                                        group_by(ID, Role) %>%
                                        filter(n() == 1)
                                
                                factor_scores = factor_scores[factor_scores$ID %in% data_wide$ID,]
                                factor_scores = factor_scores %>% 
                                        distinct
                                
                                if(ID != "all"){
                                        # Create table only for one specific pair using factor scores
                                        individual_descriptives = data.frame(c(paste0("Q", 1:11), "", 
                                                                               "Rounded factor sum Personal", "Rounded factor sum Goals",
                                                                               "Total rounded factor sum", 
                                                                               " ",
                                                                               "% rating Personal",
                                                                               "% rating Goals",
                                                                               "% rating Total"), 
                                                                             row.names = 1)
                                        client_scores = client_scores[client_scores$ID == ID, -12]
                                        counselor_scores = counselor_scores[counselor_scores$ID == ID, -12]
                                        factor_scores = factor_scores[factor_scores[['ID']] == ID,]
                                        
                                        cat("Due to rounding, the total rounded factor sum may not be the sum of rounded factor sums of Personal and Goals. \n") 
                                        cat("Scores per question are the original responses. \n")
                                } 
                                
                                # Use summed factor scores
                                sum_Personal_client = rowSums(factor_scores[factor_scores[['Role']] == 'client', 1:8])
                                sum_Personal_counselor = rowSums(factor_scores[factor_scores[['Role']] == 'counselor', 1:8])
                                satisfaction_Personal_client = ((sum_Personal_client / (8*3)) * 100)
                                satisfaction_Personal_counselor = ((sum_Personal_counselor / (8*3)) * 100)
                                
                                sum_Goals_client = rowSums(factor_scores[factor_scores[['Role']] == 'client', 9:11])
                                sum_Goals_counselor = rowSums(factor_scores[factor_scores[['Role']] == 'counselor', 9:11])
                                satisfaction_Goals_client = ((sum_Goals_client / (3*3)) * 100)
                                satisfaction_Goals_counselor = ((sum_Goals_counselor / (3*3)) * 100)
                                
                                sum_total_client = rowSums(factor_scores[factor_scores[['Role']] == 'client', 1:11])
                                sum_total_counselor = rowSums(factor_scores[factor_scores[['Role']] == 'counselor', 1:11])
                                satisfaction_total_client = (sum_total_client / (11*3)) * 100
                                satisfaction_total_counselor = (sum_total_counselor / (11*3)) * 100
                        }
                        
        
                        if(ID != "all"){
                                # Original answers
                                individual_descriptives[1:11, 1] = unlist(client_scores)
                                individual_descriptives[1:11, 2] = unlist(counselor_scores)
                                
                                # Sum scores
                                ## Personal Bond
                                individual_descriptives[13, 1] = round(sum_Personal_client)
                                individual_descriptives[13, 2] = round(sum_Personal_counselor)
                                ## Agreement on tasks and goals
                                individual_descriptives[14, 1] = round(sum_Goals_client)
                                individual_descriptives[14, 2] = round(sum_Goals_counselor)
                                ## Total
                                individual_descriptives[15, 1] = round(sum_total_client)
                                individual_descriptives[15, 2] = round(sum_total_counselor)
                                
                                # Percentage of satisfaction
                                ## Personal Bond
                                individual_descriptives[17, 1] = paste0(satisfaction_Personal_client, "%")
                                individual_descriptives[17, 2] = paste0(satisfaction_Personal_counselor, "%")
                                individual_descriptives[17, 3] = paste0(round(abs(satisfaction_Personal_client - satisfaction_Personal_counselor), 2), "% difference")
                                ## Agreement on tasks and goals
                                individual_descriptives[18, 1] = paste0(satisfaction_Goals_client, "%")
                                individual_descriptives[18, 2] = paste0(satisfaction_Goals_counselor, "%")
                                individual_descriptives[18, 3] = paste0(round(abs(satisfaction_Goals_client - satisfaction_Goals_counselor), 2), "% difference")
                                ## Total
                                individual_descriptives[19, 1] = paste0(satisfaction_total_client, "%")
                                individual_descriptives[19, 2] = paste0(satisfaction_total_counselor, "%")
                                individual_descriptives[19, 3] = paste0(round(abs(satisfaction_total_client - satisfaction_total_counselor), 2), "% difference")
                                
                                options(knitr.kable.NA = '')
                                return(kable(individual_descriptives, col.names = c("Client", "Counselor", "")))
                        } else{
                                # Calculate differences in the percentage of satisfaction per factor and overall
                                differences = cbind.data.frame(ID = data_wide$ID,
                                                    PersonalDiff = abs(satisfaction_Personal_client - satisfaction_Personal_counselor),
                                                    GoalsDiff = abs(satisfaction_Goals_client - satisfaction_Goals_counselor),
                                                    TotalSumDiff = abs(satisfaction_total_client - satisfaction_total_counselor))
                                
                                # Create categories of differences
                                binned_differences = apply(differences[, 2:4], 2, function(x){
                                                        cut(x, breaks = c(seq(0, 25, 5), 100), 
                                                            right = F, 
                                                            include.lowest = T)}) %>%
                                        as.data.frame(stringsAsFactors = TRUE) %>%
                                        cbind(differences$ID)
                                
                                        for(i in 2:ncol(binned_differences) - 1){
                                                binned_differences[, i] = ordered(binned_differences[, i], 
                                                                                  levels = c("[0,5)",
                                                                                             "[5,10)",
                                                                                             "[10,15)",
                                                                                             "[15,20)",
                                                                                             "[20,25)",
                                                                                             "[25,100]"))
                                        }
                                
                                
                                large_differences = apply(differences[, -1], 1, function(x) any(x >= 25)) %>%
                                        differences[., 'ID']
                                
                                proportion_high_difference_pairs = round((length(large_differences) / nrow(differences)) * 100, 2)
                                
                                if(plot == TRUE){ 
                                        # Plot observed pairwise differences in satisfaction percentage
                                        labels = c("0-5%",
                                                   "5-10%",
                                                   "10-15%",
                                                   "15-20%",
                                                   "20-25%",
                                                   ">= 25%")
                                        
                                        ## Personal Bond
                                        gg_Personal = suppressWarnings(
                                                  ggplot(binned_differences, aes(x = 1:100))+
                                                        geom_bar(aes(x = PersonalDiff)) + 
                                                        scale_x_discrete(drop = FALSE,
                                                                         labels = labels) +
                                                        xlab("Pair differences: Personal Bond") +
                                                        ylab("Frequency") +
                                                        theme(panel.grid.major = element_blank(), 
                                                              panel.grid.minor = element_blank(),
                                                              panel.background = element_blank(),
                                                              axis.text.x = element_text(colour = c('black', 'black','black', 'black', 'black', 'red')))
                                                  )
                                        
                                        ## Agreement on goals and tasks
                                        gg_Goals = suppressWarnings(
                                                   ggplot(binned_differences, aes(x = 1:100))+
                                                        geom_bar(aes(x = GoalsDiff)) + 
                                                        scale_x_discrete(drop = FALSE,
                                                                         labels = labels) +
                                                        xlab("Pair differences: Agreement on Tasks and Goals") +
                                                        ylab("Frequency") +
                                                        theme(panel.grid.major = element_blank(), 
                                                              panel.grid.minor = element_blank(),
                                                              panel.background = element_blank(),
                                                              axis.text.x = element_text(colour = c('black', 'black','black', 'black', 'black', 'red')))
                                                    )
                                        
                                        ## Overall
                                        gg_total = suppressWarnings(
                                                   ggplot(binned_differences, aes(x = 1:100))+
                                                        geom_bar(aes(x = TotalSumDiff)) + 
                                                        scale_x_discrete(drop = FALSE,
                                                                         labels = labels) +
                                                        xlab("Pair differences: Total") +
                                                        ylab("Frequency") +
                                                        theme(panel.grid.major = element_blank(), 
                                                              panel.grid.minor = element_blank(),
                                                              panel.background = element_blank(),
                                                              axis.text.x = element_text(colour = c('black', 'black','black', 'black', 'black', 'red')))
                                                    )
                                        
                                        # Print plots
                                        grid.arrange(gg_Personal, gg_Goals, gg_total, 
                                                     ncol = 2, nrow = 2)
                                } else{
                                        cat("Set \"plot = TRUE\" for a graphical overview of pairwise percentages of rating differences. \n")
                                        }
                                
                                cat(proportion_high_difference_pairs, "%  of the client-counselor pairs differ at least 25% in (one or more of) the ratings of Personal Bond, Agreement on tasks and goals, or the total ratings. \n")
                                cat("These differences were found in the following pairs: \n")
                                return(kable(large_differences, col.names = "ID"))

                                 
                        }
                        
                        

                }

        }

