kperscale0 <- table(gsub(x = names(data_scales_rc_training),
                         pattern = "_[0-9]*",
                         replacement = ""))

drop_items0 = list(c(2,3,8), # AcadSC
                   c(1,2,6,10), # Attain
                   c(1,2,7,8), # Cost
                   c(1,5), # Difficulty
                   c(1,5), # Expectancy
                   c(2,3,4,5), # Goals
                   c(2,4,5,8), # IntEnj
                   c(2,7,8,9)) # Utility

out0 <- convert_drop_to_keep_list(scale_names = scale_names, 
                          kperscale = kperscale0,
                          drop_items = drop_items0)


cat(make_cfa_string(scale_names = scale_names,
                keep_items = out0))


cfa0 <- model_string_builder(dat = data_scales_rc_training,
                             drop_items = drop_items0,
                             str_type = "cfa")
cat(cfa0)


cfa1 <- model_string_builder(dat = data_scales_rc_training,
                             drop_items = drop_items0,
                             str_type = "cfa",
                             combo_scales = list(NegValue = c("Attain_1", "Attain_10", "Cost_1", "Cost_7"),
                                                 STV = c("Attain", "IntEnj", "Utility")))
cat(cfa1)

cfa2 <- model_string_builder(dat = data_scales_rc_training,
                             drop_items = drop_items0,
                             str_type = "cfa",
                             combo_scales = list(NegValue = c("Attain_1", "Attain_10", "Cost_1", "Cost_7")))
cat(cfa2)

mirtstr2 <- model_string_builder(dat = data_scales_rc_training,
                             drop_items = drop_items0,
                             str_type = "mirt",
                             combo_scales = list(NegValue = c("Attain_1", "Attain_10", "Cost_1", "Cost_7")))
cat(mirtstr2$mirt_string)

mirtstr3 <- model_string_builder(dat = data_scales_rc_training,
                                 drop_items = drop_items0,
                                 str_type = "mirt",
                                 combo_scales = list(NegValue = c("Attain_1", "Attain_10", "Cost_1", "Cost_7", "Attain_5")))
cat(mirtstr3$mirt_string)
head(mirtstr3$mirt_subdat)

bfactorstr3 <- model_string_builder(dat = data_scales_rc_training,
                                 drop_items = drop_items0,
                                 str_type = "bfactor",
                                 combo_scales = list(NegValue = c("Attain_1", "Attain_10", "Cost_1", "Cost_7", "Attain_5")))
cat(bfactorstr3$bfactor_vec)
head(bfactorstr3$bfactor_subdat)

bfactorstr2 <- model_string_builder(dat = data_scales_rc_training,
                                    drop_items = drop_items0,
                                    str_type = "bfactor",
                                    combo_scales = list(NegValue = c("Attain_1", "Attain_10", "Cost_1", "Cost_7")))
length(bfactorstr2$bfactor_vec)
head(bfactorstr2$bfactor_subdat)

big_out2 <- model_string_builder(dat = data_scales_rc_training,
                                 drop_items = drop_items0,
                                 str_type = "bfactor",
                                 combo_scales = list(NegValue = c("Attain_1", "Attain_10", "Cost_1", "Cost_7")))
