variable_names <- c("obj_1_top_fr", "obj_2_top_fr", "obj_3_top_fr", "obj_4_top_fr",
                    "obj_1_flop_fr", "obj_2_flop_fr", "obj_3_flop_fr", "obj_4_flop_fr",
                    "obj_1_top_de", "obj_2_top_de", "obj_3_top_de", "obj_4_top_de",
                    "obj_1_flop_de", "obj_2_flop_de", "obj_3_flop_de", "obj_4_flop_de",
                    "obj_1_top_it", "obj_2_top_it", "obj_3_top_it", "obj_4_top_it",
                    "obj_1_flop_it", "obj_2_flop_it", "obj_3_flop_it", "obj_4_flop_it")
                    
                    
title_names <- c("title_1_top_fr", "title_2_top_fr", "title_3_top_fr", "title_4_top_fr",
                 "title_1_flop_fr", "title_2_flop_fr", "title_3_flop_fr", "title_4_flop_fr",
                 "title_1_top_de", "title_2_top_de", "title_3_top_de", "title_4_top_de",
                 "title_1_flop_de", "title_2_flop_de", "title_3_flop_de", "title_4_flop_de",
                 "title_1_top_it", "title_2_top_it", "title_3_top_it", "title_4_top_it",
                 "title_1_flop_it", "title_2_flop_it", "title_3_flop_it", "title_4_flop_it")

                    

tibbles_list <- mget(variable_names)

folders <- dw_list_folders()


folder_id <- folders[["list"]][[3]][["folders"]][[6]][["folders"]][[4]][["folders"]][[5]][["id"]]


create_and_edit_table_chart <- function(data, title, folder_id) {
  chart <- dw_create_chart(title = title, type = "table", folderId = folder_id)
  
  # Debugging
  print(paste("Creating chart with title:", title))
  print(head(data))
  print(chart)
  
  if (!is.null(chart$error)) {
    stop(paste("Error in creating chart:", chart$error$message))
  }
  
  dw_edit_chart(chart_id = chart$id, data = data)
  return(chart$id)
}

# Créer 24 tableaux dans le dossier spécifié et stocker leurs IDs dans une liste
chart_ids <- vector("character", length(tibbles_list))

for (i in seq_along(tibbles_list)) {
  chart_title <- paste("Tableau", i)
  chart_ids[i] <- create_and_edit_table_chart(data = tibbles_list[[i]], title = chart_title, folder_id = folder_id)
}

# Afficher les IDs des tableaux créés
print(chart_ids)

print(paste("Using folder_id:", folder_id))
print("Tibbles list:")
print(tibbles_list)
