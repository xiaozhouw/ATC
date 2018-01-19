#' findClass
#'
#' This function returns the ATC of the drugs in the medication.
#' @param the raw medication that user is interested in
#' @param the tier that user is interested in. Default to 4.
#' @return two lists: the first list contains the table that maps the medication to its corresponding drugs; the second list contains the tables that map drugs to its ATC
#' @export
findClass = function(medname,tier=4){
  medname = tolower(medname)
  tiername = paste0("TIER",tier)
  data_get_drug = load(file = "/data/data_get_drug.rda")
  mapname = paste0("/data/data_drug_",tier,".rda")
  data_get_map = load(file = mapname)
  res_med_to_drug = data_get_drug[which(data_get_drug$MEDNAME == medname),]
  res_drug_to_class = data_get_map[which(data_get_map$DRUG == res_med_to_drug$DRUG),]
  if(nrow(res_med_to_drug) >0)
  {
    if(nrow(res_med_to_drug)>1)
    {
      for(i in 2:nrow(res_med_to_drug))
      {
        temp_drug_to_class = data_get_map[which(data_get_map$DRUG == res_med_to_drug$DRUG[i]),]
        res_drug_to_class = rbind(res_drug_to_class,temp_drug_to_class)
      }
    }
    out = list(res_med_to_drug,res_drug_to_class)
  } else if(!(grepl("-",medname)||grepl("\\sIN\\s",medname)))
  {
    med = medname %>% as.data.frame() %>% mutate(med_search_1 = sub(pattern = "(\\D+)(\\s)(<{0,1})(>{0,1})(={0,1})(\\d+)(.*)", replacement = "\\1",x = medname)) %>%  mutate(med_search = sub(pattern = "(\\w+\\s)(\\w+)(\\s.*)",replacement = "\\1\\2",x = med_search_1)) %>%  mutate(med_search = sub(pattern = "(\\w+)\\s(\\W)(.*)",replacement = "\\1",x = med_search)) %>% select(med_search) %>% as.character()
    # create first table
    temp_med_to_drug = data.frame(MEDNAME=medname,DRUG=med)
    # Check if the drug has crreaspongding class
    if(med %in% data_get_map$DRUG)
    {
      res_drug_to_class = data_get_map[which(data_get_map$DRUG == med),]
      # no need to update the drug to class map if the drug is dlready in the map

      out = list(temp_med_to_drug,res_drug_to_class)
    } else
    {
      #Update the drug to class map
      url3 <- paste0("https://mor.nlm.nih.gov/RxClass/getSearch?query=",med,"&searchBy=drug&sourceIds=&drugSources=atc1-4%7Catc%2Cepc%7Cdailymed%2Cmeshpa%7Cmesh%2Cdisease%7Cndfrt%2Cchem%7Cdailymed%2Cmoa%7Cdailymed%2Cpe%7Cdailymed%2Cpk%7Cndfrt%2Cva%7Cndfrt&isIngredientOnly=true")
      tryCatch({
        r = GET(url3)
        res <- sub(pattern="IN\\|ATC1-4\\|{2}(.*)\\|(\\w\\d{2}\\w{2})(.*)",replacement = "\\1", x = content(r)$tableResult$paths[[1]]$path)
        res = unique(res)
        if(length(res)>0){
          temp_drug_to_class = setNames(data.frame(matrix(ncol = 2, nrow = length(res))), c("DRUG", tiername))
          for(i in 1:length(res)){
            # Edit the query
            res_search <- gsub(' ','+',x = res)
            # Find the id that also used for searching
            res_id <-  sub(pattern="IN\\|ATC1-4\\|{2}(.*)\\|(\\w\\d{2}\\w{2})(.*)",replacement = "\\2", x = content(r)$tableResult$paths[[1]]$path)
            url4 <- paste0("https://mor.nlm.nih.gov/RxClass/getRelatedTrees?className=",res_search,"&sourceId=",res_id,"&drugSources=atc1-4%7Catc%2Cepc%7Cdailymed%2Cmeshpa%7Cmesh%2Cdisease%7Cndfrt%2Cchem%7Cdailymed%2Cmoa%7Cdailymed%2Cpe%7Cdailymed%2Cpk%7Cndfrt%2Cva%7Cndfrt")

            # Obtain the therapeutic class based on the selected tier
            drug_class <- unlist(strsplit(content(GET(url4))$paths[[1]]$path,"\\|"))[tier+1] %>% tolower()
            temp_drug_to_class = rbind(temp_drug_to_class,c(med,drug_class))

          }

        } else {
          temp_drug_to_class = data.frame("DRUG" = med,tiername="")
        }
        Sys.sleep(.5)
      },
      # Error catching
      error = function(e){temp_drug_to_class = data.frame("DRUG" = med,tiername="api error")})
      out = list(temp_med_to_drug,temp_drug_to_class)
    }
  }else
  {
    if(grepl("\\sIN\\s",medname))
    {
      med_2 = medname %>% as.data.frame() %>% mutate(drug_1 = sub(pattern = "(\\w+)\\s(.*)\\sIN\\s(\\w+)\\s(.*)", replacement = "\\1",x = medname),drug_2 = sub(pattern = "(\\w+)\\s(.*)\\sIN\\s(\\w+)\\s(.*)", replacement = "\\3",x = medname)) %>% select(drug_1,drug_2) %>% as.character()
    }else if(grepl("-",medname))
    {
      med_2 = medname %>% as.data.frame() %>% mutate(drug_1 = sub(pattern = "(\\w+)-(\\w+)\\s(.*)", replacement = "\\1",x = medname),drug_2 = sub(pattern = "(\\w+)-(\\w+)\\s(.*)", replacement = "\\2",x = medname))  %>% select(drug_1,drug_2) %>% as.character()
    }
    med_to_drug_multi = data.frame("MEDNAME"=rep(medname,2),"DRUG" = med_2)
    temp_drug_to_calss_multi = list(setNames(data.frame(matrix(ncol = 2, nrow = 1)), c("DRUG", tiername)),setNames(data.frame(matrix(ncol = 2, nrow = 1)), c("DRUG", tiername)))
    for(j in 1:length(med_2))
    {
      if(med_2[j] %in% data_get_map$DRUG){
        res_drug_to_class = data_get_map[which(data_get_map$DRUG == med_2[j]),]
        # no need to update the drug to class map if the drug is dlready in the map
        temp_drug_to_calss_multi[[j]] = res_drug_to_class
      } else{
        #Update the drug to class map
        url3 <- paste0("https://mor.nlm.nih.gov/RxClass/getSearch?query=",med_2[j],"&searchBy=drug&sourceIds=&drugSources=atc1-4%7Catc%2Cepc%7Cdailymed%2Cmeshpa%7Cmesh%2Cdisease%7Cndfrt%2Cchem%7Cdailymed%2Cmoa%7Cdailymed%2Cpe%7Cdailymed%2Cpk%7Cndfrt%2Cva%7Cndfrt&isIngredientOnly=true")
        tryCatch({
          r = GET(url3)
          res <- sub(pattern="IN\\|ATC1-4\\|{2}(.*)\\|(\\w\\d{2}\\w{2})(.*)",replacement = "\\1", x = content(r)$tableResult$paths[[1]]$path)
          res = unique(res)
          if(length(res)>0){
            res_drug_to_class = data.frame("DRUG" = rep(med_2[j],length(res)), tiername = NA)
            for(i in 1:length(res)){
              # Edit the query
              res_search <- gsub(' ','+',x = res)
              # Find the id that also used for searching
              res_id <-  sub(pattern="IN\\|ATC1-4\\|{2}(.*)\\|(\\w\\d{2}\\w{2})(.*)",replacement = "\\2", x = content(r)$tableResult$paths[[1]]$path)
              url4 <- paste0("https://mor.nlm.nih.gov/RxClass/getRelatedTrees?className=",res_search,"&sourceId=",res_id,"&drugSources=atc1-4%7Catc%2Cepc%7Cdailymed%2Cmeshpa%7Cmesh%2Cdisease%7Cndfrt%2Cchem%7Cdailymed%2Cmoa%7Cdailymed%2Cpe%7Cdailymed%2Cpk%7Cndfrt%2Cva%7Cndfrt")

              # Obtain the therapeutic class based on the selected tier
              res_drug_to_class$TIER[i] <- unlist(strsplit(content(GET(url4))$paths[[1]]$path,"\\|"))[tier+1] %>% tolower()

            }
            temp_drug_to_calss_multi[[j]] = res_drug_to_class

          } else {
            temp_drug_to_calss_multi[[j]] = data.frame("DRUG" = med_2[j],tiername="")
          }
          Sys.sleep(.5)
        },
        # Error catching
        error = function(e){temp_drug_to_calss_multi[[j]] = data.frame("DRUG" = med_2[j],tiername="api error")})
      }
    }
    out = list(med_to_drug_multi,temp_drug_to_calss_multi)
  }

  return(out)
}
