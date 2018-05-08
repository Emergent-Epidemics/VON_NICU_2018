#acc_funcs
get_hier <- function(x){
  out <- diff(as.numeric(as.character(x)))
  out_level <- length(out) - sum(out == 0)
  return(out_level)
}

#adding indicators
community$mat_trans_reimburse <- rep(NA, nrow(community))
community$back_trans_reimburse <- rep(NA, nrow(community))
community$interhosp_trans_reimburse <- rep(NA, nrow(community))
for(i in 1:nrow(community)){
  use.i <- which(as.character(nicu_states_class$State) == as.character(community$stateMode[i]))
  if(length(use.i) != 1){
    next
  }
  community$mat_trans_reimburse[i] <- as.character(nicu_states_class$mat_trans_reimburse[use.i])
  community$back_trans_reimburse[i] <- as.character(nicu_states_class$back_trans_reimburse[use.i])
  community$interhosp_trans_reimburse[i] <- as.character(nicu_states_class$interhosp_trans_reimburse[use.i])
}

#building hierarchy scores
hier_cols <- grep("level",colnames(dat))
hier_mat <- as.matrix(dat[,hier_cols])
hier_level <- apply(hier_mat, 1, get_hier)

#by_com
mt <- which(dat$blindID != info_node$blindID)
if(length(mt) > 0 & length(dat$blindID) != length(info_node$blindID)){
  stop("not aligned!")
}else{
  info_node$hier_level <- hier_level
}

by_com <- by(data = info_node$hier_level, INDICES = info_node$level2, FUN = max, na.rm = TRUE)

mt2 <- which(names(by_com) != community$groupID)
if(length(mt2) > 0 & length(names(by_com)) != length(community$groupID)){
  stop("not aligned!")
}else{
  community$hier_level <- as.numeric(by_com)
}

#alternate way of getting layers
layers <- rep(NA, nrow(community))
hosp_layers <- rep(NA, nrow(community))
comms <- community$groupID
for(i in 1:length(comms)){
  use.i <- which(info_node$level2 == comms[i])
  hosp.i <- info_node$blindID[use.i]
  use.dat.i <- which(dat$blindID %in% hosp.i)
  if(length(use.dat.i) != length(hosp.i)){
    stop(i)
  }
  coms.i <- apply(dat[use.dat.i, hier_cols], 1, paste, collapse = "")
  n.coms.i <- length(unique(coms.i))
  n.hosp.i <- length(coms.i)
  layers[i] <- n.coms.i
  hosp_layers[i] <- n.hosp.i
}
community$n_layers <- layers
community$n_hosp_layers <- hosp_layers

mt3 <- which(community$groupID != born_at_3_4$community)
ratio_3_4 <- born_at_3_4$Level.3.4.Hosps/born_at_3_4$hospitals

if(length(mt3) > 0 & length(born_at_3_4$community) != length(community$groupID)){
  community$born.at.3.4 <- NA
  community$pct.born34 <- NA
  community$ratio_3_4 <- NA
  community$hospitals <- NA
  community$Level.3.4.Hosps <- NA
  community$infants <- NA
  warning("born at 3/4 not aligned!")
}else{
  community$born.at.3.4 <- born_at_3_4$born.at.3.4
  community$pct.born34 <- born_at_3_4$pct.born34
  community$ratio_3_4 <- ratio_3_4
  community$hospitals <- born_at_3_4$hospitals
  community$Level.3.4.Hosps <- born_at_3_4$Level.3.4.Hosps
  community$infants <- born_at_3_4$infants
}

community$miss <- community$infants-community$born.at.3.4

#fixing longitude
info_node$longitude <- -1 * info_node$longitude

#Adding transfer percentages
match_transfer <- match(community$groupID, transfer_percents$community)
community$transfer_prop <- transfer_percents$trans.pct[match_transfer]
community$total_hosp <- transfer_percents$totalHosps[match_transfer]
community$VONhosps <- transfer_percents$VONhosps[match_transfer]