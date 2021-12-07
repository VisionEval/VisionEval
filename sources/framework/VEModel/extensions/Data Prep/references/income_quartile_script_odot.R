#income_quartile_script.R
#------------------------

#Load income group index
IncGroupIdx_df <- read.csv("data/census_block_group/inc_bracket_columns.csv", as.is = TRUE)
#Load block group household income data
ColsToSelect_ <-
  c(IncGroupIdx_df$Column)
IncCategoriesByBG_df <-
  read.csv("data/census_block_group/ACS_13_5YR_B19101_with_ann.csv", 
           as.is = TRUE, row.names = 2)[-1, ColsToSelect_]
names(IncCategoriesByBG_df) <- c(IncGroupIdx_df$Name)
IncCategoriesByBG_df[] <- lapply(IncCategoriesByBG_df, function(x) as.numeric(x))
IncCategoriesByBG_df <- IncCategoriesByBG_df[Bg,]

#Calculate quartiles
IncCategories_ <- colSums(IncCategoriesByBG_df)
barplot(IncCategories_, horiz = TRUE)
#Number of households in each quartile
QrtlNumHh <- sum(IncCategories_) / 4
#Identify where quartiles fall between income categories
QrtlNumHhCumSum_ <- QrtlNumHh * 1:3
IncCategoriesCumSum_ <- cumsum(IncCategories_)
#Find quartile breaks
findPosBetween <- function(Val, Vals_) {
  WhichLowVal <- max(which(Val > Vals_))
  WhichHighVal <- min(which(Val < Vals_))
  Prop <- 
    unname(
      (Val - Vals_[WhichLowVal]) / (Vals_[WhichHighVal] - Vals_[WhichLowVal])
    )
  c(LowIdx = WhichLowVal,
    HighIdx = WhichHighVal,
    Prop = Prop)
}
QrtlBreaks_ls <- 
  lapply(QrtlNumHhCumSum_, findPosBetween, IncCategoriesCumSum_)
#Calculate the number of households in each quartile in each block group
calcBGHhByQuartile <- 
  function(NumByInc_, QrtlBreaks_) {
    NumHhByQrtl_ <- numeric(4)
    NumHhByQrtl_[1] <-
      sum(NumByInc_[1:QrtlBreaks_ls[[1]]["LowIdx"]]) +
      NumByInc_[QrtlBreaks_ls[[1]]["HighIdx"]] * QrtlBreaks_ls[[1]]["Prop"]
    NumHhByQrtl_[2] <-
      NumByInc_[QrtlBreaks_ls[[1]]["HighIdx"]] * (1 - QrtlBreaks_ls[[1]]["Prop"]) +
      sum(NumByInc_[(QrtlBreaks_ls[[1]]["HighIdx"] + 1):QrtlBreaks_ls[[2]]["LowIdx"]]) +
      NumByInc_[QrtlBreaks_ls[[2]]["HighIdx"]] * QrtlBreaks_ls[[2]]["Prop"]
    NumHhByQrtl_[3] <-
      NumByInc_[QrtlBreaks_ls[[2]]["HighIdx"]] * (1 - QrtlBreaks_ls[[2]]["Prop"]) +
      sum(NumByInc_[(QrtlBreaks_ls[[2]]["HighIdx"] + 1):QrtlBreaks_ls[[3]]["LowIdx"]]) +
      NumByInc_[QrtlBreaks_ls[[3]]["HighIdx"]] * QrtlBreaks_ls[[3]]["Prop"]
    NumHhByQrtl_[4] <-
      NumByInc_[QrtlBreaks_ls[[3]]["HighIdx"]] * (1 - QrtlBreaks_ls[[3]]["Prop"]) +
      sum(NumByInc_[(QrtlBreaks_ls[[3]]["HighIdx"] + 1):length(NumByInc_)])
    NumHhByQrtl_
  }

NumHh_BgQu <- t(apply(IncCategoriesByBG_df, 1, calcBGHhByQuartile, QrtlBreaks_ls))
colnames(NumHh_BgQu) <- c("HhPropIncQ1", "HhPropIncQ2", "HhPropIncQ3", "HhPropIncQ4")
NumHh_BgQu <- NumHh_BgQu[rowSums(NumHh_BgQu) > 0,]

#Calculate proportions rounded to 2 decimal points
PropHh_BgQu <- round(t(apply(NumHh_BgQu, 1, function(x) x / sum(x))), 2)
#Adjust to sum to 1
PropAdj_Bg <- 1 - rowSums(PropHh_BgQu)
for (i in 1:length(PropAdj_Bg)) {
  PropHh_BgQu[i,] <- 
    local({
      PropHh_ <- PropHh_BgQu[i,]
      PropHh_[which(PropHh_ == max(PropHh_))] <- 
        max(PropHh_) + PropAdj_Bg[i]
      PropHh_
    })
}
#Prepare data frame of proportions
PropHh_df <- data.frame(PropHh_BgQu)
names(PropHh_df) <- colnames(PropHh_BgQu)
PropHh_df$Geo <- paste0("D", rownames(PropHh_BgQu))
PropHh_df$Year <- 2010
PropHh_df <- PropHh_df[,c(5,6,1:4)]
FuturePropHh_df <- PropHh_df
FuturePropHh_df$Year <- 2038
PropHh_df <- rbind(PropHh_df, FuturePropHh_df)
#Write out data
write.table(PropHh_df, 
            file = "new_files/bzone_hh_inc_qrtl_prop.csv", 
            row.names = FALSE, 
            col.names = TRUE, 
            sep = ",")
