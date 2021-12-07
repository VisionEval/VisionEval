#Tabulate DataStore Inventory
#------------------------------
documentDatastoreTables <- function(SaveArchiveName, QueryPrep_ls) {
  GroupNames_ <- QueryPrep_ls$Listing$Datastore$Datastore$groupname
  Groups_ <- GroupNames_[-grep("/", GroupNames_)]
  if (any(Groups_ == "")) {
    Groups_ <- Groups_[-(Groups_ == "")]
  }
  TempDir <- SaveArchiveName
  dir.create(TempDir)
  for (Group in Groups_) {
    GroupDir <- file.path(TempDir, Group)
    dir.create(GroupDir)
    Tables_ <- listTables(Group, QueryPrep_ls)$Datastore
    for (tb in Tables_) {
      Listing_df <- listDatasets(tb, Group, QueryPrep_ls)$Datastore
      write.table(Listing_df, file = file.path(GroupDir, paste0(tb, ".csv")),
                  row.names = FALSE, col.names = TRUE, sep = ",")
    }
  }
  zip(paste0(SaveArchiveName, ".zip"), TempDir)
  remove_dir(TempDir)
  TRUE
}

QPrep_ls <- prepareForDatastoreQuery(
  DstoreLocs_ = c("Datastore"),
  DstoreType = "RD"
)
documentDatastoreTables("Datastore_Documentation", QPrep_ls)