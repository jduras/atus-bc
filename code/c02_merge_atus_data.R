
print(paste("Merging data from ATUS", tfirst, "to", tlast))

df_timeuse_all <- NULL
df_location_all <- NULL

t <- tfirst

i <- 1

while (t <= tlast) {

    print(paste(" year", t))

    load(paste0(edir_atus, "subset_", t, ".Rdata"))

    df_timeuse_all[[i]] <- df_timeuse
    df_location_all[[i]] <- df_location

    rm(df_timeuse, df_location)

    i <- i + 1
    t <- t + 1
    }

df_timeuse_all %<>%  bind_rows()
df_location_all %<>% bind_rows()

save(df_timeuse_all, file = paste0(edir_atus, "atus_", tfirst, "_", tlast, "_timeuse.Rdata"))
save(df_location_all, file = paste0(edir_atus, "atus_", tfirst, "_", tlast, "_location.Rdata"))


