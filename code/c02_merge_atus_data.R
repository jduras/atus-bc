
message(str_c("Merging data from ATUS ", tfst, " to ", tlst))

df_timeuse_all <-
    map_dfr(tfst:tlst,
            function(t = .) {
                load(str_c(edir_atus, "atus_", t, "_timeuse.Rdata"))
                df_timeuse
            })

df_location_all <-
    map_dfr(tfst:tlst,
        function(t = .) {
            load(str_c(edir_atus, "atus_", t, "_location.Rdata"))
            df_location
        })

save(df_timeuse_all, file = str_c(edir_atus, "atus_", tfst, "_", tlst, "_timeuse.Rdata"))
save(df_location_all, file = str_c(edir_atus, "atus_", tfst, "_", tlst, "_location.Rdata"))
