
# Utility to bind columnwise while overwriting any existing columns
bind_cols_overwrite <- function(df_old, df_new) {
    df_old[,colnames(df_new)] <- df_new
    return(df_old)
}

