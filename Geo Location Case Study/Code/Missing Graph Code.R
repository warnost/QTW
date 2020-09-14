## This code assumes you have the pivot objects in memory
library(naniar)

# Check before imputation. If you imputed already then these will appear with no NA
# Original data
vis_miss(offline,warn_large_data = F)

# Pivoted data
vis_miss(offline_pivot,warn_large_data = F)

vis_miss(online,warn_large_data = F)

vis_miss(online_pivot,warn_large_data = F)


