ep <- readRDS("data/ep.rds")
write(paste0("data/buffer_lulc/", ep$lagoslakeid, ".csv"), file.path("data", "llids.txt"))
