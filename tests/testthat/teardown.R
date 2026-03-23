# Free cached taxonomic resources after the test suite completes.
# This runs before covr saves its coverage traces, preventing the
# memory pressure that causes saveRDS() to segfault on large datasets.
clear_cached_resources()
