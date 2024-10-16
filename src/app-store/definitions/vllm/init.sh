set -e
vllm serve --max_model_len $MY_CONTEXT_SIZE --gpu_memory_utilization $MY_GPU_UTILIZATION --kv_cache_dtype=$MY_KV_CACHE_DTYPE --disable-log-requests $MY_MODEL