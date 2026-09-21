// kernel.cu
#define NUM_THREADS 256

extern "C" {

__device__ float shared_data_reduce_sum_v1(float shared_data[NUM_THREADS])
{
    static_assert(NUM_THREADS % 32 == 0,
                  "NUM_THREADS must be a multiple of 32");
    size_t const thread_idx{threadIdx.x};

    for (size_t stride{NUM_THREADS / 2}; stride > 0; stride /= 2)
    {
        if (thread_idx < stride)
        {
            shared_data[thread_idx] += shared_data[thread_idx + stride];
        }
        __syncthreads();
    }
    return shared_data[0];
}

__device__ float block_reduce_sum_v1(float const* __restrict__ input_data,
                                     float shared_data[NUM_THREADS],
                                     size_t num_elements)
{
    static_assert(NUM_THREADS % 32 == 0,
                  "NUM_THREADS must be a multiple of 32");
    size_t const num_elements_per_thread{(num_elements + NUM_THREADS - 1) /
                                         NUM_THREADS};
    size_t const thread_idx{threadIdx.x};
    float sum{0.0f};
    for (size_t i{0}; i < num_elements_per_thread; ++i)
    {
        size_t const offset{thread_idx + i * NUM_THREADS};
        if (offset < num_elements)
        {
            sum += input_data[offset];
        }
    }
    shared_data[thread_idx] = sum;
    __syncthreads();
    float const block_sum{shared_data_reduce_sum_v1 (shared_data)};
    return block_sum;
}


__global__ void kernel(float* __restrict__ output_data,
                                      float const* __restrict__ input_data,

                                      size_t num_elements_per_batch)
{
    static_assert(NUM_THREADS % 32 == 0,
                  "NUM_THREADS must be a multiple of 32");
    size_t const block_idx{blockIdx.x};
    size_t const thread_idx{threadIdx.x};
    __shared__ float shared_data[NUM_THREADS];
    float const block_sum{block_reduce_sum_v1 (
        input_data + block_idx * num_elements_per_batch, shared_data,
        num_elements_per_batch)};
    if (thread_idx == 0)
    {
        output_data[block_idx] = block_sum;
    }
}

} // extern "C"
