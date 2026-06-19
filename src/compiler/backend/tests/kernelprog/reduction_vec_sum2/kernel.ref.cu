#define NUM_THREADS 256
#define NUM_WARPS 8    
//NUM_WARPS = NUM_THREADS/32 => always

extern "C" {

__device__ float shared_data_reduce_sum_v2(float shared_data[NUM_WARPS])
{
    float sum{0.0f};
 
    for (size_t i{0}; i < NUM_WARPS; ++i)
    {
        sum += shared_data[i];
    }
    return sum;
}

__device__ float warp_reduce_sum(float val)
{
    constexpr unsigned int FULL_MASK{0xffffffff};
 
    for (size_t offset{16}; offset > 0; offset /= 2)
    {
        val += __shfl_down_sync(FULL_MASK, val, offset);
    }
    return val;
}

__device__ float block_reduce_sum_v2(float const* __restrict__ input_data,
                                     float shared_data[NUM_WARPS],
                                     size_t num_elements)
{
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
    sum = warp_reduce_sum(sum);

    if (threadIdx.x % 32 == 0)
    {
        shared_data[threadIdx.x / 32] = sum;
    }
    __syncthreads();
    float const block_sum{shared_data_reduce_sum_v2(shared_data)};
    
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
    __shared__ float shared_data[NUM_WARPS];
    float const block_sum{block_reduce_sum_v2(
        input_data + block_idx * num_elements_per_batch, shared_data,
        num_elements_per_batch)};
    if (thread_idx == 0)
    {
        output_data[block_idx] = block_sum;
    }
}
  
}