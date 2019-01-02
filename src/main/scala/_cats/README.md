## JVM Threads
- JVM Threads map 1:1 with OS Native threads (usually number of processors)
- Too many threads -> lots of context switches (overhead) -> slower than synchronous execution

## Types of tasks
1. CPU-bound tasks: Require lots of computation and hence processor resources 
1. IO-bound tasks: Does I/O, less dependent on computation. e.g. waiting for disk operation to finish. Waiting for external service to answer your request
1. Non-terminating tasks: Tasks that never signal their result. Need not be blocking or consuming CPU

## Thread pools
- Created using Executor
- Consists of a work queue and a pool of running threads
- Tasks (Runnable) are put in the work queue and threads governed by the pool take it from there
- In Scala, we avoid working with Runnable and use Future or IO as abstractions 

### Thread pool configuration
Bounded
- Limit number of threads associated with a pool.
  - e.g. For CPU Bound tasks (require a lot of computation), limit to number of processor cores

Unbounded
- No limit on number of threads; dangerous; can run out of memory
	- Use a cached pool (re-use existing threads, and delete useless threads)
- Use backpressure or other means to control number of tasks to execute


## Types of tasks and associated thread pool configuration
### CPU Bound tasks
- Bounded thread pool, pre-allocated, fixed to number of CPUs
- Problems: 
	- Any blocking operation will eat a thread --> we want to avoid blocking
	- If can't block, push those operations to a separate thread pool that is cached and unbounded

### Blocking IO (or any blocking) task
- Unbounded thread pool 
- Higher checks to ensure that only a fixed number of blocking actions are allowed at any point 

### Non-blocking IO Polls tasks
- Threads that wait for kernel to tell them if there is an outstanding async IO notification, then notify the rest of the application.
- We only want a small number of fixed, pre-allocated threads, that have highest priority.
- When async notification is received, shift to CPU thread pool (assume CPU bound task)


## Working with blocking threads
- We want to have a dedicated pool for blocking operations and a separate pool for CPU operations
- `cats.effect.IO` has a `shift` operator that allows us to switch computation to different thread pools 


## Green Threads 
- Different from JVM Threads
- Mapped n:m to OS threads
- Are not scheduled on an OS level; lightweight
- Characterized by **cooperative multitasking** (thread decides when it is giving up control vs being forcefully preempted)
	- **context switch** means storing the state of 1 task (to be resumed later) so that another can be started
	- **preempted** means that a thread saves its state and the context switch occurs
	- forcefully preempted means that the process scheduler of the OS tells the thread when to take a task, when to stop, etc. 
- Problems: 
	- Poorly designed program can consume all the CPU resources for itself, causing the entire system to hang.


## Thread scheduling
Asynchronous boundary: `IO.shift` allows us to 
1. Shift computation to a different thread pool 
1. Send computation task to the `ExecutionContext` for re-scheduling.

`ExecutionContext`s are in charge of scheduling threads from their own pool. When a thread is running, it won't change until it terminates, or a higher priority thread is ready to start work. 

`IO` without any `shift` calls is considered 1 task. It can potentially hog an entire thread for the entire runtime. Since calling `shift` triggers a context shift, IO can be thought of as a green thread. 


## Semaphores
- Has a non-negative number of permits available, say *n*
- Acquire resource -> *n = n - 1*
- Release resource -> *n = n + 1*
- If *n = 0*, semantic block until permit is available. (thread is not blocked)

### Cancellation behaviour
- If created with `Semaphore.apply` (i.e. with `Concurrent[F]` instance), then blocking acquires are cancellable.
- If created with `Semaphore.uncancelable` (i.e. with `Async[F]` instance), then blocking acquires are non-cancellable.

### Use case
- Multiple processes trying to acquire a resource, we want to limit number of acquisitions
