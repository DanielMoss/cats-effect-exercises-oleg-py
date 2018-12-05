##workerpool
[Original description](https://olegpy.com/cats-effect-exercises/#worker-pool-with-load-balancing)

###Objective
Do parallel processing, distributed over a limited number of workers, each with its own state (counters, DB connections, etc.).

###Requirements
- Processing jobs must run in parallel
- Submitting a processing request must wait if all workers are busy.
- Submission should do load balancing: wait for the first worker to finish, not for a certain one.
- Worker should become available whenever a job is completed successfully, with an exception or cancelled.

Assume the number of workers is not very large (<= 100).

###Bonus
- Generalize for any F using Concurrent typeclass.
- Add methods to WorkerPool interface for adding workers on the fly and removing all workers. If all workers are removed, submitted jobs must wait until one is added.