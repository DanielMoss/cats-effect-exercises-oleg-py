## race
[Original description](https://olegpy.com/cats-effect-exercises/#race-for-success)

## Objective
Quickly obtain data which can be requested from multiple sources of unknown latency (databases, caches, network services, etc.).

### Requirements
- The function should run requests in parallel.
- The function should wait for the first request to complete successfuly.
- Once a first request has completed, everything that is still in-flight must be cancelled.
- If all requests have failed, all errors should be reported for better debugging.

Assume that there will be <= 32 providers and they all don’t block OS threads for I/O.

### Bonus
- If returned IO is cancelled, all in-flight requests should be properly cancelled as well.
- Refactor function to allow generic effect type to be used, not only cats’ IO. (e.g. anything with Async or Concurrent instances).
- Refactor function to allow generic container type to be used (e.g. anything with Traverse or NonEmptyTraverse instances).
  - Don’t use toList. If you have to work with lists anyway, might as well push the conversion responsibility to the caller.
  - If you want to support collections that might be empty (List, Vector, Option), the function must result in a failing IO/F when passed an empty value.