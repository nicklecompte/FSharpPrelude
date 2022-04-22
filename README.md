# FSharpPrelude

This class library contains a heterogeneous, loosely organized collection of
functions and datatypes, the overarching idea being

  1. These definitions and utilities could conceivably be used in any F# 
program.
  2. We attempt to maintain a clear delineation of implementations:
      - purely functional
      - "mostly" functional 
      - imperative/object programming. **Note** Although we do use objects we
      try to avoid an "object-oriented" approach.
      - Thread-safe vs. unsafe
      - Synchronous vs. async
      - Managed vs. unmanaged. **Note** For performance reasons many 
      implementations use unmanaged memory access. I am trying to do all of this
      in F#, awkward as it is. There is a separate Unsafe module that tries to
      clean this up.

## Outline

- Prelude
- Graph
- Collections
- Math