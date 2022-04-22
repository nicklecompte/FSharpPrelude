# Project management

Below is my open-source JIRA implementation :) It is something of an unabridged 
changelog.

## 4/21/2022

Note: The first day of using this.

### Test coverage ###

- General Prelude
  - 🟩 Simple tests for the simpler helper functions and datatypes 
  - 🟥 EitherBuilder and related classes
  - 🟥 NonemptyList
- Lenses
  - 🟨 Basic tests for all lens datatypes and functions (should we include more
        sophisticated tests or tests lenses?)
- Morphisms
  - 🗺 needs dev work
- Graph
  - 🟨 BinaryRelation unit tests
  - 🟥 Tree unit tests
  - 🟥 Graph unit tests
  - 🗺 Integration tests
- Fast2DArray
  - 🟥 Fast2DArray simple unit tests
  - 🗺 ByRefFast2DArray (needs dev work)
- Unsafe2DArray
  - 🟥 unit tests
- Logging
  - 🗺 needs dev work
- Probability
  - 🟥  unit tests
- Dependency management
  - 🟥 unit tests

## Features and work needed ##

- **BY FAR THE TOP PRIORITY** after the test cases of course... finish this dumb
"Features Needed" list.... I am still doing an embarrassing amount of 
fundamental work in scoping this project out. 

- Fill out benchmarking program

- Basic research into unmanaged datatypes and methods

## Archive

## 4/21/2022

Note: The first day of using this.

### Test coverage ###

- General Prelude
  - 🟩 Simple tests for the simpler helper functions and datatypes
  - 🟥 EitherBuilder and related classes
  - 🟥 NonemptyList
- Lenses
  - 🟨 Basic tests for all lens datatypes and functions (should we include more
    sophisticated tests or tests lenses?)
- Morphisms
  - 🗺 needs dev work
- Graph
  - 🟨 BinaryRelation unit tests
  - 🟥 Tree unit tests
  - 🟥 Graph unit tests
  - 🗺 Integration tests
- Fast2DArray
  - 🟥 Fast2DArray simple unit tests
  - 🗺 ByRefFast2DArray (needs dev work)
- Unsafe2DArray
  - 🟥 unit tests
- Logging
  - 🗺 needs dev work
- Probability
  - 🟥  unit tests
- Dependency management
  - 🟥 unit tests
- 🟥 Scope out the rest of the tests needed up to this point

## Features and work needed ##

- **BY FAR THE TOP PRIORITY** after the test cases of course 😇... finish this
dumb  "Features Needed" list.... I am still doing an embarrassing amount of
fundamental work in scoping this project out.

- Fill out benchmarking program

- Basic research into unmanaged datatypes and methods

## How to read this poorly-conceived document 🙃

As stated above, this can be thought of like an unabridged changelog, including 
stages where the software is nowhere near release. The most recent update is 
kept at the top, with the ongoing agenda items listed as separate level-three 
headers, in descending order of priority. These agenda items can be thought of 
as "epics" in agile development parlance, with a bullet list of "user stories."
A day's update includes a detailed report of the work done and planned between 
the last release and the current.

After the day is fully written, is it copy-pasted into the Archive. So there is
some redundancy.

**Legend**

🗺: task needs considerable planning work

🟥: task is well-planned but needs completion

🟨: task is completed but raised issues that require significant attention

🟩: task is successfully complete