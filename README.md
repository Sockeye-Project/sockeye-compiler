# Sockeye Compiler
This is the compiler for the Sockeye language.
Currently there are two versions.
The current version is in `src` the deprecated version 1 is in `src/v1`.


## Build
To build the compiler run `make sockeye`.
This will create the compiler binary in `bin/sockeye`.

## Testing

Tests can be run with `make test`. Tests are defined 
in `src/tests`. Each test consist of two file with the same base name:
A soc file that will be compiled to prolog and a prolog test file. The
prolog test file defines a predicate `test` that will be executed. It 
can make use of predicates defined in helpers to signal error conditions,
but in the most simple case, the predicate should simply be true
iff the test succeeds.

The tests are also run on the gitlab CI and the current test status be found
[here](https://gitlab.inf.ethz.ch/OU-ROSCOE/sockeye/sockeye-compiler/-/jobs/artifacts/master/raw/build/test_report.txt?job=test).

### Dependencies
The Sockeye compiler has some dependencies. Stay tuned for a list. A sufficient but probably not necessary set are the Haskell dependencies for Barrelfish.

For the testing infrastructure, Eclipse CLP should be installed, and the
binary on the path using the name eclipseclp (note the non standard name).

## Documentation
The Sockeye compiler is work in progress.
The best source of documentation for the language can be found in the [Barrelfish Technote](http://www.barrelfish.org/publications/TN-025-Sockeye.pdf).

For a discussion on where the Sockeye compiler is headed, see [this NetOS Wiki page](https://wiki.netos.ethz.ch/Sockeye/Language).

## Merging from Barrelfish code

To update this repo from Barrelfish

```
git remote add mas git@gitlab.inf.ethz.ch:OU-ROSCOE/barrelfish/asplos20-capabilities/barrelfish-asplos20-caps.git
git fetch mas
git checkout TOBEMERGED
git filter-branch --subdirectory-filter tools/sockeye/
git mv {*.hs,tests,v1} src
find -iname Hakefile -delete
git add .
git commit
git checkout master
git merge TOBEMERGED
```

