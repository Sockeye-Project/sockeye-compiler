# Sockeye Compiler
This is the compiler for the Sockeye language.
Currently there are two versions.
The current version is in `src` the deprecated version 1 is in `src/v1`.

## Build
To build the compiler run `make sockeye`.
This will create the compiler binary in `bin/sockeye`.

### Dependencies
The Sockeye compiler has some dependencies. Stay tuned for a list. A sufficient but probably not necessary set are the Haskell dependencies for Barrelfish.

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

