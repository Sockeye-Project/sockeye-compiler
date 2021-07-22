# Sockeye Compiler
This is the compiler for the Sockeye language.
Currently there are two versions.
The current version is in `src` the deprecated version 1 is in `src/v1`.
In `socs` are all our sockeye definitions.
`src-pl` contains the Prolog tools that use the sockeye compiler output.


## Dependencies and build
This is tested on ubuntu 20.04, check out the `docker/Dockerfile`
for required packages. In a nutshell you need to install 
```sudo apt install libghc-aeson-dev libghc-missingh-dev```
and download and install eclipseclp from eclipseclp.org. The
Makefile assumes that you have a `eclipseclp` on your path. One way
to achieve this is by downloading the binary distribution and setting
up a syslink: 
```
curl https://eclipseclp.org/Distribution/CurrentRelease/7.0_54%20x86_64_linux%20Intel-64bit-Linux/eclipse_basic.tgz -o eclipse_basic.tgz
tar xfvz eclipse_basic.tgz 
ln -s /eclipse/lib/x86_64_linux/eclipse.exe /usr/bin/eclipseclp
```

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

## TODO / Wishlist

* Any sorts of checks, typechecks, node exists checks,
* warning of translates to non equal node size. Might be tricky actually, because variables, which are not known at compile time.
* Include files
* Bit patterns [syntax proposal here](https://wiki.netos.ethz.ch/BarrelFish/DesignNotes/Sockeye/Sockeye2019)
* Generic C backend. Replace custom generation for in kernel BF/MAS code. 
* Expressing configuration options (memory translation AND interrupt controllers please)
