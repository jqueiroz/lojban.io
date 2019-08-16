# Lojban teaching platform

The main goal of this project is to create a robust teaching platform for the artificial language [Lojban](https://mw.lojban.org/papri/Lojban).
This project is heavily inspired by Duolingo, but it is somewhat more specific in that it attempts to exploit some of Lojban's unique features, such as machine-parsability.

An experimental version of the platform is available at [http://lojban.johnjq.com](http://lojban.johnjq.com).

Documentation for the currently deployed version may be found at [http://lojban.johnjq.com/docs](http://lojban.johnjq.com/docs).

## Quickstart (using Docker)

The simplest way to run this project is to execute the webserver inside a [Docker](https://www.docker.com/) container.
First, run `./dist/docker-build.sh` to build the image.
Then, run `./dist/docker-run.sh` to start the container.
This command will bind the webserver running on the container to port 8080 on the host, thus letting you access the application at [http://localhost:8080](http://localhost:8080).
To publish a port other than 8080, use `./dist/docker-run.sh -p <port>`.

By default, `./dist/docker-build.sh` uses an [intermediary image](https://cloud.docker.com/u/johnjq/repository/docker/johnjq/lojban-tool-dependencies) to speed up the build.
This image includes development tools such as stack and ghc as well as prebuilt dependent libraries, so that only the project's own source code needs to be compiled.
If you would like to perform a full build instead, run `./dist/docker-build.sh --full` (but beware that compiling all of the dependencies may take between 10 minutes and one hour).

## Quickstart (using Nix)

If you are making significant changes and/or rebuilding frequently, you will probably benefit from building and running the project outside of Docker.
First, rebuilding after making small changes will be quicker as it will not be necessary to recompile all source files (due to docker layers).
Second, you will be able to use helper scripts to run development tools such as ghci and code linters.

All you need is to install the [Nix package manager](https://nixos.org/nix/).
On most Linux distributions, this can be achieved by running `curl https://nixos.org/nix/install | sh` and following the instructions, if any (see also [Getting Nix](https://nixos.org/nix/download.html)).

Once you have installed Nix, you should be able to start the webserver by running `./run-server.sh` (for the first run, this may take between 10 minutes and one hour as all of the dependencies will need to be compiled).
The webserver may then be accessed at [http://localhost:8000](http://localhost:8000).

Other useful commands:
* `make`: builds the webserver as well as the associated documentation for the Haskell code.
* `make server`: builds the webserver.
* `make docs`: builds the documentation for the Haskell code.
* `make less`: compiles _*.less_ files into _*.css_ files (this step is also performed when running `make server`).
* `./run-ghci.sh`: starts a [GHCi](https://wiki.haskell.org/GHC/GHCi) prompt.
* `./verify-lint.sh`: run source code linters ([hlint](https://hackage.haskell.org/package/hlint) for Haskell code and [prettier](https://prettier.io/) for Javascript/Less/CSS code)
* `./verify-tests.sh`: run tests for the project.

For more details on building and running, see [Building and running (using Nix)](#building-and-running-using-nix).

## Building and running (using Nix)

In order to build and run this project, all you need is a Linux machine with the [Nix package manager](https://nixos.org/nix/) installed somewhere in the path.
On the off-chance that you are running [NixOS](https://nixos.org/), then you are already all set.
Otherwise, if you are running any other Linux distribution, please refer to [Getting Nix](https://nixos.org/nix/download.html) for details on how to install Nix.
In most cases, the setup is pretty simple: as a regular user, just run `curl https://nixos.org/nix/install | sh` and follow the instructions, if any.

The reason we use Nix is that it enables the use of [nix shells](https://nixos.org/nixos/nix-pills/developing-with-nix-shell.html), which are isolated and reproducible build environments.
Even though our build process relies on third party tools, such as stack and ghc for compiling Haskell code and [less](http://lesscss.org/usage/) for compiling _\*.less_ files into _\*.css_ files, you do not need to install these tools into your environment.
Rather, we employ build scripts that wrap the execution of these tools into isolated environments.

Once you have installed Nix, you should be able to build the project by running `make`.
When you build the project for the first time, Nix will spend some time retrieving (but not installing) required packages such as stack and less, which will be saved to the Nix store under `/nix/store`.
This step should take at most a couple of minutes with a good internet connection.
Next, Nix will use the downloaded packages to spawn an isolated shell environment, in which it will run `stack build`.
For the first build, all of the Haskell dependencies will be compiled, so this step is expected to take a substancial amount of time (between 10 minutes and one hour, depending heavily on computer specs),
Subsequent builds will be much faster (roughly a minute for a clean build of the project).

By default, `make` will build both the webserver executable and the associated documentation, powered by [Haddock](https://haskell-haddock.readthedocs.io/en/latest/).
If you only want to build the webserver, run `make server`.
If you only want to build the documentation, run `make docs`.
If you only want to compile the _\*.less_ files, perhaps because you are editing them and want to see the style changes on the fly, run `make less`.

When you run `make docs`, the project's documentation will be placed into _".stack-work/install/x86\_64-linux-nix/lts-13.27/8.6.5/doc/index.html"_ (the command's output shows the full path).
Similarly, when you run `make server`, the webserver executable will be placed into _".stack-work/install/x86\_64-linux-nix/lts-13.27/8.6.5/bin/lojto"_, and you may execute it directly if you want.

However, the preferred method for running the webserver is to use the helper script `./run-server.sh`.
This script automatically builds the server by running `make server` and then executes it by running `stack exec server` in an isolated nix-shell environment.
Upon running `./run-server.sh`, you should be able to access the webserver at [http://localhost:8000](http://localhost:8000).
