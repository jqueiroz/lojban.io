# Lojban teaching platform

The main goal of this project is to create a robust teaching platform for the artificial language [Lojban](https://mw.lojban.org/papri/Lojban).
This project is inspired by Duolingo, but it is somewhat more specific in that it exploits some of Lojban's unique features, such as machine-parsability.
It is written almost entirely in Haskell.

An experimental version of the platform is available at [https://lojban.io](https://lojban.io).
<!-- TODO: You may also install the progressive web app from... -->

Haskell documentation for the currently deployed version may be found at [https://lojban.io/docs](https://lojban.io/docs).

<!-- TODO: Contributing (as a nonprogrammer) -->

<!-- TODO: Contributing (as programmer) -->
<!-- The remainder of this documentation... -->

<!-- TODO: we assume a Linux environment -->

## Quickstart (using Docker)

The simplest way to run this project is to execute the webserver inside a [Docker](https://www.docker.com/) container.
First, run `./virtualization/docker-build.sh` to build the image.
Then, run `./virtualization/docker-run.sh` to start the container.
This command will bind the webserver running on the container to port 8080 on the host, thus letting you access the application at [http://localhost:8080](http://localhost:8080).
To publish a port other than 8080, use `./virtualization/docker-run.sh -p <port>`.

By default, `./virtualization/docker-build.sh` uses an [intermediary image](https://hub.docker.com/r/johnjq/lojban-tool-dependencies) to speed up the build.
This image includes development tools such as stack and ghc as well as prebuilt dependent libraries, so that only the project's own source code needs to be compiled.
If you would like to perform a full build instead, run `./virtualization/docker-build.sh --full` (but beware that compiling all of the dependencies may take between 10 minutes and one hour).

## Quickstart (using Nix)

If you are making significant changes and/or rebuilding frequently, you will probably benefit from building and running the project outside of Docker.
First, rebuilding after making small changes will be quicker as it will not be necessary to recompile all source files (due to docker layers).
Second, you will be able to use helper scripts to run development tools such as ghci and code linters.

All you need is to install the [Nix package manager](https://nixos.org/nix/).
On most Linux distributions, this can be achieved by running `curl https://nixos.org/nix/install | sh` and following the instructions, if any (see also [Getting Nix](https://nixos.org/nix/download.html)).

Once you have installed Nix, you should be able to start the project by running the following two commands:
1. `./run-redis.sh`: start a redis server listening to a unix socket at _/tmp/lojbanios-redis-dev.sock_.
2. `./run-server.sh`: build the project and then start the webserver.
For the first run, this may take between 10 minutes and one hour as all of the dependencies will need to be compiled.
The webserver may then be accessed at [http://localhost:8000](http://localhost:8000).

Other useful commands:
* `./run-server.sh -p <port>`: builds the webserver executable (including assets) and then starts it on the specified port (defaults to port 8000).
* `make`: builds the webserver executable (including assets) as well as the associated documentation for the Haskell code.
* `make server`: builds the webserver executable (including assets).
* `make docs`: builds the documentation for the Haskell code.
* `make css`: compiles _*.less_ files (located in `./assets/less`) into _*.css_ files (to be saved in `./static/style`). This step is also performed when running `make server`.
* `make js`: compiles _*.ts_ files (located in `./assets/typescript`) into _*.js_ files (to be saved in `./static/scripts`). This step is also performed when running `make server`.
* `./run-ghci.sh`: starts a [GHCi](https://wiki.haskell.org/GHC/GHCi) prompt.
* `./verify-lint.sh`: runs source code linters ([hlint](https://hackage.haskell.org/package/hlint) for Haskell code and [prettier](https://prettier.io/) for Typescript/Javascript/Less/CSS code)
* `./verify-tests.sh`: runs tests for the project.
* `./buildscripts/stack.sh`: runs [stack](https://docs.haskellstack.org/en/stable/README/) inside an isolated nix-shell environment, eg. `./buildscripts/stack.sh build` will build the webserver executable and `./buildscripts/stack.sh haddock --no-haddock-deps` will build the Haskell documentation.

For more details on building and running, see [Building and running (using Nix)](#building-and-running-using-nix).

## Building and running (using Nix)

In order to build and run this project, all you need is a Linux machine with the [Nix package manager](https://nixos.org/nix/) installed somewhere in the path.
On the off-chance that you are running [NixOS](https://nixos.org/), then you are already all set.
Otherwise, if you are running any other Linux distribution, please refer to [Getting Nix](https://nixos.org/nix/download.html) for details on how to install Nix.
In most cases, the setup is pretty simple: as a regular user, just run `curl https://nixos.org/nix/install | sh` and follow the instructions, if any.

The reason we use Nix is that it enables the use of [nix shells](https://nixos.org/nixos/nix-pills/developing-with-nix-shell.html), which are pure and reproducible build environments.
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
If you only want to compile the _\*.less_ files (located in `./assets/less`), perhaps because you are editing them and want to see the style changes on the fly, run `make css`.
Similarly, if you only want to compile the _\*.ts_ files (located in `./assets/typescript`), run `make js`.

When you run `make docs`, the project's documentation will be placed into _".stack-work/install/x86\_64-linux-nix/lts-13.27/8.6.5/doc/index.html"_ (the command's output shows the full path, which you may copy into a browser).
Similarly, when you run `make server`, the webserver executable will be placed into _".stack-work/install/x86\_64-linux-nix/lts-13.27/8.6.5/bin/lojto"_, and you may execute it directly if you want as long as you set the appropriate environment variable: `LOJBAN_TOOL_ENVIRONMENT=dev`.

However, the preferred method for running the webserver is to use the helper script `./run-server.sh`.
This script automatically builds the server by running `make server` and then executes it by running `stack exec server` in an isolated nix-shell environment.
Upon running `./run-server.sh`, you should be able to access the webserver at [http://localhost:8000](http://localhost:8000).
