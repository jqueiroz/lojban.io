# lojban.io

The main goal of this project is to create a robust teaching platform for the artificial language [Lojban](https://mw.lojban.org/papri/Lojban).
This project is inspired by Duolingo, but it is somewhat more specific in that it exploits some of Lojban's unique features, such as machine-parsability.
It is written almost entirely in Haskell.

An experimental version of the platform is available at [https://lojban.io](https://lojban.io).
<!-- TODO: You may also install the progressive web app from... -->

Haskell documentation for the currently deployed version may be found at [https://lojban.io/docs](https://lojban.io/docs).

## Contributing (as a nonprogrammer)

There are many ways to contribute to this project, even if you are not familiar with computer programming.
Some of them involve editing (human-readable) text files.
In these cases, you may either send me the new file via [email](mailto:jonathan@lojban.io) or learn [how to create a pull request](https://docs.github.com/en/github/collaborating-with-issues-and-pull-requests/creating-a-pull-request).
A pull request allows you to make changes in a project and then propose these changes to others (in this case, me).

If you have any ideas or suggestions, whether or not you are willing or able to work on them, please feel free to [open an issue](https://github.com/jqueiroz/lojban.io/issues/new) explaining them.
You may also open an issue if you have a proposal which you would like to discuss before making a contribution.
Don't worry about the name, issues are not necessarily bad things :)

Also feel free to email me directly: [jonathan@lojban.io](mailto:jonathan@lojban.io).

To help you get started, we prepared a list of suggested ways to contribute, ranging from highly localised, and hence low commitment, to very broad.
Of course, contributions outside of this list would be equally appreciated!

### Providing glosses for brivla places

For each brivla covered in courses or decks, we need curated glosses (short definitions) for each of the places in that brivla.
<!-- TODO: This allows us to generate exercises such as [img] -->
For example, we came up with the following glosses for the word _tavla_ &ndash; "x<sub>1</sub> talks/speaks to x<sub>2</sub> about subject x<sub>3</sub> in language x<sub>4</sub>":
* x<sub>1</sub>: speaker;
* x<sub>2</sub>: listener;
* x<sub>3</sub>: subject;
* x<sub>4</sub>: language.

To contribute place-specific glosses for new brivla, all you need is to add them to the file [resources/language/dictionary-generation/english/brivla-places.yaml](https://github.com/jqueiroz/lojban.io/blob/master/resources/language/dictionary-generation/english/brivla-places.yaml), following the established pattern.
You may also edit this file to improve the glosses for existing words.

<!-- TODO: As soon as you contribute a new word, it will be available as a new card in the "Isolated brivla" deck, and you will be helping other people study that word. How cool is that? -->

### Curating translations

Personally, I really like the [Contextualized brivla](https://lojban.io/decks/contextualized-brivla/) deck.
It helps me learn new words while also getting exposed to common patterns used by Lojbanists.
But it relies very heavily on (good) translations, which are difficult to find.

I have a script which takes translations from [Tatoeba](https://tatoeba.org/eng/sentences/search?query=&from=eng&to=jbo&user=&orphans=no&unapproved=no&has_audio=&tags=&list=&native=&trans_filter=limit&trans_to=und&trans_link=&trans_user=&trans_orphan=&trans_unapproved=&trans_has_audio=&sort=relevance&sort_reverse=) and generates an initial version of a "translation catalogue" file for a chosen set of brivla.
Each file contains translations for around 15 brivla.
But translations from Tatoeba are often incorrect, so these files require manual curation, consisting of removing and/or fixing bad translations, as well as adding new ones.

If you are interested, you may work on any of the draft translation catalogue files at [resources/decks/english/brivla/sentences/drafts](https://github.com/jqueiroz/lojban.io/tree/master/resources/decks/english/brivla/sentences/drafts), for example [drafts/06.yaml](https://github.com/jqueiroz/lojban.io/tree/master/resources/decks/english/brivla/sentences/drafts/06.yaml).
When you believe one of these files is ready, please let me know.
I will briefly review it and then move it out of the "drafts" folder.
The 15 or so brivla covered in that file will then be available as new cards in the Contextualized brivla deck.

Finally, even files [outside of the "drafts" folder](https://github.com/jqueiroz/lojban.io/tree/master/resources/decks/english/brivla/sentences) may contain mistakes, so feel free to review them as well if you're interested.
Likewise, additional translations will never hurt, so feel free to add new ones.
Not being a draft just means that a file meets the quality bar for consumption, not that it is perfect.

#### Guidelines for translations

Ideally, Lojban sentences for the Contextualized brivla deck should be intuitively parsable by someone who has gone through the entirety of the [Getting started with Lojban](https://lojban.io/courses/introduction/) course, and understandable by someone equipped with hover hints and/or a dictionary.

<!-- TODO: image of hover hints -->

To make exercises less repetitive, we aspire to have at least five translations per brivla.
But the more the merrier.

#### Reviewers welcome

If you have an intermediate or higher knowledge of Lojban and would like to help, please let me know, and I will designate you as the primary approver for the translation catalogue files.

### Preparing video lectures

If you are interested, you may prepare video lectures for one or more lessons in the [Getting started with Lojban](https://lojban.io/courses/introduction/) course, as an alternative to the written text, and I will gladly link to your video in the corresponding lesson.

### Improving existing lessons

Lessons for the [Getting started with Lojban](https://lojban.io/courses/introduction/) course are Markdown files located in [resources/courses/english/grammar/introduction/lectures](https://github.com/jqueiroz/lojban.io/tree/master/resources/courses/english/grammar/introduction/lectures) (with some lightweight embedded HTML, mostly for styling word definitions).
Feel free to edit those files to make improvements.

## Contributing (as a programmer)

If you are a programmer in any language ([Brainfuck](https://en.wikipedia.org/wiki/Brainfuck) and [Whitespace](https://en.wikipedia.org/wiki/Whitespace_(programming_language)) excluded), then there are a few more ways in which you could contribute to this project.

### Contributing new exercise types

The client-side code for the elementary exercise types (e.g. single-choice exercises, typing exercises, and so on) is powered by some ancient, good ol' JavaScript and jQuery code which I wrote back in 2016.
Well, I have since converted that code to TypeScript, but that was the extent of my refactoring.
I intend to port exercises to React, to allow others to contribute new exercise types as React components.

That work hasn't started yet.
But if you are interested in implementing new exercise types, let me know, and I will prioritize it.

<!--
### Contribute a new course or deck (including exercises) 
Currently, there are three ways to create a new course 

not just creating a new course; you may also take an existing course outside and port/create exercises 

The reason this requires programming knowledge right now is that... see next item 

### Develop a separate platform for course and deck creation

### Use one of our APIs to build something 

## Contributing (as a Haskell programmer) 

Hello, fellow Haskeller!
I hope that you are having fun writing <s>pointless</s> point-free functions (within reason).

If you know Haskell (or are learning Haskell), feel free to... 

If you are interested in parsing ... generalize sentence canonicalizer ... tests located in...
-->

## Quickstart (using Docker)

<!--
TODO: we assume a Linux environment 
Windows with WSL should also work, though we haven't tested it. (TODO: test it)
-->

The simplest way to run this project is to execute the webserver inside a [Docker](https://www.docker.com/) container.
First, run `./virtualization/docker-build.sh` to build the image.
Then, run `./virtualization/docker-run.sh` to start the container.
This command will bind the webserver running on the container to port 8080 on the host, thus letting you access the application at [http://localhost:8080](http://localhost:8080).
To publish a port other than 8080, use `./virtualization/docker-run.sh -p <port>`.

By default, `./virtualization/docker-build.sh` uses an [intermediary image](https://hub.docker.com/r/johnjq/lojbanios-dependencies) to speed up the build.
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

<!-- TODO: Section: Technical aspects (parsing, canonicalization, exercise generation, etc) -->

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
Similarly, when you run `make server`, the webserver executable will be placed into _".stack-work/install/x86\_64-linux-nix/lts-13.27/8.6.5/bin/lojbanios"_, and you may execute it directly if you want as long as you set the appropriate environment variable: `LOJBANIOS_ENVIRONMENT=dev`.

However, the preferred method for running the webserver is to use the helper script `./run-server.sh`.
This script automatically builds the server by running `make server` and then executes it by running `stack exec server` in an isolated nix-shell environment.
Upon running `./run-server.sh`, you should be able to access the webserver at [http://localhost:8000](http://localhost:8000).
