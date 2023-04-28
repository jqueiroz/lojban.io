# lojban.io

The main goal of this project is to create a robust teaching platform for the artificial language [Lojban](https://mw.lojban.org/papri/Lojban).
This project is inspired by Duolingo, but it is somewhat more specific in that it exploits some of Lojban's unique features, such as machine-parsability.
It is written almost entirely in Haskell.

An experimental version of the platform is available at [https://lojban.io](https://lojban.io).
You may also install the progressive web app from the [Play Store](https://play.google.com/store/apps/details?id=io.lojban.twa).

Haskell documentation for the currently deployed version may be found at [https://lojban.io/docs](https://lojban.io/docs).

You may run this project on any Linux environment (including the [Windows Subsystem for Linux](https://docs.microsoft.com/en-us/windows/wsl/install-win10)) without installing any packages into your system.
Well, you do need to install either [Nix](#quickstart-using-nix) or [docker](#quickstart-using-docker), depending on how you intend to run it, but that's all; this project runs with libraries and binaries independent of those installed on your system.

<!-- maybe: "An interesting feature of this project is sentence canonicalization..." (from announcement) -->

## Table of contents
* [Contributing (as a nonprogrammer)](#contributing-as-a-nonprogrammer)
  + [Providing glosses for brivla places](#providing-glosses-for-brivla-places)
  + [Curating translations](#curating-translations)
    - [Guidelines for translations](#guidelines-for-translations)
    - [Reviewers welcome](#reviewers-welcome)
  + [Preparing video lectures](#preparing-video-lectures)
  + [Improving existing lessons](#improving-existing-lessons)
* [Contributing (as a programmer)](#contributing-as-a-programmer)
  + [Contributing new exercise types](#contributing-new-exercise-types)
  + [Contributing a new course or deck (or porting an existing one)](#contributing-a-new-course-or-deck-or-porting-an-existing-one)
    - [1. You may define the course entirely in Haskell](#1-you-may-define-the-course-entirely-in-haskell)
    - [2. You may define the course in Haskell, but load translations from a separate YAML file](#2-you-may-define-the-course-in-haskell-but-load-translations-from-a-separate-yaml-file)
    - [3. You may define the course entirely in a JSON file](#3-you-may-define-the-course-entirely-in-a-json-file)
  * [Building a separate platform for course creation](#building-a-separate-platform-for-course-creation)
  * [Implementing alternative exercises for mobile](#implementing-alternative-exercises-for-mobile)
  * [Adding support for alternative ortographies](#adding-support-for-alternative-ortographies)
  * [Building something on top of our APIs](#building-something-on-top-of-our-apis)
* [Contributing (as a Haskell programmer)](#contributing-as-a-haskell-programmer)
  + [Making code improvements](#making-code-improvements)
  + [Deeper technical aspects](#deeper-technical-aspects)
* [Quickstart (using Docker)](#quickstart-using-docker)
* [Quickstart (using Nix)](#quickstart-using-nix)
* [Building and running (using Nix)](#building-and-running-using-nix)

## Contributing (as a nonprogrammer)

There are many ways to contribute to this project, even if you are not familiar with computer programming.
Some of them involve editing (human-readable) text files.
In these cases, you may either send me the new file via [email](mailto:jonathan@lojban.io) or learn [how to create a pull request](https://docs.github.com/en/github/collaborating-with-issues-and-pull-requests/creating-a-pull-request).
A pull request allows you to make changes in a project and then propose these changes to others (in this case, me).

If you have any ideas or suggestions, whether or not you are willing or able to work on them, please feel free to [open an issue](https://github.com/jqueiroz/lojban.io/issues/new) explaining them.
You may also open an issue if you have a proposal which you would like to discuss before making a contribution.
Don't worry about the name, issues are not necessarily bad things :-)

Also feel free to email me directly: [jonathan@lojban.io](mailto:jonathan@lojban.io).

To help you get started, we prepared a list of suggested ways to contribute, ranging from highly localised, and hence low commitment, to very broad.
Of course, contributions outside of this list would be equally appreciated!

### Providing glosses for brivla places

For each brivla covered in courses or decks, we need curated glosses (short definitions) for each of the places in that brivla.
This allows us to automatically generate certain exercises (e.g. [1](docs/exercises/powered_by_glosses/1.png), [2](docs/exercises/powered_by_glosses/2.png), [3](docs/exercises/powered_by_glosses/3.png)).
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

Ideally, Lojban sentences for the Contextualized brivla deck should be intuitively parsable by someone who has gone through the entirety of the [Getting started with Lojban](https://lojban.io/courses/introduction/) course, and understandable as long as that person is equipped with [hover hints](docs/hover.png) and/or a dictionary.

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

If you are a programmer in any language ([Brainfuck](https://en.wikipedia.org/wiki/Brainfuck), [Whitespace](https://en.wikipedia.org/wiki/Whitespace_(programming_language)), [Turing machines](https://en.wikipedia.org/wiki/Turing_machine), [Lambda calculus](https://en.wikipedia.org/wiki/Lambda_calculus), [μ-recursive functions](https://en.wikipedia.org/wiki/General_recursive_function) and [Conway's Game of Life](https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life) sadly excluded), then there are a few more ways in which you could contribute to this project.

### Contributing new exercise types

The client-side code for the elementary exercise types (e.g. single-choice exercises, typing exercises, and so on) is powered by some ancient, good ol' JavaScript and jQuery code which I wrote back in 2016.
Well, I have since converted that code to TypeScript, but that was the extent of my refactoring.
I intend to port exercises to React, to allow others to contribute new exercise types as React components.

That work hasn't started yet.
But if you are interested in implementing new exercise types, let me know, and I will prioritize it.

We will need to converge on the object model for the new exercise type, and then you can work on the client side while I work on the server side. (Or, if you know Haskell, you can do both.)

### Contributing a new course or deck (or porting an existing one) 

There are two major aspects in creating a new course:
1. Writing the lesson texts.
This can be done by pretty much anyone, and only (a small amount of) Markdown knowledge is required.
2. Preparing suitable exercises for each lesson.
This currently requires programming knowledge (but see the next section, [Building a separate platform for course creation](#building-a-separate-platform-for-course-creation)).

In addition to creating an entirely new course, you may alternatively work on top of an existing one and just add exercises.
It would be great to see [la karda](https://mw.lojban.org/papri/la_karda) or [The Crash Course](https://mw.lojban.org/papri/The_Crash_Course_(a_draft)), for example.
Interactive exercises for each section of [The Complete Lojban Language](https://lojban.github.io/cll/) would be highly desired as well, though that would likely require substantially more effort.

Below, we describe the three main approaches for creating a new course (or deck, with minor modifications).
Decks are similar to courses, except they have cards instead of lessons.
Cards are generally associated with a single Lojban word or construct, and require accompanying exercises for students to practice that word.

If you are interested in preparing a deck, we have a few suggestions as well:
* A deck covering useful vocabulary for chatting, such as _coi_, _doi_, _ki'e_, _mi'e_, a few discursives (e.g. _ku'i_), a few attitudinals, and common phrases (see also the [IRC cheat sheet](https://mw.lojban.org/papri/IRC_cheat_sheet) and [Useful phrases in Lojban](https://www.omniglot.com/language/phrases/lojban.htm)).
* A deck covering abstractors (e.g. _du'u_, _nu_, _ka_, _si'o_) in a contextualized setting.
* A deck covering different question words (e.g. _xu_, _mo_, _xo_, _pei_) in a contextualized setting.
* A deck covering [BAI](https://lojban.org/publications/cll/cll_v1.1_xhtml-section-chunks/section-all-BAI.html).
* A deck covering tenses.
* A deck covering discursives and/or evidentials.
* A deck covering numbers.
* More generally, a deck covering anything which is suitable for flashcard-based software, such as [Memrise](https://www.memrise.com/courses/english/lojban/), [Quizlet](https://quizlet.com/subject/lojban/) and [Anki](https://ankiweb.net/shared/decks/lojban).
That said, ideally decks should be more interesting than just mapping words to definitions, though we understand that may not always be possible.

Of course, also feel free to work on something else entirely!

#### 1. You may define the course entirely in Haskell

  This allows for the utmost flexibility, and is the approach taken by [Getting started with Lojban](https://lojban.io/courses/introduction/).
  We rely on concepts such as [TranslationGenerator](https://lojban.io/documentation/lojbanios-0.1.0.0/src/Core.html#TranslationGenerator) and [ExerciseGenerator](https://lojban.io/documentation/lojbanios-0.1.0.0/src/Core.html#ExerciseGenerator).
  Fundamentally, an ExerciseGenerator is a pure function which takes a random seed as input and produces an exercise as output.
  Likewise for TranslationGenerator.

  Generators may be combined using functions such as [combineGeneratorsUniformly](https://lojban.io/documentation/lojbanios-0.1.0.0/src/Util.html#combineGeneratorsUniformly) and its weighted counterpart, [combineGenerators](https://lojban.io/documentation/lojbanios-0.1.0.0/src/Util.html#combineGenerators).
  The resulting algebra enables very precise control over exercise frequencies, and makes it easy to assemble a diverse and entertaining set of exercises by recursively combining (and reusing) smaller generators.

  There are also many useful utility functions in [Study.Framework.Lojban.ExerciseGenerators](https://lojban.io/documentation/lojbanios-0.1.0.0/Study-Framework-Lojban-ExerciseGenerators.html) for building high-level exercise generators from relatively low-level primitives, such as individual translations or words.
  These functions are generally customizable using strategies, and often accept parameters such as [SentenceCanonicalizer](https://lojban.io/documentation/lojbanios-0.1.0.0/Language-Lojban-Core.html#t:SentenceCanonicalizer), [SentenceComparer](https://lojban.io/documentation/lojbanios-0.1.0.0/Core.html#t:SentenceComparer) and [SimpleBridiDisplayer](https://lojban.io/documentation/lojbanios-0.1.0.0/Language-Lojban-Core.html#t:SimpleBridiDisplayer).
  Examples are [generateTranslationExercise](https://lojban.io/documentation/lojbanios-0.1.0.0/src/Study.Framework.Lojban.ExerciseGenerators.html#generateTranslationExercise), [generateBlacklistedWordTranslationExercise](https://lojban.io/documentation/lojbanios-0.1.0.0/src/Study.Framework.Lojban.ExerciseGenerators.html#generateBlacklistedWordTranslationExercise), [generateMorphologicalClassExercise](https://lojban.io/documentation/lojbanios-0.1.0.0/src/Study.Framework.Lojban.ExerciseGenerators.html#generateMorphologicalClassExercise), [generateFillingBlanksExerciseByAlternatives](https://lojban.io/documentation/lojbanios-0.1.0.0/src/Study.Framework.Lojban.ExerciseGenerators.html#generateFillingBlanksExerciseByAlternatives), [generateFillingBlanksExerciseByExpression](https://lojban.io/documentation/lojbanios-0.1.0.0/src/Study.Framework.Lojban.ExerciseGenerators.html#generateFillingBlanksExerciseByExpression), and several others.

  These concepts, combined with Haskell's high level abstractions, give rise to a very powerful way of defining exercises.

  For an example of a course defined entirely in Haskell, see all files under [haskell/src/Study/Courses/English/Grammar/Introduction](https://github.com/jqueiroz/lojban.io/tree/master/haskell/src/Study/Courses/English/Grammar/Introduction), where we define the course "Getting started with Lojban".
  We also recommend reading the documentation of

  * the exercise generator utilities in [Study.Framework.Lojban.ExerciseGenerators](https://lojban.io/documentation/lojbanios-0.1.0.0/Study-Framework-Lojban-ExerciseGenerators.html);
  * the translation manipulation utilities in [Study.Framework.Lojban.TranslationUtils](https://lojban.io/documentation/lojbanios-0.1.0.0/Study-Framework-Lojban-TranslationUtils.html);
  * and maybe the extractor utilities in [Study.Framework.Lojban.Extractors](https://lojban.io/documentation/lojbanios-0.1.0.0/Study-Framework-Lojban-Extractors.html).

  The module [Language.Lojban.Canonicalization](https://lojban.io/documentation/lojbanios-0.1.0.0/Language-Lojban-Canonicalization.html) contains sensible general-purpose sentence canonicalizers, and [Language.Lojban.Presentation](https://lojban.io/documentation/lojbanios-0.1.0.0/Language-Lojban-Presentation.html) encloses multiple strategies for displaying bridi.
  See also [haskell/src/Study/Courses/English/Grammar/Introduction/Strategies.hs](https://github.com/jqueiroz/lojban.io/blob/master/haskell/src/Study/Courses/English/Grammar/Introduction/Strategies.hs) for the definition of strategies used in the "Getting started with Lojban" course, including the custom sentence comparer.

#### 2. You may define the course in Haskell, but load translations from a separate YAML file

  This is a bit less flexible, in that translations may not be combined anymore using the generator algebra.
  But the advantage is that anyone will be able to contribute to your course by editing the (human-readable) translation files.

  For an example of a course following this approach, see all files under [haskell/src/Study/Courses/English/Grammar/Crash](https://github.com/jqueiroz/lojban.io/tree/master/haskell/src/Study/Courses/English/Grammar/Crash), where we define a course based on a tiny subset of [The Crash Course](https://mw.lojban.org/papri/The_Crash_Course_(a_draft)) by [la gleki](https://github.com/lagleki).
  Our version of this course is highly incomplete, and hence not listed on our website, but it is nevertheless available at [https://lojban.io/courses/crash/](https://lojban.io/courses/crash/).
  The corresponding translation files, which power the interactive exercises, are located in [resources/courses/english/grammar/crash/translations](https://github.com/jqueiroz/lojban.io/tree/master/resources/courses/english/grammar/crash/translations).
  Similarly, the vocabulary files are located in [resources/courses/english/grammar/crash/vocabulary](https://github.com/jqueiroz/lojban.io/tree/master/resources/courses/english/grammar/crash/vocabulary).

#### 3. You may define the course entirely in a JSON file

As long as you are able to provide a JSON file with information about your course, such as title, lessons, exercises for each lesson, and so on, we will be able to host the course on our website.
I actually prepared a [draft schema](https://github.com/jqueiroz/lojban.io/tree/master/schemas/lojbanios.graphql) in [GraphQL](https://graphql.org/) syntax with the necessary definitions for both courses and decks.
It is not yet final, but it is pretty close.
If you are interested in creating a new course or deck, please let me know, and I will (1) finalize the schema; and (2) start working on the server side implementation for consuming these files.

Technically, you could write this JSON file defining your course or deck by hand.
But you will likely want to use a programming language for greater readability and flexibility.
For example, you could have separate Markdown files for each lesson, and additionally a Python script which reads these files, combines them with other data (e.g. a list of exercises and/or translations), and then produces the final JSON output.

The upside of this approach is that it is accessible to anyone with programming knowledge, and not only to Haskell programmers.
If you choose a popular programming such as Python, then in principle many others will be able to contribute.
It is also very flexible, and gives you full control over how to structure your course.

The downside, of course, is that it still requires programming knowledge.

### Building a separate platform for course creation

This related to [3. You may define the course entirely in a JSON file](#3-you-may-define-the-course-entirely-in-a-json-file).
As mentioned there, the existence of a well-defined schema allows programmers to define their own courses and decks.
But more than that, it also enables the development of a separate, user-friendly platform for programmers and nonprogrammers alike to colaboratively produce new courses and decks.

I have a few ideas on this subject already, so if you are interested, please let me know and we can work closely on the design.
I can also help integrate with our exercise scripts, to allow real-time preview of exercises being created, and expose APIs for validating solutions if needed.

### Implementing alternative exercises for mobile

Currently the exercises for mobile are the same as those for the desktop version.
But typing on mobile is generally much less convenient than typing on a desktop.
It would be nice to have a user-specific setting allowing sentences to be constructed by clicking on words in the correct order, instead of typing them.

This will require a server-side change for the exercises API to return a list of words potentially useful in the answer (which I can make), and a client-side change to present this new interface to the user.
Ideally, the client should display the words returned by the server as well as broadly used words such as the _FA_-series, the _SE_-series, _cu_, and some terminators.

I have a few ideas already, so please let me know if you are interested and we can discuss.

### Adding support for alternative ortographies

It would be nice to have a user-specific setting for switching all Lojban content to an alternative ortography, such as [la zbalermorna](https://jackhumbert.github.io/zbalermorna/write-up/) or [la krulermorna](http://lojban.pw/articles/krulermorna/).
This could be done using some JavaScript code which looks for content tagged as Lojbanic and automatically converts it to the user's preferred ortography.

### Building something on top of our APIs

We have not done so yet, but it would be pretty simple to expose a REST endpoint which runs our sentence canonicalization logic against an arbitrary input, optionally with some customizable settings, and returns the results.
If you are interested, please let me know, and I will do that right away.

## Contributing (as a Haskell programmer)

Hello, fellow Haskeller!
I hope that you are having loads of fun writing <s>pointless</s> point-free functions (within reason).
I know I am.

If you know Haskell (or are learning Haskell), feel free to explore the code and submit a PR enhancing or implementing whatever you would like.
The documentation at [https://lojban.io/docs](https://lojban.io/docs) should be useful.
Also, I have many ideas about features I would like to see implemented, so please reach out to me at [jonathan@lojban.io](mailto:jonathan@lojban.io) if you'd like some suggestions.

### Making code improvements

I started this project back in 2016, mostly because I wanted to learn and practice Haskell, and partly to learn Lojban as well.
As a result, some parts of the code, particularly the ones written earliest, do not follow best practices.
For example, there are several instances of partial functions.
Less importantly, monad transformers are used directly instead of e.g. [mtl](https://hackage.haskell.org/package/mtl) or [freer monads](https://hackage.haskell.org/package/freer).

Feel free to make any code improvements you deem necessary or useful.
They will be deeply appreciated.
Also feel free to point out bad patterns, even if you are unable to address them directly, and I will eventually get those fixed.
I am still learning, and this project remains an excellent excuse for me to exercise my Haskell.

<!-- TODO: pandoc - more generic syntax, instead of random snippets of embedded html -->

### Deeper technical aspects

If you are interested in the deeper technical aspects of this project, most notably parsing and sentence canonicalization, you should check out the [zasni-gerna](https://github.com/YoshikuniJujo/zasni-gerna) Lojban parser from [Yoshikuni Jujo](https://github.com/YoshikuniJujo) as well as our own sentence canonicalization logic in [haskell/src/Language/Lojban/Canonicalization/Internals.hs](https://github.com/jqueiroz/lojban.io/blob/master/haskell/src/Language/Lojban/Canonicalization/Internals.hs).
Our sentence canonicalization logic takes as input the parse tree produced by zasni-gerna.

Beware that I do not make any assurances regarding the quality, understandability, maintainability, or overall fitness and suitability of that particular piece of s... I mean, code.
It works though (for the modest subset of the language which it was intended to cover)
To see what is currently supported, you may check our unit tests in [haskell/tests/language/Language/Lojban/Canonicalization/Tests.hs](https://github.com/jqueiroz/lojban.io/blob/master/haskell/tests/language/Language/Lojban/Canonicalization/Tests.hs).

Whatever does not work is not the fault of zasni-gerna, but rather of our own incomplete handling of the resulting parse tree.
I generally extend the sentence canonicalizer on demand, whenever new Lojban constructs need to be supported to address the needs of exercises in new lessons.

You may also be interested in reading about [parsing expression grammars](https://en.wikipedia.org/wiki/Parsing_expression_grammar) (PEGs), which were the approach used by Yoshikuni Jujo to make the zasni-gerna parser.
He also created a Haskell library called [papillon](https://github.com/YoshikuniJujo/papillon), a PEG parser generator which was used to make the zasni-gerna library.

## Quickstart (using Docker)

The remainder of this document focuses on building and running the project locally.
We assume a Linux environment, but the [Windows subsystem for Linux](https://docs.microsoft.com/en-us/windows/wsl/install-win10) also works.
If you are running WSL 1, you may build and run the project [using Nix](https://github.com/jqueiroz/lojban.io#quickstart-using-nix).
If you are running WSL 2, you may build and run the project using either Nix or Docker.

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
On most Linux distributions, this can be achieved by running `curl -L https://nixos.org/nix/install | sh` and following the instructions, if any (see also [Getting Nix](https://nixos.org/nix/download.html)).

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
* `./buildscripts/stack.sh`: runs [stack](https://docs.haskellstack.org/en/stable/README/) inside an isolated nix-shell environment, eg. `./buildscripts/stack.sh build` will build the webserver executable (excluding assets) and `./buildscripts/stack.sh haddock --no-haddock-deps` will build the Haskell documentation.

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

By default, `make` will build both the webserver executable (including assets) and the associated documentation, powered by [Haddock](https://haskell-haddock.readthedocs.io/en/latest/).
If you only want to build the webserver (including assets), run `make server`.
If you only want to build the documentation, run `make docs`.
If you only want to compile the _\*.less_ files (located in `./assets/less`), perhaps because you are editing them and want to see the style changes on the fly, run `make css`.
Similarly, if you only want to compile the _\*.ts_ files (located in `./assets/typescript`), run `make js`.

When you run `make docs`, the project's documentation will be placed into _".stack-work/install/x86\_64-linux-nix/lts-16.31/8.8.4/doc/index.html"_ (the command's output shows the full path, which you may copy into a browser).
Similarly, when you run `make server`, the webserver executable will be placed into _".stack-work/install/x86\_64-linux-nix/lts-16.31/8.8.4/bin/lojbanios"_, and you may execute it directly if you want as long as you set the appropriate environment variable: `LOJBANIOS_ENVIRONMENT=dev`.

However, the preferred method for running the webserver is to use the helper script `./run-server.sh`.
This script automatically builds the server by running `make server` and then executes it by running `stack exec server` in an isolated nix-shell environment, with the appropriate environment variables.
Upon running `./run-server.sh`, you should be able to access the webserver at [http://localhost:8000](http://localhost:8000).

