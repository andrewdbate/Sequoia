[![License: GPL v3](https://img.shields.io/badge/license-GNU%20GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Build Status](https://travis-ci.org/andrewdbate/Sequoia.svg?branch=master)](https://travis-ci.org/andrewdbate/Sequoia)
[![Build Status](https://ci.appveyor.com/api/projects/status/github/andrewdbate/Sequoia?branch=master&svg=true)](https://ci.appveyor.com/project/andrewdbate/sequoia)

# Sequoia: An Open Source OWL 2 DL Reasoner for Java

Welcome to the official repository for Sequoia!

Sequoia is an ontology reasoner that supports OWL 2 DL ontologies. Sequoia can be used from the command line, through
the Protégé plug-in, or via the OWL API.

Feel free to fork this repository and submit pull requests.

If you discover a bug, please report it [here](https://github.com/andrewdbate/Sequoia/issues) on GitHub.

Sequoia is free and open-source software that is licensed under the [GNU GPL v3](https://github.com/andrewdbate/Sequoia/blob/master/LICENSE) only.

_Note:_ To compile Sequoia you will need JDK 8 installed. Compiling with Java 9 is not supported at this time.

#### Current Limitations
 * Datatypes are not supported.
 * Neither nominals nor ABox assertions are supported.
 * SWRL rules are not supported.

## Publications
The following publications describe the algorithms and implementation of Sequoia.
1. Andrew Bate‚ Boris Motik‚ Bernardo Cuenca Grau‚ František Simančík, Ian Horrocks.
   [**Extending Consequence−Based Reasoning to SRIQ**](https://www.cs.ox.ac.uk/files/8182/paper.pdf).
   In _Principles of Knowledge Representation and Reasoning: Proceedings of the Fifteenth International Conference_.
   Pages 187–196. AAAI Press. 2016.
   **(Main reference on Sequoia)**
   
   [Paper (PDF)](https://www.cs.ox.ac.uk/files/8182/paper.pdf) | [Slides (PDF)](https://www.cs.ox.ac.uk/files/8181/slides.pdf)
   
2. Andrew Bate‚ Boris Motik‚ Bernardo Cuenca Grau‚ František Simančík, Ian Horrocks.
   [**Extending Consequence−Based Reasoning to SRIQ (Technical Report)**](https://arxiv.org/abs/1602.04498).
   arXiv:1602.04498 \[cs.AI\]. February, 2016.
   
   [Technical Report (PDF)](https://arxiv.org/pdf/1602.04498.pdf)
   
3. Andrew Bate‚ Boris Motik‚ Bernardo Cuenca Grau‚ František Simančík, Ian Horrocks.
   [**Extending Consequence−Based Reasoning to SHIQ**](https://www.cs.ox.ac.uk/files/7444/paper.pdf)
   In _Proceedings of the 28th International Workshop on Description Logics_.
   Vol. 1350 of CEUR Workshop Proceedings. CEUR−WS.org. 2015.

   [Paper (PDF)](https://www.cs.ox.ac.uk/files/7444/paper.pdf) | [Technical Report (PDF)](https://www.cs.ox.ac.uk/files/7864/techreport.pdf)

## Development and Building

Sequoia is developed by [Andrew Bate](https://www.linkedin.com/in/andrewdbate/) and is actively maintained.

To build Sequoia, you will need [SBT](https://www.scala-sbt.org/) installed on your system.

The Sequoia reasoner is comprised of multiple subprojects:
 * `reasoner-macros` contains Scala macros used throughout the other subprojects.
 * `reasoner-kernel` contains the implementation of the core algorithm and data structures.
 * `reasoner-owl-api` contains the implementation of the OWL API bindings.
 * `reasoner-cli` contains the implementation of the command-line interface of the reasoner.
 * `reasoner-protege-plugin` contains the implementation of the Protégé plugin.

To compile all subprojects and run all tests in a single command, first clone this repository,
and then from the root directory, type `sbt test`. The tests will take several minutes to complete.

#### Building the Command-Line Client
From the root directory, type `sbt` to launch the SBT REPL. Then type `project cli` and hit Enter, followed by `universal:packageBin` and hit Enter.

#### Building the Protégé Plugin
From the root directory, type `sbt` to launch the SBT REPL. Then type `project protegeplugin` and hit Enter, followed by `osgiBundle` and hit Enter.

### Acknowledgements

Previous versions of Sequoia were developed at the
[Knowledge Representation and Reasoning group](https://www.cs.ox.ac.uk/activities/knowledge/)
at the
[Department of Computer Science](https://www.cs.ox.ac.uk/)
of the
[University of Oxford](https://www.ox.ac.uk).
