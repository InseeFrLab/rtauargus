\docType{package}
\name{rtauargus-package}
\alias{rtauargus-package}
\title{Package 'rtauargus' : Tau-Argus depuis R}

\description{

The \emph{rtauargus} provides an \strong{R} interface for
\strong{Tau-Argus}.

}

\details{

It allows to:

\itemize{
  \item{create inputs (rda, arb, hst and tab files) from data in R format ;}
  \item{generate the sequence of instructions to be executed in batch mode (arb file);}
  \item{launch a τ-Argus batch in command line;}
  \item{retrieve the results in R.}
}

These different operations can be executed in one go, but also in a modular way.
They allow to integrate the tasks performed by τ-Argus in a processing chain written in R.

The package presents other additional functionalities, such as:  

- managing the protection of several tables at once;
- creating a hierarchical variable from correspondence table.

It's possible to choose a tabular or microdata approach, but the tabular 
one is, from now on, encouraged.

The syntax of some arguments is very close to the batch syntax of
Tau-Argus batch syntax. This allows to use a large number of functions without
multiplying the arguments of the functions. Moreover, the package can be adapted
more easily to possible modifications of the software (new available methods,
additional options...). The syntax rules for writing batch can be found 
in the Tau-Argus reference manual.

}

\section{Note}{

The package was developped with Tau-Argus software v.4.2. It is not compatible 
with the v.3.5 of Tau-Argus and earlier.


}

\section{Localisation of TauArgus.exe}{

The function \code{\link{tab_rtauargus}}, \emph{via} \code{\link{run_arb}},
needs to know where the TauArgus.exe file is located on your disk. 
The option \code{rtauargus.tauargus_exe} contains the information 
of its location and has to be filled just after the loading of the package.
The code to modify the default location is:
\code{options(rtauargus.tauargus_exe = "path/to/TauArgus.exe")}
It persists for all the current R session.

This instruction can be placed in a specific program file to persist during 
the session. It's also possible to keep this information permanently 
by writing it in .Rprofile for example.

}

\references{

Tau-Argus : \itemize{

  \item{\href{https://github.com/sdcTools/tauargus}{source code, 
  last release to download, support}(sdcTools github repository)}
  
  \item{\href{https://research.cbs.nl/casc/tau.htm}{manual}
  (on CBS website)}

  

}

}
