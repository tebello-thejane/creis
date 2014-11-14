##Creissels' wordlist parser and searcher.

This project aims to salvage Creissels' 1996 wordlist. In particular

* The original wordlist is in an outdated MacOS text file format, 
* Creissels' narrow transcription scheme, while preserving every important aspect of the words, is unusable in its present form, and
* The wordlist is not searchble for Setswana words in plain text.

This project translates the transcription -- taking advantage of modern Unicode &c -- and provides a simple interface to perform plain text searches.

I wrote this program to allow me to further my personal Sesotho research (descriptions of the tones of individual words in Sesotho are near-impossible 
to come by, and Setswana is a reasonable proxy). Perhaps somebody else might also find it useful.

###Technical details
####Requirements

In order to compile the source, you need GHC. Development was on GHC 7.8.3, but it seems to compile with 7.6.3 as well.

You also need cabal-install. Development was on 1.10.

Haskell libraries will be fetched automatically and installed in a sandbox, so you don't need much to compile the source besides a working GHC and cabal-install.

You need the dev versions of the readline library installed on your system. **This requirement makes the software incompatible with Windows**. Sorry. Maybe it will work under Cygwin/MinGW. I don't know.

####Configuring and installing

Be sure to update your Cabal index before starting by running `cabal update`. If `cabal --version` produces a number lower than 1.10, then you should also run `cabal install cabal-install` afterwards.

To configure the application, run `./configure`, then run `cabal install` to build it.

The main executable will be found under `bin/creis`.

To remove all the files produced by the build process (including the main executable), then run `./clean`.

####Text file had to be processed slightly in order to work:

* Encoding had to be set to UTF-8.
* Mac LF line endings had to be converted to *nix CRLF.

The original file (from the [Comparative Bantu Online Dictionary](http://www.cbold.ish-lyon.cnrs.fr/)) is available here:
  [Tswana.Creissels1996.txt](http://www.cbold.ish-lyon.cnrs.fr/Load.aspx?Langue=Tswana&Type=Text&Fichier=Tswana.Creissels1996.txt)
