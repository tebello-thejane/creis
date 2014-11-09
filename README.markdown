##Creissels' wordlist parser and searcher.

This project aims to salvage Creissels' 1996 wordlist. In particular
* The original wordlist is in an outdated MacOS text file format, 
* The Creissels' narrow transcription scheme, while preserving every important aspect of the words, is unusable in its present form, and
* The file is wordlist is not searchble for Setswana words in plain text.

This project translates the transcription -- taking advantage of modern Unicode &c -- and provides a simple interface to perform palin text searches.

I wrote this program to allow me to further my personal Sesotho research (descriptions of the tones of individual words in Sesotho are near-impossible 
to come by, and Setswana is a reasonable proxy). Perhaps somebody else might also find it useful.

####Text file had to be processed slightly in order to work:
* Encoding had to be set to UTF-8.
* Mac LF line endings had to be converted to *nix CRLF.

The origin file (from the [Comparative Bantu Online Dictionary](http://www.cbold.ish-lyon.cnrs.fr/)) is available here:
  [Tswana.Creissels1996.txt](http://www.cbold.ish-lyon.cnrs.fr/Load.aspx?Langue=Tswana&Type=Text&Fichier=Tswana.Creissels1996.txt)
