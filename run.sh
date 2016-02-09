#!/bin/bash
ghci -isrc  -package-db ~/.cabal-dev/Users-adam-repos-fay-transliterate/master/packages-7.8.3.conf/ -no-user-package-db -package fay-text src/Translit/Hangeul.hs -XOverloadedStrings
