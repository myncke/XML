#! /bin/bash
echo "==============================================================================="
echo -e " Building: Project Functional Programming "
echo "==============================================================================="

  mkdir .bin &2>/dev/null

  cp -r src/* .bin

  cd .bin
  ghc Main.hs -fno-warn-tabs -XGeneralizedNewtypeDeriving -framework IOKit -framework CoreFoundation
  cd ..
  cp .bin/Main RUN


echo "==============================================================================="
echo -e " Test: WHILE.XML"
echo "==============================================================================="
  ./RUN  Examples/POPO.XML
