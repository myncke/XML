#! /bin/bash
echo "==============================================================================="
echo -e " Building: Project Functional Programming "
echo "==============================================================================="

  mkdir .bin &2>/dev/null

  cp -r src/* .bin

  cd .bin
  ghc Main.hs -fno-warn-tabs -XDeriveAnyClass
  cd ..
  cp .bin/Main RUN


echo "==============================================================================="
echo -e " Test: SimpleRuby.example"
echo "==============================================================================="
  ./RUN  Examples/SimpleRuby.example
