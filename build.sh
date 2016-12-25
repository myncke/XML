#! /bin/bash
echo "===============================================================================" 
echo " Building: Project Functional Programming "
echo "===============================================================================" 

  mkdir .bin &2>/dev/null

  cp -r src/* .bin

  cd .bin
  ghc Main.hs -fno-warn-tabs
  cd ..
  cp .bin/Main RUN


echo "===============================================================================" 
echo " Test: SimpleRuby.example"
echo "===============================================================================" 
  ./RUN  Examples/SimpleRuby.example
