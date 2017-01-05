module Utils where

import ParseData

openTag ::  String -> String
openTag s = _TAG_START ++ s

closeTag :: String -> String
closeTag s = _TAG_END  ++ s
