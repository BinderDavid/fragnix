{-# LINE 1 "./Test/Framework/Runners/Console/Colors.hs" #-}
{-# LINE 1 "dist/dist-sandbox-235ea54e/build/autogen/cabal_macros.h" #-}
                                                                

                                   






                                    






                          






                                






                          






                                






                        






                                






                      






                        






                     






                       






                  






                    






                        






                         






                       






                   






                      






                          







{-# LINE 2 "./Test/Framework/Runners/Console/Colors.hs" #-}
{-# LINE 1 "./Test/Framework/Runners/Console/Colors.hs" #-}
module Test.Framework.Runners.Console.Colors where

import Text.PrettyPrint.ANSI.Leijen


colorFail, colorPass :: Doc -> Doc
colorFail = red
colorPass = green

colorPassOrFail :: Bool -> Doc -> Doc
colorPassOrFail True  = colorPass
colorPassOrFail False = colorFail