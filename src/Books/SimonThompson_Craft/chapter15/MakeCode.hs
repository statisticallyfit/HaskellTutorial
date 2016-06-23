module MakeCode  (codes, codeTable) where

import Types
import Frequency (frequency)
import MakeTree  (makeTree)
import CodeTable (codeTable)


-- combining frequency calculation and tree converstion
codes :: [Char] -> Tree
codes = makeTree . frequency