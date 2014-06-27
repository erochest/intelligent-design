

module ID.GP
    (
    ) where


import           Filesystem.Path.CurrentOS
import           Prelude                   hiding (FilePath, writeFile)
import           Text.XML

import           ID.GP.Types


-- TODO Output JSON information about the gene.
outputMeta :: FilePath -> Gene Element -> Double -> IO ()
outputMeta = undefined
