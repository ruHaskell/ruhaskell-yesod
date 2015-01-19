module Model where

import ClassyPrelude.Yesod
import Database.Persist.Quasi
import Text.Markdown (Markdown)
import Yesod.Text.Markdown ()
import Text.Printf (printf)

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

renderDateAsTuple :: UTCTime -> String
renderDateAsTuple time =
    case toGregorian . utctDay $ time of
        (y, m, d) -> printf "(%d, %02d, %02d)" y m d

withRemaining :: [a] -> [(a, Int)]
withRemaining xs = zip xs (reverse (enumFromTo 0 (length xs - 1)))
