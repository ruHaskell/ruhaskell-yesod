module Model where

import ClassyPrelude.Yesod
import Database.Persist.Quasi
import Text.Markdown (Markdown)
import Yesod.Text.Markdown ()

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")
