module Errors
  ( customFormatters
  , badRequest
  , notFound
  )
where

import Data.Aeson
import Data.Text (Text)
import Servant

-- Custom error formatter.
customFormatters :: ErrorFormatters
customFormatters =
  defaultErrorFormatters
    { bodyParserErrorFormatter = bodyParserJsonFormatter
    }

-- Custom JSON error formatter for body parser errors.
bodyParserJsonFormatter :: ErrorFormatter
bodyParserJsonFormatter _ _ err =
  err400
    { errBody = encode $ object ["error" .= err]
    , errHeaders = [("Content-Type", "application/json")]
    }

-- Helper for bad request errors.
badRequest :: Text -> ServerError
badRequest msg =
  err400
    { errBody = encode $ object ["error" .= msg]
    , errHeaders = [("Content-Type", "application/json")]
    }

-- Helper for not found errors.
notFound :: Text -> ServerError
notFound msg =
  err404
    { errBody = encode $ object ["error" .= msg]
    , errHeaders = [("Content-Type", "application/json")]
    }
