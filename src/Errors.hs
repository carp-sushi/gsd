module Errors
  ( customErrorFormatters
  , badRequest
  , notFound
  )
where

import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import Data.Text (Text)
import Network.HTTP.Types.Header
import Servant hiding (Header)

-- Format body parse errors as JSON.
customErrorFormatters :: ErrorFormatters
customErrorFormatters =
  defaultErrorFormatters
    { bodyParserErrorFormatter = bodyParserJsonFormatter
    }

-- Custom JSON formatter for body parser errors.
bodyParserJsonFormatter :: ErrorFormatter
bodyParserJsonFormatter _ _ = mk400

-- Helper for creating bad request server errors.
badRequest :: Text -> ServerError
badRequest = mk400

-- Helper for creating not found server errors.
notFound :: Text -> ServerError
notFound msg =
  err404
    { errBody = jsonError msg
    , errHeaders = jsonHeader
    }

-- Internal helper. NOTE: Don't know why this can't just be the badRequest function, but changing
-- it to be so results in call point errors in handlers.
mk400 :: (ToJSON err) => err -> ServerError
mk400 err =
  err400
    { errBody = jsonError err
    , errHeaders = jsonHeader
    }

-- JSON error body
jsonError :: (ToJSON err) => err -> BS.ByteString
jsonError err =
  encode $ object ["error" .= err]

-- JSON error content type headers
jsonHeader :: [Header]
jsonHeader =
  [("Content-Type", "application/json")]
