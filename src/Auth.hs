{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Auth
  ( Account (..)
  , authHandler
  ) where

import Errors (badRequest)

import Data.ByteString (ByteString)
import Network.Wai (Request, requestHeaders)
import Servant
import Servant.Server.Experimental.Auth

import Data.Text (Text)
import Data.Text.Encoding (decodeASCII)

-- Some account address
newtype Account = Account
  { accountAddress :: Text
  }

-- Specify the data returned after auth.
type instance AuthServerData (AuthProtect "account-auth") = Account

-- Simulate account verification
-- TODO: query database against an accounts or users table
verifyAccount :: ByteString -> Handler Account
verifyAccount header =
  if address /= admin
    then throwError $ badRequest "Invalid account address header"
    else return $ Account address
  where
    address = decodeASCII header
    admin = "tp18vd8fpwxzck93qlwghaj6arh4p7c5n89x8kska"

-- Wire up the lookup function as an auth handler.
authHandler :: AuthHandler Request Account
authHandler = mkAuthHandler $ \req ->
  case lookup "x-account-address" (requestHeaders req) of
    Nothing -> throwError $ badRequest "Missing account address header"
    Just address -> verifyAccount address
