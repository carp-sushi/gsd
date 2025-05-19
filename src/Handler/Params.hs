module Handler.Params (getPageParams) where

-- Helper to get page number and page size.
getPageParams :: Maybe Int -> Maybe Int -> (Int, Int)
getPageParams maybePage maybeSize =
  (getPage maybePage, getSize maybeSize)
  where
    getPage Nothing = 1
    getPage (Just page) = max page 1
    getSize Nothing = 10
    getSize (Just size) = max 1 (min size 100)
