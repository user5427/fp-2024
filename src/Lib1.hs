module Lib1
    ( completions
    ) where

-- | This function returns a list of words
-- to be autocompleted in your program's repl.
completions :: [String]
completions = ["add-book remove-book list-books create-collection add-to-collection remove-from-collection list-collection"]
