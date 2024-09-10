module Lib1
    ( completions
    ) where

-- | This function returns a list of words
-- to be autocompleted in your program's repl.
completions :: [String]
completions = ["add_book remove_book list_books create_collection add_to_collection remove_from_collection list_collection merge_collections remove_collection"]
