module Yaha.Templates where

import Text.Printf (printf)

unexpectedSymErr sym pos = printf "Unexpected symbol \"%c\" at position %d" sym pos

