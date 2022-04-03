module Rewrite where

import Syntax (Formula, Rltn)

-- | Convert the formula to prenex normal form (PNF), where all quantifiers
-- occor on the outside with a body (aka matrix) where only propositional
-- connectives are used.
pnf :: Formula Rltn -> Formula Rltn
pnf frm = undefined