-- | This modules defines primitive fusion operations for pull streams.

module Run.Streaming.Pull
  ( fuse
  , chain
  , for
  ) where

import Prelude
import Run (Run)
import Run.Streaming as RS

-- | Connects a Client to a Server (or less generally, a Consumer to a Producer),
-- | where the Client pulls information from the Server.
fuse ∷ ∀ r i o a. RS.Client r i o a → RS.Server r i o a → Run r a
fuse ra rb = join $ RS.fuse <$> RS.runAwait ra <*> RS.runYield rb

-- | Connects a Client to a dynamic Server which depends on the request
-- | of the client. Practically, this means one can write a stateful Server
-- | jump started by an initial request from the Client.
chain ∷ ∀ r i o a. RS.Client r i o a → (i → RS.Server r i o a) → Run r a
chain ra k = RS.runAwait ra >>= RS.interleave (RS.runYield <$> k)

-- | Fulfills a Client/Consumer with effects.
for ∷ ∀ r i o a. RS.Client r i o a → (i → Run r o) → Run r a
for ra k = RS.runAwait ra >>= RS.substitute k
