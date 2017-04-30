-- | This modules defines primitive fusion operations for push streams.

module Run.Streaming.Push
  ( fuse
  , chain
  , for
  ) where

import Prelude
import Run (Run)
import Run.Streaming as RS

-- | Connects a Server to a Client (or less generally, a Producer to a Consumer),
-- | where the Server pushes information to the Client.
fuse ∷ ∀ r i o a. RS.Server r i o a → RS.Client r i o a → Run r a
fuse ra rb = join $ RS.fuse <$> RS.runYield ra <*> RS.runAwait rb

-- | Connects a Server to a dynamic Client which depends on the output
-- | of the Server. Practically, this means one can write a stateful Client
-- | jump started by an initial event from the Server.
chain ∷ ∀ r i o a. RS.Server r i o a → (o → RS.Client r i o a) → Run r a
chain ra k = RS.runYield ra >>= RS.interleave (RS.runAwait <$> k)

-- | Loops over a Server/Producer with effects.
for ∷ ∀ r i o a. RS.Server r i o a → (o → Run r i) → Run r a
for ra k = RS.runYield ra >>= RS.substitute k
