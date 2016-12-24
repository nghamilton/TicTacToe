{-- from 

*Main> :i ReaderT
type role ReaderT representational representational nominal
newtype ReaderT r (m :: k -> *) (a :: k)
  = ReaderT {runReaderT :: r -> m a}

--}
newtype ReaderT r (m :: k -> *) (a :: k)
  = ReaderT {runReaderT :: r -> m a}

-- what are KindSignatures?
