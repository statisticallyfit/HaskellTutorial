


-- help todo how is this function used?
newtype Reader r a = Reader {runReader :: r -> a}

ask :: Reader a a
ask = Reader id