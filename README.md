# Monad Transformer Step by Step

Tutorials on stacking monad transformers on the Internet usually are either too simple (only accounts for IO, Either, Maybe) or too complicated. This [paper](https://page.mi.fu-berlin.de/scravy/realworldhaskell/materialien/monad-transformers-step-by-step.pdf) seems to capture a nice balance on monad transofmers that incorporate common monads such as Reader, Write, etc. Struggling while implementing this paper will give you a lot of insights about many Monad related magic and its world (think Monad, Monad Transformers, MonadReader, MonadWriter, MonadError, MonadState, MonadIO, etc).

This code is an attempt to follow the tutorial. Code is a little bit different for my own understanding and also since Haskell has evolved. Copy of PDF is attached in this code. Credit due to the author.
