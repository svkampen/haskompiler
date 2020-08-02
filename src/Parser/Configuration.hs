module Parser.Configuration where

import Data.Default

data ParseConfiguration = PConfig
    { qqSupport :: Bool -- ^ Enable parser support for quasiquoting (antiquotations).
    } deriving (Show)

instance Default ParseConfiguration where
    def = PConfig False
