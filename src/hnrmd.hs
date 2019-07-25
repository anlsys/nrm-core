{-|
Module      : hnrmd
Description : hnrmd
Copyright   : (c) 2019, UChicago Argonne, LLC.
License     : BSD3
Maintainer  : fre@freux.fr
-}

{-# language NoImplicitPrelude #-}
module Main
  ( main
  )
where
import           Protolude
import Nrm.Node.Hwloc (getHwlocData)

main :: IO ()
main = do
  hwlocData <- getHwlocData
  pPrint $ extractOSindexes (Proxy :: Proxy PUId) hwlocData
  pPrint $ extractOSindexes (Proxy :: Proxy CoreId) hwlocData
