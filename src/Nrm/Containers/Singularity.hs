{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Nrm.Containers.Singularity
Description : Singularity container runtime
Copyright   : (c) 2019, UChicago Argonne, LLC.
License     : BSD3
Maintainer  : fre@freux.fr

This module offers an interface to singularity containers.
-}
module Nrm.Containers.Singularity
  ( Singularity (..)
  )
where

data Singularity = Singularity
