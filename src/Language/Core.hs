module Language.Core (
  Composable(..)
) where


class Composable c where
  compose :: c -> c -> c
