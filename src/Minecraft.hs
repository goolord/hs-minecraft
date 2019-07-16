{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language DeriveFunctor #-}
{-# language DerivingStrategies #-}
{-# language GADTs #-}
{-# language KindSignatures #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language RankNTypes #-}
{-# language StandaloneDeriving #-}
{-# language TypeInType #-}
{-# language TypeOperators #-}

module Minecraft where

import Data.Kind
import Topaz.Types

data Universe
  = Mine
  | Inventory [Item]
  | UItem Item
  | Creeper
  | Pickaxe
  | Lava

data Item 
  = Diamond
  | Coal

data MineVal :: Universe -> Type where
  VItem :: Item -> MineVal ('UItem x)
  VWoodPickaxe :: MineVal 'Pickaxe
  VDiamondPickaxe :: MineVal 'Pickaxe

data Craft :: (Universe -> Type) -> Universe -> Type where
  CraftInventory :: Rec f '[ 'UItem u ] -> Craft f ('Inventory us)
  CraftVal :: MineVal u -> Craft f u
  PlayMinecraft :: MineVal ('Inventory '[]) -> Craft f 'Pickaxe
  MineDiamonds :: MineVal ('Inventory xs) -> MineVal 'Pickaxe -> Craft f ('Inventory (x ': xs))
  DigDown :: MineVal 'Lava -> Craft f ('Inventory '[])

playMinecraft :: forall (u :: Universe).
     (forall (f :: Universe -> Type). Craft f u)
  -> MineVal u
playMinecraft e0 = go e0 where
  go :: forall v. Craft MineVal v -> MineVal v
  go = \case
    PlayMinecraft x -> VWoodPickaxe
    MineDiamonds resources pickaxe -> case pickaxe of
      VWoodPickaxe -> go $ CraftInventory $ RecCons (VItem Coal) RecNil
      VDiamondPickaxe -> go $ CraftInventory $ RecCons (VItem Diamond) RecNil

