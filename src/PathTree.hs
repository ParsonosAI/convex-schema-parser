module PathTree
  ( PathTree (..),
    buildPathTree,
  )
where

import qualified Convex.Action.Parser as Action
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as Map

data PathTree = FuncNode Action.ConvexFunction | DirNode (Map.Map String PathTree) deriving (Show)

buildPathTree :: [Action.ConvexFunction] -> PathTree
buildPathTree = foldl (flip insertFunc) (DirNode Map.empty)
  where
    insertFunc :: Action.ConvexFunction -> PathTree -> PathTree
    insertFunc func (DirNode dir) = DirNode (go (splitOn "/" (Action.funcPath func)) func dir)
    insertFunc _ node = node

    go :: [String] -> Action.ConvexFunction -> Map.Map String PathTree -> Map.Map String PathTree
    go [] func dir = Map.insert (Action.funcName func) (FuncNode func) dir
    go (p : ps) func dir =
      let subTree = Map.findWithDefault (DirNode Map.empty) p dir
          newSubTree = case subTree of
            DirNode subDirMap -> DirNode (go ps func subDirMap)
            FuncNode _ -> error $ "Path conflict: cannot create submodule in path containing function: " ++ p
       in Map.insert p newSubTree dir
