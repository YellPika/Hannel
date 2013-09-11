module Control.Concurrent.Hannel.Trail (
    Trail (), syncLock, path,
    create, extend,
    dependencies, coherent,
    PathElement (ChooseLeft, ChooseRight, Swap),
) where

import Data.IORef (IORef) 
import Data.List (isSuffixOf)
import Control.Concurrent.Hannel.SyncLock (SyncLock)

import qualified Control.Concurrent.Hannel.SyncLock as SyncLock

-- A path is a record of actions taken by an event.
type Path = [PathElement]

data PathElement
    -- These indicate a choice that was taken. Rather than carry useful data,
    -- they exist simply to differentiate between different paths taken.
    = ChooseLeft
    | ChooseRight
    -- Indicates that an event performed a swap with another event.
    -- The trail refers to the swap partner.
    -- The first completion reference is for receiving other threads.
    -- The second completion reference is for notifying other threads.
    | Swap Trail CompletionRef CompletionRef
  deriving Eq

-- Represents an individual 'search thread'.
data Trail = Trail {
    -- This lock must be acquired before commiting to the trail's root.
    syncLock :: SyncLock,
    path :: Path
} deriving Eq

-- When a search thread completes, it adds its
-- trail and associated commit action to the list.
type CompletionRef = IORef [(Trail, IO ())]

-- A path 'a' extends a path 'b' if 'b' is a suffix of 'a'.
extends :: Path -> Path -> Bool
a `extends` b = b `isSuffixOf` a

-- Two paths are comparable if either one extends the other.
comparable :: Path -> Path -> Bool
comparable a b = a `extends` b || b `extends` a

-- Creates a new trail with an empty path for the current thread.
create :: IO Trail
create = do
    syncLock' <- SyncLock.create
    return $ Trail syncLock' []

-- Extends a trail's path with a path element.
extend :: Trail -> PathElement -> Trail
extend trail element = trail { path = element:path trail }

-- Determines the dependencies of a trail.
dependencies :: Trail -> [Trail]
dependencies thread =
    thread :
    case path thread of
        [] -> []
        ChooseLeft:xs -> dependencies thread { path = xs }
        ChooseRight:xs -> dependencies thread { path = xs }
        Swap partner cRef1 cRef2 : xs ->
            extend partner (Swap (thread { path = xs }) cRef2 cRef1) :
            dependencies thread { path = xs } ++
            dependencies partner

-- Determines whether two trails are an acceptable swap pair.
coherent :: Trail -> Trail -> Bool
coherent front back =
    syncLock front /= syncLock back &&
    foreach frontDeps (sameLock extends back ) &&
    foreach backDeps  (sameLock extends front) &&
    foreach frontDeps (foreach backDeps . sameLock comparable)
  where
    foreach = flip all
    x `implies` y = not x || y

    frontDeps = dependencies front
    backDeps = dependencies back

    sameLock f thread thread' =
        (syncLock thread == syncLock thread') `implies`
        (path thread `f` path thread')
