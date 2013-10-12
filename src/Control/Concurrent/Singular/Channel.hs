{-# LANGUAGE Safe #-}

module Control.Concurrent.Singular.Channel (
    Channel, newChannel,
    swapFront, swapBack
) where

import Control.Concurrent.Singular.Event
import qualified Control.Concurrent.Singular.Primitive.Channel as Primitive

import Control.Applicative ((<$>))

newtype Channel f b = Channel (Primitive.Channel f b)

newChannel :: IO (Channel f b)
newChannel = Channel <$> Primitive.newChannel

swapFront :: Channel f b -> f -> Event b
swapFront (Channel channel) = fromPrimitive . Primitive.swapFront channel

swapBack :: Channel f b -> b -> Event f
swapBack (Channel channel) = fromPrimitive . Primitive.swapBack channel
