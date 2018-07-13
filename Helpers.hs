
{-# LANGUAGE LambdaCase #-}

module Helpers where


enumToNum :: (Enum a, Num b) => a -> b
enumToNum = fromIntegral . fromEnum


brutalLookup :: (Eq a) => a -> [(a,b)] -> b
brutalLookup ele store = flip id (lookup ele store) $ \case
  Just b -> b
  Nothing -> error "brutalLookup failed as it couldn\'t find an element in a map. You shouldn\'t be seeing this; it\'s TwixNinja\'s fault. He sucks."
  -- Note flip id is backwards function application. It's the same thing as flip ($)
