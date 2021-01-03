{-# language LambdaCase, RecordWildCards, PatternSynonyms, OverloadedStrings #-}
module Pure.Data.View.SanitizeXSS (sanitize,Options(..)) where

import Pure.Data.View (View(..),Features(..))
import Pure.Data.View.Patterns (pattern Null)
import Pure.Data.Txt as Txt (Txt,toLower,splitOn,isPrefixOf,fromTxt,toTxt)
import Text.HTML.SanitizeXSS (safeTagName,sanitizeAttribute)
import qualified Data.Map as Map (mapMaybeWithKey)

data Options = Options
  { allowCustomViews :: Bool
  , allowDataAttributes :: Bool
  }

defaultOptions :: Options
defaultOptions = Options 
  { allowCustomViews = True 
  , allowDataAttributes = True
  }

-- | Sanitize HTML and SVG views and discard raw views.
sanitize :: Options -> View -> View
sanitize Options {..} = go
  where
    go = \case
      HTMLView {..}
        | safeTagName (Txt.fromTxt (Txt.toLower tag))
        , fs <- sanitizeFeatures allowDataAttributes features
        , cs <- fmap go children
        -> HTMLView { features = fs, children = cs, .. }

      SVGView {..}
        | safeTagName (Txt.fromTxt (Txt.toLower tag))
        , fs <- sanitizeFeatures allowDataAttributes features
        , cs <- fmap go children
        -> SVGView { features = fs, children = cs, .. }

      KHTMLView {..}
        | safeTagName (Txt.fromTxt (Txt.toLower tag))
        , fs <- sanitizeFeatures allowDataAttributes features
        , kcs <- fmap (fmap go) keyedChildren
        -> KHTMLView { features = fs, keyedChildren = kcs, .. }
        
      KSVGView {..}
        | safeTagName (Txt.fromTxt (Txt.toLower tag))
        , fs <- sanitizeFeatures allowDataAttributes features
        , kcs <- fmap (fmap go) keyedChildren
        -> KSVGView { features = fs, keyedChildren = kcs, .. }

      tv@TextView{}  -> tv

      -- These cases must not have come from de-serialization.
      SomeView a | allowCustomViews -> SomeView a
      LazyView f a | allowCustomViews -> LazyView f a
      TaggedView __w v | allowCustomViews -> TaggedView __w v
      PortalView pp pd pv | allowCustomViews -> PortalView pp pd pv
      ComponentView __w r c p | allowCustomViews -> ComponentView __w r c p
      Prebuilt v | allowCustomViews -> Prebuilt v
      
      -- RawView and any unsafe tags are discarded
      _ -> Null

-- | Sanitize features of HTML and SVG views. Note that listeners
-- are kept because they are not serialized with the base libraries
-- and, thus, must have been constructed on the client.
sanitizeFeatures :: Bool -> Features -> Features
sanitizeFeatures allowDataAttributes Features_ {..} =
  Features_
    { attributes = Map.mapMaybeWithKey (cleanAttribute allowDataAttributes) attributes 
    , properties = Map.mapMaybeWithKey (cleanAttribute allowDataAttributes) properties
    , styles     = Map.mapMaybeWithKey cleanStyle     styles
    , ..
    } 

cleanAttribute :: Bool -> Txt -> Txt -> Maybe Txt
cleanAttribute allowDataAttributes k v 
  | allowDataAttributes
  , "data-" `Txt.isPrefixOf` k
  = Just v
  
  | otherwise
  = fmap (Txt.toTxt . snd) (sanitizeAttribute (Txt.fromTxt k,Txt.fromTxt v))

cleanStyle :: Txt -> Txt -> Maybe Txt
cleanStyle k v 
  | Just kv <- fmap snd (sanitizeAttribute ("style",Txt.fromTxt (k <> ": " <> v)))
  , [_,v] <- Txt.splitOn ":" (Txt.toTxt kv)
  = Just v

  | otherwise
  = Nothing