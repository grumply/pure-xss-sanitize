module Pure.Data.View.SanitizeXSS (sanitize,Options(..)) where

import Pure.Data.View
import Pure.Data.Txt as Txt
import Text.HTML.SanitizeXSS (safeTagName,sanitizeAttribute)

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
sanitize Options {..} = \case
  HTMLView {..}
    | safeTagName (Txt.toLower tag)
    , fs <- sanitizeFeatures features
    , cs <- fmap sanitize children
    -> HTMLView { features = fs, children = cs, .. }

  SVGView {..}
    | safeTagName (Txt.toLower tag)
    , fs <- sanitizeFeatures features
    , cs <- fmap sanitize children
    -> SVGView { features = fs, children = cs, .. }

  KHTMLView {..}
    | safeTagName (Txt.toLower tag)
    , fs <- sanitizeFeatures features
    , kcs <- fmap (fmap sanitize) keyedChildren
    -> KHTMLView { features = fs, keyedChildren = kcs, .. }
    
  KSVGView {..}
    | safeTagName (Txt.toLower tag)
    , fs <- sanitizeFeatures features
    , kcs <- fmap (fmap sanitize) keyedChildren
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
sanitizeFeatures allowDataAttributes Features {..} =
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
  -> Just v
  
  | otherwise
  -> fmap snd (sanitizeAttribute (k,v))

cleanStyle :: Txt -> Txt -> Maybe Txt
cleanStyle k v = 
  case fmap snd (sanitizeAttribute ("style",k <> ": " <> v)) of
    Just kv | [k,v] <- Txt.splitOn ":" kv -> Just v
    _ -> Nothing