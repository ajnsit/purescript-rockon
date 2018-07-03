module RockOn where

import Prelude

import Concur.Core (Widget, display, mapView, mkLeafWidget, wrapViewEvent)
import Concur.Core.Discharge (discharge, dischargeAsync)
import Concur.React.Props (Props, mkProp)
import RockOn.Component (HWComponent, runHW)
import Control.MultiAlternative (class MultiAlternative, orr)
import Control.ShiftMap (class ShiftMap, shiftMap)
import Data.Array (singleton)
import Data.Either (Either(..))
import Effect.Console (log)
import React as R
import React.DOM as D
import React.DOM.Props as P
import Unsafe.Coerce (unsafeCoerce)

-- Hardware components + UI views
data HUI = HUI HUIRecord
type HUIRecord =
  { html :: Array R.ReactElement
  , hwui :: Array HWComponent
  }

instance semigroupHUI :: Semigroup HUI where
  append (HUI a) (HUI b) = HUI
    { html: a.html <> b.html
    , hwui: a.hwui <> b.hwui
    }

instance monoidHUI :: Monoid HUI where
  mempty = HUI
    { html: mempty
    , hwui: mempty
    }

type HTML = Array R.ReactElement

type NodeName v = v -> v
type NodeTag v = Array P.Props -> v -> v
type LeafTag v = Array P.Props -> v

mapHTMLName :: NodeName HTML -> NodeName HUI
mapHTMLName f (HUI h) = HUI h { html = f h.html }

mapHTMLTag :: NodeTag HTML -> NodeTag HUI
mapHTMLTag f a = mapHTMLName (f a)

mapHTMLLeafTag :: LeafTag HTML -> LeafTag HUI
mapHTMLLeafTag f a = liftHTMLView (f a)

liftHTMLView :: HTML -> HUI
liftHTMLView html = HUI { hwui: mempty, html: html }

-- Convert react components to hw
liftHTMLWidget :: forall a. Widget HTML a -> Widget HUI a
liftHTMLWidget = mapView liftHTMLView

-- Building HW widgets
hwWidget :: forall a. HWComponent -> Widget HUI a
hwWidget c = display (HUI { html: mempty, hwui: singleton c })
hwWidgets :: forall a. Array HWComponent -> Widget HUI a
hwWidgets cs = display (HUI { html: mempty, hwui: cs })

-- BIG HACK! We use UnsafeCoerce to allow this to typecheck. This MIGHT cause RUNTIME errors! Verify!
-- | Wrap a widget with a node that can have eventHandlers attached
el :: forall m a. ShiftMap (Widget HUI) m => NodeTag HTML -> Array (Props a) -> m a -> m a
el e props = shiftMap (wrapViewEvent \h v -> (mapHTMLTag e) (map (mkProp h) (unsafeCoerce props)) v)

-- BIG HACK! We use UnsafeCoerce to allow this to typecheck. This MIGHT cause RUNTIME errors! Verify!
-- | Promote a leaf node to a widget
elLeaf :: forall a. LeafTag HTML -> Array (Props a) -> Widget HUI a
elLeaf e props = mkLeafWidget \h -> (mapHTMLLeafTag e) (map (mkProp h) (unsafeCoerce props))

-- | Wrap some widgets with a node that can have eventHandlers attached
el' :: forall m a. ShiftMap (Widget HUI) m => MultiAlternative m => NodeTag HTML -> Array (Props a) -> Array (m a) -> m a
el' e props = el e props <<< orr

-- React apparently requires wrapping state inside an object
-- type ComponentState =  { view :: HUI }
-- mkComponentState :: HUI -> ComponentState
-- mkComponentState v = { view: v }

componentClass :: forall a. Widget HUI a -> R.ReactClass {}
componentClass winit = R.component "Concur" component
  where
    -- purescript-aff guarantees that sync computations in Aff are resolved synchronously.
    -- That is a good thing in general, but if it happens during init, then setState gets
    --  called before the component has had a chance to mount itself (init is called before mount).
    -- So inside init, we force async widget resolution by using `dischargeAsync` instead of `discharge`.
    --  instead of using setState to set the new view.
    component this = do
      HUI newv <- dischargeAsync (handler this) winit
      pure
        { state: newv
        , render: render this
        }
    handler this (Right r) = do
      prev <- R.getState this
      HUI newv <- discharge (handler this) r
      -- Run the hardware effects
      runHW prev.hwui newv.hwui
      void $ R.writeState this newv
    handler _ (Left err) = do
      log ("FAILED! " <> show err)
      pure unit
    -- TODO: Refine the div wrapper. This is just a placeholder.
    render this = D.div' <$> do
      v <- R.getState this
      pure v.html

renderComponent :: forall a. Widget HUI a -> R.ReactElement
renderComponent init = R.createLeafElement (componentClass init) {}
  -- R.createFactory (componentClass init) {}
