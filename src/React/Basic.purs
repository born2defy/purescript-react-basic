module React.Basic
  ( Component
  , Render
  , render
  , unsafeRender
  , CreateComponent
  , JSX
  , component
  , useState
  , useEffect
  , useReducer
  , StateUpdate(..)
  , Ref
  , readRef
  , renderRef
  , writeRef
  , useRef
  , Context
  , mkContext
  , useContext
  , runContext
  , runContextKeyed
  , Key
  , class ToKey
  , toKey
  , empty
  , keyed
  , fragment
  , element
  , elementKeyed
  , displayName
  , module Data.Tuple
  , module Data.Tuple.Nested
  ) where

import Prelude

import Data.Function.Uncurried (Fn2, Fn4, mkFn2, runFn2, runFn4)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toNullable)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (tuple2, (/\))
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, mkEffectFn1, runEffectFn1, runEffectFn2, runEffectFn3)
import Unsafe.Coerce (unsafeCoerce)

newtype Component ctx props = Component (EffectFn1 props JSX)

newtype Render ctx a = Render (Effect a)

derive newtype instance functorRender :: Functor (Render ctx)
derive newtype instance applyRender :: Apply (Render ctx)
derive newtype instance bindRender :: Bind (Render ctx)

-- | render
render :: ∀ ctx. JSX -> Render ctx JSX
render jsx = Render (pure jsx)

-- | Conditional logic is not allowed in Render, making
-- | Applicative `pure` unsafe. It's still occasionally
-- | required, however, to extract Render logic into more
-- | advanced helper functions. Never nest `unsafeRender`
-- | in a conditionally or dynamically (if, case, for).
unsafeRender :: forall ctx a. a -> Render ctx a
unsafeRender a = Render (pure a)

type CreateComponent ctx props = Effect (Component ctx props)

-- | Revised Version
component
  :: forall props ctx
   . String
  -> (props -> Render ctx JSX)
  -> CreateComponent ctx props
component name renderFn =
  let c = Component (mkEffectFn1 (unsafeCoerce renderFn))
   in runEffectFn2 unsafeSetDisplayName name c

-- ORIGINAL VERSION
-- component
--   :: forall props ctx
--    . String
--   -> (props -> Render ctx JSX)
--   -> CreateComponent props
-- component name renderFn =
--   let c = Component (mkEffectFn1 (unsafeCoerce renderFn))
--    in runEffectFn2 unsafeSetDisplayName name c

data Context ctx

mkContext :: ∀ ctx. ctx -> Context ctx
mkContext = mkContext_

foreign import mkContext_ :: ∀ ctx. ctx -> Context ctx

useContext :: ∀ ctx. Context ctx -> Render ctx ctx
useContext = Render <<< useContext_

foreign import useContext_ :: ∀ ctx. Context ctx -> Effect ctx

runContext
  :: forall ctx props
   . Component ctx { | props }
  -> Context ctx
  -> ctx
  -> { | props }
  -> JSX
runContext (Component c) ctxObj ctx props = runFn4 runContext_ c ctxObj ctx props

foreign import runContext_ :: ∀ ctx props. Fn4 (EffectFn1 { | props } JSX) (Context ctx) ctx { | props } JSX

runContextKeyed
  :: forall ctx props
   . Component ctx { | props }
  -> Context ctx
  -> ctx
  -> { key :: String | props }
  -> JSX
runContextKeyed (Component c) ctxObj ctx props = runFn4 runContextKeyed_ c ctxObj ctx props

foreign import runContextKeyed_ :: ∀ ctx props. Fn4 (EffectFn1 { | props } JSX) (Context ctx) ctx { key :: String | props } JSX

-- | Create a `JSX` node from a `Component`, by providing the props.
-- |
-- | This function is for non-React-Basic React components, such as those
-- | imported from FFI.
-- |
-- | __*See also:* `Component`, `elementKeyed`__
element
  :: forall props
   . Component Void { | props }
  -> { | props }
  -> JSX
element (Component c) props = runFn2 element_ c props

-- | Create a `JSX` node from a `Component`, by providing the props and a key.
-- |
-- | This function is for non-React-Basic React components, such as those
-- | imported from FFI.
-- |
-- | __*See also:* `Component`, `element`, React's documentation regarding the special `key` prop__
elementKeyed
  :: forall ctx props
   . Component ctx { | props }
  -> { key :: String | props }
  -> JSX
elementKeyed = runFn2 elementKeyed_

foreign import element_
  :: forall props
   . Fn2 (EffectFn1 { | props } JSX) { | props } JSX

foreign import elementKeyed_
  :: forall ctx props
   . Fn2 (Component ctx { | props }) { key :: String | props } JSX


-- | useState
useState
  :: forall state ctx
   . state
  -> Render ctx (Tuple state ((state -> state) -> Effect Unit))
useState initialState = Render do
  { value, setValue } <- runEffectFn1 useState_ initialState
  pure (Tuple value (runEffectFn1 setValue))

-- | useEffect
useEffect :: ∀ ctx. Array Key -> Effect (Effect Unit) -> Render ctx Unit
useEffect refs effect = Render (runEffectFn2 useEffect_ effect refs)

-- | useReducer
-- | TODO: add note about conditionally updating state
useReducer
  :: forall state action ctx
   . ToKey state
  => (state -> action -> state)
  -> state
  -> Maybe action
  -> Render ctx (Tuple state (action -> Effect Unit))
useReducer reducer initialState initialAction = Render do
  { state, dispatch } <- runEffectFn3 useReducer_ (mkFn2 reducer) initialState (toNullable initialAction)
  pure (Tuple state (runEffectFn1 dispatch))

-- | Used by the `reducer` function to describe the kind of state
-- | update or side effects desired.
-- |
-- | __*See also:* `ComponentSpec`__
data StateUpdate state
  = NoUpdate
  | Update               state
  | SideEffects                (Effect Unit)
  | UpdateAndSideEffects state (Effect Unit)

data Ref a

readRef :: forall a. Ref a -> Effect a
readRef = runEffectFn1 readRef_

renderRef :: forall ctx a. Ref a -> Render ctx a
renderRef ref = Render (readRef ref)

writeRef :: forall a. Ref a -> a -> Effect Unit
writeRef = runEffectFn2 writeRef_

useRef
  :: forall ctx a
   . a
  -> Render ctx (Ref a)
useRef initialValue = Render do
  runEffectFn1 useRef_ initialValue

-- | Keys represent values React uses to check for changes.
-- | This is done using JavaScript's reference equality (`===`),
-- | so complicated types may want to implement `ToKey` so that
-- | it returns a primative like a `String`. A timestamp appended
-- | to a unique ID, for example. Less strict cases can implement
-- | `ToKey` using `unsafeCoerce`, while some extreme cases may
-- | need a hashing or stringifying mechanism.
data Key

class ToKey a where
  toKey :: a -> Key

instance trString :: ToKey String where
  toKey = unsafeCoerce

instance trInt :: ToKey Int where
  toKey = unsafeCoerce

instance trNumber :: ToKey Number where
  toKey = unsafeCoerce

instance trBoolean :: ToKey Boolean where
  toKey = unsafeCoerce

instance trRecord :: ToKey (Record a) where
  toKey = unsafeCoerce

instance trArray :: ToKey (Array a) where
  toKey = unsafeCoerce

instance trNullable :: ToKey (Nullable a) where
  toKey = unsafeCoerce

instance trMaybe :: ToKey (Maybe a) where
  toKey a = toKey (toNullable a)

-- | Represents rendered React VDOM (the result of calling `React.createElement`
-- | in JavaScript).
-- |
-- | `JSX` is a `Monoid`:
-- |
-- | - `append`
-- |   - Merge two `JSX` nodes using `React.Fragment`.
-- | - `mempty`
-- |   - The `empty` node; renders nothing.
-- |
-- | __*Hint:* Many useful utility functions already exist for Monoids. For example,
-- |   `guard` can be used to conditionally render a subtree of components.__
foreign import data JSX :: Type

instance semigroupJSX :: Semigroup JSX where
  append a b = fragment [ a, b ]

instance monoidJSX :: Monoid JSX where
  mempty = empty

-- | An empty `JSX` node. This is often useful when you would like to conditionally
-- | show something, but you don't want to (or can't) modify the `children` prop
-- | on the parent node.
-- |
-- | __*See also:* `JSX`, Monoid `guard`__
foreign import empty :: JSX

-- | Apply a React key to a subtree. React-Basic usually hides React's warning about
-- | using `key` props on components in an Array, but keys are still important for
-- | any dynamic lists of child components.
-- |
-- | __*See also:* React's documentation regarding the special `key` prop__
keyed :: String -> JSX -> JSX
keyed = runFn2 keyed_

-- | Render an Array of children without a wrapping component.
-- |
-- | __*See also:* `JSX`__
foreign import fragment :: Array JSX -> JSX


-- | Retrieve the Display Name from a `ComponentSpec`. Useful for debugging and improving
-- | error messages in logs.
-- |
-- | __*See also:* `displayNameFromSelf`, `createComponent`__
foreign import displayName
  :: forall props ctx
   . Component ctx props
  -> String


-- |
-- | Internal utility or FFI functions
-- |

foreign import unsafeSetDisplayName
  :: forall props ctx
   . EffectFn2 String (Component ctx props) (Component ctx props)

foreign import useState_
  :: forall state
   . EffectFn1
       state
       { value :: state
       , setValue :: EffectFn1 (state -> state) Unit
       }

foreign import useEffect_
  :: EffectFn2
       (Effect (Effect Unit))
       (Array Key)
       Unit

foreign import useReducer_
  :: forall state action
   . EffectFn3
       (Fn2 state action state)
       state
       (Nullable action)
       { state :: state
       , dispatch :: EffectFn1 action Unit
       }

foreign import readRef_
  :: forall a
   . EffectFn1
       (Ref a)
       a

foreign import writeRef_
  :: forall a
   . EffectFn2
       (Ref a)
       a
       Unit

foreign import useRef_
  :: forall a
   . EffectFn1
       a
       (Ref a)

foreign import keyed_ :: Fn2 String JSX JSX

