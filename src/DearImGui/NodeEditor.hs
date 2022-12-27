module DearImGui.NodeEditor 
  ( Raw.createEditorContext
  , Raw.EditorContext'(..)
  , module DearImGui.NodeEditor
  ) where
import UnliftIO
import Data.Text
import DearImGui
import qualified DearImGui.Raw.NodeEditor as Raw
import qualified DearImGui.Internal.Text as Text
import Foreign
import DearImGui.Raw.DrawList

withNodeEditor :: MonadUnliftIO m => Raw.EditorContext' -> Text -> ImVec2 -> m a -> m a
withNodeEditor cxt name pos f = bracket (beginEditor cxt name pos) (const $ do
  Raw.endEditor
  Raw.setCurrentEditor (Raw.EditorContext' nullPtr)
  ) (const f)

beginEditor :: MonadIO m => Raw.EditorContext' -> Text -> ImVec2 -> m ()
beginEditor cxt name pos = liftIO $ Text.withCString name $ \namePtr ->
    with pos $ \posPtr -> do
      Raw.setCurrentEditor cxt
      Raw.beginEditor namePtr posPtr

withNode :: MonadUnliftIO m => Int -> m a -> m a
withNode nodeId = bracket (Raw.beginNode (fromIntegral nodeId)) (const Raw.endNode) . const

withPin :: MonadUnliftIO m => Int -> Bool -> m a -> m a
withPin pinId isInput = bracket (Raw.beginPin (fromIntegral pinId) isInput) (const Raw.endPin) . const

linkNodes :: MonadUnliftIO m => Int -> Int -> Int -> m ()
linkNodes linkId inId outId = Raw.linkNodes (fromIntegral linkId) (fromIntegral inId) (fromIntegral outId)

getNodeBackgroundDrawList :: MonadUnliftIO m => Int -> m DrawList
getNodeBackgroundDrawList nodeId = Raw.getNodeBackgroundDrawList (fromIntegral nodeId)