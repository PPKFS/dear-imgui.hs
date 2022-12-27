{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module DearImGui.Raw.NodeEditor where


-- base
import Control.Monad.IO.Class
  ( MonadIO, liftIO )
import Foreign
import Foreign.C
import System.IO.Unsafe
  ( unsafePerformIO )

-- dear-imgui
import DearImGui.Context
import DearImGui.Enums
import DearImGui.Structs
import DearImGui.Raw.DrawList (DrawList(..))

-- inline-c
import qualified Language.C.Inline as C
import qualified Data.Map as Map
-- inline-c-cpp
import qualified Language.C.Inline.Cpp as Cpp
import Language.C.Inline.Context
  ( Context(..) )

data EditorContext' = EditorContext' (Ptr EditorContext)

C.context (Cpp.cppCtx <> C.bsCtx <> imguiContext <> nodeContext)
C.include "imgui.h"
C.include "imgui_node_editor.h"
Cpp.using "namespace ImGui"
Cpp.using "namespace ax::NodeEditor"
Cpp.using "namespace ax::NodeEditor::Details"

createEditorContext :: MonadIO m => m EditorContext'
createEditorContext = liftIO $ do
  EditorContext' <$> [C.block| EditorContext* { 
    Config cfg;
    cfg.DragButtonIndex = 0;
    cfg.SelectButtonIndex = 2;
    cfg.NavigateButtonIndex = 1;
    cfg.ContextMenuButtonIndex = 2;
    return CreateEditor( &cfg ); } |]

destroyEditorContext :: MonadIO m => EditorContext' -> m ()
destroyEditorContext (EditorContext' cxt) = liftIO $ 
  [C.exp| void { DestroyEditor( $(EditorContext* cxt) )} |]

setCurrentEditor :: MonadIO m => EditorContext' -> m ()
setCurrentEditor (EditorContext' cxt) = liftIO $ 
  [C.exp| void { SetCurrentEditor( $(EditorContext* cxt) )} |]
beginEditor :: MonadIO m => CString -> Ptr ImVec2 -> m ()
beginEditor label posPtr = liftIO $ 
  [C.exp| void { ax::NodeEditor::Begin( $(char* label), *$(ImVec2* posPtr)  )} |]
endEditor :: MonadIO m => m ()
endEditor = liftIO [C.exp| void { ax::NodeEditor::End()} |]

beginNode :: MonadIO m => CInt -> m ()
beginNode nodeId = liftIO $ 
  [C.exp| void { BeginNode( $(int nodeId) )} |]
endNode :: MonadIO m => m ()
endNode = liftIO $ 
  [C.exp| void { EndNode() } |]
beginPin :: MonadIO m => CInt -> Bool -> m ()
beginPin pinId isInput = let pinKind = if isInput then 0 else 1 in liftIO $ 
  [C.exp| void { BeginPin( $(int pinId), (PinKind) $(int pinKind))} |]
endPin :: MonadIO m => m ()
endPin = liftIO $ 
  [C.exp| void { EndPin() } |]

linkNodes :: MonadIO m => CInt -> CInt -> CInt -> m ()
linkNodes linkId inId outId = liftIO 
  [C.exp| void { Link( $(int linkId), $(int inId), $(int outId))} |]

getNodeBackgroundDrawList :: (MonadIO m) => CInt -> m DrawList
getNodeBackgroundDrawList nodeId = liftIO do
  DrawList <$> [C.exp|
    ImDrawList* {
      GetNodeBackgroundDrawList( $(int nodeId) )
    }
  |]