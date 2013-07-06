module FCAbstract.Plugin (
  plugin
) where

import GhcPlugins
import FCAbstract.CESK as CESK

plugin = defaultPlugin {
  installCoreToDos = install
}

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todo = do
  reinitializeGlobals
  putMsgS "Hello!"
  return (CoreDoPluginPass "Say name" pass : todo)

pass :: ModGuts -> CoreM ModGuts
pass = bindsOnlyPass (mapM printBind)
  where printBind :: CoreBind -> CoreM CoreBind
        printBind bndr@(NonRec b e)
          | occNameString (nameOccName (varName b)) == "stupid" = do
            putMsgS "I FOUND STUPID!"
            putMsgS $ "binding named " ++ showSDoc (ppr bndr)
            putMsgS $ "evaled to " ++ showSDoc (ppr (CESK.eval e))
            return bndr
        printBind bndr = do
          putMsgS $ "binding named " ++ showSDoc (ppr bndr)
          return bndr
