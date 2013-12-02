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

pprs :: Outputable a => a -> String
pprs = showSDoc tracingDynFlags . ppr

pass :: ModGuts -> CoreM ModGuts
pass guts = do
  mapM_ tyConFun (mg_tcs guts)
  let bindList = concat (map extractBinds (mg_binds guts))
  let target = findStupid bindList
  putMsgS $ "stupid evaled to " ++
    pprs (CESK.eval (Let (Rec bindList) (Var target)))
  return $ guts
  where
    findStupid :: [(CoreBndr, CoreExpr)] -> CoreBndr
    findStupid ((b, _) : bes)
      | occNameString (nameOccName (varName b)) == "stupid" = b
      | otherwise = findStupid bes

    extractBinds :: CoreBind -> [(CoreBndr, CoreExpr)]
    extractBinds (NonRec b e) = [(b, e)]
    extractBinds (Rec x) = x

    printBind :: CoreBind -> CoreM CoreBind
    printBind bndr@(NonRec b e)
      | occNameString (nameOccName (varName b)) == "stupid" = do
        putMsgS "I FOUND STUPID!"
        putMsgS $ "binding named " ++ pprs bndr
        putMsgS $ "evaled to " ++ pprs (CESK.eval e)
        return bndr
    printBind bndr = do
      putMsgS $ "binding named " ++ pprs bndr
      return bndr

    tyConFun :: TyCon -> CoreM ()
    tyConFun tc = do
      putMsgS "--- tycon ---"
      putMsgS $ pprs tc
      putMsgS $ "name = " ++ pprs (tyConName tc)
      putMsgS $ "kind = " ++ pprs (tyConKind tc)
      putMsgS $ "unique = " ++ pprs (tyConUnique tc)
      putMsgS $ "tyVars = " ++ pprs (tyConTyVars tc)
      putMsgS $ "dataCons = " ++ pprs (tyConDataCons tc)
      putMsgS $ "stupidTheta = " ++ pprs (tyConStupidTheta tc)
      putMsgS $ "parent = " ++ pprs (tyConParent tc)
      putMsgS "-------------"
