-----------------------------------------------------------------------------
--
-- Module      :  Cahier
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  charles.stpierre@gmail.com
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Cahier where

import Data.Map
import Graphics.UI.Gtk
import Control.Concurrent.STM

data InfoWidget = InfoWidget { chemin::FilePath
                             , nouveau::Bool
                             , modifie::Bool
                             } deriving (Eq,Show)

changerChemin :: InfoWidget -> FilePath -> InfoWidget
changerChemin info path = info {chemin=path} -- InfoWidget path (modifie info)

changerEtat :: Bool -> InfoWidget -> InfoWidget
changerEtat etat info = info {modifie=etat} -- InfoWidget (chemin info) etat

changerNouveauExistant :: InfoWidget -> Bool -> InfoWidget
changerNouveauExistant info status = info {nouveau=status} -- InfoWidget (chemin info) etat

--Record pour faire le lien entre la feuille du cahier et un element
data Cahier a = Cahier { obtNotebook::Notebook
                       , obtInfos::TVar (Map Widget a)
                       }

actualiserInfos :: Cahier a -> Int -> (a -> a) -> Bool -> IO ()
actualiserInfos cahier page fcn etat =
  let (notebook, infosTVAR) = (obtNotebook cahier, obtInfos cahier)
      actualise widgetEnfant = atomically $ do --STM
                                              infos <- readTVar infosTVAR
                                              case Data.Map.lookup widgetEnfant infos of
                                                Just iw -> writeTVar infosTVAR (insert widgetEnfant (fcn iw) infos)
                                                Nothing -> return ()
  in  notebookGetNthPage notebook page >>= maybe (return ()) actualise

ajouterSansChemin :: Cahier InfoWidget -> Int -> IO ()
ajouterSansChemin cahier page =
  let (notebook, infos) = (obtNotebook cahier, obtInfos cahier)
      actualise widgetEnfant = atomically (readTVar infos >>= \x -> writeTVar infos (insert widgetEnfant (InfoWidget "Nouveau" True False) x))
  in  notebookGetNthPage notebook page >>= maybe (return ()) actualise

-- Lorsqu'on ajoute un chemin, l'etat est toujours non modifié
ajouterChemin :: Cahier InfoWidget -> Int -> FilePath -> IO ()
ajouterChemin cahier page fichier =
  let (notebook, infos) = (obtNotebook cahier, obtInfos cahier)
      actualise widgetEnfant = atomically (readTVar infos >>= \x -> writeTVar infos (insert widgetEnfant (InfoWidget fichier False False) x))
  in  notebookGetNthPage notebook page >>= maybe (return ()) actualise

enleverChemin :: Cahier InfoWidget -> Int -> IO ()
enleverChemin cahier page =
  let (notebook, infos) = (obtNotebook cahier, obtInfos cahier)
      actualise widgetEnfant = atomically (readTVar infos >>= \x -> writeTVar infos (delete widgetEnfant x))
  in  notebookGetNthPage notebook page >>= maybe (return ()) actualise

contientChemin :: Cahier InfoWidget -> Int -> IO Bool
contientChemin cahier page =
  let (notebook, infos) = (obtNotebook cahier, obtInfos cahier)
      actualise widgetEnfant = atomically $ fmap (Data.Map.member widgetEnfant) (readTVar infos)
  in  notebookGetNthPage notebook page >>= maybe (return False) actualise

obtenirChemin :: Cahier InfoWidget -> Int -> IO (Maybe FilePath)
obtenirChemin cahier page =
  let (notebook, infosTVAR) = (obtNotebook cahier, obtInfos cahier)
      actualise widgetEnfant = atomically $ do --STM
                                              infos <- readTVar infosTVAR
                                              case Data.Map.lookup widgetEnfant infos of
                                                Just iw -> return . Just $ chemin iw
                                                Nothing -> return Nothing
  in notebookGetNthPage notebook page >>= maybe (return Nothing) actualise

obtenirNouveauExistant :: Cahier InfoWidget -> Int -> IO (Maybe Bool)
obtenirNouveauExistant cahier page =
  let (notebook, infosTVAR) = (obtNotebook cahier, obtInfos cahier)
      actualise widgetEnfant = atomically $ do --STM
                                              infos <- readTVar infosTVAR
                                              case Data.Map.lookup widgetEnfant infos of
                                                Just iw -> return . Just $ nouveau iw
                                                Nothing -> return Nothing
  in  notebookGetNthPage notebook page >>= maybe (return Nothing) actualise

--listerChemin :: Cahier -> Int -> IO (Map Widget FilePath)
--listerChemin cahier page = atomically $ readTVar (obtNotebookInfos cahier)




