{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Graphics.UI.Gtk

import System.Directory
import System.FilePath
import qualified Data.Text as T
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe

import Debug.Trace

import qualified Database.HSQL.SQLite3 as BD
import System.Environment
import System.IO

import ListeDeControle
import GUI
import Persistance


--data ReferenceTab = ReferenceTab { ref :: TreeRowReference }

ouvrirDansUneFenetre :: WidgetClass w => String -> w -> IO ()
ouvrirDansUneFenetre nom widget = do
  --sw <- scrolledWindowNew Nothing Nothing
  --containerAdd sw widget
  fenetre <- windowNew
  containerAdd fenetre widget --sw
  windowSetDefaultSize fenetre 600 400
  windowSetPosition fenetre WinPosCenter
  set fenetre [windowTitle := nom]
  widgetShowAll fenetre


--Vérifier si c'est déjà ouvert dans le list store
-- Cette fonction présente une liste d'élément de projet à ouvrir
ouvrirElementDeProjet :: ElementDeProjet a =>
                         BD.Connection ->
                         (BD.Connection -> IO [ElementReference w]) ->
                         (BD.Connection -> Integer -> MaybeT IO a) ->
                         MaybeT IO a
ouvrirElementDeProjet bd obtRef obt = MaybeT $ do
  d <- dialogNew
  vb <- dialogGetUpper d
  ls <- obtRef bd >>= listStoreNew
  tv <- treeViewNewWithModel ls
  col <- treeViewColumnNew
  textRend   <- cellRendererTextNew
  set textRend [cellTextEditable := False, cellTextEditableSet := False]
  cellLayoutPackStart  col textRend True
  cellLayoutSetAttributes col textRend ls (\r -> [cellText := T.unpack . nom $ r] )
  treeViewAppendColumn tv col
  lNom <- labelNew $ Just ("Sélectionner l'élément à ouvrir :" :: T.Text)
  boxPackStart vb lNom PackNatural 0
  boxPackStart vb tv PackNatural 0
  bOK     <- dialogAddButton d ("Ouvrir" :: T.Text)  ResponseOk
  bCancel <- dialogAddButton d ("Annuler" :: T.Text) ResponseCancel
  widgetShowAll d
  res <- dialogRun d
  toto <- case res of
    ResponseOk -> runMaybeT $ do sel <- lift $ treeViewGetSelection tv
                                 ti  <- MaybeT $ treeSelectionGetSelected sel
                                 tt  <- lift $ listStoreGetValue ls (listStoreIterToIndex ti)
                                 obt bd (identifiant tt)
    _          -> return Nothing
  widgetDestroy d
  return toto

creerMenuNouveau :: BD.Connection -> IO MenuItem
creerMenuNouveau bd = do

  itemNouvelleLDC <- menuItemNewWithLabel ("Liste de contrôle" :: T.Text)
  itemNouvelleLDC `on` menuItemActivate $ do
    d <- dialogNew
    vb <- dialogGetUpper d
    lNom <- labelNew $ Just ("Nom de la liste :" :: T.Text)
    eNom <- entryNew
    boxPackStart vb lNom PackNatural 0
    boxPackStart vb eNom PackNatural 0
    bOK     <- dialogAddButton d ("Continuer" :: T.Text) ResponseOk
    bCancel <- dialogAddButton d ("Annuler" :: T.Text)    ResponseCancel
    widgetShowAll d
    res <- dialogRun d
    case res of
      ResponseOk -> do
        strNomLDC <- entryGetText eNom
        widgetDestroy d
        ldc <- ajouter bd :: IO ListeDeControle
        let ldc2 = ldc { nomLDC=T.pack strNomLDC }
        actualiserNom bd (identifiant ldc2) strNomLDC
        creerWidgetEdition bd ldc2 Nothing >>= ouvrirDansUneFenetre (T.unpack.nom $ ldc2)
      _          -> widgetDestroy d

  itemNouvelleReunion <- menuItemNewWithLabel ("Réunion" :: T.Text)
  itemNouvelleReunion `on` menuItemActivate $ (ajouter bd :: IO Reunion)>>= \c -> creerWidgetEdition bd c Nothing >>= ouvrirDansUneFenetre "Réunion"

  itemNouveauContact <- menuItemNewWithLabel ("Contact" :: T.Text)
  itemNouveauContact  `on` menuItemActivate $ (ajouter bd :: IO Contact)>>= \c -> creerWidgetEdition bd c Nothing >>= ouvrirDansUneFenetre "Contact"

  menu <- menuNew;
  menuShellAppend menu itemNouvelleLDC
  menuShellAppend menu itemNouvelleReunion
  menuShellAppend menu itemNouveauContact

  menuItem <- menuItemNewWithLabel ("Nouveau" :: T.Text)
  menuItemSetSubmenu menuItem menu
  return menuItem

creerMenuOuvrir :: BD.Connection -> IO MenuItem
creerMenuOuvrir bd = do
  menu <- menuNew;

  itemNouvelleLDC <- menuItemNewWithLabel ("Liste de contrôle" :: T.Text)
  itemNouvelleLDC `on` menuItemActivate $ void $
    runMaybeT $ (ouvrirElementDeProjet bd listerRefLDC obtenir :: MaybeT IO ListeDeControle) >>= \ldc ->
                lift $ creerWidgetEdition bd ldc Nothing >>= ouvrirDansUneFenetre (T.unpack.nom $ ldc)
  menuShellAppend menu itemNouvelleLDC

  itemNouvelleReunion <- menuItemNewWithLabel ("Réunion" :: T.Text)
  itemNouvelleReunion `on` menuItemActivate $ void $
    runMaybeT $ (ouvrirElementDeProjet bd listerRefReunions obtenir :: MaybeT IO Reunion) >>= \ldc ->
                lift $ creerWidgetEdition bd ldc Nothing >>= ouvrirDansUneFenetre (T.unpack.nom $ ldc)
  menuShellAppend menu itemNouvelleReunion

  itemContact <- menuItemNewWithLabel ("Contact" :: T.Text)
  itemContact `on` menuItemActivate $ void $
    runMaybeT $ (ouvrirElementDeProjet bd listerRefContacts obtenir :: MaybeT IO Contact) >>= \ldc ->
                lift $ creerWidgetEdition bd ldc Nothing >>= ouvrirDansUneFenetre (T.unpack.nom $ ldc)
  menuShellAppend menu itemContact

  menuItem <- menuItemNewWithLabel ("Ouvrir" :: T.Text)
  menuItemSetSubmenu menuItem menu
  return menuItem

creerMenuFichier :: BD.Connection -> IO MenuItem
creerMenuFichier db = do
  menu <- menuNew;
  menuShellAppend menu =<< creerMenuNouveau db
  menuShellAppend menu =<< creerMenuOuvrir db
  menuItemFichier <- menuItemNewWithLabel ("Fichier" :: T.Text)
  menuItemSetSubmenu menuItemFichier menu
  return menuItemFichier

main :: IO ()
main = do

  hSetBuffering stdout NoBuffering

  args <- getArgs
  fichier <- if 0==length args
             then fmap (`combine` "PetitGestionnaire.sql3") getUserDocumentsDirectory
             else return $ head args

  traceIO $ "The folder is : " ++ fichier
  dbConn <- ouvrirConnection fichier

  initGUI

  window <- windowNew

  -- Set window.
  windowSetDefaultSize window 1200 800
  windowSetPosition window WinPosCenter
  set window [windowTitle := ("Le petit gestionnaire" :: T.Text)]

  -- Le menu
  barre <- menuBarNew;
  menuShellAppend barre =<< creerMenuFichier dbConn

  --Barre de status
  sb <- statusbarNew

  -- Assemblage
  boite <- vBoxNew False 0
  boxPackStart boite barre PackNatural 0
  hpane <- hPanedNew

  --set hpane [panedMaxPosition:=1000]

  --Ajouter la forest de projet
  --Ajouter le notebook
  nb <- notebookNew
  nouvelleForestDeProjets dbConn nb >>= panedAdd1 hpane
  panedAdd2 hpane nb
  boxPackStart boite hpane PackGrow 0
  panedSetPosition hpane 200
  boxPackEnd boite sb PackNatural 0

  --cid <- statusbarGetContextId sb ("toto" :: T.Text)
  --statusbarPush sb cid ("tltlt" :: T.Text)

  window `containerAdd` boite
  widgetShowAll window

  onDestroy window $ do
    BD.disconnect dbConn
    mainQuit
  mainGUI
