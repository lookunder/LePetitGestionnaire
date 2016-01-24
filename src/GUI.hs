-----------------------------------------------------------------------------
--
-- Module      :  GUI
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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module GUI ( nouvelleForestDeProjets
           , Affichable (..)
           ) where

import Graphics.UI.Gtk

import qualified Database.HSQL.SQLite3 as BD
import Persistance
import ListeDeControle
import Data.Time.LocalTime
import Data.Time.Calendar
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import qualified Data.Text as T
import Data.Char
import qualified Data.ByteString.Lazy as B
--import Text.VCard.Format.Directory
import Data.IORef
import Data.List
import Data.Maybe
import Data.Tree
import Debug.Trace

import Control.Monad

import Paths_LePetitGestionnaire

class Sauvegardeable a where
  doitEtreSauve :: a -> Bool
  sauvegarde    :: a -> IO a
  changeEtat    :: a -> b -> a

class ElementDeProjet b => Affichable b where
  --consulter :: WidgetClass a => b -> IO a
  creerWidgetEdition    :: BD.Connection -> b -> Maybe (TreeView, TreeStore (ElementReference w), TreeRowReference) -> IO Widget
  --sauver    :: WidgetClass a => b -> IO a
  --fermeture :: WidgetClass a => b -> IO ()

--instance Eq (ElementReference WidgetClass) where
--  (==) a b =  (identifiant a == identifiant b) && (nom a == nom b) && (mPos a == mPos b)

{--
effaceParticipant :: BD.Connection -> ComboBoxEntry -> Reunion -> IO ()
effaceParticipant bd cb r = do
    mIter <- comboBoxGetActiveIter cb --Obtenir l'entrée,
    print $ "Effacer element : " ++ show mIter
    case mIter of
      Just ti -> listStoreGetValue tl (listStoreIterToIndex ti) >>= supprimerParticipant bd r >>
                 writeIORef valPrecedante Nothing
      Nothing -> return ()
    containerRemove vbox hbox
--}

textColumn :: ColumnId Contact T.Text
textColumn = makeColumnIdString 0

ajouterParticipant :: (VBoxClass v) => BD.Connection -> v -> Reunion -> Maybe Contact -> IO () --(HBox,ComboBox)
ajouterParticipant bd vbox r mCont = do
  hbox <- hBoxNew False 0
  valPrecedante <- newIORef mCont
  bEfface <- buttonNew
  imageNewFromStock "gtk-close" IconSizeMenu >>= buttonSetImage bEfface

  contacts <- lister bd :: IO [Contact]
  tl <- listStoreNew contacts
  treeModelSetColumn tl textColumn (T.dropWhile(==' ').nom)
  cb <- comboBoxNewWithModel tl
  textRend <- cellRendererTextNew
  cellLayoutPackEnd cb textRend False
  cellLayoutSetAttributes cb textRend tl (\contact -> [cellText := T.dropWhile (==' ').nom $ contact])

  boxPackStart hbox cb PackNatural 0
  boxPackStart hbox bEfface PackNatural 0
  boxPackStart vbox hbox PackNatural 0

  maybe (return ()) (comboBoxSetActive cb) (mCont >>= \cont -> findIndex (\c-> identifiant c==identifiant cont) contacts)

  bEfface `on` buttonActivated $ do
    mIter <- comboBoxGetActiveIter cb --Obtenir l'entrée,
    print $ "Effacer element : " ++ show mIter
    case mIter of
      Just ti -> listStoreGetValue tl (listStoreIterToIndex ti) >>= supprimerParticipant bd r >>
                 writeIORef valPrecedante Nothing
      Nothing -> return ()
    containerRemove vbox hbox

  cb `on` changed $ do
    mIter <- comboBoxGetActiveIter cb
    print $ "Iterateur changed : " ++ show mIter
    case mIter of
      Just ti -> do
                 readIORef valPrecedante >>= maybe (return ()) (supprimerParticipant bd r) -- eliminer le precedent
                 nouveau <- listStoreGetValue tl (listStoreIterToIndex ti)
                 ajtLien2 bd r nouveau -- Ajouter le nouveau
                 writeIORef valPrecedante (Just nouveau) --Actualiser la valeur
      Nothing -> return ()

  return ()



actualiserArbre mTrf e =
    case mTrf of
      (Just (_,ts,trf)) -> do
        cheminValide <- treeRowReferenceValid trf
        if cheminValide
        then do
          tp <- treeRowReferenceGetPath trf
          treeStoreChange ts tp (\f -> f { nomRef=T.pack e })
        else return False
      Nothing -> return False

actualiserTab :: WidgetClass w => Maybe Widget -> w -> String-> IO Bool
actualiserTab mNb w e =
   case mNb of
      (Just nb) -> do
        (lMenu,hbox) <- creerWidgetTab (castToNotebook nb) (castToWidget w) e
        notebookSetTabLabel (castToNotebook nb) w hbox
        widgetShowAll hbox
        return False
      Nothing -> return False


instance Affichable Reunion where

 creerWidgetEdition bd r mTrf = do
      fichierXML <- getDataFileName "BoiteRéunion.xml"
      g <- builderNew
      builderAddObjectsFromFile g fichierXML ["BoitePrincipale" :: T.Text]

      ref <- newIORef r
      vbox <- builderGetObject g castToScrolledWindow ("BoitePrincipale" :: T.Text)

      eObj          <- builderGetObject g castToEntry      ("eObj" :: T.Text)
      calDate       <- builderGetObject g castToCalendar   ("calDate" :: T.Text)
      bParticipants <- builderGetObject g castToVBox       ("bParticipants" :: T.Text)
      bAjouter      <- builderGetObject g castToButton     ("bAjouter" :: T.Text)
      sbHeure       <- builderGetObject g castToSpinButton ("sbHeure" :: T.Text)
      sbMinute      <- builderGetObject g castToSpinButton ("sbMinute" :: T.Text)
      sbDuree       <- builderGetObject g castToSpinButton ("sbDuree" :: T.Text)
      tvCR          <- builderGetObject g castToTextView   ("tvCR" :: T.Text)

      adjustmentNew 12 0 23 1 1 1 >>= spinButtonSetAdjustment sbHeure
      adjustmentNew 0 0 59 1 1 1 >>=  spinButtonSetAdjustment sbMinute
      adjustmentNew 30 0 1000 15 10 1 >>= spinButtonSetAdjustment sbDuree

      entrySetText eObj $ T.unpack $ objet r
      widgetGrabFocus eObj
      eObj `on` focusOutEvent $ liftIO $ do
            e <- entryGetText eObj
            actualiserObject bd (identifiant r) e
            return False

      eObj `on` editableChanged $ do
            e <- entryGetText eObj
            actualiserArbre mTrf e
            mNb <- widgetGetAncestor vbox gTypeNotebook
            actualiserTab mNb vbox e
            return ()

      let (annee, mois, jour) = (toGregorian.localDay.zonedTimeToLocalTime.debut) r
      calendarSelectMonth calDate (mois-1) (fromInteger annee)
      calendarSelectDay calDate jour
      afterDaySelected calDate $ calendarGetDate calDate >>= actualiserDebut bd (identifiant r)

      let parts = participants r
      if 0==length parts
      then ajouterParticipant bd bParticipants r Nothing
      else mapM_ (ajouterParticipant bd bParticipants r . Just) parts
      bAjouter `on` buttonActivated $ ajouterParticipant bd bParticipants r Nothing >> widgetShowAll bParticipants

      let (TimeOfDay h m s) = (localTimeOfDay.zonedTimeToLocalTime.debut) r
      spinButtonSetValue sbHeure (fromIntegral h :: Double)
      spinButtonSetValue sbMinute (fromIntegral m :: Double)

      sbHeure `on` editableChanged $ do
        strHeure  <- entryGetText sbHeure
        strMinute <- entryGetText sbMinute
        let strHeureSafe  = if "" == strHeure  then "0" else strHeure
        let strMinuteSafe = if "" == strMinute then "0" else strMinute
        actualiserHeure bd (identifiant r) (read strHeureSafe :: Int,read strMinuteSafe :: Int)

      sbMinute `on` editableChanged $ do
        strHeure  <- entryGetText sbHeure
        strMinute <- entryGetText sbMinute
        let strHeureSafe  = if "" == strHeure  then "0" else strHeure
        let strMinuteSafe = if "" == strMinute then "0" else strMinute
        actualiserHeure bd (identifiant r) (read strHeureSafe :: Int,read strMinuteSafe :: Int)

      onOutput sbMinute $ do
        toto <- spinButtonGetValueAsInt sbMinute
        if toto < 10
          then entrySetText sbMinute ['0', intToDigit toto] >> return False
          else return True

      spinButtonSetValue sbDuree (fromIntegral (duree r) :: Double)
      sbDuree `on` editableChanged $ entryGetText sbDuree >>= actualiserDuree bd (identifiant r)

      tbCR <- textViewGetBuffer tvCR
      textBufferSetText tbCR $ T.unpack $ compteRendu r
      tvCR `on` focusOutEvent $ liftIO $ do
        (tiDebut,tiFin) <- textBufferGetBounds tbCR
        t <- textBufferGetText tbCR tiDebut tiFin True
        actualiserCR bd (identifiant r) t
        return False

      return (toWidget vbox)

modifyIORef'' ref fcn = modifyIORef' ref fcn >> readIORef ref

instance Affichable Contact where

 creerWidgetEdition bd r mTrf = do
      fichierXML <- getDataFileName "BoiteContact.xml"
      g <- builderNew
      builderAddObjectsFromFile g fichierXML ["BoitePrincipale" :: T.Text]
      ref <- newIORef r
      v <- builderGetObject g castToVBox ("BoitePrincipale" :: T.Text)

      ePrenom   <- builderGetObject g castToEntry ("ePrenom" :: T.Text)
      e2Prenom  <- builderGetObject g castToEntry ("e2Prenom" :: T.Text)
      eNom      <- builderGetObject g castToEntry ("eNom" :: T.Text)
      eTitre    <- builderGetObject g castToEntry ("eTitre" :: T.Text)
      eOrg      <- builderGetObject g castToEntry ("eOrg" :: T.Text)
      eTelTrav  <- builderGetObject g castToEntry ("eTelTravail" :: T.Text)
      eTelCell  <- builderGetObject g castToEntry ("eTelCell" :: T.Text)
      eFax      <- builderGetObject g castToEntry ("eFax" :: T.Text)
      eCourriel <- builderGetObject g castToEntry ("eCourriel" :: T.Text)

      entrySetText ePrenom   ((T.unpack.prenom) r)
      entrySetText e2Prenom  ((T.unpack.initiale) r)
      entrySetText eNom      ((T.unpack.nomContact) r)
      entrySetText eTitre    ((T.unpack.titre) r)
      entrySetText eOrg      ((T.unpack.Prelude.head.organisation) r)
      entrySetText eTelTrav  (telTravail r)
      entrySetText eTelCell  (telCell r)
      --entrySetText eFax ((fax r)
      entrySetText eCourriel (courriel r)

      widgetGrabFocus ePrenom

      ePrenom  `on` focusOutEvent $ liftIO $ entryGetText ePrenom  >>= \e ->
        modifyIORef'' ref (\f -> f { prenom=T.pack e }) >>= actualiser bd >> return False

      e2Prenom `on` focusOutEvent $ liftIO $ entryGetText e2Prenom >>= \e ->
        modifyIORef'' ref (\f -> f { initiale=T.pack e }) >>= actualiser bd >> return False

      eNom     `on` focusOutEvent $ liftIO $ entryGetText eNom     >>= \e ->
        modifyIORef'' ref (\f -> f { nomContact=T.pack e }) >>= actualiser bd >> return False

      eTitre   `on` focusOutEvent $ liftIO $ entryGetText eTitre   >>= \e ->
        modifyIORef'' ref (\f -> f { titre=T.pack e }) >>= actualiser bd >> return False

      eOrg     `on` focusOutEvent $ liftIO $ entryGetText eOrg     >>= \e ->
        modifyIORef'' ref (\f -> f { organisation=(T.split (==';').T.pack) e }) >>= actualiser bd >> return False

      eTelTrav   `on` focusOutEvent $ liftIO $ entryGetText eTelTrav   >>= \e ->
        modifyIORef'' ref (\f -> f { telTravail=e }) >>= actualiser bd >> return False

      eTelCell   `on` focusOutEvent $ liftIO $ entryGetText eTelCell   >>= \e ->
        modifyIORef'' ref (\f -> f { telCell= e }) >>= actualiser bd >> return False

      eCourriel   `on` focusOutEvent $ liftIO $ entryGetText eCourriel   >>= \e ->
        modifyIORef'' ref (\f -> f { courriel= e }) >>= actualiser bd >> return False

      bImport <- builderGetObject g castToButton ("bImport" :: T.Text)
      bFermer <- builderGetObject g castToButton ("bFermer" :: T.Text)

      bFermer `on` buttonActivated $ widgetGetToplevel v >>= widgetDestroy

      -- FIXME : L'import the vcard ne fonctionne pas. Mettre à jour le standard.
      bImport `on` buttonActivated $ do
        p <- widgetGetParentWindow v
        boite <- fileChooserDialogNew (Just ("Sélectionner le fichier vCard" :: T.Text)) Nothing FileChooserActionOpen [("Annuler", ResponseCancel), ("Importer", ResponseOk)]
        widgetShowAll boite
        respId <- dialogRun boite
        case respId of
                   ResponseOk -> do
                   --fileChooserAddFilter
                      mFichier <- fileChooserGetFilename boite
                      case mFichier of
                        Just f -> do
                         contenu <- B.readFile f
                         --let vcard = readVCards f contenu
                         --(print.show) vcard
                         entrySetText ePrenom   ((T.unpack.prenom) r)
                         entrySetText e2Prenom  ((T.unpack.initiale) r)
                         entrySetText eNom      ((T.unpack.nomContact) r)
                         entrySetText eTitre    ((T.unpack.titre) r)
                         entrySetText eOrg      ((T.unpack.head.organisation) r)
                         entrySetText eTelTrav  (telTravail r)
                         entrySetText eTelCell  (telCell r)
                         --entrySetText eFax ((fax r)
                         entrySetText eCourriel (courriel r)
                        Nothing -> return ()
                   otherwise  -> return ()
        widgetDestroy boite
      return (toWidget v)


instance Affichable Projet where

 creerWidgetEdition bd r mTrf = do
  fichierXML <- getDataFileName "BoiteLDC.xml"
  g <- builderNew
  builderAddObjectsFromFile g fichierXML ["BoitePrincipale" :: T.Text]
  ref <- newIORef r
  vbox <- builderGetObject g castToVBox ("BoitePrincipale" :: T.Text)

  eNom   <- builderGetObject g castToEntry ("eNom" :: T.Text)
  entrySetText eNom $ (T.unpack.nom) r

  eNom `on` focusOutEvent $ liftIO $ entryGetText eNom  >>= \e -> do
    modifyIORef'' ref (\f -> f { nomProjet=T.pack e }) >>= actualiser bd
    return False

  eNom `on` editableChanged $ do
    e <- entryGetText eNom
    actualiserArbre mTrf e
    mNb <- widgetGetAncestor vbox gTypeNotebook
    actualiserTab mNb vbox e
    return ()

    widgetGrabDefault eNom

  return (toWidget vbox)

suppressionMultiple :: ElementDeProjet a => BD.Connection -> TreeStore a -> [TreePath] -> IO ()
suppressionMultiple bd ts []  = return ()
suppressionMultiple bd ts tps =
  --FIXME : Trop long, pourquoi est-ce que la 2e query ne fonctionne pas?
  mapM_ (\tp -> treeStoreGetTree ts tp >>= \t -> treeStoreNew [t] >>= \nt -> treeModelForeach nt (suppSousArbre bd nt)) tps >>
  --mapM_ (\tp -> treeStoreGetTree ts tp >>= \t -> return $ fmap (suppLien bd) $ flatten t ) tps >>
  mapM_ (treeStoreRemove ts) tps

suppSousArbre :: ElementDeProjet a => BD.Connection -> TreeStore a -> TreeIter -> IO Bool
suppSousArbre bd ts ti = do
  treeModelGetPath ts ti >>= treeStoreGetValue ts >>= suppLien bd
  return False
{--
insertionMultiple :: ElementDeProjet a => BD.Connection -> TreeStore a -> IO a -> [TreePath] -> IO [TreePath]
insertionMultiple bd ts fcn []  = fcn >>= \obj -> insertion bd ts obj [] >>= \x -> return [x]
insertionMultiple bd ts fcn tps = fcn >>= \obj -> mapM (insertion bd ts obj) tps
-}
insertion :: ElementDeProjet a => BD.Connection -> TreeStore a -> a -> TreePath -> IO TreePath
insertion db ts obj tp = do
  mti <- treeModelGetIter ts tp
  case mti of
    Just ti -> treeModelIterNChildren ts (Just ti) >>= \n -> treeStoreInsert ts tp n obj >>
               treeStoreGetValue ts tp >>= \p -> ajtLien db p obj >> return (tp++[n])
    Nothing -> treeModelIterNChildren ts Nothing   >>= \n -> treeStoreInsert ts tp n obj >> return (tp++[n])


instance Affichable ListeDeControle where

 creerWidgetEdition bd ldc mTrf = do
  fichierXML <- getDataFileName "BoiteLDC.xml"
  g <- builderNew
  builderAddObjectsFromFile g fichierXML ["BoitePrincipale" :: T.Text]
  ref <- newIORef ldc
  vbox <- builderGetObject g castToVBox ("BoitePrincipale" :: T.Text)

  eNom   <- builderGetObject g castToEntry ("eNom" :: T.Text)
  entrySetText eNom $ (T.unpack.nom) ldc

  eNom `on` focusOutEvent $ liftIO $ entryGetText eNom  >>= \e -> do
    modifyIORef'' ref (\f -> f { nomLDC=T.pack e }) >>= actualiser bd
    return False

  eNom `on` editableChanged $ do
    e <- entryGetText eNom
    actualiserArbre mTrf e
    mNb <- widgetGetAncestor vbox gTypeNotebook
    actualiserTab mNb vbox e
    return ()

  let estDeplacable ts tp        = return True
      donneeSurDeplacement ts tp = selectionDataSet selectionTypeInteger tp >> return True
      effacerOrigine             = treeStoreRemove

  let peutRecevoir ts tp = do
        mvs <- selectionDataGet selectionTypeInteger :: SelectionDataM (Maybe [Int])
        --On évite de déplacer sur un enfant ou sur le parent immédiat
        ReaderT $ \r -> print $ "\nTreepath a deplacer : "++ show mvs
        ReaderT $ \r -> print $ "Treepath parent pour reception : "++ show tp
        return $ maybe False (\vs -> not (isPrefixOf vs tp || (init vs == tp))) mvs

      surDeplacement ts tp = do
        mvs <- selectionDataGet selectionTypeInteger :: SelectionDataM (Maybe [Int])
        --ReaderT $ \r -> do print $ "\nTreepath a bouger: "++ show mvs
        --ReaderT $ \r -> do print $ "Treepath parent: "++ show tp
        case mvs of
          Just vs -> if 0 == length vs
                     then return False
                     else ReaderT $ \r -> do --changer le lien en bd
                                          valeur <- treeStoreGetValue ts vs
                                          if 0==length (init tp)
                                          then redirigerLienLDC bd valeur ldc
                                          else treeStoreGetValue ts (init tp) >>= redirigerLien bd valeur

                                          -- Ajouter au nouvel endroit
                                          treeStoreGetTree ts vs >>= treeStoreInsertTree ts (init tp) (last tp)
                                          return True
          Nothing -> return False

  let source = DragSourceIface estDeplacable donneeSurDeplacement effacerOrigine
  let destination = DragDestIface peutRecevoir surDeplacement
  ts <- treeStoreNewDND (donnees ldc) (Just source) (Just destination)

  --ts <- treeStoreNew (donnees ldc)
  tv   <- builderGetObject g castToTreeView ("arbre" :: T.Text)
  treeViewSetModel tv ts
  treeViewSetReorderable tv True
  treeViewSetEnableTreeLines tv True
  col <- treeViewColumnNew

  toggleRend <- cellRendererToggleNew
  set toggleRend [cellToggleActivatable := True]
  cellLayoutPackStart  col toggleRend False
  cellLayoutSetAttributes col toggleRend ts (\r -> [cellToggleActive := etat r] )
  toggleRend `on` cellToggled $ \s -> do
    let tp = stringToTreePath s
    treeStoreChange ts tp (\v -> v { etat = not.etat $ v } )
    treeStoreGetValue ts tp >>= actualiser bd

  textRend   <- cellRendererTextNew
  set textRend [cellTextEditable := True, cellTextEditableSet := True]
  cellLayoutPackStart  col textRend True
  cellLayoutSetAttributes col textRend   ts (\r -> [cellText := T.unpack . description $ r] )
  textRend `on` edited $ \tp s -> do
    treeStoreChange ts tp (\v -> v { description = T.pack s } )
    treeStoreGetValue ts tp >>= actualiser bd

  treeViewAppendColumn tv col

  tv `on` buttonPressEvent $ do
    boutton <- eventButton
    clic    <- eventClick
    (x,y)   <- eventCoordinates
    case (boutton,clic) of
      (RightButton,SingleClick) -> liftIO $ do
        sel <- treeViewGetSelection tv
        tps <- treeSelectionGetSelectedRows sel

        theMenu <- menuNew
        menuAttachToWidget theMenu tv

        mi1 <- menuItemNewWithLabel ("Ajouter" :: T.Text)
        mi1 `on` menuItemActivated $ do
          obj <- ajouter bd :: IO ElementDeControle
          let tp = if 0==length tps then [] else head tps
          mti <- treeModelGetIter ts tp
          case mti of
            Just ti -> treeModelIterNChildren ts mti >>= \n -> treeStoreInsert ts tp n obj >>
                       treeStoreGetValue ts tp >>= \p -> ajtLien bd p obj
            Nothing -> treeModelIterNChildren ts mti >>= \n -> treeStoreInsert ts tp n obj >> ajtLienLDC bd ldc obj

        mi2 <- menuItemNewWithLabel ("Supprimer" :: T.Text)
        mi2 `on` menuItemActivated $ suppressionMultiple bd ts tps

        menuShellAppend theMenu mi1
        menuShellAppend theMenu mi2
        menuPopup theMenu Nothing
        widgetShowAll theMenu
        return True

      -- Pour deselectionner tous le elements
      (LeftButton,SingleClick) -> liftIO $ do
        mTP <- treeViewGetPathAtPos tv (floor x, floor y)
        case mTP of
          Just a -> return False
          Nothing -> treeViewGetSelection tv >>= treeSelectionUnselectAll >> return False

      _                        -> return False

  return (toWidget vbox)


--TODO element identite pour RefReuions et RefLDCs

noeudReunions = RefReunions 0 "Réunions" Nothing
noeudLDCs     = ReferenceLDCs 0 "Listes de contrôle" Nothing

obtenirReunionPourProjet :: BD.Connection -> ElementReference w -> MaybeT IO (Tree (ElementReference w))
obtenirReunionPourProjet bd projet = MaybeT $ do
  reunions <- listerReunionRef bd projet
  return $ if 0==length reunions
           then Nothing
           else Just $ Node noeudReunions $ fmap (\r -> Node r []) reunions

obtenirLDCPourProjet :: BD.Connection -> ElementReference w -> MaybeT IO (Tree (ElementReference w))
obtenirLDCPourProjet bd projet = MaybeT $ do
  reunions <- listerLDCRef bd projet
  return $ if 0==length reunions
           then Nothing
           else Just $ Node noeudLDCs $ fmap (\r -> Node r []) reunions

obtRefElemProjet :: BD.Connection -> Tree (ElementReference w) -> IO (Tree (ElementReference w))

obtRefElemProjet bd (Node a []) = do reunions <- runMaybeT $ obtenirReunionPourProjet bd a
                                     ldcs     <- runMaybeT $ obtenirLDCPourProjet bd a
                                     return $ Node a $ maybeToList reunions ++ maybeToList ldcs

obtRefElemProjet bd (Node a xs) = do reunions <- runMaybeT $ obtenirReunionPourProjet bd a
                                     ldcs     <- runMaybeT $ obtenirLDCPourProjet bd a
                                     reste    <- mapM (obtRefElemProjet bd) xs
                                     return $ Node a $ maybeToList reunions ++ maybeToList ldcs ++ reste

fcnNonImplementee = messageDialogNew Nothing
                                     [DialogModal, DialogDestroyWithParent]
                                     MessageError
                                     ButtonsClose
                                     ("Fonction non implémentée." :: T.Text)

class OuvrirDans a where
  ouvrirDans :: a -> BD.Connection -> TreeStore (ElementReference w) -> TreePath -> IO ()
--ouvrirDansUneFenetre :: WidgetClass w => String -> w -> IO ()

trouverEnfantRec :: TreeStore (ElementReference w) -> ElementReference w -> TreeIter -> MaybeT IO TreeIter
trouverEnfantRec tm sb ti = do
  valEnfant <- lift $ treeModelGetPath tm ti >>= treeStoreGetValue tm
  if valEnfant==sb
  then MaybeT $ return $ Just ti
  else (MaybeT $ treeModelIterNext tm ti) >>= trouverEnfantRec tm sb

trouverEnfant :: TreeStore (ElementReference w) -> TreePath -> ElementReference w -> MaybeT IO TreeIter
trouverEnfant tm tp sb= do
  ti <- MaybeT $ treeModelGetIter tm tp
  tiEnfant <- MaybeT $ treeModelIterChildren tm ti
  trouverEnfantRec tm sb tiEnfant

cmdAjt :: ElementDeProjet a => Referenciable a =>
          TreeStore (ElementReference w) -> TreePath -> (ElementReference w -> IO a) -> ElementReference w -> IO TreePath
cmdAjt ts tp fcn sousGroupe = do
  value <- treeStoreGetValue ts tp
  r <- fcn value
  let nouvelleRef = reference r
  ajtDansArbre ts tp nouvelleRef sousGroupe
  {-
  mRs <- runMaybeT $ trouverEnfant ts tp sousGroupe -- Recherche si the sous-groupe existe
  case mRs of
    Just rsDir -> treeModelGetPath ts rsDir >>= \tp -> treeStoreInsert ts tp 0 nouvelleRef >> return (tp++[0])
    Nothing    -> do
              let nn = Node sousGroupe [Node nouvelleRef []] --Ajout du sous-groupe et de l'enfant
              treeStoreInsertTree  ts tp 0 nn >> return (tp++[0,0])
-}
ajtDansArbre :: TreeStore (ElementReference w) -> TreePath -> ElementReference w -> ElementReference w -> IO TreePath
ajtDansArbre ts tp nouvelleRef sousGroupe = do
  mRs <- runMaybeT $ trouverEnfant ts tp sousGroupe -- Recherche si the sous-groupe existe
  case mRs of
    Just rsDir -> treeModelGetPath ts rsDir >>= \tp -> treeStoreInsert ts tp 0 nouvelleRef >> return (tp++[0])
    Nothing    -> do
              let nn = Node sousGroupe [Node nouvelleRef []] --Ajout du sous-groupe et de l'enfant
              treeStoreInsertTree  ts tp 0 nn >> return (tp++[0,0])

cmdAjt2 :: ElementDeProjet a => Referenciable a => TreeStore (ElementReference w) -> TreePath -> (ElementReference w -> IO a) -> IO TreePath
cmdAjt2 ts tp fcn = do
  value <- treeStoreGetValue ts tp
  r <- fcn value
  treeStoreInsert ts tp 0 (reference r) >> return (tp++[0])

creerWidgetTab :: Notebook -> Widget -> String -> IO (Label, HBox)
creerWidgetTab nb w nom = do
       --print.show $ IconSizeUser 16
       image <- imageNewFromStock "gtk-close" IconSizeMenu
       bFermeture <- buttonNew
       buttonSetImage bFermeture image

       hbox <- hBoxNew False 2
       lTab <- labelNew (Just nom )
       boxPackStart hbox lTab PackNatural 0
       boxPackStart hbox bFermeture PackNatural 0
       lMenu <- labelNew (Just nom )

       bFermeture `on` buttonActivated $ do
         mPosition <- obtPagePourWidget nb w
         case mPosition of
           (Just pos) -> notebookRemovePage nb pos
           Nothing -> return ()

       return ( lMenu, hbox)

affDansNB :: (Affichable a) =>
           BD.Connection -> Notebook -> TreeView -> TreeStore (ElementReference Widget) -> TreePath -> ElementReference Widget -> Maybe a -> IO ()
affDansNB bd nb tv ts tp value obj1 = do
    trf <- treeRowReferenceNew ts tp
    let mTrf = trf >>= \trf -> Just (tv,ts,trf)
    case obj1 of
     (Just obj) -> do
       w   <- creerWidgetEdition bd obj mTrf
       treeStoreSetValue ts tp $ value { mPos = Just w }
       (lMenu, hbox) <- creerWidgetTab nb w (T.unpack.nom $ value )
       pos <- notebookAppendPageMenu nb w hbox lMenu
       widgetShowAll hbox
       widgetShowAll nb >> notebookSetCurrentPage nb pos
       return ()
     Nothing -> return ()

findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM _ [] = return Nothing
findM f (x:xs) = do y <- f x
                    if y then return (Just x) else findM f xs

obtPagePourWidget :: WidgetClass w => Notebook -> w -> IO (Maybe Int)
obtPagePourWidget nb widget = do
  nombrePages <- notebookGetNPages nb
  findM fcn [0 .. nombrePages]
  where fcn i = fmap (\a -> a==(Just . castToWidget) widget) (notebookGetNthPage nb i)

instance Show Widget where
 show a = "Widget"

cmdOuvrirDansNotebook :: BD.Connection -> Notebook -> TreeView -> TreeStore (ElementReference Widget) -> TreePath -> IO ()
cmdOuvrirDansNotebook bd nb tv ts tp = do
  value <- treeStoreGetValue ts tp
  case mPos value of
    Just pos -> do
                  iPos <- obtPagePourWidget nb pos
                  case iPos of
                   Just p -> notebookSetCurrentPage nb p
                   Nothing -> return ()
    Nothing -> do
      let idVal = identifiant value
      --print $ show value
      case value of
        ReferenceLDC {} ->
           runMaybeT (obtenir bd idVal :: MaybeT IO ListeDeControle) >>= affDansNB bd nb tv ts tp value

        ReferenceLDCs {}-> fcnNonImplementee >>= \w -> dialogRun w >> widgetDestroy w
        RefProjet {}    ->
            runMaybeT (obtenir bd idVal :: MaybeT IO Projet) >>= affDansNB bd nb tv ts tp value

        RefReunion {}   ->
            runMaybeT (obtenir bd idVal :: MaybeT IO Reunion) >>= affDansNB bd nb tv ts tp value

        RefReunions {}  -> fcnNonImplementee >>= \w -> dialogRun w >> widgetDestroy w
        RefContact {}  ->
            runMaybeT (obtenir bd idVal :: MaybeT IO Contact) >>= affDansNB bd nb tv ts tp value


cmdSupprimer :: WidgetClass w => BD.Connection -> Notebook -> TreeStore (ElementReference w) -> TreePath -> IO ()
cmdSupprimer bd nb ts tp = do
  value <- treeStoreGetValue ts tp
  -- Obtenir le bon info ref.
  -- Enelever l'élément
  -- Enlever la page
  case mPos value of
    Just pos -> do
                  iPos <- obtPagePourWidget nb pos
                  case iPos of
                   Just p -> notebookRemovePage nb p
                   Nothing -> return ()
    Nothing -> return ()
  supprimer bd value
  void $ treeStoreRemove ts tp

actualiserSelection :: TreeStore (ElementReference Widget) -> TreeView -> TreeSelection -> Widget -> TreeIter -> IO Bool
actualiserSelection ts tv sel w ti = do
   tp <- treeModelGetPath ts ti
   val <- treeStoreGetValue ts tp
   case mPos val of
    Just widget -> if widget == w
                   then treeViewExpandToPath tv tp >> treeSelectionSelectPath sel tp >> return True
                   else return False
    Nothing     -> return False

actualiserWigetDansArbre :: TreeStore (ElementReference Widget) -> Widget -> TreeIter -> IO Bool
actualiserWigetDansArbre ts w ti = do
  tp <- treeModelGetPath ts ti
  val <- treeStoreGetValue ts tp
  case mPos val of
    Just widget -> if widget == w
                   then treeStoreChange ts tp (\f -> f { mPos=Nothing }) >> return True
                   else return False
    Nothing     -> return False




nouvelleForestDeProjets :: BD.Connection -> Notebook -> IO TreeView
nouvelleForestDeProjets bd nb = do

  let fcn t = case t of
                ReferenceLDC {}  -> [cellPixbufStockId := ("gtk-paste" :: T.Text)]
                ReferenceLDCs {} -> [cellPixbufStockId := ("gtk-directory" :: T.Text)]
                RefProjet {}     -> [cellPixbufStockId := ("gtk-page-setup" :: T.Text)]
                RefReunion {}    -> [cellPixbufStockId := ("gtk-properties" :: T.Text)]
                RefReunions {}   -> [cellPixbufStockId := ("gtk-directory" :: T.Text)]
                RefContact {}    -> [cellPixbufStockId := ("gtk-yes" :: T.Text)]

  projetsRacine <- listerRefProjets bd --obtenir la racine
  arbresRefProjet <- mapM (listerRefProjet bd) projetsRacine >>= mapM (obtRefElemProjet bd) :: IO [Tree (ElementReference Widget)]

  let estDeplacable ts tp        = do
        val <- treeStoreGetValue ts tp
        case val of
                ReferenceLDC {}  -> return True
                ReferenceLDCs {} -> return False
                RefProjet {}     -> return True
                RefReunion {}    -> return True
                RefReunions {}   -> return False
                RefContact {}    -> return False

      donneeSurDeplacement ts tp = selectionDataSet selectionTypeInteger tp >> return True
      effacerOrigine             = treeStoreRemove

  let peutRecevoir ts tp = do
          mvs <- selectionDataGet selectionTypeInteger :: SelectionDataM (Maybe [Int])
          ReaderT $ \r -> print $ "\nTreepath a bouger: "++ show mvs
          ReaderT $ \r -> print $ "Treepath position de réception: "++ show tp
          case mvs of
            Nothing -> return False
            Just vs -> ReaderT $ \r -> do
              elemOrigine <- treeStoreGetValue ts vs
              mT <- treeStoreLookup ts tp
              let newTp = maybe (init tp) (const tp) mT
              elemDestination <- treeStoreGetValue ts newTp
              return $ case (elemOrigine,elemDestination) of
                --(ReferenceLDC _ _ _, ReferenceLDCs _ _ _) -> not $ (isPrefixOf vs tp || (init vs == tp))
                (ReferenceLDC {}, RefProjet {}) -> not (isPrefixOf vs newTp || (init vs == newTp))
                (RefProjet {}   , RefProjet {}) -> not (isPrefixOf vs newTp || (init vs == newTp))
                --(RefReunion _ _ _  , RefReunions _ _ _  ) -> not $ (isPrefixOf vs tp || (init vs == tp))
                (RefReunion {}  , RefProjet {}) -> not (isPrefixOf vs newTp || (init vs == newTp))
                (_,_)                                     -> False

      surDeplacement ts tp = do
            mvs <- selectionDataGet selectionTypeInteger :: SelectionDataM (Maybe [Int])
            --ReaderT $ \r -> do print $ "\nTreepath a bouger: "++ show mvs
            --ReaderT $ \r -> do print $ "Treepath position de recu: "++ show tp
            case mvs of
              Nothing -> return False
              Just vs -> if 0 == length vs
                         then return False
                         else ReaderT $ \r -> do --changer le lien en bd
                                                  valeur <- treeStoreGetValue ts vs
                                                  let tpParent = init tp
                                                      posEnfant = last tp
                                                  print $ "Pos parent: "++ show tpParent
                                                  if 0==length tpParent
                                                  then
                                                    -- Mettre au premier niveau
                                                    case valeur of
                                                      RefProjet {} -> deplacerSurRacine bd valeur
                                                      otherwise -> return ()
                                                  else
                                                    treeStoreGetValue ts tpParent >>= deplacerSurProjet bd valeur

                                                  -- Ajouter au nouvel endroit
                                                  case valeur of
                                                      RefProjet {}    -> treeStoreGetTree ts vs >>= treeStoreInsertTree ts tpParent posEnfant
                                                      RefReunion {}   -> void $ ajtDansArbre ts tpParent valeur noeudReunions
                                                      ReferenceLDC {} -> void $ ajtDansArbre ts tpParent valeur noeudLDCs
                                                      otherwise             -> return ()

                                                  return True


  let source = DragSourceIface estDeplacable donneeSurDeplacement effacerOrigine
  let destination = DragDestIface peutRecevoir surDeplacement
  ts <- treeStoreNewDND arbresRefProjet (Just source) (Just destination)

  --ts <- treeStoreNewDND arbresRefProjet Nothing Nothing
  tv <- treeViewNewWithModel ts
  treeViewSetReorderable tv True
  treeViewSetEnableTreeLines tv True
  --treeViewSetHoverExpand tv True
  col <- treeViewColumnNew
  sel <- treeViewGetSelection tv

  textRend    <- cellRendererTextNew
  imageRend   <- cellRendererPixbufNew
  set textRend [cellTextEditable := False, cellTextEditableSet := False]
  cellLayoutPackStart col imageRend False
  cellLayoutSetAttributes col imageRend ts fcn
  cellLayoutPackStart col textRend False
  cellLayoutSetAttributes col textRend ts (\r -> [cellText := T.unpack . nom $ r] )
  textRend `on` edited $ \tp s -> do
    val <- treeStoreGetValue ts tp
    treeStoreSetValue ts tp $ val { nomRef = T.pack s }

  treeViewAppendColumn tv col

  nb `on` switchPage $ \pos -> do
    print $ "Changement de page a "++show pos
    mW <- notebookGetNthPage nb pos
    case mW of
      Just w -> treeModelForeach ts (actualiserSelection ts tv sel w)
      Nothing -> return ()

  nb `on` pageReordered $ \w pos ->
    print $ "Reordonnancement de page a "++show pos

  nb `on` pageRemoved $ \w pos -> do
    print $ "Élimination de page a "++show pos++" et du widget "++show w
    treeModelForeach ts (actualiserWigetDansArbre ts w)

  nb `on` pageAdded $ \w pos ->
    print $ "Ajout de page a "++show pos

  tv `on` buttonPressEvent $ do
    boutton <- eventButton
    clic    <- eventClick
    (x,y)   <- eventCoordinates
    case (boutton,clic) of
      (RightButton,SingleClick) -> liftIO $ do

        tps <- treeSelectionGetSelectedRows sel
        theMenu <- menuNew
        menuAttachToWidget theMenu tv
        if 0==length tps

        then do
          mi1 <- menuItemNewWithLabel ("Ajouter projet" :: T.Text)
          mi1 `on` menuItemActivated $ do
            nouveauProjet <- ajouter bd :: IO Projet
            insertion bd ts (reference nouveauProjet) [] >>= cmdOuvrirDansNotebook bd nb tv ts
          mapM_ (menuShellAppend theMenu) [mi1]
          menuPopup theMenu Nothing
          widgetShowAll theMenu

        else
         when (1==length tps) $ do
          let tp = head tps
          value <- treeStoreGetValue ts tp
          mi1 <- menuItemNewWithLabel ("Ajouter sous-projet" :: T.Text)
          mi1 `on` menuItemActivated $ do
            let obj = ajouterAProjet bd :: ElementReference w -> IO Projet
            cmdAjt2 ts tp obj >>= cmdOuvrirDansNotebook bd nb tv ts

          mi2 <- menuItemNewWithLabel ("Supprimer" :: T.Text)
          mi2 `on` menuItemActivated $ cmdSupprimer bd nb ts tp

          mi3 <- menuItemNewWithLabel ("Ajouter une liste de contrôle" :: T.Text)
          mi3 `on` menuItemActivated $ do
            let obj = ajouterAProjet bd :: ElementReference w -> IO ListeDeControle
            cmdAjt ts tp obj noeudLDCs >>= cmdOuvrirDansNotebook bd nb tv ts

          mi4 <- menuItemNewWithLabel ("Ajouter une réunion" :: T.Text)
          mi4 `on` menuItemActivated $ do
            let obj = ajouterAProjet bd :: ElementReference w -> IO Reunion
            cmdAjt ts tp obj noeudReunions >>= cmdOuvrirDansNotebook bd nb tv ts

          mi5 <- menuItemNewWithLabel ("Ouvrir" :: T.Text)
          mi5 `on` menuItemActivated $ cmdOuvrirDansNotebook bd nb tv ts tp

          mapM_ (menuShellAppend theMenu) $ case value of
            ReferenceLDC {} -> [mi2,mi5]
            ReferenceLDCs {} -> []
            RefProjet {}    -> [mi1,mi2,mi3,mi4]
            RefReunion {}   -> [mi2,mi5]
            RefReunions {}  -> []
            RefContact {}   -> [mi2,mi5]

          menuPopup theMenu Nothing
          widgetShowAll theMenu

        return True

      (LeftButton,DoubleClick) -> liftIO $ do
        sel <- treeViewGetSelection tv
        tps <- treeSelectionGetSelectedRows sel
        when (1==length tps) $ do
          let tp = head tps
          cmdOuvrirDansNotebook bd nb tv ts tp
        return True

      -- Pour deselectionner tous le elements
      (LeftButton,SingleClick) -> liftIO $ do
        mTP <- treeViewGetPathAtPos tv (floor x, floor y)
        case mTP of
          Just a -> return False
          Nothing -> treeViewGetSelection tv >>= treeSelectionUnselectAll >> return False

      _                        -> return False
  return tv
