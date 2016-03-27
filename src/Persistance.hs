{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}

-----------------------------------------------------------------------------
--
-- Module      :  Persistance
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

module Persistance (
  ouvrirConnection,
  Persiste (ajouter,obtenir,actualiser,lister,ajouterAProjet,supprimer),
  --Liable (ajtLien),
  ajtLien,
  ajtLienLDC,
  ajtLien2,
  suppLien,
  supprimerParticipant,
  listerRefLDC,
  listerRefReunions,
  listerRefContacts,
  listerRefProjet,
  listerRefProjets,
  listerReunionRef,
  listerLDCRef,
  actualiserNom,
  actualiserObject,
  actualiserDebut,
  actualiserDuree,
  actualiserCR,
  actualiserHeure,
  redirigerLien,
  redirigerLienLDC,
  deplacerSurRacine,
  deplacerSurProjet
) where

import qualified Database.HSQL.SQLite3 as BD
import ListeDeControle
import Control.Applicative
import Data.Maybe
import Data.Tree
import Data.List
import qualified Data.Text as T
import System.IO
import Data.Time
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.String.Utils

ouvrirConnection :: String -> IO BD.Connection
ouvrirConnection nom = do
  bd <- BD.connect nom ReadWriteMode
  BD.execute bd "PRAGMA foreign_keys = ON;"
  BD.execute bd "CREATE TABLE IF NOT EXISTS EDC (ID INTEGER PRIMARY KEY ASC, ETAT INTEGER DEFAULT 0, DESCRIPTION TEXT DEFAULT \"\")"
  BD.execute bd "CREATE TABLE IF NOT EXISTS EDC_ARBRE (PARENT INTEGER NOT NULL REFERENCES EDC(ID), ENFANT INTEGER NOT NULL REFERENCES EDC(ID) ON DELETE CASCADE)"
  BD.execute bd "CREATE TABLE IF NOT EXISTS LDC (ID INTEGER PRIMARY KEY ASC, NOM TEXT DEFAULT \"\")"
  BD.execute bd "CREATE TABLE IF NOT EXISTS LDC_LIENS (LISTE INTEGER NOT NULL REFERENCES LDC(ID), ENFANTS INTEGER NOT NULL REFERENCES EDC(ID) ON DELETE CASCADE)"
  BD.execute bd "CREATE TABLE IF NOT EXISTS REUNION (ID INTEGER PRIMARY KEY ASC, NOM TEXT DEFAULT \"\", DEBUT TEXT DEFAULT now, DUREE INTEGER DEFAULT 0, ORDREDUJOUR TEXT DEFAULT \"\", COMPTERENDU TEXT DEFAULT \"\")"
  BD.execute bd "CREATE TABLE IF NOT EXISTS CONTACT (ID INTEGER PRIMARY KEY ASC, PRENOM TEXT DEFAULT \"\", INITIALE TEXT DEFAULT \"\", NOM TEXT DEFAULT \"\", COURRIEL TEXT DEFAULT \"\", TELTRAVAIL TEXT DEFAULT \"\", TELCELL TEXT DEFAULT \"\", TITRE TEXT DEFAULT \"\", ORG TEXT DEFAULT \"\")"
  BD.execute bd "CREATE TABLE IF NOT EXISTS PARTICIPANTS (ID INTEGER PRIMARY KEY ASC, REUNION INTEGER NOT NULL REFERENCES REUNION(ID), CONTACT INTEGER NOT NULL REFERENCES CONTACT(ID))"
  BD.execute bd "CREATE TABLE IF NOT EXISTS EXCUSES (REUNION INTEGER NOT NULL REFERENCES REUNION(ID), CONTACT INTEGER NOT NULL REFERENCES CONTACT(ID))"
  BD.execute bd "CREATE TABLE IF NOT EXISTS PROJET (ID INTEGER PRIMARY KEY ASC, NOM TEXT DEFAULT \"\")"
  BD.execute bd "CREATE TABLE IF NOT EXISTS PROJET_REUNION (ID INTEGER PRIMARY KEY ASC, PROJET INTEGER NOT NULL REFERENCES PROJET(ID), REUNION INTEGER NOT NULL REFERENCES REUNION(ID))"
  BD.execute bd "CREATE TABLE IF NOT EXISTS PROJET_LDC (ID INTEGER PRIMARY KEY ASC, PROJET INTEGER NOT NULL REFERENCES PROJET(ID), LDC INTEGER NOT NULL REFERENCES LDC(ID))"
  BD.execute bd "CREATE TABLE IF NOT EXISTS PROJET_ARBRE (PARENT INTEGER NOT NULL REFERENCES PROJET(ID), ENFANT INTEGER NOT NULL REFERENCES PROJET(ID) ON DELETE CASCADE)"
  return bd

--FIXME Eliminer ajouterAProjet de cette interface
class Persiste a where
  creer   :: BD.Connection -> BD.Statement -> IO a
  ajouter :: BD.Connection -> IO a
  obtenir :: BD.Connection -> Integer -> MaybeT IO a
  lister  :: BD.Connection -> IO [a]
  actualiser     :: BD.Connection -> a -> IO ()
  ajouterAProjet :: BD.Connection -> ElementReference w -> IO a
  supprimer      :: BD.Connection -> a -> IO ()

--class (ElementDeProjet a, ElementDeProjet b) => Liable a b where
--  ajtLien :: BD.Connection -> a -> b -> IO ()
--  suppLien :: BD.Connection -> a -> b -> IO ()
{--
instance Liable ElementDeControle ElementDeControle where

 ajtLien bd parent enfant = do
  let idParent = identifiant parent
      idEnfant = identifiant enfant
  BD.execute bd $ "INSERT INTO EDC_ARBRE (PARENT, ENFANT) VALUES ("++show idParent++","++show idEnfant++")"
--}

suppLien :: ElementDeProjet a => BD.Connection -> a -> IO ()
suppLien bd e = do
  let idEnfant = show (identifiant e)
  BD.inTransaction bd $ \conn -> do
    BD.execute bd $ "DELETE FROM EDC_ARBRE WHERE ENFANT="++idEnfant
    BD.execute bd $ "DELETE FROM EDC_ARBRE WHERE PARENT="++idEnfant
    BD.execute bd $ "DELETE FROM LDC_LIENS WHERE ENFANTS="++idEnfant --FIXME
    BD.execute bd $ "DELETE FROM EDC WHERE ID="++idEnfant

--TODO : Faire un type participant?
supprimerParticipant :: BD.Connection -> Reunion -> Contact -> IO ()
supprimerParticipant bd r c = do
  let obt1er = "SELECT ID FROM PARTICIPANTS WHERE CONTACT="++(show.identifiant) c++" AND REUNION="++(show.identifiant) r++" LIMIT 1"
  BD.execute bd $ "DELETE FROM PARTICIPANTS WHERE ID=("++obt1er++")"

ajtLien :: ElementDeProjet a => BD.Connection -> a -> a -> IO ()
ajtLien bd parent enfant = do
  let idParent = identifiant parent
      idEnfant = identifiant enfant
  BD.execute bd $ "INSERT INTO EDC_ARBRE (PARENT, ENFANT) VALUES ("++show idParent++","++show idEnfant++")"

ajtLienLDC :: BD.Connection -> ListeDeControle -> ElementDeControle -> IO ()
ajtLienLDC bd parent enfant = do
  let idParent = identifiant parent
      idEnfant = identifiant enfant
  BD.execute bd $ "INSERT INTO LDC_LIENS (LISTE, ENFANTS) VALUES ("++show idParent++","++show idEnfant++")"

ajtLien2 :: (ElementDeProjet a, ElementDeProjet b) => BD.Connection -> a -> b -> IO ()
ajtLien2 bd parent enfant = do
  let idParent = identifiant parent
      idEnfant = identifiant enfant
  BD.execute bd $ "INSERT INTO PARTICIPANTS (REUNION, CONTACT) VALUES ("++show idParent++","++show idEnfant++")"
{--
instance Persiste (Reference Projet) where
  creer b s = do
    rowid <- BD.getFieldValue s "ID"
    nom   <- (BD.getFieldValue s "NOM") :: IO String
    return $ Reference rowid (T.pack nom) Nothing

  ajouter bd = undefined

  obtenir bd rowid = MaybeT $ return Nothing

  actualiser bd e = undefined

  lister bd = undefined

  ajouterAProjet bd e = undefined


  supprimer bd p = do
  -- FIXME inTransaction
    mapM_(supprimer bd) =<< listerLDCRef2 bd p
    mapM_(supprimer bd) =<< listerReunionRef bd p
    mapM_(\x -> supprimer bd (rootLabel x)) =<< (subForest <$> listerRefProjet bd p )
    BD.execute bd $ "DELETE FROM PROJET_ARBRE WHERE ENFANT="++show (identifiant p)
    --BD.execute bd $ "DELETE FROM PROJET_ARBRE WHERE PARENT="++show i --Supposer etre inutile
    BD.execute bd $ "DELETE FROM PROJET WHERE ID="++show (identifiant p)

instance Persiste (Reference ListeDeControle) where
  creer b s = do
    rowid <- BD.getFieldValue s "ID"
    nom   <- (BD.getFieldValue s "NOM") :: IO String
    return $ Reference rowid (T.pack nom) Nothing

  ajouter bd = undefined

  obtenir bd rowid = MaybeT $ return Nothing

  actualiser bd e = undefined

  lister bd = undefined

  ajouterAProjet bd e = undefined

  supprimer bd p = do

      -- FIXME inTransaction
        -- obtenir liste de tete
        edcs <- BD.query bd ("SELECT * FROM LDC_LIENS as ll, EDC as e where e.id=ll.enfants and ll.LISTE="++show (identifiant p)) >>= BD.collectRows (creer bd) :: IO [ElementDeControle]

        -- effacer lien liste de tete
        BD.execute bd $ "DELETE FROM LDC_LIENS WHERE LISTE="++show (identifiant p)

        -- obtenir tous les arbres EDC
        arbreEdcs <- mapM (obtListeEDC bd) edcs

        --supprimer les liens d'edcs
        let elemTete = fmap (identifiant) (concat $ fmap flatten arbreEdcs)

        mapM (suppLiensEDC bd) elemTete

        --suuprimer les edc
        mapM (suppArbre bd) arbreEdcs

        BD.execute bd $ "DELETE FROM PROJET_LDC WHERE LDC="++show (identifiant p)
        BD.execute bd $ "DELETE FROM LDC WHERE ID="++show (identifiant p)
--}
instance Persiste (ElementReference w) where
  creer b s = do
    rowid <- BD.getFieldValue s "ID"
    nom   <- BD.getFieldValue s "NOM" :: IO String
    return $ ReferenceLDC rowid (T.pack nom) Nothing

  ajouter bd = undefined

  obtenir bd rowid = MaybeT $ return Nothing

  actualiser bd e = undefined

  lister bd = undefined

  ajouterAProjet bd e = undefined

--FIXME : La suppression est trop lente
  supprimer bd e =
    case e of
      (ReferenceLDC i _ _)  -> do

        -- obtenir liste de tete
          edcs <- BD.query bd ("SELECT * FROM LDC_LIENS as ll, EDC as e where e.id=ll.enfants and ll.LISTE="++show i) >>= BD.collectRows (creer bd) :: IO [ElementDeControle]

        -- effacer lien liste de tete
          BD.execute bd $ "DELETE FROM LDC_LIENS WHERE LISTE="++show i

        -- obtenir tous les arbres EDC
          arbreEdcs <- mapM (obtListeEDC bd) edcs

        --supprimer les liens d'edcs
          let elemTete = fmap identifiant (concat $ fmap flatten arbreEdcs)

          mapM_ (suppLiensEDC bd) elemTete

        --suuprimer les edc
          mapM_ (suppArbre bd) arbreEdcs

          BD.execute bd $ "DELETE FROM PROJET_LDC WHERE LDC="++show i
          BD.execute bd $ "DELETE FROM LDC WHERE ID="++show i

      ReferenceLDCs {} -> return () --Ne représente rien en BD

      p@(RefProjet i _ _)     -> do
          mapM_(supprimer bd) =<< listerLDCRef bd p
          mapM_(supprimer bd) =<< listerReunionRef bd p
          mapM_(supprimer bd . rootLabel) =<< (subForest <$> listerRefProjet bd p )
          BD.execute bd $ "DELETE FROM PROJET_ARBRE WHERE ENFANT="++show i
          --BD.execute bd $ "DELETE FROM PROJET_ARBRE WHERE PARENT="++show i --Supposer etre inutile
          BD.execute bd $ "DELETE FROM PROJET WHERE ID="++show i

      (RefReunion i _ _)    ->
        --Éliminer les liens projet - réunion
        BD.inTransaction bd $ \conn -> do
          BD.execute bd $ "DELETE FROM PROJET_REUNION WHERE REUNION="++show i
         --Éliminer les réunions
          BD.execute bd $ "DELETE FROM REUNION WHERE ID="++show i

      RefReunions {}   -> return () --Ne représente rien en BD

      (RefContact i _ _)    -> BD.execute bd $ "DELETE FROM CONTACT WHERE ID="++show i

suppArbre :: BD.Connection -> Tree ElementDeControle -> IO ()
suppArbre bd edcs = mapM_ (supprimer bd) (flatten edcs)

suppLiensEDC :: BD.Connection -> Integer -> IO ()
suppLiensEDC bd i = do BD.execute bd $ "DELETE FROM EDC_ARBRE WHERE ENFANT="++show i
                       BD.execute bd $ "DELETE FROM EDC_ARBRE WHERE PARENT="++show i

instance Persiste ElementDeControle where
  creer b s = do
    idEDC <- BD.getFieldValue s "ID"
    etatEDC <- BD.getFieldValue s "ETAT" :: IO Integer
    descEDC <- BD.getFieldValue s "DESCRIPTION" :: IO String
    return $ ElementDeControle idEDC (intABool etatEDC) (T.pack descEDC)

  ajouter bd = do
    BD.execute bd "INSERT INTO EDC (etat,description) VALUES (0,\"Nouveau\")"
    stat  <- BD.query bd "SELECT * FROM EDC where ID=(SELECT last_insert_rowid())"
    head <$> BD.collectRows (creer bd) stat

  obtenir bd rowid = MaybeT $ do
    res <- BD.query bd ("SELECT * FROM EDC where ID="++show rowid) >>= BD.collectRows (creer bd)
    return $ listToMaybe res

  actualiser bd e = do
    let strEtat = if etat e then "1" else "0"
    BD.execute bd $ "UPDATE EDC SET ETAT='"++strEtat++"',DESCRIPTION='"++replaceApos (T.unpack (nom e))++"' WHERE ID="++show (identifiant e)

  lister bd = BD.query bd "SELECT * FROM EDC" >>= BD.collectRows (creer bd)

  ajouterAProjet bd p = undefined

  supprimer bd e = BD.execute bd $ "DELETE FROM EDC WHERE ID="++(show.identifiant) e



instance Persiste ListeDeControle where
  creer bd s = do
    idLDC  <- BD.getFieldValue s "ID" :: IO Integer
    nomLDC <- BD.getFieldValue s "NOM" :: IO String
    statRec  <- BD.query bd $ "SELECT * FROM LDC_LIENS as ll, EDC as e where ll.ENFANTS=e.ID and ll.LISTE="++show idLDC
    res <- BD.collectRows (creer bd) statRec >>= mapM (obtListeEDC bd)
    return $ ListeDeControle idLDC (T.pack nomLDC) res

  ajouter bd = do
    BD.execute bd "INSERT INTO LDC (nom) VALUES ('')"
    stat  <- BD.query bd "SELECT * FROM LDC where ID=(SELECT last_insert_rowid())"
    BD.fetch stat
    rowid <- BD.getFieldValue stat "ID"
    BD.closeStatement stat
    edc <- ajouter bd :: IO ElementDeControle
    BD.execute bd $ "INSERT INTO LDC_LIENS (LISTE,ENFANTS) VALUES ("++show rowid++","++(show.identifiant) edc++")"
    return $ ListeDeControle rowid (T.pack "") [ Node edc [] ]

  obtenir bd rowid = MaybeT $ do
    res <- BD.query bd ("SELECT * FROM LDC where ID="++show rowid) >>= BD.collectRows (creer bd)
    return $ listToMaybe res

  actualiser bd e = do
    let colonnes = ["NOM"]
        fcn = [T.unpack.nomLDC]
        res = zipWith (\a b -> a++"='"++replaceApos b++"'") colonnes $ fmap ($e) fcn
    BD.execute bd $ "UPDATE LDC SET "++intercalate "," res++" WHERE ID="++show (identifiant e)
    --FIXME Mettre a jour les enfants?

  lister bd = BD.query bd "SELECT * FROM LDC" >>= BD.collectRows (creer bd)

  ajouterAProjet bd p = do
    r <- ajouter bd :: IO ListeDeControle
    let idParent = identifiant p
        idEnfant = identifiant r
    BD.execute bd $ "INSERT INTO PROJET_LDC (PROJET,LDC) VALUES ("++show idParent++","++show idEnfant++")"
    return r

  supprimer bd e = supprimer bd $ ReferenceLDC (identifiant e) "" Nothing

redirigerLien ::  BD.Connection -> ElementDeControle -> ElementDeControle -> IO ()
redirigerLien bd enfant parent = do
  let idParent = identifiant parent
      idEnfant = identifiant enfant
  BD.inTransaction bd $ \conn -> do
    BD.execute bd $ "DELETE FROM EDC_ARBRE WHERE ENFANT="++show idEnfant
    BD.execute bd $ "DELETE FROM LDC_LIENS WHERE ENFANTS="++show idEnfant
    BD.execute bd $ "INSERT INTO EDC_ARBRE (PARENT,ENFANT) VALUES ("++show idParent++","++show idEnfant++")"

redirigerLienLDC ::  BD.Connection -> ElementDeControle -> ListeDeControle -> IO ()
redirigerLienLDC bd enfant parent = do
  let idParent = identifiant parent
      idEnfant = identifiant enfant
  BD.inTransaction bd $ \conn -> do
    BD.execute bd $ "DELETE FROM EDC_ARBRE WHERE ENFANT="++show idEnfant
    BD.execute bd $ "DELETE FROM LDC_LIENS WHERE ENFANTS="++show idEnfant
    BD.execute bd $ "INSERT INTO LDC_LIENS (LISTE,ENFANTS) VALUES ("++show idParent++","++show idEnfant++")"

deplacerSurRacine ::  BD.Connection -> ElementReference w -> IO ()
deplacerSurRacine bd enfant = do
  let idEnfant = identifiant enfant
  BD.inTransaction bd $ \conn ->
    BD.execute bd $ "DELETE FROM PROJET_ARBRE WHERE ENFANT="++show idEnfant

deplacerSurProjet :: BD.Connection -> ElementReference w -> ElementReference w -> IO ()
deplacerSurProjet bd enfant parent = do
  let idParent = identifiant parent
      idEnfant = identifiant enfant
  case (enfant,parent) of
    (ReferenceLDC {}, RefProjet {})     -> do
      print $ "ReferenceLDC:" ++ show (idParent,idEnfant)
      BD.execute bd $ "UPDATE PROJET_LDC SET PROJET="++show idParent++" WHERE LDC="++show idEnfant

    (RefProjet {}, RefProjet {}) -> BD.inTransaction bd $ \conn -> do
      print $ "RefProjet:" ++ show (idParent,idEnfant)
      BD.execute bd $ "DELETE FROM PROJET_ARBRE WHERE ENFANT="++show idEnfant
      BD.execute bd $ "INSERT INTO PROJET_ARBRE (PARENT,ENFANT) VALUES ("++show idParent++","++show idEnfant++")"

    (RefReunion {}, RefProjet {} )  -> do
      print $ "RefReunion:" ++ show (idParent,idEnfant)
      BD.execute bd $ "UPDATE PROJET_REUNION SET PROJET="++show idParent++" WHERE REUNION="++show idEnfant

    (_,_)                                     -> return ()

instance Persiste Reunion where
  creer bd s = do
    id    <- BD.getFieldValue s "ID"
    obj   <- BD.getFieldValue s "NOM" :: IO String
    debut <- BD.getFieldValue s "DEBUT" :: IO String
    duree <- BD.getFieldValue s "DUREE" :: IO Integer
    odj   <- BD.getFieldValue s "ORDREDUJOUR" :: IO String
    cr    <- BD.getFieldValue s "COMPTERENDU" :: IO String
    contacts <- BD.query bd ("SELECT * FROM CONTACT WHERE ID IN (SELECT c.ID FROM PARTICIPANTS as p, CONTACT as c WHERE p.REUNION="++show id++" AND p.CONTACT=c.ID)") >>= BD.collectRows (creer bd)
    return $ Reunion id (T.pack obj) (readTime defaultTimeLocale "%FT%H:%M%z" debut) duree contacts [] (T.pack odj) (T.pack cr)

  ajouter bd = do
    zt <- getZonedTime
    let ztStr = formatTime defaultTimeLocale "%FT%H:%M%z" zt
    BD.execute bd $ "INSERT INTO REUNION (DEBUT) VALUES ('"++replaceApos ztStr++"')"
    stat  <- BD.query bd "SELECT * FROM REUNION where ID=(SELECT last_insert_rowid())"
    head <$> BD.collectRows (creer bd) stat

  obtenir bd rowid = MaybeT $ do
    res <- BD.query bd ("SELECT * FROM REUNION where ID="++show rowid) >>= BD.collectRows (creer bd)
    return $ listToMaybe res

  actualiser bd e = undefined

  lister bd = BD.query bd "SELECT * FROM REUNION" >>= BD.collectRows (creer bd)

  supprimer bd e = BD.execute bd $ "DELETE FROM REUNION WHERE ID="++(show.identifiant) e

  ajouterAProjet bd p = do
    r <- ajouter bd :: IO Reunion
    let idParent = identifiant p
        idEnfant = identifiant r
    BD.execute bd $ "INSERT INTO PROJET_REUNION (PROJET,REUNION) VALUES ("++show idParent++","++show idEnfant++")"
    return r



instance Persiste Contact where
  creer b s = do
    id       <- BD.getFieldValue s "ID"
    prenom   <- BD.getFieldValue s "PRENOM" :: IO String
    initiale <- BD.getFieldValue s "INITIALE" :: IO String
    nom      <- BD.getFieldValue s "NOM" :: IO String
    courriel <- BD.getFieldValue s "COURRIEL" :: IO String
    telCell  <- BD.getFieldValue s "TELCELL" :: IO String
    telTravail <- BD.getFieldValue s "TELTRAVAIL" :: IO String
    titre    <- BD.getFieldValue s "TITRE" :: IO String
    org      <- BD.getFieldValue s "ORG" :: IO String
    return $ Contact id (T.pack prenom) (T.pack initiale) (T.pack nom) (T.pack titre) courriel telTravail telCell ((T.split (==';').T.pack) org)

  ajouter bd = do
    BD.execute bd "INSERT INTO CONTACT (NOM) VALUES ('')"
    stat  <- BD.query bd "SELECT * FROM CONTACT where ID=(SELECT last_insert_rowid())"
    head <$> BD.collectRows (creer bd) stat

  obtenir bd rowid = MaybeT $ do
    res <- BD.query bd ("SELECT * FROM CONTACT where ID="++show rowid) >>= BD.collectRows (creer bd)
    return $ listToMaybe res

  actualiser bd e = do
    let colonnes = ["PRENOM","INITIALE","NOM","COURRIEL","TELCELL","TELTRAVAIL","TITRE", "ORG"]
        fcn = [T.unpack.prenom,T.unpack.initiale,T.unpack.nomContact,courriel,telCell,telTravail,T.unpack.titre,T.unpack.T.intercalate ";".organisation]
        res = zipWith (\a b -> a++"='"++replaceApos b++"'") colonnes $ fmap ($e) fcn
    --(print.show) colonnes
    BD.execute bd $ "UPDATE CONTACT SET "++intercalate "," res++" WHERE ID="++show (identifiant e)

  lister bd = BD.query bd "SELECT * FROM CONTACT" >>= BD.collectRows (creer bd)

  ajouterAProjet bd e = undefined

  supprimer bd e = BD.execute bd $ "DELETE FROM CONTACT WHERE ID="++(show.identifiant) e




instance Persiste Projet where
  creer bd s = do
    id <- BD.getFieldValue s "ID"
    nom <- BD.getFieldValue s "NOM" :: IO String
    ldcs <- BD.query bd ("SELECT * FROM LDC WHERE ID IN (SELECT l.ID FROM PROJET_LDC as pl, LDC as l WHERE pl.PROJET="++show id++" AND pl.LDC=l.ID)") >>= BD.collectRows (creer bd)
    reunions <- BD.query bd ("SELECT * FROM REUNION WHERE ID IN (SELECT r.ID FROM PROJET_REUNION as pr, REUNION as r WHERE pr.PROJET="++show id++" AND pr.REUNION=r.ID)") >>= BD.collectRows (creer bd)
    return $ Projet id (T.pack nom) ldcs reunions --[]

  ajouter bd = do
    BD.execute bd "INSERT INTO PROJET (NOM) VALUES ('')"
    stat  <- BD.query bd "SELECT * FROM PROJET where ID=(SELECT last_insert_rowid())"
    head <$> BD.collectRows (creer bd) stat

  obtenir bd rowid = MaybeT $ do
    res <- BD.query bd ("SELECT * FROM PROJET where ID="++show rowid) >>= BD.collectRows (creer bd)
    return $ listToMaybe res

  actualiser bd e = do
    let colonnes = ["NOM"]
        fcn = [T.unpack.nomProjet]
        res = zipWith (\a b -> a++"='"++replaceApos b++"'") colonnes $ fmap ($e) fcn
    --(print.show) colonnes
    BD.execute bd $ "UPDATE PROJET SET "++intercalate "," res++" WHERE ID="++show (identifiant e)
    --FIXME Mettre a jour les enfants?

  lister bd = BD.query bd "SELECT * FROM PROJET" >>= BD.collectRows (creer bd)

  ajouterAProjet bd p = do
    r <- ajouter bd :: IO Projet
    let idParent = identifiant p
        idEnfant = identifiant r
    BD.execute bd $ "INSERT INTO PROJET_ARBRE (PARENT,ENFANT) VALUES ("++show idParent++","++show idEnfant++")"
    return r

  supprimer bd e = supprimer bd $ RefProjet (identifiant e) "" Nothing



creerRefLDC :: BD.Connection -> BD.Statement -> IO (ElementReference w)
creerRefLDC b s = do
    rowid <- BD.getFieldValue s "ID"
    nom   <- BD.getFieldValue s "NOM" :: IO String
    return $ ReferenceLDC rowid (T.pack nom) Nothing

listerRefLDC :: BD.Connection -> IO [ElementReference w]
listerRefLDC bd = BD.query bd "SELECT * FROM LDC" >>= BD.collectRows (creerRefLDC bd)

creerRefReunions :: BD.Connection -> BD.Statement -> IO (ElementReference w)
creerRefReunions b s = do
    rowid <- BD.getFieldValue s "ID"
    nom   <- BD.getFieldValue s "NOM" :: IO String
    return $ RefReunions rowid (T.pack nom) Nothing

listerRefReunions :: BD.Connection -> IO [ElementReference w]
listerRefReunions bd = BD.query bd "SELECT * FROM REUNION" >>= BD.collectRows (creerRefReunion bd)

creerRefContact :: BD.Connection -> BD.Statement -> IO (ElementReference w)
creerRefContact b s = do
    rowid <- BD.getFieldValue s "ID"
    nom   <- BD.getFieldValue s "NOM" :: IO String
    return $ RefContact rowid (T.pack nom) Nothing

listerRefContacts :: BD.Connection -> IO [ElementReference w]
listerRefContacts bd = BD.query bd "SELECT * FROM CONTACT" >>= BD.collectRows (creerRefContact bd)

creerRefReunion :: BD.Connection -> BD.Statement -> IO (ElementReference w)
creerRefReunion b s = do
    rowid <- BD.getFieldValue s "ID"
    nom   <- BD.getFieldValue s "NOM" :: IO String
    return $ RefReunion rowid (T.pack nom) Nothing

listerReunionRef :: BD.Connection -> ElementReference w -> IO [ElementReference w]
listerReunionRef bd r = BD.query bd ("SELECT * FROM REUNION as r, PROJET_REUNION as pr WHERE pr.REUNION=r.ID and pr.PROJET="++(show.identifiant) r)
                    >>= BD.collectRows (creerRefReunion bd)

--listerLDCTeteRef
--      (ReferenceLDC i _ _)  -> do
--        -- obtenir liste de tete
--        stat  <- BD.query bd $ "SELECT * FROM LDC_LIENS where LISTE="++show i

listerLDCRef :: BD.Connection -> ElementReference w -> IO [ElementReference w]
listerLDCRef bd r = BD.query bd ("SELECT * FROM LDC as r, PROJET_LDC as pr WHERE pr.LDC=r.ID and pr.PROJET="++(show.identifiant) r)
                >>= BD.collectRows (creerRefLDC bd)

--listerLDCRef2 :: BD.Connection -> Reference Projet -> IO [Reference ListeDeControle]
--listerLDCRef2 bd r = BD.query bd ("SELECT * FROM LDC as r, PROJET_LDC as pr WHERE pr.LDC=r.ID and pr.PROJET="++(show.identifiant) r)
--                >>= BD.collectRows (creer bd)

listerRefProjet :: BD.Connection -> ElementReference w -> IO (Tree (ElementReference w))
listerRefProjet bd p = do
  let selInit = "select ea.parent,e.id,e.nom from PROJET as e, PROJET_ARBRE as ea where e.id=ea.enfant and ea.parent=" ++show (identifiant p)
      selRec  = "SELECT edcs.id,e.id,e.nom FROM PROJET as e, PROJET_ARBRE as ea, edcs WHERE ea.parent = edcs.id and ea.enfant=e.id"
      selFin  = "SELECT * FROM edcs"
  stat  <- BD.query bd $ "WITH RECURSIVE edcs(p,id,nom) AS ( "++selInit++" UNION "++selRec++" ) "++selFin
  elementsDeRef <- recObtRef stat (Node p [])
  BD.closeStatement stat
  return elementsDeRef

creerRefProjet :: BD.Connection -> BD.Statement -> IO (ElementReference w)
creerRefProjet b s = do
    rowid <- BD.getFieldValue s "ID"
    nom   <- BD.getFieldValue s "NOM" :: IO String
    return $ RefProjet rowid (T.pack nom) Nothing

listerRefProjets :: BD.Connection -> IO [ElementReference w]
listerRefProjets bd = BD.query bd "SELECT * FROM PROJET WHERE ID NOT IN (SELECT DISTINCT ENFANT FROM PROJET_ARBRE)"
                  >>= BD.collectRows (creerRefProjet bd)

obtListeEDC :: BD.Connection -> ElementDeControle -> IO (Tree ElementDeControle)
obtListeEDC bd p = do
  let selInit = "select ea.parent,e.id,e.etat,e.description from EDC as e, EDC_ARBRE as ea where e.id=ea.enfant and ea.parent=" ++show (identifiant p)
      selRec  = "SELECT edcs.id,e.id,e.etat,e.description FROM EDC as e, EDC_ARBRE as ea, edcs WHERE ea.parent = edcs.id and ea.enfant=e.id"
      selFin  = "SELECT * FROM edcs"
  stat  <- BD.query bd $ "WITH RECURSIVE edcs(p,id,etat,d) AS ( "++selInit++" UNION "++selRec++" ) "++selFin
  elementsDeRef <- recObtEDC stat (Node p [])
  BD.closeStatement stat
  return elementsDeRef

intABool :: Integer -> Bool
intABool i = i/=0

boolAInt :: Bool -> Integer
boolAInt b = if b then 1 else 0

recObtRef :: BD.Statement -> Tree (ElementReference w) -> IO (Tree (ElementReference w))
recObtRef stat arbre = do
  resATraiter <- BD.fetch stat
  if resATraiter
  then do
    parentEDC <- BD.getFieldValue stat "p"
    idEDC <- BD.getFieldValue stat "id"
    etatEDC <- BD.getFieldValue stat "nom" :: IO String
    let elem = RefProjet idEDC (T.pack etatEDC) Nothing
    recObtRef stat $ insertion elem parentEDC arbre
  else return arbre

recObtEDC :: BD.Statement -> Tree ElementDeControle -> IO (Tree ElementDeControle)
recObtEDC stat arbre = do
  resATraiter <- BD.fetch stat
  if resATraiter
  then do
    parentEDC <- BD.getFieldValue stat "p"
    idEDC <- BD.getFieldValue stat "id"
    etatEDC <- BD.getFieldValue stat "etat" :: IO Integer
    descEDC <- BD.getFieldValue stat "d" :: IO String
    let elem = ElementDeControle idEDC (intABool etatEDC) (T.pack descEDC)
    recObtEDC stat $ insertion elem parentEDC arbre
  else return arbre

insertion :: ElementDeProjet a => a -> Integer -> Tree a -> Tree a
insertion e parent (Node tete es) | parent == identifiant tete = Node tete (es++[Node e []])
                                  | otherwise      = Node tete $ map (insertion e parent) es

actualiserNom :: BD.Connection -> Integer -> String -> IO ()
actualiserNom bd rowid str =
  BD.execute bd $ "UPDATE LDC SET NOM='"++replaceApos str++"' WHERE ID="++show rowid

actualiserObject :: BD.Connection -> Integer -> String -> IO ()
actualiserObject bd rowid s =
  BD.execute bd $ "UPDATE REUNION SET NOM='"++replaceApos s++"' WHERE ID="++show rowid

actualiserDebut :: BD.Connection -> Integer -> (Int,Int,Int) -> IO ()
actualiserDebut bd rowid (a,m,j) = do
  --id <- BD.getFieldValue stat "ID"
  stat <- BD.query bd $ "SELECT DEBUT FROM REUNION where ID="++show rowid
  BD.fetch stat
  debut  <- BD.getFieldValue stat "DEBUT" :: IO String
  BD.closeStatement stat
  let zt = readTime defaultTimeLocale "%FT%H:%M%z" debut
      zt2 = ZonedTime (LocalTime (fromGregorian (toInteger a) (m+1) j) ((localTimeOfDay.zonedTimeToLocalTime) zt)) (zonedTimeZone zt)
      ztStr = formatTime defaultTimeLocale "%FT%H:%M%z" zt2
  BD.execute bd $ "UPDATE REUNION SET DEBUT='"++replaceApos ztStr++"' WHERE ID="++show rowid

actualiserHeure :: BD.Connection -> Integer -> (Int,Int) -> IO ()
actualiserHeure bd rowid (h,m) = do
  --id <- BD.getFieldValue stat "ID"
  stat <- BD.query bd $ "SELECT DEBUT FROM REUNION where ID="++show rowid
  BD.fetch stat
  debut  <- BD.getFieldValue stat "DEBUT" :: IO String
  BD.closeStatement stat
  let zt = readTime defaultTimeLocale "%FT%H:%M%z" debut
      --zt2 = ZonedTime (LocalTime (fromGregorian (toInteger a) m j) ((localTimeOfDay.zonedTimeToLocalTime) zt)) (zonedTimeZone zt)
      zt2 = ZonedTime (LocalTime ((localDay.zonedTimeToLocalTime) zt) (TimeOfDay h m 0)) (zonedTimeZone zt)
      ztStr = formatTime defaultTimeLocale "%FT%H:%M%z" zt2
  BD.execute bd $ "UPDATE REUNION SET DEBUT='"++replaceApos ztStr++"' WHERE ID="++show rowid

actualiserDuree :: BD.Connection -> Integer -> String -> IO ()
actualiserDuree bd rowid str = BD.execute bd $ "UPDATE REUNION SET DUREE='"++replaceApos str++"' WHERE ID="++show rowid

replaceApos = replace "'" "''"

actualiserCR :: BD.Connection -> Integer -> String -> IO ()
actualiserCR bd rowid str = BD.execute bd $ "UPDATE REUNION SET COMPTERENDU='"++replaceApos str++"' WHERE ID="++show rowid


