-----------------------------------------------------------------------------
--
-- Module      :  ListeDeControle
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

{-# LANGUAGE OverloadedStrings, DeriveGeneric, TypeSynonymInstances, FlexibleInstances #-}

module ListeDeControle (
  ElementDeControle (..),
  ListeDeControle (..),
  Projet (..),
  Reunion (..),
  Contact (..),
  ElementDeProjet (..),
  ElementReference (..),
  Referenciable(..),
  Reference(..)
) where

import Data.Aeson
import Data.Tree
import Data.Time.LocalTime
import qualified Data.Text as T
import GHC.Generics
import Data.Monoid

class ElementDeProjet a where
  identifiant :: a -> Integer
  nom         :: a -> T.Text

data ElementDeControle = ElementDeControle { idEDC :: Integer
                                           , etat :: Bool
                                           , description :: T.Text
                                           } deriving (Eq,Generic,Ord,Show)

instance ElementDeProjet ElementDeControle where
  identifiant = idEDC
  nom         = description

data ListeDeControle = ListeDeControle { idLDC :: Integer
                                       , nomLDC :: T.Text
                                       , donnees :: Forest ElementDeControle
                                       } deriving (Eq,Generic,Show)

--FIXME separer ElementDeProjet et Identifiable : Bug entre ref et obj
instance ElementDeProjet ListeDeControle where
  identifiant = idLDC
  nom         = nomLDC

instance FromJSON ElementDeControle

instance ToJSON ElementDeControle

instance FromJSON ListeDeControle

instance ToJSON ListeDeControle

--instance ElementDeProjet ListeDeControle where
--   identifiant a = idEDC . rootLabel $ a
--   nom         a = description . rootLabel $ a






{--data Reunion = Reunion { idReunion :: Integer
                       , objet :: T.Text
                       , debut :: ZonedTime
                       , fin   :: ZonedTime
                       , participants :: [T.Text]
                       , excuses :: [T.Text]
                       , ordreDuJour :: T.Text
                       , compteRendu :: T.Text
                       } deriving (Generic,Show)
                       --}

data Reunion = Reunion { idReunion :: Integer
                       , objet :: T.Text
                       , debut :: ZonedTime
                       , duree   :: Integer
                       , participants :: [Contact]
                       , excuses :: [T.Text]
                       , ordreDuJour :: T.Text
                       , compteRendu :: T.Text
                       } deriving (Generic,Show)

instance FromJSON Reunion

instance ToJSON Reunion

instance ElementDeProjet Reunion where
   identifiant = idReunion
   nom         = objet


data Projet = Projet { idProjet :: Integer
                     , nomProjet :: T.Text
                     , listesdeControle :: [ListeDeControle]
                     , reunions :: [Reunion]
                     --, participantsP :: [T.Text]
                     } deriving (Generic,Show)

instance ElementDeProjet Projet where
   identifiant = idProjet
   nom         = nomProjet

type ListeDeProjets = Forest Projet

instance FromJSON Projet

instance ToJSON Projet

data Contact = Contact { idContact :: Integer
                       , prenom :: T.Text
                       , initiale :: T.Text
                       , nomContact :: T.Text
                       , titre :: T.Text
                       , courriel :: String
                       , telTravail :: String
                       , telCell :: String
                       , organisation :: [T.Text]
                       } deriving (Generic,Show)

instance ElementDeProjet Contact where
   identifiant = idContact
   nom       c = prenom c <> " " <> initiale c <> " " <> nomContact c

instance FromJSON Contact

instance ToJSON Contact

--Contient le chemin du fichier et si il doit etre sauvegarde
data ReferenceFichier a = ReferenceFichier { modifie :: Bool
                                           , chemin :: Maybe FilePath
                                           , donnee :: a
                                           , numTab :: Int
                                           } deriving (Eq, Show)

--TODO : Implémenter les type fantômes
data Reference a = Reference { idARef :: Integer
                             , nomARef :: T.Text
                             , mAPos :: Maybe Int
                             } deriving (Eq,Generic,Show)

instance ElementDeProjet (Reference Reunion) where
   identifiant (Reference i _ _) = i
   nom         (Reference _ n _) = n

instance ElementDeProjet (Reference [Reunion]) where
   identifiant (Reference i _ _) = 0
   nom         (Reference _ n _) = "Réunions"

instance ElementDeProjet (Reference Contact) where
   identifiant (Reference i _ _) = i
   nom         (Reference _ n _) = n

instance ElementDeProjet (Reference Projet) where
   identifiant (Reference i _ _) = i
   nom         (Reference _ n _) = n

instance ElementDeProjet (Reference ListeDeControle) where
   identifiant (Reference i _ _) = i
   nom         (Reference _ n _) = n

instance ElementDeProjet (Reference [ListeDeControle]) where
   identifiant (Reference i _ _) = 0
   nom         (Reference _ n _) = "Listes de contrôle"

data ElementReference w = ReferenceLDC { idRef :: Integer
                                     , nomRef :: T.Text
                                     , mPos :: Maybe w
                                     }
                      | RefReunion   { idRef :: Integer
                                     , nomRef :: T.Text
                                     , mPos :: Maybe w
                                     }
                      | RefReunions  { idRef :: Integer
                                     , nomRef :: T.Text
                                     , mPos :: Maybe w
                                     }
                      | RefProjet    { idRef :: Integer
                                     , nomRef :: T.Text
                                     , mPos :: Maybe w
                                     }
                      | RefContact   { idRef :: Integer
                                     , nomRef :: T.Text
                                     , mPos :: Maybe w
                                     }
                      | ReferenceLDCs{ idRef :: Integer
                                     , nomRef :: T.Text
                                     , mPos :: Maybe w
                                     } deriving (Generic,Show)

-- FIXME
instance Eq (ElementReference w) where
  (==) a b =  (identifiant a == identifiant b) && (nom a == nom b)

instance ElementDeProjet (ElementReference w) where

   identifiant (ReferenceLDC i _ _) = i
   identifiant (RefProjet i _ _)    = i
   identifiant (RefReunion i _ _)   = i
   identifiant (RefReunions i _ _)   = i
   identifiant (RefContact i _ _)   = i
   identifiant (ReferenceLDCs i _ _)   = i

   nom         (ReferenceLDC _ n _) = n
   nom         (RefReunion _ n _)   = n
   nom         (RefProjet _ n _)    = n
   nom         (RefReunions _ n _)    = n
   nom         (RefContact _ n _)    = n
   nom         (ReferenceLDCs _ n _)    = n

class ElementDeProjet a => Referenciable a where
  reference :: a -> ElementReference w
  --elemVide  :: ElementReference

instance Referenciable ListeDeControle where
  reference e = ReferenceLDC (identifiant e) (nom e) Nothing

instance Referenciable Contact where
  reference e = RefContact (identifiant e) (nom e) Nothing

instance Referenciable Projet where
  reference e = RefProjet (identifiant e) (nom e) Nothing

instance Referenciable Reunion where
  reference e = RefReunion (identifiant e) (nom e) Nothing






