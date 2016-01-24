{-# LANGUAGE OverloadedStrings #-}

import Test.HUnit
import Data.Aeson
import Data.Maybe
import Data.Tree
import ListeDeControle
import qualified Data.Text as T


elem1 = ElementDeControle 0 False "Parent"
elem2 = ElementDeControle 1 False "Enfant 1"
elem3 = ElementDeControle 2 False "Enfant 2"

racine = Node elem1 [Node elem2 [], Node elem3 []]

--arbreEnJSON = "[{\"identifiant\":0,\"etat\":false,\"description\":\"Parent\"},[[{\"identifiant\":1,\"etat\":false,\"description\":\"Enfant 1\"},[]],[{ \"identifiant\":2,\"etat\":false,\"description\":\"Enfant 2\"},[]]]]"

test1 = TestCase $ assertEqual "t1==t2" (fromJust . decode $ encode racine) racine --arbreEnJSON

tests = TestList [ TestLabel "test1" test1
                 ]

main = runTestTT tests
