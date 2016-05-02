{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-name-shadowing -fno-warn-unused-matches #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.Crawl where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput)
import Text.Julius (RawJS (..))

import Crawl.Stats.Player (Player (Player))
import qualified Crawl.Stats.Player as Player
import Crawl.Stats.Data (CrawlData)
import qualified Crawl.Stats.Data as CrawlData
import Crawl.Stats.Named (Named)
import qualified Crawl.Stats.Named as Named
import qualified Data.Text as Text

renderField :: FieldView App -> Widget
renderField f = [whamlet|
<tr>
  <td>#{fvLabel f}
  <td>^{fvInput f}
|]


 -- hp :: Integer,
 -- ev :: Integer,
 -- str :: Integer,
 -- int :: Integer,
 -- dex :: Integer,
 -- weapon :: Weapon,
 -- armour :: Armour,
 -- shield :: Shield,
 -- fightingSkill :: Integer,
 -- macesSkill :: Integer,
 -- armourSkill :: Integer,
 -- shieldSkill :: Integer

boundedSettings :: (Show i, Num i) => i -> i -> FieldSettings m -> FieldSettings m
boundedSettings min max fs = fs {fsAttrs = [("min", Text.pack $ show min), ("max", Text.pack $ show max)] ++ fsAttrs fs}

selectDataField :: (Named a, Eq a, RenderMessage App Text) => (CrawlData -> [a]) -> Handler (Field Handler a)
selectDataField f = do
  app <- getYesod
  let namedList = f $ crawlData app
  return $ selectFieldList [(Text.pack $ Named.name x, x) | x <- namedList]

playerForm :: Form Player
playerForm extra = do
  -- Basics
  (hpRes, hpView) <- mreq intField (boundedSettings 1 999 "HP") (Just 1)
  (strRes, strView) <- mreq intField (boundedSettings 1 999 "Str") (Just 1)
  (intRes, intView) <- mreq intField (boundedSettings 1 999 "Int") (Just 1)
  (dexRes, dexView) <- mreq intField (boundedSettings 1 999 "Dex") (Just 1)
  let basicFields = [hpView, strView, intView, dexView]

  -- Skills
  (fightingSkillRes, fightingSkillView) <- mreq intField (boundedSettings 0 27 "Fighting") (Just 0)
  (macesSkillRes, macesSkillView) <- mreq intField (boundedSettings 0 27 "Maces & Flails") (Just 0)
  (armourSkillRes, armourSkillView) <- mreq intField (boundedSettings 0 27 "Armour") (Just 0)
  (shieldSkillRes, shieldSkillView) <- mreq intField (boundedSettings 0 27 "Shields") (Just 0)

  let skillFields = [fightingSkillView, macesSkillView, armourSkillView, shieldSkillView]

  -- Equipment
  bodyArmourField <- lift $ selectDataField CrawlData.armour
  (bodyArmourRes, bodyArmourView) <- mreq bodyArmourField "Body Armour" Nothing
  weaponField <- lift $ selectDataField CrawlData.weapons
  (weaponRes, weaponView) <- mreq weaponField "Weapon" Nothing
  shieldField <- lift $ selectDataField CrawlData.shields
  (shieldRes, shieldView) <- mreq shieldField "Shield" Nothing
  let equipmentFields = [bodyArmourView, weaponView, shieldView]

  let widget = $(widgetFile "playerForm")
  let result = Player
                 <$> hpRes
                 <*> pure 1
                 <*> strRes
                 <*> intRes
                 <*> dexRes
                 <*> weaponRes
                 <*> bodyArmourRes
                 <*> shieldRes
                 <*> fightingSkillRes
                 <*> macesSkillRes
                 <*> armourSkillRes
                 <*> shieldSkillRes
  return (result, widget)

getCrawlR :: Handler Html
getCrawlR = do
  ((res, characterWidget), enctype) <- runFormGet playerForm
  defaultLayout $(widgetFile "crawl")
