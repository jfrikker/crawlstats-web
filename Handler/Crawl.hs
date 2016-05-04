{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-name-shadowing -fno-warn-unused-matches #-}

module Handler.Crawl where

import Import

import Crawl.Stats.Player (Player(Player))
import qualified Crawl.Stats.Player as Player
import Crawl.Stats.Monster (Monster)
import Crawl.Stats.Data (CrawlData)
import qualified Crawl.Stats.Data as CrawlData
import Crawl.Stats.Named (Named)
import qualified Crawl.Stats.Named as Named
import qualified Data.Text as Text

renderField :: FieldView App -> Widget
renderField f = [whamlet|
<tr>
  <td>
    <label for=#{fvId f}>#{fvLabel f}
  <td>^{fvInput f}
|]

boundedSettings :: Int -> Int -> FieldSettings m -> FieldSettings m
boundedSettings min max fs = fs {fsAttrs = [("min", Text.pack $ show min), ("max", Text.pack $ show max)] ++ fsAttrs fs}

selectDataField :: (Named a, Eq a, RenderMessage App Text) => (CrawlData -> [a]) -> Handler (Field Handler a)
selectDataField f = do
  app <- getYesod
  let namedList = f $ crawlData app
  return $ selectFieldList [(Text.pack $ Named.name x, x) | x <- namedList]

playerForm :: Player -> PartialForm Player
playerForm defPlayer = do
  -- Basics
  (hpRes, hpView) <- mreq intField (boundedSettings 1 999 "HP") (Just $ Player.hp defPlayer)
  (evRes, evView) <- mreq intField (boundedSettings 1 999 "EV") (Just $ Player.ev defPlayer)
  (strRes, strView) <- mreq intField (boundedSettings 1 999 "Str") (Just $ Player.str defPlayer)
  (intRes, intView) <- mreq intField (boundedSettings 1 999 "Int") (Just $ Player.int defPlayer)
  (dexRes, dexView) <- mreq intField (boundedSettings 1 999 "Dex") (Just $ Player.dex defPlayer)
  let basicFields = [hpView, evView, strView, intView, dexView]

  -- Skills
  (fightingSkillRes, fightingSkillView) <- mreq intField (boundedSettings 0 27 "Fighting") (Just $ Player.fightingSkill defPlayer)
  (macesSkillRes, macesSkillView) <- mreq intField (boundedSettings 0 27 "Maces & Flails") (Just $ Player.macesSkill defPlayer)
  (armourSkillRes, armourSkillView) <- mreq intField (boundedSettings 0 27 "Armour") (Just $ Player.armourSkill defPlayer)
  (shieldSkillRes, shieldSkillView) <- mreq intField (boundedSettings 0 27 "Shields") (Just $ Player.shieldSkill defPlayer)

  let skillFields = [fightingSkillView, macesSkillView, armourSkillView, shieldSkillView]

  -- Equipment
  bodyArmourField <- lift $ selectDataField CrawlData.armour
  (bodyArmourRes, bodyArmourView) <- mreq bodyArmourField "Body Armour" (Just $ Player.armour defPlayer)
  weaponField <- lift $ selectDataField CrawlData.weapons
  (weaponRes, weaponView) <- mreq weaponField "Weapon" (Just $ Player.weapon defPlayer)
  shieldField <- lift $ selectDataField CrawlData.shields
  (shieldRes, shieldView) <- mreq shieldField "Shield" (Just $ Player.shield defPlayer)
  let equipmentFields = [bodyArmourView, weaponView, shieldView]

  let widget = $(widgetFile "playerForm")
  let result = Player
                 <$> hpRes
                 <*> evRes
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

combatForm :: MForm Handler (FormResult [Monster], [FieldView App])
combatForm = do
  monsterField <- lift $ selectDataField CrawlData.monsters
  l <- sequence $ replicate 5 $ mreq monsterField "ignored" Nothing
  let (monsters, views) = unzip l
  return (sequenceA monsters, views)

crawlForm :: Player -> Form Player
crawlForm player extra = do
  (player', playerWidget) <- playerForm player
  (_, monsterViews) <- combatForm
  let widget = [whamlet|
^{extra}
^{playerWidget}
<section>
  <.section_container>
    <.section_header>Combat Outcomes
    <.section_body>
      <table>
        $forall monsterView <- monsterViews
          <tr>
            <td>^{fvInput monsterView}
|]
  return (player', widget)

getCrawlR :: Handler Html
getCrawlR = do
  ((res, characterWidget), enctype) <- runFormGet $ crawlForm def
  defaultLayout $(widgetFile "crawl")
