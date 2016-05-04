{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-name-shadowing -fno-warn-unused-matches #-}
{-# LANGUAGE RankNTypes #-}

module Handler.Crawl where

import Import hiding ((<.>))

import Crawl.Stats.Lens.Player (Player)
import qualified Crawl.Stats.Lens.Player as Player
import Crawl.Stats.Monster (Monster)
import Crawl.Stats.Data (CrawlData)
import qualified Crawl.Stats.Data as CrawlData
import Crawl.Stats.Named (Named)
import qualified Crawl.Stats.Named as Named
import qualified Data.Text as Text
import Control.Lens hiding (lensField, (<.>))

boundedSettings :: Int -> Int -> FieldSettings m -> FieldSettings m
boundedSettings min max fs = fs {fsAttrs = [("min", Text.pack $ show min), ("max", Text.pack $ show max)] ++ fsAttrs fs}

selectDataField :: (Named a, Eq a, RenderMessage App Text) => (CrawlData -> [a]) -> Handler (Field Handler a)
selectDataField f = do
  app <- getYesod
  let namedList = f $ crawlData app
  return $ selectFieldList [(Text.pack $ Named.name x, x) | x <- namedList]

lensField :: Lens' d a -> d -> (Maybe a -> AForm Handler a) -> AForm Handler (d -> d)
lensField lens defInst field = set lens <$> field (Just $ defInst ^.lens)

infixl 4 <.>

(<.>) :: Applicative f => f (a -> b) -> f (b -> c) -> f (a -> c)
(<.>) = liftA2 $ flip (.)

basicsForm :: Player -> PartialForm (Player -> Player)
basicsForm p = (flip renderTable) mempty $
  (lensField Player.hp p $ areq intField (boundedSettings 1 999 "HP"))
  <.> (lensField Player.ev p $ areq intField (boundedSettings 1 999 "EV"))
  <.> (lensField Player.str p $ areq intField (boundedSettings 1 999 "Str"))
  <.> (lensField Player.int p $ areq intField (boundedSettings 1 999 "Int"))
  <.> (lensField Player.dex p $ areq intField (boundedSettings 1 999 "Dex"))

skillsForm :: Player -> PartialForm (Player -> Player)
skillsForm p = (flip renderTable) mempty $
  (lensField Player.fightingSkill p $ areq intField (boundedSettings 0 27 "Fighting"))
  <.> (lensField Player.macesSkill p $ areq intField (boundedSettings 0 27 "Maces & Flails"))
  <.> (lensField Player.armourSkill p $ areq intField (boundedSettings 0 27 "Armour"))
  <.> (lensField Player.shieldSkill p $ areq intField (boundedSettings 0 27 "Shields"))

equipmentForm :: Player -> PartialForm (Player -> Player)
equipmentForm p = do
  bodyArmourField <- lift $ selectDataField CrawlData.armour
  weaponField <- lift $ selectDataField CrawlData.weapons
  shieldField <- lift $ selectDataField CrawlData.shields
  (flip renderTable) mempty $
    (lensField Player.weapon p $ areq weaponField "Weapon")
    <.> (lensField Player.armour p $ areq bodyArmourField "Armour")
    <.> (lensField Player.shield p $ areq shieldField "Shield")

playerForm :: Player -> PartialForm Player
playerForm defPlayer = do
  (basicsRes, basicsView) <- basicsForm defPlayer
  (skillsRes, skillsView) <- skillsForm defPlayer
  (equipRes, equipView) <- equipmentForm defPlayer

  let widget = [whamlet|
<section>
  <.section_container>
    <.section_header>Character
    <.section_body>
      <.subsection>
        <.subsection_header>Basics
        <.subsection_body>
          <table>
            ^{basicsView}
      <.subsection>
        <.subsection_header>Skills
        <.subsection_body>
          <table>
            ^{skillsView}
      <.subsection>
        <.subsection_header>Equipment
        <.subsection_body>
          <table>
            ^{equipView}
|]
  let result = basicsRes <.> skillsRes <.> equipRes <*> pure def
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
