{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-name-shadowing -fno-warn-unused-matches #-}
{-# LANGUAGE RankNTypes #-}

module Handler.Crawl where

import Import hiding ((<.>))
import Control.Lens hiding (lensField, (<.>), (??))
import Numeric.Probability.Distribution ((??))
import qualified Data.Text as Text

import Crawl.Stats.Lens.Player (Player)
import qualified Crawl.Stats.Lens.Player as Player
import Crawl.Stats.Monster (Monster)
import qualified Crawl.Stats.Data as CrawlData
import qualified Crawl.Stats.Named as Named
import Crawl.Stats.Attack (Attack)
import qualified Crawl.Stats.Attack as Attack
import qualified Crawl.Stats.Dice as Dice

boundedSettings :: Int -> Int -> FieldSettings m -> FieldSettings m
boundedSettings min max fs = fs {fsAttrs = [("min", Text.pack $ show min), ("max", Text.pack $ show max)] ++ fsAttrs fs}

selectDataField :: (CrawlData.Loaded a, Eq a, RenderMessage App Text) => Handler (Field Handler a)
selectDataField = do
  app <- getYesod
  let namedList = CrawlData.list $ crawlData app
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
  bodyArmourField <- lift $ selectDataField
  weaponField <- lift $ selectDataField
  shieldField <- lift $ selectDataField
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

attackReport :: (Attack -> String) -> Player -> Monster -> Widget
attackReport f player monster = [whamlet|
  <.combat_outcome_player>
    #{playerData}
  <.combat_outcome_monster>
    #{monsterData}
  |]
  where playerData = f $ Attack.PM player monster
        monsterData = f $ Attack.MP monster player

confidenceReport :: (Show a, Eq a, Ord a) => (Attack -> Dice.Dist a) -> Dice.Probability -> Player -> Monster -> Widget
confidenceReport f confidence = attackReport confidenceR
  where confidenceR = Dice.formatInterval . Dice.confidenceInterval confidence . Dice.cdt . Dice.toRandomVar . f

rangeReport :: (Show a, Eq a, Ord a) => (Attack -> Dice.Dist a) -> Player -> Monster -> Widget
rangeReport f = attackReport range
  where range = Dice.formatInterval . Dice.range . Dice.toRandomVar . f

chanceReport :: (Attack -> Dice.Dist Bool) -> Player -> Monster -> Widget
chanceReport f = attackReport chance
  where chance attack = Dice.formatPercent (id ?? (f attack))

hitChanceReport :: Player -> Monster -> Widget
hitChanceReport = chanceReport Attack.testHit

blockChanceReport :: Player -> Monster -> Widget
blockChanceReport = chanceReport Attack.block

turnsToDeathReport :: Dice.Probability -> Player -> Monster -> Widget
turnsToDeathReport confidence = attackReport turnsToDeath
  where turnsToDeath = Dice.formatInterval . Dice.confidenceInterval confidence . zip ((/10) <$> [0..]) . Attack.deadAfter

defenderHpReport :: Player -> Monster -> Widget
defenderHpReport = rangeReport Attack.defenderMaxHp

defenderAcReport :: Player -> Monster -> Widget
defenderAcReport = attackReport defenderAc
  where defenderAc = show . Attack.ac

baseDamageReport :: Dice.Probability -> Player -> Monster -> Widget
baseDamageReport = confidenceReport Attack.damage

attackSpeedReport :: Player -> Monster -> Widget
attackSpeedReport = rangeReport $ map (\a -> fromIntegral a / 10) . Attack.weaponSpeed

unwrapResult :: Monoid m => FormResult m -> m
unwrapResult (FormSuccess a) = a
unwrapResult _ = mempty

combatForm :: FormResult Player -> MForm Handler Widget
combatForm player = do
  monsterField <- lift $ selectDataField
  monsterFields <- (sequence $ replicate 5 $ mreq monsterField "ignored" Nothing) :: MForm Handler [(FormResult Monster, FieldView App)]
  let reportRows = [("Turns to Death (90% confidence)", turnsToDeathReport 0.9),
                    ("Defender HP", defenderHpReport),
                    ("Base Damage (90% confidence)", baseDamageReport 0.9),
                    ("Hit Chance" :: Text, hitChanceReport),
                    ("Block Chance", blockChanceReport),
                    ("Defender AC", defenderAcReport),
                    ("Attack Speed", attackSpeedReport)]
  return $ [whamlet|
    <section>
      <.section_container>
        <.section_header>Combat Outcomes
        <.section_body>
          <table>
            <tr>
              <th>
              $forall (_, monsterView) <- monsterFields
                <th>
                  ^{fvInput monsterView}
            $forall (title, reportFunc) <- reportRows
              <tr>
                <th>
                  #{title}
                $forall (monsterRes, _) <- monsterFields
                  <td.combat_outcome>
                    ^{unwrapResult $ liftA2 reportFunc player monsterRes}
    |]

crawlForm :: Player -> Form ()
crawlForm player extra = do
  (player', playerWidget) <- playerForm player
  combatWidget <- combatForm player'
  let widget = [whamlet|
    ^{extra}
    ^{playerWidget}
    ^{combatWidget}
  |]
  return (FormSuccess (), widget)

handleCrawlR :: Handler Html
handleCrawlR = do
  ((res, crawlWidget), crawlEnctype) <- runFormPost $ crawlForm def
  defaultLayout $(widgetFile "crawl")
