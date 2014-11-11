-- | Provides a conduit from XML to entries.
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | A module that parses the XML in a very raw way. These types
-- are not intended to be used as anything but intermediate values.
--
-- For example, we do not store anything in maps, but instead in
-- raw lists which are similar to association lists. This
-- preserves the original order of the input.
module Data.JMDict.XML.Parser
    (
    Entry
  , entryUniqueId
  , entryKanjiElements
  , entryReadingElements
  , entrySenses

  , KanjiElement
  , kanjiInfo
  , kanjiPriority

  , ReadingElement
  , readingPhrase
  , readingNoKanji
  , readingRestrictKanji
  , readingInfo
  , readingPriority

  , Sense
  , senseRestrictKanji
  , senseRestrictReading
  , sensePartOfSpeech
  , senseRelated
  , senseAntonym
  , senseFields
  , senseMisc
  , senseInfo
  , senseSources
  , senseDialects
  , senseGlosses

  , LanguageSource
  , sourceOrigin
  , sourceLanguage
  , sourceFull
  , sourceWaseieigo
  )
where

import           Control.Applicative hiding (many)
import           Control.Monad
import           Control.Monad.Catch (MonadThrow)
import           Control.Lens
import           Data.Conduit
import           Data.Default
import           Data.Maybe
import qualified Data.Text as T
import           Data.XML.Types
import qualified Text.XML.Stream.Parse as X

data Entry = Entry {
      _entryUniqueId :: Int
    , _entryKanjiElements :: [KanjiElement]
    , _entryReadingElements :: [ReadingElement] -- ^ Guaranteed non-empty.
    , _entrySenses :: [Sense]
    }
    deriving (Show, Read)

data KanjiElement = KanjiElement {
      _kanjiPhrase :: T.Text
    , _kanjiInfo :: [T.Text] -- ^ TODO figure out all the possible values
    , _kanjiPriority :: [T.Text]
    }
    deriving (Show, Read)

data ReadingElement = ReadingElement {
      _readingPhrase :: T.Text
    , _readingNoKanji :: Bool -- ^ If true, denotes an irregularity
    , _readingRestrictKanji :: [T.Text] -- ^ If null, denotes that it applies to all
    , _readingInfo :: [T.Text]
    , _readingPriority :: [T.Text]
    }
    deriving (Show, Read)

data Sense = Sense {
      _senseRestrictKanji :: [T.Text] -- ^ If null, unrestricted
    , _senseRestrictReading :: [T.Text] -- ^ If null, unrestricted
    , _sensePartOfSpeech :: [T.Text]
    , _senseRelated :: [T.Text]
    , _senseAntonym :: [T.Text]
    , _senseFields :: [T.Text]
    , _senseMisc :: [T.Text]
    , _senseInfo :: [T.Text]
    , _senseSources :: [LanguageSource]
    , _senseDialects :: [T.Text]
    , _senseGlosses :: [Gloss]
    }
    deriving (Show, Read)

data LanguageSource = LanguageSource {
      _sourceOrigin :: T.Text -- ^ Origin word
    , _sourceLanguage :: T.Text
    , _sourceFull :: Bool -- ^ Default True
    , _sourceWaseieigo :: Bool -- ^ Default False
    }
    deriving (Show, Read)

-- | NB: Doesn't support <pri>, since its not used.
data Gloss = Gloss {
      _glossDefinition :: T.Text
    , _glossLanguage :: T.Text
    , _glossGender :: Maybe T.Text -- ^ Unused?
    }
    deriving (Show, Read)

makeLenses ''Entry
makeLenses ''KanjiElement
makeLenses ''ReadingElement
makeLenses ''Sense
makeLenses ''LanguageSource
makeLenses ''Gloss

-- | It would be nice if this was in Text.XML.Stream.Parse. I should
-- try to submit a patch for that or something.
many1 :: MonadThrow m => String -> Consumer Event m (Maybe a) -> Consumer Event m [a]
many1 s c = (:) <$> X.force s c <*> X.many c

hasAttr :: Name -> X.AttrParser Bool
hasAttr name = fmap isJust $ X.optionalAttr name

defaultAttr :: Name -> T.Text -> X.AttrParser T.Text
defaultAttr name de = X.optionalAttr name >>= \case
    Nothing -> return de
    Just re -> return re

xmlNamespace :: T.Text
xmlNamespace = "http://www.w3.org/XML/1998/namespace"

parseJMDict :: MonadThrow m => ConduitM Event Entry m ()
parseJMDict = void . X.tagNoAttr "JMdict" $ yieldWhileJust parseEntry

-- | Thanks to Michael Snoyman for this on StackOverflow.
yieldWhileJust :: Monad m => ConduitM a b m (Maybe b) -> Conduit a m b
yieldWhileJust consumer = loop
    where loop = do
            mx <- consumer
            case mx of
                Nothing -> return ()
                Just x -> yield x >> loop

parseEntry :: MonadThrow m => Consumer Event m (Maybe Entry)
parseEntry = X.tagNoAttr "entry" $ Entry
    <$> X.force "<ent_seq> required" parseEntryId
    <*> X.many parseKanjiElement
    <*> many1 "at least one <r_ele> required" parseReadingElement
    <*> X.many parseSense

-- | Parses the unique entry ID for a specific entry.
-- Corresponds to the <ent_seq>id</ent_seq> pattern.
parseEntryId :: MonadThrow m => Consumer Event m (Maybe Int)
parseEntryId = X.tagNoAttr "ent_seq" $ read . T.unpack <$> X.content

parseKanjiElement :: MonadThrow m => Consumer Event m (Maybe KanjiElement)
parseKanjiElement = X.tagNoAttr "k_ele" $ KanjiElement
    <$> X.force "<keb> required" (X.tagNoAttr "keb" X.content)
    <*> X.many (X.tagNoAttr "ke_inf" X.content)
    <*> X.many (X.tagNoAttr "ke_pri" X.content)

parseReadingElement :: MonadThrow m => Consumer Event m (Maybe ReadingElement)
parseReadingElement = X.tagNoAttr "r_ele" $ ReadingElement
    <$> X.force "<reb> required" (X.tagNoAttr "reb" X.content)
    <*> fmap isNothing (X.tagNoAttr "re_nokanji" X.content)
    <*> X.many (X.tagNoAttr "re_restr" X.content)
    <*> X.many (X.tagNoAttr "re_inf" X.content)
    <*> X.many (X.tagNoAttr "re_pri" X.content)

parseSense :: MonadThrow m => Consumer Event m (Maybe Sense)
parseSense = X.tagNoAttr "sense" $ Sense
    <$> X.many (X.tagNoAttr "stagk" X.content)
    <*> X.many (X.tagNoAttr "stagr" X.content)
    <*> X.many (X.tagNoAttr "pos" X.content)
    <*> X.many (X.tagNoAttr "xref" X.content)
    <*> X.many (X.tagNoAttr "ant" X.content)
    <*> X.many (X.tagNoAttr "field" X.content)
    <*> X.many (X.tagNoAttr "misc" X.content)
    <*> X.many (X.tagNoAttr "s_inf" X.content)
    <*> X.many parseLanguageSource
    <*> X.many (X.tagNoAttr "dial" X.content)
    <*> X.many parseGloss

parseLanguageSource :: MonadThrow m => Consumer Event m (Maybe LanguageSource)
parseLanguageSource = X.tagName "lsource" parseLanguageSourceAttr $
    \(lang, ty, was) -> LanguageSource
        <$> X.content
        <*> return lang
        <*> return ty
        <*> return was

parseLanguageSourceAttr :: X.AttrParser (T.Text, Bool, Bool)
parseLanguageSourceAttr = (,,) 
    <$> defaultAttr (Name "lang" (Just xmlNamespace) Nothing) "eng"
    <*> fmap not (hasAttr "ls_type")
    <*> hasAttr "ls_wasei"

parseGloss :: MonadThrow m => Consumer Event m (Maybe Gloss)
parseGloss = X.tagName "gloss" parseGlossAttr $ \(lang, gend) -> Gloss
    <$> X.content
    <*> return lang
    <*> return gend

parseGlossAttr :: X.AttrParser (T.Text, Maybe T.Text)
parseGlossAttr = (,)
    <$> defaultAttr (Name "lang" (Just xmlNamespace) Nothing) "eng"
    <*> X.optionalAttr "g_gend"
