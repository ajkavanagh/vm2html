{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}


module LibPandoc
    ( convertVimWikiLinks
    ) where



-- for Pandoc processing
import qualified Data.Text              as T
import qualified Text.Pandoc            as TP
import qualified Text.Pandoc.Builder    as B
import qualified Text.Pandoc.Definition as TPD
import qualified Text.Pandoc.Walk       as TPW



-- to handle parsing out vimwiki [[text|link]] style links into Links
import           Text.Parsec            (parse, try)
import           Text.Parsec.Char       (anyChar, char)
import           Text.Parsec.Combinator (many1, manyTill, notFollowedBy)
import           Text.Parsec.Text       (Parser)

import           Control.Applicative    ((<|>))


-- | Process Block level items in the Pandoc AST to look for Str/Space segments
-- that might contain vimwiki links.  We have to process the Block level items,
-- as we need to only to Para, Plain, DefinitionList (for the Inlines), Header
-- and Table [Inline] blocks.  This is so we ONLY process the 'str' segements
-- that make sense.
--
-- The [Inline] segments are further complicated as we are interested in
-- Str,Space,Str,Space, segments; but also have to look inside the other
-- Inline section's [Inline] blocks for the same.  i.e. it's recursive.
--
-- So it's quite a bit of construction, de-construction and parsing to convert
-- them into links.  This will need a few tests when it's turned into a library.

-- NOTE: this is using the version of Pandoc that runs with Strings.  When
-- updated to Pandoc version that uses Text the T.unpack and T.pack will be
-- removed.


convertVimWikiLinks :: TP.Pandoc -> TP.Pandoc
convertVimWikiLinks = TPW.walk processBlock


processBlock :: TP.Block -> TP.Block
processBlock (TP.Plain ls) = TP.Plain $ processInlines ls
processBlock (TP.Para ls) = TP.Para $ processInlines ls
processBlock (TP.LineBlock lls) = TP.LineBlock $ map processInlines lls
processBlock (TP.DefinitionList dl) = TP.DefinitionList $ map doDefn dl
  where
      doDefn :: ([TP.Inline], [[TP.Block]]) -> ([TP.Inline], [[TP.Block]])
      doDefn (ls, bls) = (processInlines ls, bls)
processBlock (TP.Header int attr ls) = TP.Header int attr $ processInlines ls
processBlock (TP.Table ls a b c d) = TP.Table (processInlines ls) a b c d
-- finally just ignore the rest
processBlock x = x


-- search through the inlines for Str,Space ... sequences, join them, parse
-- them, convert them back to inlines.  i.e. we are trying to find the links in
-- amongst all the processed AST whilst keeping the AST intact.
processInlines :: [TP.Inline] -> [TP.Inline]
processInlines [] = []
processInlines ys@(x:xs) = case x of
    (TPD.Str text)       -> buildStrSequence [] ys
    (TPD.Emph ls)        -> processEmbedded1 TPD.Emph ls xs
    (TPD.Strong ls)      -> processEmbedded1 TPD.Strong ls xs
    (TPD.Strikeout ls)   -> processEmbedded1 TPD.Strikeout ls xs
    (TPD.Superscript ls) -> processEmbedded1 TPD.Superscript ls xs
    (TPD.Subscript ls)   -> processEmbedded1 TPD.Subscript ls xs
    (TPD.SmallCaps ls)   -> processEmbedded1 TPD.SmallCaps ls xs
    (TPD.Quoted qt ls)   -> processEmbedded1 (TPD.Quoted qt) ls xs
    (TPD.Cite cite ls)   -> processEmbedded1 (TPD.Cite cite) ls xs
    -- everything else is just passed through and ignored.
    _                    -> x : processInlines xs


-- | process the inlines inside the Inline constructor, and then continue with the
-- rest.
processEmbedded1 :: ([TP.Inline] -> TP.Inline) -> [TP.Inline] -> [TP.Inline] -> [TP.Inline]
processEmbedded1 inlineCons ls ys = (inlineCons $ processInlines ls) : processInlines ys


-- | build up a Str,Space,Str... sequence and then call the processStrSequence on
-- it.  The first non Str or Space is then handed back to processInLines
buildStrSequence :: [TP.Inline] -> [TP.Inline] -> [TP.Inline]
buildStrSequence xs (y@(TPD.Str _):ys) = buildStrSequence (xs ++ [y]) ys
buildStrSequence xs (y@TPD.Space:ys) = buildStrSequence (xs ++ [y]) ys
buildStrSequence xs ys = (processStrSequence xs) ++ processInlines ys


-- | now build a string out of the Str+Space sequence and parse it, reconstruct
--it and return it as a sequence of inlines
processStrSequence :: [TP.Inline] -> [TP.Inline]
processStrSequence xs = concatMap toInline $ parseToLinkPart $ T.concat $ map toStr xs
  where
      toStr :: TP.Inline -> T.Text
      toStr (TPD.Str txt) = T.pack txt
      toStr TPD.Space     = " "


-- | convert LinkPart pieces back into a sequence of Inline elements.  Uses the
-- Text.Pandoc.Builder 'text' function to convert text sequences.
toInline :: LinkPart -> [TP.Inline]
toInline (RegularText text) = B.toList $ B.text $ T.unpack text
toInline (Link link desc) =
    [TP.Link TPD.nullAttr (B.toList $ B.text $ T.unpack desc) (T.unpack link, T.unpack desc)]


-- | Parse a string and find any vimwiki style links as LinkPart

data LinkPart = RegularText T.Text
              | Link T.Text T.Text
              deriving Show


parseToLinkPart :: T.Text -> [LinkPart]
parseToLinkPart txt =
    case parse matchManyWikiLinks "" txt of
        Right lps -> concat lps
        Left _    -> [RegularText txt]


matchStartLink :: Parser ()
matchStartLink = char '[' >> char '[' >> pure ()


matchEndLink :: Parser ()
matchEndLink = char ']' >> char ']' >> pure ()


matchSepInLink :: Parser ()
matchSepInLink = char '|' >> pure ()


matchAllChars :: Parser [LinkPart]
matchAllChars = do
    s <- many1 anyChar
    pure [RegularText $ T.pack s]


matchPureLinkText :: Parser Char
matchPureLinkText = do
    c <- anyChar
    notFollowedBy (char '|')
    pure c

matchLink :: Parser [LinkPart]
matchLink = do
    before <- manyTill anyChar (try matchStartLink)
    linkText <- manyTill matchPureLinkText (try matchEndLink)
    pure [RegularText (T.pack before), Link (T.pack (linkText <> ".html")) (T.pack linkText)]

matchLink2 :: Parser [LinkPart]
matchLink2 = do
    before <- manyTill anyChar (try matchStartLink)
    linkText <- manyTill anyChar (try matchSepInLink)
    desc <- manyTill anyChar (try matchEndLink)
    pure [RegularText (T.pack before),
          Link (T.pack (linkText <> ".html")) (T.pack desc)]


matchWikiLink :: Parser [LinkPart]
matchWikiLink = try matchLink <|> try matchLink2 <|> matchAllChars


matchManyWikiLinks :: Parser [[LinkPart]]
matchManyWikiLinks = many1 matchWikiLink
