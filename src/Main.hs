--------------------------------------------------------------------------------
module Main
    ( main
    ) where


--------------------------------------------------------------------------------
import           Data.List          (intersperse)
import           Data.Monoid        ((<>))
import qualified Data.Text.IO       as T
import qualified Text.Pandoc        as Pandoc
import           Text.Pandoc.Pretty (($$), (<+>))
import qualified Text.Pandoc.Pretty as PP


--------------------------------------------------------------------------------
main :: IO ()
main = do
    contents <- T.getContents
    errOrDoc <- Pandoc.runIO $ Pandoc.readMarkdown Pandoc.def contents
    case errOrDoc of
        Left err  -> fail (show err)
        Right doc -> putStrLn $ PP.render Nothing $ ppPandoc doc


--------------------------------------------------------------------------------
ppPandoc :: Pandoc.Pandoc -> PP.Doc
ppPandoc (Pandoc.Pandoc _ blocks) = ppBlocks blocks


--------------------------------------------------------------------------------
ppBlocks :: [Pandoc.Block] -> PP.Doc
ppBlocks = mconcat . intersperse PP.blankline . map ppBlock


--------------------------------------------------------------------------------
ppNestedBlocks :: [Pandoc.Block] -> PP.Doc
ppNestedBlocks = PP.vcat . map ppBlock


--------------------------------------------------------------------------------
ppBlock :: Pandoc.Block -> PP.Doc
ppBlock (Pandoc.Para xs) = ppInlines xs

ppBlock (Pandoc.CodeBlock _ code) =
    PP.text "{noformat}" $$ PP.text code $$ PP.text "{noformat}"

ppBlock (Pandoc.OrderedList _ xss) =
    PP.vcat (map (PP.text "#" <+>) $ map ppNestedBlocks xss)

ppBlock (Pandoc.BulletList xss) =
    PP.vcat (map (PP.text "*" <+>) $ map ppNestedBlocks xss)

ppBlock (Pandoc.Header n _ xs) =
    PP.text ("h" ++ show n ++ ".") <+> ppInlines xs

ppBlock (Pandoc.BlockQuote xs) =
    PP.text "bq." <+> ppNestedBlocks xs

ppBlock Pandoc.HorizontalRule = PP.text "----"

ppBlock Pandoc.Null = mempty

-- Soft unsupported
ppBlock (Pandoc.Plain xs) = ppInlines xs
ppBlock (Pandoc.Div _ xs) = ppNestedBlocks xs

-- Hard unsupported
ppBlock b@(Pandoc.RawBlock _ _)     = unsupported b
ppBlock b@(Pandoc.Table _ _ _ _ _)  = unsupported b
ppBlock b@(Pandoc.LineBlock _)      = unsupported b
ppBlock b@(Pandoc.DefinitionList _) = unsupported b


--------------------------------------------------------------------------------
ppInlines :: [Pandoc.Inline] -> PP.Doc
ppInlines = mconcat . map ppInline


--------------------------------------------------------------------------------
ppInline :: Pandoc.Inline -> PP.Doc
ppInline (Pandoc.Str x)        = PP.text x
ppInline Pandoc.Space          = PP.space
ppInline Pandoc.SoftBreak      = PP.space
ppInline Pandoc.LineBreak      = PP.cr  -- Not really right.

ppInline (Pandoc.Strong xs)      = PP.text "*" <> ppInlines xs <> PP.text "*"
ppInline (Pandoc.Emph xs)        = PP.text "_" <> ppInlines xs <> PP.text "_"
ppInline (Pandoc.Strikeout xs)   = PP.text "-" <> ppInlines xs <> PP.text "-"
ppInline (Pandoc.Subscript xs)   = PP.text "~" <> ppInlines xs <> PP.text "~"
ppInline (Pandoc.Superscript xs) = PP.text "^" <> ppInlines xs <> PP.text "^"

ppInline (Pandoc.Code _ t) =
    PP.text " {{" <> PP.text (escapeInlineCode t) <> PP.text "}} "

ppInline (Pandoc.Link _ xs (tgt, _)) =
    PP.text "[" <> ppInlines xs <> PP.text "|" <> PP.text tgt <> PP.text "]"

ppInline (Pandoc.Image _ _xs (tgt, _)) =
    PP.text "!" <> PP.text tgt <> PP.text "!"

-- Soft unsupported
ppInline (Pandoc.SmallCaps xs)   = ppInlines xs
ppInline (Pandoc.Quoted    _ xs) = ppInlines xs
ppInline (Pandoc.Cite      _ xs) = ppInlines xs
ppInline (Pandoc.Math      _ x)  = PP.text x
ppInline (Pandoc.Span      _ xs) = ppInlines xs

-- Hard unsupported
ppInline i@(Pandoc.RawInline _ _) = unsupported i
ppInline i@(Pandoc.Note _)        = unsupported i


--------------------------------------------------------------------------------
escapeInlineCode :: String -> String
escapeInlineCode = concatMap $ \c -> case c of
    '('  -> "\\("
    ')'  -> "\\)"
    '{'  -> "\\{"
    '}'  -> "\\}"
    '['  -> "\\["
    ']'  -> "\\]"
    '\\' -> "\\\\"
    _    -> [c]


--------------------------------------------------------------------------------
unsupported :: Show markup => markup -> a
unsupported x = error $ "Unsupported: " ++ show x
