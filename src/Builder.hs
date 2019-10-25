module Builder
  ( applyTemplates
  ) where

import           Files     (Content)
import           Parser
import qualified Data.Text           as T
-- type Content = Maybe (Data.Text.Text, Tags, DatePublished, DateOccured)
-- FilePath isn't really filepath. it's relative: sitename\partition\article.md. Will come as [String] of breadcrumbs, empty if Main page.

-- главная: хедер главной (один из разделов просто выбран главным), тулово, футер (path = [])
-- другой раздел (статьи, например): хедер раздела, тулово, футер (path = [part])
-- внутри раздела (например, статья): хедер конечной, тулово, футер (path = [part, article])
-- 
-- хедеры всех трёх типов у меня одинаковые. футеры тоже. но оставим возможность делать их разными.
-- если в тулове раздела есть список -- рендерим его картинками-ссылками.

applyTemplates :: (FilePath, Content) -> String
applyTemplates (path, Just (c, t, p, o)) = T.unpack c
  -- where cm = T.unpack . T.append c $ T.pack "azaza" -- $ concat $ splitPath path