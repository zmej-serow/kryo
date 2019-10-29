module Builder
  ( applyTemplates
  ) where

import Files     (Content)
import Parser
import Data.Text
-- type Content = Maybe (Data.Text.Text, Tags, DatePublished, DateOccured)
-- FilePath isn't really filepath. it's relative: sitename\partition\article.md. Will come as [String] of breadcrumbs, empty if Main page.

-- главная: хедер главной (один из разделов просто выбран главным), тулово, футер (path = [])
-- другой раздел (статьи, например): хедер раздела, тулово, футер (path = [section])
-- внутри раздела (например, статья): хедер конечной, тулово, футер (path = [section, article])
-- 
-- хедеры всех трёх типов у меня одинаковые. футеры тоже. но оставим возможность делать их разными.
-- если в тулове раздела есть список -- рендерим его картинками-ссылками.

applyTemplates :: (FilePath, Content) -> Text
applyTemplates (f, c) = applyTemplates' (splitPath f, c)

applyTemplates' :: (Breadcrumbs, Content) -> Text
applyTemplates' ([], c)   = mainTemplate c
applyTemplates' (path, c) = subTemplates path c

mainTemplate :: Content -> Text
mainTemplate (Just (c, t, p, o)) = c

subTemplates :: Breadcrumbs -> Content -> Text
subTemplates [s] (Just (c, t, p, o)) = append c $ pack $ "section " ++ s ++" here!"
subTemplates xs  (Just (c, t, p, o)) = append c $ pack "article here!"
