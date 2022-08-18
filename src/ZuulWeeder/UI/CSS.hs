-- |
-- Module      : ZuulWeeder.UI.CSS
-- Description : CSS utility
-- Copyright   : (c) Red Hat, 2022
-- License     : Apache-2.0
module ZuulWeeder.UI.CSS
  ( with',
    mkIcon,
    title,
    spinner,
    hxNavLink,
    hxTrigger,
    hxPost,
    hxTarget,
  )
where

import Lucid
import Lucid.Base (makeAttribute)
import ZuulWeeder.Prelude

with' :: With a => a -> Text -> a
with' x n = with x [class_ n]

-- After adding css class, run `nix run .#tailwind` to update the tailwind.css file. Then hard refresh the web page.

mkIcon :: Maybe Text -> Text -> Html ()
mkIcon iconTitle name =
  with i_ ([class_ ("pr-1 font-bold align-bottom " <> name <> maybe "" (mappend " color-") iconTitle)] <> titleAttr) mempty
  where
    titleAttr = maybeToList (title_ <$> iconTitle)

title :: Text -> Html ()
title = with' h2_ "font-bold" . toHtml

spinner :: Html ()
spinner = with span_ [class_ "htmx-indicator font-semibold text-white", id_ "spinner"] "◌"

hxNavLink :: [Attribute] -> Text -> Maybe Text -> Html () -> Html ()
hxNavLink xs url extraClass =
  with
    a_
    ( xs
        <> [ hxGet url,
             hxPushUrl,
             hxIndicator "#spinner",
             hxTarget "#main",
             class_ ("cursor-pointer hover:font-semibold" <> maybe "" (mappend " ") extraClass),
             href_ url
           ]
    )

hxTrigger, hxTarget, hxGet, hxPost, hxIndicator :: Text -> Attribute
hxTrigger = makeAttribute "hx-trigger"
hxTarget = makeAttribute "hx-target"
hxGet = makeAttribute "hx-get"
hxPost = makeAttribute "hx-post"
hxIndicator = makeAttribute "hx-indicator"

hxPushUrl :: Attribute
hxPushUrl = makeAttribute "hx-push-url" "true"
