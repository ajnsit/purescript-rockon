module RockOn.DOM where

import Prelude hiding (div,map,sub)

import Concur.Core (Widget)
import Concur.Core (display) as R
import Concur.React.Props (Props)
import RockOn (HTML, HUI, el', elLeaf, liftHTMLWidget)
import Control.MultiAlternative (class MultiAlternative)
import Control.ShiftMap (class ShiftMap)
import React.DOM as D

-- Wrappers for all DOM elements from purescript-react
-- TODO: Generate these mechanically somehow

type El = forall m a. MultiAlternative m => ShiftMap (Widget HUI) m => Array (Props a) -> Array (m a) -> m a
type El' = forall m a. MultiAlternative m => ShiftMap (Widget HUI) m => Array (m a) -> m a
type ElLeaf = forall a. Array (Props a) -> Widget HUI a

display :: forall a. HTML -> Widget HUI a
display = liftHTMLWidget <<< R.display

-- These adapter functions are needed because we changed the signature of `el` etc.
--   in RockOn, compared to in ordinary Concur

adapt1 :: forall a b. (a -> b) -> a -> Array b
adapt1 = (<<<) pure

adapt2 :: forall a b c. (a -> b -> c) -> a -> b -> Array c
adapt2 = (<<<) ((<<<) pure)

-------------------------------------------------------------------------------------------------------------------

text :: forall a. String -> Widget HUI a
text = display <<< pure <<< D.text

int :: forall a. Int -> Widget HUI a
int = display <<< pure <<< D.int

number :: forall a. Number -> Widget HUI a
number = display <<< pure <<< D.number

a :: El
a = el' (adapt2 D.a)

a' :: El'
a' = a []

abbr :: El
abbr = el' (adapt2 D.abbr)

abbr' :: El'
abbr' = abbr []

address :: El
address = el' (adapt2 D.address)

address' :: El'
address' = address []

area :: ElLeaf
area = elLeaf (adapt1 D.area)

area' :: forall a. Widget HUI a
area' = area []

article :: El
article = el' (adapt2 D.article)

article' :: El'
article' = article []

aside :: El
aside = el' (adapt2 D.aside)

aside' :: El'
aside' = aside []

audio :: El
audio = el' (adapt2 D.audio)

audio' :: El'
audio' = audio []

b :: El
b = el' (adapt2 D.b)

b' :: El'
b' = b []

base :: ElLeaf
base = elLeaf (adapt1 D.base)

base' :: forall a. Widget HUI a
base' = base []

bdi :: El
bdi = el' (adapt2 D.bdi)

bdi' :: El'
bdi' = bdi []

bdo :: El
bdo = el' (adapt2 D.bdo)

bdo' :: El'
bdo' = bdo []

big :: El
big = el' (adapt2 D.big)

big' :: El'
big' = big []

blockquote :: El
blockquote = el' (adapt2 D.blockquote)

blockquote' :: El'
blockquote' = blockquote []

body :: El
body = el' (adapt2 D.body)

body' :: El'
body' = body []

br :: ElLeaf
br = elLeaf (adapt1 D.br)

br' :: forall a. Widget HUI a
br' = br []

button :: El
button = el' (adapt2 D.button)

button' :: El'
button' = button []

canvas :: El
canvas = el' (adapt2 D.canvas)

canvas' :: El'
canvas' = canvas []

caption :: El
caption = el' (adapt2 D.caption)

caption' :: El'
caption' = caption []

cite :: El
cite = el' (adapt2 D.cite)

cite' :: El'
cite' = cite []

code :: El
code = el' (adapt2 D.code)

code' :: El'
code' = code []

col :: ElLeaf
col = elLeaf (adapt1 D.col)

col' :: forall a. Widget HUI a
col' = col []

colgroup :: El
colgroup = el' (adapt2 D.colgroup)

colgroup' :: El'
colgroup' = colgroup []

_data :: El
_data = el' (adapt2 D._data)

_data' :: El'
_data' = _data []

datalist :: El
datalist = el' (adapt2 D.datalist)

datalist' :: El'
datalist' = datalist []

dd :: El
dd = el' (adapt2 D.dd)

dd' :: El'
dd' = dd []

del :: El
del = el' (adapt2 D.del)

del' :: El'
del' = del []

details :: El
details = el' (adapt2 D.details)

details' :: El'
details' = details []

dfn :: El
dfn = el' (adapt2 D.dfn)

dfn' :: El'
dfn' = dfn []

dialog :: El
dialog = el' (adapt2 D.dialog)

dialog' :: El'
dialog' = dialog []

div :: El
div = el' (adapt2 D.div)

div' :: El'
div' = div []

dl :: El
dl = el' (adapt2 D.dl)

dl' :: El'
dl' = dl []

dt :: El
dt = el' (adapt2 D.dt)

dt' :: El'
dt' = dt []

em :: El
em = el' (adapt2 D.em)

em' :: El'
em' = em []

embed :: ElLeaf
embed = elLeaf (adapt1 D.embed)

embed' :: forall a. Widget HUI a
embed' = embed []

fieldset :: El
fieldset = el' (adapt2 D.fieldset)

fieldset' :: El'
fieldset' = fieldset []

figcaption :: El
figcaption = el' (adapt2 D.figcaption)

figcaption' :: El'
figcaption' = figcaption []

figure :: El
figure = el' (adapt2 D.figure)

figure' :: El'
figure' = figure []

footer :: El
footer = el' (adapt2 D.footer)

footer' :: El'
footer' = footer []

form :: El
form = el' (adapt2 D.form)

form' :: El'
form' = form []

h1 :: El
h1 = el' (adapt2 D.h1)

h1' :: El'
h1' = h1 []

h2 :: El
h2 = el' (adapt2 D.h2)

h2' :: El'
h2' = h2 []

h3 :: El
h3 = el' (adapt2 D.h3)

h3' :: El'
h3' = h3 []

h4 :: El
h4 = el' (adapt2 D.h4)

h4' :: El'
h4' = h4 []

h5 :: El
h5 = el' (adapt2 D.h5)

h5' :: El'
h5' = h5 []

h6 :: El
h6 = el' (adapt2 D.h6)

h6' :: El'
h6' = h6 []

head :: El
head = el' (adapt2 D.head)

head' :: El'
head' = head []

header :: El
header = el' (adapt2 D.header)

header' :: El'
header' = header []

hr :: ElLeaf
hr = elLeaf (adapt1 D.hr)

hr' :: forall a. Widget HUI a
hr' = hr []

html :: El
html = el' (adapt2 D.html)

html' :: El'
html' = html []

i :: El
i = el' (adapt2 D.i)

i' :: El'
i' = i []

iframe :: El
iframe = el' (adapt2 D.iframe)

iframe' :: El'
iframe' = iframe []

img :: ElLeaf
img = elLeaf (adapt1 D.img)

img' :: forall a. Widget HUI a
img' = img []

input :: ElLeaf
input = elLeaf (adapt1 D.input)

input' :: forall a. Widget HUI a
input' = input []

ins :: El
ins = el' (adapt2 D.ins)

ins' :: El'
ins' = ins []

kbd :: El
kbd = el' (adapt2 D.kbd)

kbd' :: El'
kbd' = kbd []

keygen :: ElLeaf
keygen = elLeaf (adapt1 D.keygen)

keygen' :: forall a. Widget HUI a
keygen' = keygen []

label :: El
label = el' (adapt2 D.label)

label' :: El'
label' = label []

legend :: El
legend = el' (adapt2 D.legend)

legend' :: El'
legend' = legend []

li :: El
li = el' (adapt2 D.li)

li' :: El'
li' = li []

link :: ElLeaf
link = elLeaf (adapt1 D.link)

link' :: forall a. Widget HUI a
link' = link []

main :: El
main = el' (adapt2 D.main)

main' :: El'
main' = main []

map :: El
map = el' (adapt2 D.map)

map' :: El'
map' = map []

mark :: El
mark = el' (adapt2 D.mark)

mark' :: El'
mark' = mark []

menu :: El
menu = el' (adapt2 D.menu)

menu' :: El'
menu' = menu []

menuitem :: ElLeaf
menuitem = elLeaf (adapt1 D.menuitem)

menuitem' :: forall a. Widget HUI a
menuitem' = menuitem []

meta :: ElLeaf
meta = elLeaf (adapt1 D.meta)

meta' :: forall a. Widget HUI a
meta' = meta []

meter :: El
meter = el' (adapt2 D.meter)

meter' :: El'
meter' = meter []

nav :: El
nav = el' (adapt2 D.nav)

nav' :: El'
nav' = nav []

noscript :: El
noscript = el' (adapt2 D.noscript)

noscript' :: El'
noscript' = noscript []

object :: El
object = el' (adapt2 D.object)

object' :: El'
object' = object []

ol :: El
ol = el' (adapt2 D.ol)

ol' :: El'
ol' = ol []

optgroup :: El
optgroup = el' (adapt2 D.optgroup)

optgroup' :: El'
optgroup' = optgroup []

option :: El
option = el' (adapt2 D.option)

option' :: El'
option' = option []

output :: El
output = el' (adapt2 D.output)

output' :: El'
output' = output []

p :: El
p = el' (adapt2 D.p)

p' :: El'
p' = p []

param :: ElLeaf
param = elLeaf (adapt1 D.param)

param' :: forall a. Widget HUI a
param' = param []

picture :: El
picture = el' (adapt2 D.picture)

picture' :: El'
picture' = picture []

pre :: El
pre = el' (adapt2 D.pre)

pre' :: El'
pre' = pre []

progress :: El
progress = el' (adapt2 D.progress)

progress' :: El'
progress' = progress []

q :: El
q = el' (adapt2 D.q)

q' :: El'
q' = q []

rp :: El
rp = el' (adapt2 D.rp)

rp' :: El'
rp' = rp []

rt :: El
rt = el' (adapt2 D.rt)

rt' :: El'
rt' = rt []

ruby :: El
ruby = el' (adapt2 D.ruby)

ruby' :: El'
ruby' = ruby []

s :: El
s = el' (adapt2 D.s)

s' :: El'
s' = s []

samp :: El
samp = el' (adapt2 D.samp)

samp' :: El'
samp' = samp []

script :: El
script = el' (adapt2 D.script)

script' :: El'
script' = script []

section :: El
section = el' (adapt2 D.section)

section' :: El'
section' = section []

select :: El
select = el' (adapt2 D.select)

select' :: El'
select' = select []

small :: El
small = el' (adapt2 D.small)

small' :: El'
small' = small []

source :: ElLeaf
source = elLeaf (adapt1 D.source)

source' :: forall a. Widget HUI a
source' = source []

span :: El
span = el' (adapt2 D.span)

span' :: El'
span' = span []

strong :: El
strong = el' (adapt2 D.strong)

strong' :: El'
strong' = strong []

style :: El
style = el' (adapt2 D.style)

style' :: El'
style' = style []

sub :: El
sub = el' (adapt2 D.sub)

sub' :: El'
sub' = sub []

summary :: El
summary = el' (adapt2 D.summary)

summary' :: El'
summary' = summary []

sup :: El
sup = el' (adapt2 D.sup)

sup' :: El'
sup' = sup []

table :: El
table = el' (adapt2 D.table)

table' :: El'
table' = table []

tbody :: El
tbody = el' (adapt2 D.tbody)

tbody' :: El'
tbody' = tbody []

td :: El
td = el' (adapt2 D.td)

td' :: El'
td' = td []

textarea :: El
textarea = el' (adapt2 D.textarea)

textarea' :: El'
textarea' = textarea []

tfoot :: El
tfoot = el' (adapt2 D.tfoot)

tfoot' :: El'
tfoot' = tfoot []

th :: El
th = el' (adapt2 D.th)

th' :: El'
th' = th []

thead :: El
thead = el' (adapt2 D.thead)

thead' :: El'
thead' = thead []

time :: El
time = el' (adapt2 D.time)

time' :: El'
time' = time []

title :: El
title = el' (adapt2 D.title)

title' :: El'
title' = title []

tr :: El
tr = el' (adapt2 D.tr)

tr' :: El'
tr' = tr []

track :: ElLeaf
track = elLeaf (adapt1 D.track)

track' :: forall a. Widget HUI a
track' = track []

u :: El
u = el' (adapt2 D.u)

u' :: El'
u' = u []

ul :: El
ul = el' (adapt2 D.ul)

ul' :: El'
ul' = ul []

var :: El
var = el' (adapt2 D.var)

var' :: El'
var' = var []

video :: El
video = el' (adapt2 D.video)

video' :: El'
video' = video []

wbr :: ElLeaf
wbr = elLeaf (adapt1 D.wbr)

wbr' :: forall a. Widget HUI a
wbr' = wbr []
