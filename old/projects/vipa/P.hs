module P where

import Data.List
import Data.Char
import Lexicon

data ParseTree a b =  Ep | Leaf a | Branch b [ParseTree a b] 
                   deriving Eq

instance (Show a, Show b) => Show (ParseTree a b) where
  show Ep            = "[]"
  show (Leaf t)      = show t
  show (Branch l ts) = "[." ++ show l  ++ " " 
                            ++ show ts ++ "]"
type Pos = [Int]

tense :: Agreement -> Agreement
tense    = filter (`elem` [Past,Pres,Futu,Prog]) 

-- Copy from FPH
prefix :: Eq a => [a] -> [a] -> Bool
prefix []     ys     = True
prefix (x:xs) []     = False
prefix (x:xs) (y:ys) = (x==y) && prefix xs ys 

pos ::  ParseTree a b -> [Pos]
pos Ep            = [[]]
pos (Leaf _)      = [[]]
pos (Branch _ ts) = [] : [ i:p | (i,t) <- zip [0..] ts, 
                                     p <- pos t ]

subtree :: ParseTree a b -> Pos -> ParseTree a b 
subtree t             []     = t
subtree (Branch _ ts) (i:is) = subtree (ts!!i) is 

subtrees :: ParseTree a b -> [ParseTree a b]
subtrees t = [ subtree t p | p <- pos t ]

type Rel a = [(a,a)]

properdominance :: ParseTree a b -> Rel Pos 
properdominance t = [ (p,q) | p <- pos t, 
                              q <- pos t, 
                              p /= q, 
                              prefix p q ]

dominance :: ParseTree a b -> Rel Pos 
dominance t = [ (p,q) | p <- pos t, 
                        q <- pos t, 
                        prefix p q ]

sisters :: Pos -> Pos -> Bool
sisters [i]    [j]    = i /= j
sisters (i:is) (j:js) = i == j && sisters is js
sisters  _      _     = False 

sisterhood :: ParseTree a b -> Rel Pos 
sisterhood t = [ (p,q) | p <- pos t, 
                         q <- pos t, 
                         sisters p q ]

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

cCommand :: ParseTree a b -> Rel Pos 
cCommand t = (sisterhood t) @@ (dominance t)

branchingPos :: ParseTree a b -> [Pos]
branchingPos t = let ps = pos t in 
  [ p | p <- ps, (p++[0]) `elem` ps, (p++[1]) `elem` ps ]

precede :: Pos -> Pos -> Bool
precede (i:is) (j:js) = i < j || (i == j && precede is js)
precede  _      _     = False

precedence :: ParseTree a b -> Rel Pos
precedence t = [ (p,q) | p <- pos t, 
                         q <- pos t, 
                         precede p q ]

split2 :: [a] -> [([a],[a])]
split2 []     = [([],[])]
split2 (x:xs) = [([],(x:xs))]
             ++ (map (\(ys,zs) -> ((x:ys),zs)) (split2 xs))

splitN :: Int -> [a] -> [[[a]]]
splitN n xs 
  | n <= 1    = error "cannot split"
  | n == 2    = [ [ys,zs] | (ys,zs) <- split2 xs ]
  | otherwise = [ ys:rs   | (ys,zs) <- split2 xs, 
                             rs     <- splitN (n-1) zs ]

gener :: Int -> String -> [String]
gener 0 alphabet = [[]]
gener n alphabet = [ x:xs | x  <- alphabet,
                            xs <- gener (n-1) alphabet ]

gener' :: Int -> String -> [String]
gener' n alphabet = gener   n    alphabet 
                 ++ gener' (n+1) alphabet

generateAll  :: String -> [String]
generateAll alphabet = gener' 0 alphabet

many :: Parser a b -> Parser a [b]
many p = (p <:> many p) <|> (succeed [])

zeroOrOne :: Parser a b -> Parser a [b]
zeroOrOne p = (p <:> (succeed [])) <|> (succeed [])

oneOrMore :: Parser a b -> Parser a [b]
oneOrMore p = (p <:> many p)

type Parser a b = [a] -> [(b,[a])]

type PARSER a b = Parser a (ParseTree a b)

infixr 4 <|>
 
(<|>) :: Parser a b -> Parser a b -> Parser a b 
(p1 <|> p2) xs = p1 xs ++ p2 xs 

infixl 6 <:>

(<:>) :: Parser a b -> Parser a [b] -> Parser a [b]
(p <:> q) xs = [ (r:rs,zs) | (r,ys)  <- p xs, 
                             (rs,zs) <- q ys ]

succeed :: b -> Parser a b 
succeed r xs = [(r,xs)]

instance Show Cat where
  show (Cat "_"  label agr subcatlist) = label ++ show agr
  show (Cat phon label agr subcatlist) = phon  ++ " " 
                                               ++ label ++ show agr

phon :: Cat -> String
phon (Cat ph _ _ _) = ph

catLabel :: Cat -> CatLabel
catLabel (Cat _ label _ _) = label

fs :: Cat -> Agreement 
fs (Cat _ _ agr _) = agr

subcatList :: Cat -> [Cat]
subcatList (Cat _ _ _ cats) = cats

type Words = [String]

assign :: Feat -> Cat -> [Cat]
assign f c@(Cat phon label fs subcatlist) = 
  [Cat phon label fs' subcatlist | 
         fs' <- combine c (Cat "" "" [f] []) ]

combine :: Cat -> Cat -> [Agreement]
combine cat1 cat2 = [ feats | length (tense feats) <= 1 ]
  where 
    feats = (nub . sort) (fs cat1 ++ fs cat2)

scan :: String -> String
scan []                      = []
scan (x:xs) | x `elem` ".,?" = ' ':x:scan xs
            | otherwise      =     x:scan xs

lexer :: String -> Words 
lexer = preproc . words . (map toLower) . scan

preproc :: Words -> Words
preproc []                 = []
preproc ["."]              = []
preproc ["?"]              = []
preproc (",":xs)           = preproc xs
preproc (x:xs)             = x : preproc xs

lookupWord :: (String -> [Cat]) -> String -> [Cat]
lookupWord db w = [ c | c <- db w ]

collectCats :: (String -> [Cat]) -> Words -> [[Cat]]
collectCats db words = 
  let
    listing = map (\ x -> (x,lookupWord db x)) words
    unknown = map fst (filter (null.snd) listing)
  in
    if unknown /= [] then 
      error ("unknown words: " ++ show unknown)
    else initCats (map snd listing) 

initCats :: [[Cat]] -> [[Cat]]
initCats []         = [[]]
initCats (cs:rests) = [ c:rest | c    <- cs, 
                                 rest <- initCats rests ]

t2c :: ParseTree Cat Cat -> Cat
t2c (Leaf   c)   = c
t2c (Branch c _) = c

leafP :: CatLabel -> PARSER Cat Cat
leafP label []     = []
leafP label (c:cs) = [(Leaf c,cs) | catLabel c == label ]

assignT :: Feat ->  ParseTree Cat Cat 
                -> [ParseTree Cat Cat]
assignT f (Leaf   c)    = [Leaf   c'    | c' <- assign f c]
assignT f (Branch c ts) = [Branch c' ts | c' <- assign f c]

sRule :: PARSER Cat Cat
sRule = \ xs -> 
       [ (Branch (Cat "_" "S" fs []) ([np']++vps),zs) | 
         (np,ys) <- parseNP xs,
         (vps,zs) <- (oneOrMore parseVP) ys, 
         np'     <- assignT Nom np,
         fs <- map fs (map t2c vps)]

parseSent :: PARSER Cat Cat
parseSent = sRule 

npRule :: PARSER Cat Cat 
npRule = \ xs -> 
  [ (Branch (Cat "_" "NP" [] []) (cd++mw++cns++ap),us) | 
  	(cd,ys) <- (zeroOrOne parseCD) xs, 
    (mw,zs) <- (zeroOrOne parseMW) ys, 
    (cns,ts)  <- (oneOrMore parseCN) zs,
    (ap, us) <- (many parseAP) ts ]
    

parseNP :: PARSER Cat Cat
parseNP = leafP "NP" <|> npRule

parseCD :: PARSER Cat Cat
parseCD = leafP "CD"

parseMW :: PARSER Cat Cat
parseMW = leafP "MW"

parseCN :: PARSER Cat Cat
parseCN = leafP "CN"

ppRule :: PARSER Cat Cat
ppRule = \ xs -> 
   [ (Branch (Cat "_" "PP" fs []) [prep,np'],zs) | 
     (prep,ys) <- parsePrep xs, 
     (np,zs)   <- parseNP ys,
      np'      <- assignT AccOrDat np, 
      fs       <- combine (t2c prep) (t2c np') ]

parsePP :: PARSER Cat Cat
parsePP = ppRule 

parseAP :: PARSER Cat Cat
parseAP = leafP "ADJ"

parseAux :: PARSER Cat Cat
parseAux = leafP "AUX"

parseAug :: PARSER Cat Cat
parseAug = leafP "AUG"

parsePrep :: PARSER Cat Cat
parsePrep = leafP "PREP"

parseNPorPPorAPorAugorVP :: PARSER Cat Cat
parseNPorPPorAPorAugorVP = parseNP <|> parsePP  <|> parseAP <|> parseAug <|> parseVP

parseNPorPPorAPorAugorVPMany :: [Cat] -> [([ParseTree Cat Cat],[Cat])]
parseNPorPPorAPorAugorVPMany = many parseNPorPPorAPorAugorVP

parseVP :: PARSER Cat Cat 
parseVP = finPastVpRule <|> finPresVpRule <|> finFutVpRule <|> finProgVpRule 

vpRule :: PARSER Cat Cat
vpRule = \xs -> 
 [ (Branch (Cat "_" "VP" fs []) (aux++[vp]++xps),ts) |
   (aux, ys)   <- (many parseAux) xs, 
   (vp,zs)     <- leafP "VP" ys, 
   subcatlist  <- [subcatList (t2c vp)],
   (xps,ts)    <- parseNPorPPorAPorAugorVPMany zs, 
   fs       <- getFeatureFromAux aux,
   match subcatlist (map t2c xps) ]

getFeatureFromAux :: [ParseTree Cat Cat] -> [[Feat]]
getFeatureFromAux aux = case () of _ 
										| length aux == 0 -> [[Pres]] 
										| length (tense (fs (t2c (aux !! 0)))) == 0 -> [[Pres]]  
										| otherwise -> [fs (t2c (aux !! 0))]

match :: [Cat] -> [Cat] -> Bool
match []     []     = True
match _      []     = False
match []      _     = False
match (x:xs) (y:ys) = catLabel x == catLabel y 
	          && match xs ys 

finPastVpRule :: PARSER Cat Cat
finPastVpRule = \xs -> [(vp',ys) | (vp,ys) <- vpRule xs, 
	      	    vp'    <- assignT Past vp ]

finPresVpRule :: PARSER Cat Cat
finPresVpRule = \xs -> [(vp',ys) | (vp,ys) <- vpRule xs, 
	      	    vp'    <- assignT Pres vp ]

finFutVpRule :: PARSER Cat Cat
finFutVpRule = \xs -> [(vp',ys) | (vp,ys) <- vpRule xs, 
	       	   vp'    <- assignT Futu vp ]

finProgVpRule :: PARSER Cat Cat
finProgVpRule = \xs -> [(vp',ys) | (vp,ys) <- vpRule xs, 
	       	   vp'    <- assignT Prog vp ]

prs :: String -> [ParseTree Cat Cat]
prs string = let ws = lexer string 
     in  [ s | catlist <- collectCats lexicon ws, 
            (s,[])  <- parseSent catlist ]

s = "Tấm chỉ có cá bống làm bạn"
catlist = collectCats lexicon (lexer s)
catlist0 = catlist !! 0 
np = [catlist0 !! 0]
vp = drop 1 $ catlist0 
parsed = parseSent (catlist !! 0)
ps = prs s

main = do 
	putStrLn "Test"
	putStrLn $ show (prs "Tấm Cám là hai chị em cùng-cha-khác-mẹ")
	putStrLn $ show (prs "Bố Tấm đã mất")
	putStrLn $ show (prs "Tấm ở với mẹ con Cám")
	putStrLn $ show (prs "Họ suốt-ngày hành-hạ Tấm")
	putStrLn $ show (prs "Tấm rất cô-đơn")

	putStrLn $ show (prs "Tấm chỉ có cá bống làm bạn")
	putStrLn $ show (prs "Cám phát-hiện ra Bống")
	putStrLn $ show (prs "Cám làm-thịt Bống")
	putStrLn $ show (prs "Tấm không tìm thấy Bống")
	putStrLn $ show (prs "Tấm khóc")
	
	putStrLn $ show (prs "Bụt hiện lên")
	putStrLn $ show (prs "Bụt giúp Tấm tìm xương Bống")
	putStrLn $ show (prs "Tấm chôn xương Bống dưới chân giường")
	putStrLn $ show (prs "Vua mở hội")
	putStrLn $ show (prs "Mẹ con Cám đi xem")
	
	putStrLn $ show (prs "Họ không muốn Tấm đi")
	putStrLn $ show (prs "Mẹ-ghẻ trộn thóc với gạo")
	putStrLn $ show (prs "Mẹ sai Tấm tách-riêng chúng ra")
	putStrLn $ show (prs "Tấm không-thể đi dự hội")
	putStrLn $ show (prs "Tấm khóc")
	
	putStrLn $ show (prs "Bụt lại hiện lên")
	putStrLn $ show (prs "Bụt nhờ chim giúp Tấm")
	putStrLn $ show (prs "Tấm không có quần áo đẹp đi dự hội")
	putStrLn $ show (prs "Bụt bảo Tấm đào xương bống lên")
	putStrLn $ show (prs "Xương bống hóa-thành quần áo đẹp")
	
	putStrLn $ show (prs "Tấm mặc quần áo đi dự hội")
	putStrLn $ show (prs "Tấm làm-rơi giày trong lễ hội")
	putStrLn $ show (prs "Vua nhặt được đôi giày")
	putStrLn $ show (prs "Tấm ướm vừa giày")
	putStrLn $ show (prs "Tấm trở-thành hoàng-hậu làm mẹ con Cám tức-giận")
	
	putStrLn $ show (prs "Tấm về giỗ cha")
	putStrLn $ show (prs "Tấm nghe lời mẹ-ghẻ trèo lên hái cau")
	putStrLn $ show (prs "Mẹ-ghẻ chặt cây cau")
	putStrLn $ show (prs "Tấm rơi xuống ao chết-đuối")
	putStrLn $ show (prs "Cám thế Tấm vào cung làm hoàng-hậu")
	
	putStrLn $ show (prs "Tấm hóa-thành chim bay vào cung")
	putStrLn $ show (prs "Vua linh-cảm chim là Tấm")
	putStrLn $ show (prs "Vua quấn-quýt bên chim")
	putStrLn $ show (prs "Cám ghen-tức")
	putStrLn $ show (prs "Cám giết chim")
	
	putStrLn $ show (prs "Chim chết hóa-thành cây thị")
	putStrLn $ show (prs "Vua hái quả thị đem vào phòng")
	putStrLn $ show (prs "Cám về nhà thăm mẹ")
	putStrLn $ show (prs "Tấm chui ra từ quả thị gặp vua")
	putStrLn $ show (prs "Vua lại phong Tấm làm hoàng-hậu")

	putStrLn $ show (prs "Cám trở-về cung")
	putStrLn $ show (prs "Tấm sai người dội nước-sôi giết Cám")
	putStrLn $ show (prs "Tấm đem xác Cám làm nước mắm")
	putStrLn $ show (prs "Tấm tặng mắm cho mẹ-ghẻ")
	putStrLn $ show (prs "Mẹ-ghẻ ăn khen ngon")
	
{-
Tấm Cám là hai chị em cùng-cha-khác-mẹ
Bố Tấm đã mất
Tấm ở với mẹ con Cám
Họ suốt-ngày hành-hạ Tấm
Tấm rất cô-đơn

Tấm chỉ có cá bống làm bạn
Cám phát-hiện ra Bống
Cám làm-thịt Bống
Tấm không tìm thấy Bống
Tấm khóc

Bụt hiện lên
Bụt giúp Tấm tìm xương Bống
Tấm chôn xương Bống dưới chân giường
Vua mở hội
Mẹ con Cám đi xem

Họ không muốn Tấm đi
Mẹ-ghẻ trộn thóc với gạo
Mẹ sai Tấm tách-riêng chúng ra
Tấm không-thể đi dự hội
Tấm khóc

Bụt lại hiện lên
Bụt nhờ chim giúp Tấm
Tấm không có quần áo đẹp đi dự hội
Bụt bảo Tấm đào xương bống lên
Xương bống hóa-thành quần áo đẹp

Tấm mặc quần áo đi dự hội
Tấm làm-rơi giày trong lễ hội
Vua nhặt được đôi giày
Tấm ướm vừa giày
Tấm trở-thành hoàng-hậu làm mẹ con Cám tức-giận

Tấm về giỗ cha
Tấm nghe lời mẹ-ghẻ trèo lên hái cau
Mẹ-ghẻ chặt cây cau
Tấm rơi xuống ao chết-đuối
Cám thế Tấm vào cung làm hoàng-hậu

Tấm hóa-thành chim bay vào cung
Vua linh-cảm chim là Tấm
Vua quấn-quýt bên chim
Cám ghen-tức
Cám giết chim

Chim chết hóa-thành cây thị
Vua hái quả thị đem vào phòng
Cám về nhà thăm mẹ
Tấm chui ra từ quả thị gặp vua
Vua lại phong Tấm làm hoàng-hậu

Cám trở-về cung
Tấm sai người dội nước-sôi giết Cám
Tấm đem xác Cám làm nước mắm
Tấm tặng mắm cho mẹ-ghẻ
Mẹ-ghẻ ăn khen ngon
-}