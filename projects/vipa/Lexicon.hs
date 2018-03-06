module Lexicon where

type CatLabel = String
type Phon     = String

data Cat      = Cat Phon CatLabel Agreement [Cat]
              deriving Eq

type Agreement = [Feat]

data Feat =  Past | Futu  | Prog | Pres | Moda | Nega | Nom | AccOrDat
          deriving (Eq,Show,Ord)

lexicon :: String -> [Cat]


lexicon "chúng"   	= [Cat "chung"   	"NP" [] []]         -- they/them (negative connotation)
lexicon "họ"  		= [Cat "ho"  		"NP" [] []]         -- they/them

lexicon "bống"    	= [Cat "bong"  		"CN" [] []]         -- goby/fish name
lexicon "bụt"    	= [Cat "but"  		"CN" [] []]         -- Buddha
lexicon "cám"    	= [Cat "cam"  		"CN" [] []]         -- person's name
lexicon "tấm"    	= [Cat "tam"  		"CN" [] []]         -- person's name
lexicon "hai"   	= [Cat "hai"   		"CD" [] []]         -- two
lexicon "đôi"     	= [Cat "doi"     	"MW" [] []]         -- measure word, mean double

lexicon "người"		= [Cat "nguoi" 		"CN" [] []]         -- human
lexicon "bố"   		= [Cat "bo"   		"CN" [] []]         -- father
lexicon "cha"   	= [Cat "cha"   		"CN" [] []]         -- father
lexicon "mẹ"   		= [Cat "me"   		"CN" [] []]         -- mother
lexicon "mẹ-ghẻ"	= [Cat "me-ghe"		"CN" [] []]         -- stepmother
lexicon "con"   	= [Cat "con"   		"CN" [] []]         -- son/daughter
lexicon "chị"  		= [Cat "chi"   		"CN" [] []]         -- elder sister
lexicon "em"   		= [Cat "em"   		"CN" [] []]         -- younger sister
lexicon "bạn"		= [Cat "ban"		"CN" [] []]         -- friend
lexicon "vua"  		= [Cat "vua"   		"CN" [] []]         -- king
lexicon "hoàng-hậu" = [Cat "hoang-hau"	"CN" [] []]         -- queen
lexicon "lời"  		= [Cat "loi"   		"CN" [] []]         -- story
lexicon "cá" 		= [Cat "ca"   		"CN" [] []]         -- fish
lexicon "chim" 		= [Cat "chim"  		"CN" [] []]         -- bird
lexicon "xác"  		= [Cat "xac"   		"CN" [] []]         -- dead body
lexicon "xương"		= [Cat "xuong" 		"CN" [] []]         -- bone
lexicon "nước" 		= [Cat "nuoc"  		"CN" [] []]         -- water
lexicon "mắm"  		= [Cat "mam"   		"CN" [] []]         -- fish sauce
lexicon "nước-sôi"	= [Cat "nuoc-soi"	"CN" [] []]         -- boild water
lexicon "gạo"   	= [Cat "gao"   		"CN" [] []]         -- rice
lexicon "thóc" 		= [Cat "thoc"  		"CN" [] []]         -- whole rice
lexicon "áo"   		= [Cat "ao"   		"CN" [] []]         -- shirt
lexicon "quần" 		= [Cat "quan"  		"CN" [] []]         -- pants
lexicon "giày" 		= [Cat "giay"  		"CN" [] []]         -- shoes
lexicon "lễ"   		= [Cat "le"   		"CN" [] []]         -- festival
lexicon "hội"   	= [Cat "hoi"   		"CN" [] []]         -- festival
lexicon "giỗ"  		= [Cat "gio"   		"CN" [] []]         -- veneration of ancestors' death
lexicon "cây"  		= [Cat "cay"   		"CN" [] []]         -- tree
lexicon "quả"  		= [Cat "qua"   		"CN" [] []]         -- fruit
lexicon "cau"  		= [Cat "cau"   		"CN" [] []]         -- a kind of fruit
lexicon "thị"  		= [Cat "thi"   		"CN" [] []]         -- a kind of fruit
lexicon "chân" 		= [Cat "chan"  		"CN" [] []]         -- legs
lexicon "giường"	= [Cat "giuong"		"CN" [] []]         -- bed
lexicon "phòng"		= [Cat "phong" 		"CN" [] []]         -- room
lexicon "nhà"  		= [Cat "nha"   		"CN" [] []]         -- house
lexicon "cung" 		= [Cat "cung"  		"CN" [] []]         -- harem
lexicon "ao"   		= [Cat "ao"   		"CN" [] []]         -- pond

lexicon "cùng-cha-khác-mẹ"	= [Cat "cung-cha-khac-me"		"ADJ" [] []]         -- paternal half-sibling
lexicon "đẹp"		= [Cat "dep" 		"ADJ" [] []]         -- beautiful
lexicon "ngon"  	= [Cat "ngon"  		"ADJ" [] []]         -- delicious (food)

lexicon "đã"	    = [Cat "da"		    "AUX" [Past] []]         -- 
lexicon "đang"	    = [Cat "dang"	    "AUX" [Prog] []]         -- 
lexicon "sẽ"	    = [Cat "se"		    "AUX" [Futu] []]         -- 
lexicon "không-thể" = [Cat "khong-the"  "AUX" [Moda] []]         -- can't 
lexicon "không"	    = [Cat "khong"	    "AUX" [Nega] []]         -- don't/not
lexicon "chỉ"	    = [Cat "chi"	    "AUX" [Pres] []]         -- only
lexicon "lại"	    = [Cat "lai"	    "AUX" [Pres] []]         -- again
lexicon "rất"	    = [Cat "rat"	    "AUX" [Pres] []]         -- very
lexicon "suốt-ngày" = [Cat "suot-ngay"  "AUX" [Pres] []]         -- everyday

lexicon "trong"		= [Cat "trong" 		"PREP" [] []]         -- in
lexicon "cho"   	= [Cat "cho"   		"PREP" [] []]         -- for
lexicon "từ" 		= [Cat "tu" 		"PREP" [] []]         -- from
lexicon "dưới"	 	= [Cat "duoi"   	"PREP" [] []]         -- under
lexicon "với" 		= [Cat "voi" 		"PREP" [] []]         -- with
lexicon "bên" 		= [Cat "ben" 		"PREP" [] []]         -- with

lexicon "lên" 		= [Cat "len" 		"AUG" [] []]         -- up
lexicon "ra" 		= [Cat "ra" 		"AUG" [] []]         -- out
lexicon "thấy" 		= [Cat "thay" 		"AUG" [] []]         -- found
lexicon "được" 		= [Cat "duoc" 		"AUG" [] []]         -- beneficial
lexicon "vừa" 		= [Cat "vua" 		"AUG" [] []]         -- fit
lexicon "xuống"		= [Cat "xuong" 		"AUG" [] []]         -- down
lexicon "vào" 		= [Cat "vao" 		"AUG" [] []]         -- in

-- INTRANS (8)
lexicon "ăn"		= [Cat "an"			"VP" [] []]         -- eat
lexicon "chết"		= [Cat "chet"		"VP" [] []]         -- die
lexicon "chết-đuối"	= [Cat "chet-duoi"	"VP" [] []]         -- drown
lexicon "cô-đơn"	= [Cat "co-don"		"VP" [] []]         -- alone
lexicon "đi"		= [Cat "di"			"VP" [] []]         -- go
lexicon "ghen-tức"	= [Cat "ghen-tuc"	"VP" [] []]         -- envy
lexicon "khóc"		= [Cat "khoc"		"VP" [] []]         -- cry
lexicon "mất"		= [Cat "mat"		"VP" [] []]         -- lose
lexicon "tức-giận"	= [Cat "tuc-gian"	"VP" [] []]         -- angry

-- NP (16)
lexicon "là"		= [Cat "la"			"VP" [] [Cat "_" "NP" [] []]]         -- is
lexicon "hành-hạ"	= [Cat "hanh-ha"	"VP" [] [Cat "_" "NP" [] []]]         -- torture
lexicon "làm-thịt"	= [Cat "lam-thit"	"VP" [] [Cat "_" "NP" [] []]]         -- butcher
lexicon "mở"		= [Cat "mo"			"VP" [] [Cat "_" "NP" [] []]]         -- open
lexicon "dự"		= [Cat "du"			"VP" [] [Cat "_" "NP" [] []]]         -- attend
lexicon "trở-thành"	= [Cat "tro-thanh"	"VP" [] [Cat "_" "NP" [] []]]         -- become
lexicon "về"		= [Cat "ve"			"VP" [] [Cat "_" "NP" [] []]]         -- back
lexicon "nghe"		= [Cat "nghe"		"VP" [] [Cat "_" "NP" [] []]]         -- listen
lexicon "hái"		= [Cat "hai"		"VP" [] [Cat "_" "NP" [] []]]         -- gather
lexicon "chặt"		= [Cat "chat"		"VP" [] [Cat "_" "NP" [] []]]         -- cut
lexicon "giết"		= [Cat "giet"		"VP" [] [Cat "_" "NP" [] []]]         -- kill
lexicon "thăm"		= [Cat "tham"		"VP" [] [Cat "_" "NP" [] []]]         -- visit
lexicon "gặp"		= [Cat "gap"		"VP" [] [Cat "_" "NP" [] []]]         -- meet
lexicon "trở-về"	= [Cat "tro-ve"		"VP" [] [Cat "_" "NP" [] []]]         -- back
lexicon "dội"		= [Cat "doi"		"VP" [] [Cat "_" "NP" [] []]]         -- pour
lexicon "mặc"		= [Cat "mac"		"VP" [] [Cat "_" "NP" [] []]]         -- wear

-- AP (1)
lexicon "khen"		= [Cat "khen"		"VP" [] [Cat "_" "ADJ" [] []]]         -- praise

-- PP (1)
lexicon "ở"			= [Cat "o"			"VP" [] [Cat "_" "PP" [] []]]         -- stay
lexicon "quấn-quýt"	= [Cat "quan-quyt"	"VP" [] [Cat "_" "PP" [] []]]         -- become close (with)

-- INTRANS / PP (1)
lexicon "xem"		= [Cat "xem"		"VP" [] [],
					   Cat "xem"		"VP" [] [Cat "_" "PP" [] []]]         -- watch

-- AUG (2)
lexicon "hiện"		= [Cat "hien"		"VP" [] [Cat "_" "AUG" [] []]]         -- appear
lexicon "trèo"		= [Cat "treo"		"VP" [] [Cat "_" "AUG" [] []]]         -- climb

-- NP VP (6)
lexicon "muốn"		= [Cat "muon"		"VP" [] [Cat "_" "NP" [] [], Cat "_" "VP" [] []]]         -- want
lexicon "sai"		= [Cat "sai"		"VP" [] [Cat "_" "NP" [] [], Cat "_" "VP" [] []]]         -- order
lexicon "nhờ"		= [Cat "nho"		"VP" [] [Cat "_" "NP" [] [], Cat "_" "VP" [] []]]         -- ask
lexicon "bảo"		= [Cat "bao"		"VP" [] [Cat "_" "NP" [] [], Cat "_" "VP" [] []]]         -- tell
lexicon "linh-cảm"	= [Cat "linh-cam"	"VP" [] [Cat "_" "NP" [] [], Cat "_" "VP" [] []]]         -- presage
lexicon "phong"		= [Cat "phong"		"VP" [] [Cat "_" "NP" [] [], Cat "_" "VP" [] []]]         -- bestow

-- NP / (NP VP) (4)
lexicon "có"		= [Cat "co"			"VP" [] [Cat "_" "NP" [] []],
					   Cat "co"			"VP" [] [Cat "_" "NP" [] [], Cat "_" "VP" [] []]]         -- have
lexicon "hóa-thành"	= [Cat "hoa-thanh"	"VP" [] [Cat "_" "NP" [] []],
					   Cat "hoa-thanh"	"VP" [] [Cat "_" "NP" [] [], Cat "_" "VP" [] []]]         -- transform
lexicon "làm"		= [Cat "lam"		"VP" [] [Cat "_" "NP" [] []],
					   Cat "lam"		"VP" [] [Cat "_" "NP" [] [], Cat "_" "VP" [] []]]         -- work/do
lexicon "giúp"		= [Cat "giup"		"VP" [] [Cat "_" "NP" [] []],
					   Cat "giup"		"VP" [] [Cat "_" "NP" [] [], Cat "_" "VP" [] []]]         -- help

-- NP PP (4)
lexicon "chôn"		= [Cat "chon"		"VP" [] [Cat "_" "NP" [] [], Cat "_" "PP" [] []]]         -- burry
lexicon "trộn"		= [Cat "tron"		"VP" [] [Cat "_" "NP" [] [], Cat "_" "PP" [] []]]         -- mix
lexicon "làm-rơi"	= [Cat "lam-roi"	"VP" [] [Cat "_" "NP" [] [], Cat "_" "PP" [] []]]         -- drop
lexicon "tặng"		= [Cat "tang"		"VP" [] [Cat "_" "NP" [] [], Cat "_" "PP" [] []]]         -- give as a gift

-- NP AUG (2)
lexicon "tách-riêng"= [Cat "tach-rieng"	"VP" [] [Cat "_" "NP" [] [], Cat "_" "AUG" [] []]]         -- split
lexicon "đào"		= [Cat "dao"		"VP" [] [Cat "_" "NP" [] [], Cat "_" "AUG" [] []]]         -- dig

-- AUG NP (5)
lexicon "phát-hiện"	= [Cat "phat-hien"	"VP" [] [Cat "_" "AUG" [] [], Cat "_" "NP" [] []]]         -- discover
lexicon "nhặt"		= [Cat "nhat"		"VP" [] [Cat "_" "AUG" [] [], Cat "_" "NP" [] []]]         -- pick up
lexicon "ướm"		= [Cat "uom"		"VP" [] [Cat "_" "AUG" [] [], Cat "_" "NP" [] []]]         -- try (shoes)
lexicon "rơi"		= [Cat "roi"		"VP" [] [Cat "_" "AUG" [] [], Cat "_" "NP" [] []]]         -- drop
lexicon "bay"		= [Cat "bay"		"VP" [] [Cat "_" "AUG" [] [], Cat "_" "NP" [] []]]         -- fly

-- NP / (AUG NP) (2)
lexicon "đem"		= [Cat "dem"		"VP" [] [Cat "_" "NP" [] []],
					   Cat "dem"		"VP" [] [Cat "_" "AUG" [] [], Cat "_" "NP" [] []]]         -- bring
lexicon "tìm"		= [Cat "tim"		"VP" [] [Cat "_" "NP" [] []],
					   Cat "tim"		"VP" [] [Cat "_" "AUG" [] [], Cat "_" "NP" [] []]]         -- search for
-- AUG PP (1)
lexicon "chui"		= [Cat "chui"		"VP" [] [Cat "_" "AUG" [] [], Cat "_" "PP" [] []]]         -- crouch

-- NP AUG NP (1)
lexicon "thế"		= [Cat "the"		"VP" [] [Cat "_" "NP" [] [], Cat "_" "AUG" [] [], Cat "_" "NP" [] []]]          -- replace

lexicon _ = []

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